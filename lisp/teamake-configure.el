;;; teamake-configure --- CMake configuration for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-process)
(require 'teamake-preset)

(defun teamake-configure--list-generators ()
  "List all generators supported by CMake binary."
  (let* ((output (teamake-cmake-command-to-string "--help"))
         (generators-part (substring output (string-match "Generators" output)))
         (generator-name-expression "[* ]+\\([ -a-zA-Z0-9=]+?\\)[ \n]?+=")
         (generators '()))
    (save-match-data
      (let ((pos 0))
        (while (string-match generator-name-expression generators-part pos)
          (push (match-string 1 generators-part) generators)
          (setq pos (match-end 0))))
      (setq generators (reverse generators)))))

(transient-define-suffix teamake-configure--configure-current ()
  :description "Configure current"
  (interactive)
  (let* ((project (transient-scope))
         (current-args (transient-args 'teamake-configure))
         (source-dir (plist-get project :source-dir))
         (expanded-args (seq-map
                         (lambda (value)
                           (teamake-configure--expand-macro-in-current-value value project))
                         current-args)))
    (teamake-set-current-values 'teamake-configure project current-args)
    (apply #'teamake-process-invoke-cmake
           project
           "-S"
           source-dir
           "-B"
           (plist-get project :binary-dir)
           expanded-args)))

(transient-define-suffix teamake-configure--configure-preset ()
  :description "Configure preset"
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-configuration project)))
    (apply #'teamake-process-invoke-cmake
           project
           "-S"
           (plist-get project :source-dir)
           (format "--preset=%s" (plist-get preset :name)))))

(transient-define-suffix teamake-configure--save-current ()
  "Save the current configure as current for later use."
  (interactive)
  (let ((project (transient-scope))
        (values (transient-args 'teamake-configure)))
    (teamake-set-current-values 'teamake-configure project values)
    (teamake-save-project project)))

(transient-define-suffix teamake-configure--save-current-as ()
  "Save the current configure as current for later use."
  (interactive)
  (let* ((project (transient-scope))
         (existing-names (teamake-get-save-names 'teamake-configure project))
         (name (completing-read "Configuration name (match will overwrite): " existing-names)))
    (teamake-set-save-values 'teamake-configure project name (transient-args 'teamake-configure))))

(transient-define-suffix teamake-configure--load ()
  "Load a previously saved configuration."
  (interactive)
  (let* ((project (transient-scope))
         (existing-names (teamake-get-save-names 'teamake-configure project))
         (name-to-load (completing-read "Configuration: " existing-names '() t))
         (values (teamake-get-save-value 'teamake-configure project name-to-load)))
    (teamake-set-current-values 'teamake-configure project values)
    (teamake-setup-transient 'teamake-configure project)))

(defun teamake-configure--cache-variables-as-switches (cache-variables-plist)
  "Create statements like <key>=<val> from CACHE-VARIABLES-PLIST."
  (let* ((values '())
         (counter 0)
         (key (nth counter cache-variables-plist))
         (value (plist-get cache-variables-plist key)))
    (while key
      (add-to-list 'values (format "%s=%s" (substring (format "%s" key) 1) value))
      (setq counter (+ counter 2)
            key (nth counter cache-variables-plist)
            value (plist-get cache-variables-plist key)))
    values))

(defun teamake-configure--preset-to-values (preset)
  "Parse all values from PRESET into values for `teamake-configure'."
  (let ((values '()))
    (let ((warnings (plist-get preset :warnings))
          (errors (plist-get preset :errors))
          (debug (plist-get preset :debug))
          (trace (plist-get preset :trace)))
      (if warnings
          (progn
            (if (plist-member warnings :dev)
                (add-to-list 'values (if (eq (plist-get warnings :dev) :json-false)
                                         "-Wno-dev"
                                       "-Wdev")))
            (if (plist-member warnings :deprecated)
                (add-to-list 'values (if (eq (plist-get warnings :deprecated)
                                             :json-false)
                                         "-Wno-deprecated"
                                       "-Wdeprecated")))
            (if (and (plist-member warnings :uninitialized)
                     (not (eq (plist-get warnings :uninitialized) :json-false)))
                (add-to-list 'values "--warn-uninitialized"))
            (if (and (plist-member warnings :unusedCli)
                     (eq (plist-get warnings :unusedCli) t))
                (add-to-list 'values "--no-warn-unused-cli"))
            (if (and (plist-member warnings :systemVars)
                     (not (eq (plist-get warnings :systemVars) :json-false)))
                (add-to-list 'values "--check-system-vars"))
            )
        )
      (if errors
          (progn
            (if (plist-member errors :dev)
                (add-to-list 'values (if (eq (plist-get errors :dev) :json-false)
                                         "Wno-error=dev"
                                       "-Werror=dev")))
            (if (plist-member errors :deprecated)
                (add-to-list 'values (if (eq (plist-get errors :deprecated) :json-false)
                                         "Wno-error=deprecated"
                                       "-Werror=deprecated")))
            )
        )
      (if debug
          (progn
            (if (plist-get debug :output) (add-to-list 'values "--debug-output"))
            (if (plist-get debug :tryCompile) (add-to-list 'values "--debug-trycompile"))
            (if (plist-get debug :find) (add-to-list 'values "--debug-find"))
            )
        )
      (if trace
          (progn
            (if (plist-member trace :mode)
                (cond ((string= (plist-get trace :mode) "on")
                       (add-to-list 'values "--trace"))
                      ((string= (plist-get trace :mode) "off")
                       ;; nop
                       )
                      ((string= (plist-get trace :mode) "expand")
                       (add-to-list 'values "--trace-expand"))))
            (if (plist-member trace :format)
                (cond ((string= (plist-get trace :format) "human")
                       (add-to-list 'values "--trace-format=human"))
                      ((string= (plist-get trace :format) "json-v1")
                       (add-to-list 'values "--trace-format=json-v1"))))
            (if (plist-member trace :source)
                (seq-do
                 (lambda (file)
                   (add-to-list 'values (format "--trace-source=%s" file)))
                 (teamake-preset--get-property-as-list trace :source)))
            (if (plist-member trace :redirect)
                (add-to-list 'values
                             (format "--trace-redirect=%s"
                                     (plist-get trace :redirect)))
              )
            )
        )
      (if (plist-member preset :cacheVariables)
          (seq-do
           (lambda (val)
             (add-to-list 'values (format "-D%s" val)))
           (teamake-configure--cache-variables-as-switches
            (plist-get preset :cacheVariables)))
        )

      (if (plist-member preset :generator)
          (add-to-list 'values (format "-G=%s" (plist-get preset :generator))))
      (if (plist-member preset :toolchainFile)
          (add-to-list 'values (format "--toolchain=%s" (plist-get preset :toolchainFile))))
      (if (plist-member preset :graphviz)
          (add-to-list 'values (format "--graphviz=%s" (plist-get preset :graphviz))))
      (if (plist-member preset :binaryDir)
          (add-to-list 'values (format "-B=%s" (plist-get preset :binaryDir))))
      (if (plist-member preset :installDir)
          (add-to-list 'values (format "--install-prefix=%s" (plist-get preset :installDir))))
      (if (plist-member preset :architecture)
          (add-to-list 'values (format "-A=%s" (plist-get preset :architecture))))
      (if (plist-member preset :toolset)
          (add-to-list 'values (format "-T=%s" (plist-get preset :toolset))))
      )
    values))

(defun teamake-configure--expand-macro-in-current-value (value project)
  "Replace any macro expressions in VALUE for PROJECT.

Use current configure preset as base for preset specific expansions."
  (teamake-expand-expression
   value
   (plist-get project :source-dir)
   (lambda (text source-dir)
     (teamake-preset--expand-macro
      text (teamake-preset--get-current project :configurePresets) source-dir))))

(transient-define-suffix teamake-configure--select-preset ()
  :description "Read from preset"
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-configuration project))
         (values (teamake-configure--preset-to-values preset)))
    (if (plist-member preset :binaryDir)
        (plist-put project :binary-dir
                   (teamake-configure--expand-macro-in-current-value
                    (plist-get preset :binaryDir) project)))
    (teamake-set-current-values 'teamake-configure project values)
    (teamake-setup-transient 'teamake-configure project)))

(defun teamake-configure--possible (project)
  "Determine if PROJECT contain enough information for `teamake-configure'."
  (teamake-project-has-valid-source-dir-p project))

(transient-define-suffix teamake-configure--setup (project)
  "Setup `teamake-configure' from PROJECT."
  (interactive)
  (unless (teamake-configure--possible project)
    (user-error "Project not correctly configured with source dir"))
  (teamake-setup-transient 'teamake-configure project))

(transient-define-suffix teamake-configure--binary-dir ()
  :transient 'transient--do-replace
  :description
  (lambda ()
    (let ((binary-dir (plist-get (transient-scope) :binary-dir))
          (option "-B="))
      (format " Binary dir (%s)"
              (if binary-dir
                  (propertize (format "%s%s" option binary-dir) 'face 'transient-value)
                option))))
  (interactive)
  (let* ((project (transient-scope))
         (binary-dir (read-directory-name "Binary dir: " (plist-get project :binary-dir) '() '())))
    (plist-put project :binary-dir binary-dir)
    (teamake-setup-transient 'teamake-configure project)))

;;;###autoload
(transient-define-prefix teamake-configure (project)
  [:description
   (lambda ()
     ;; (teamake-project-heading "CMake Configure" (transient-scope))
     (format "%s %s\n\n%s\n"
             (propertize "CMake Configure" 'face 'teamake-heading)
             (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)
             (propertize (format "cmake -S %s\n      -B %s \n      <options>"
                                 (plist-get (transient-scope) :source-dir)
                                 (plist-get (transient-scope) :binary-dir))))
     )
   ["Options"
    ("b" teamake-configure--binary-dir)
    (5 "C" "Pre-load a script to populate the cache" "-C"
     :class transient-option
     :prompt "Select script for cache varmup: "
     :reader transient-read-file)
    ("D" " Create or update a cmake cache entry." "-D"
     :class transient-option
     :prompt "List entries as <var>[:<type>]=<value> and comma separate them: "
     :multi-value repeat)
    ("ge" "Generator" "-G="
     :prompt "Generator: "
     :choices (lambda () (teamake-configure--list-generators)))
    ("ts" "Specify toolset name if supported by generator" "-T="
     :prompt "Toolset: ")
    ("a" " Specify platform name if supported by generator" "-A="
     :prompt "Platform: ")
    ("tc" "Toolchain file" "--toolchain="
     :prompt "Toolchain: "
     :reader transient-read-file)
    ("i" " Installation path" "--install-prefix="
     :prompt "Install path: "
     :reader transient-read-directory)
    ("pr" teamake-configure--select-preset)
    ("gr" "Generate graphviz of dependencies"
     "--graphviz="
     :prompt "Graphviz output: "
     :reader transient-read-file)
    ]
   ]
  [
   ["Warnings"
    ("-ww" "Enable developer warnings" "-Wdev")
    ("-wW" "Suppress developer warnings" "-Wno-dev")
    ("-wd" "Enable deprecation warnings" "-Wdeprecated")
    ("-wD" "Suppress deprecation warnings" "-Wno-deprecated")
    ("-we" "Make developer warnings errors" "-Werror=dev")
    ("-wE" "Make developer warnings not errors" "-Wno-error=dev")
    ("-wm" "Make deprecated macro and function warnings errors"  "-Werror=deprecated")
    ("-wM" "Make deprecated macro and function warnings not errors" "-Wno-error=deprecated")
    ]
   [5 "Debug"
    ("-cli" "Don't warn about command line options"
     "--no-warn-unused-cli")
    ("-csv" "Find problems with variable usage in system files"
     "--check-system-vars")
    ("-cne" "Compile no warnings as error"
     "--compile-no-warning-as-error")
    ("-lc" " Prepend log messages with context, if given"
     "--log-context")
    ("-dt" " Do not delete the try_compile build tree"
     "--debug-trycompile")
    ("-do" " Put cmake in a debug mode"
     "--debug-output")
    ("-df" " Put cmake find in a debug mode"
     "--debug-find")
    ("dfp" " Limit cmake debug-find to the comma-separated list of packages"
     "--debug-find-pkg="
     :prompt "Packages (comma separated): ")
    ("dfv" " Limit cmake debug-find to the comma-separated list of result variables"
     "--debug-find-var="
     :prompt "Variables (comma separated): ")
    ("dsi" " Dump information about this system"
     "--system-information="
     :prompt "Select system dump file: "
     :reader transient-read-file)
    ]
   ]
  [
   [6 "Trace"
    ("-trm" "Put cmake in trace mode" "--trace")
    ("-tre" "Put cmake in trace mode with variable expansion"
     "--trace-expand")
    ("trf-format" " Set the output format of the trace"
     "--trace-format="
     :prompt "Format: "
     :choices ("human" "json-v1"))
    ("trs" "Trace only this CMake file/module"
     "--trace-source="
     :prompt "CMake file/module: "
     :multi-value repeat
     :reader transient-read-file)
    ("trr" " Redirect trace output to a file instead of stderr"
     "--trace-redirect="
     :prompt "Trace output: "
     :reader transient-read-file)
    ]
   [6 "Profiling"
    ("pf" "Output format for profiling CMake scripts"
     "--profiling-format="
     :prompt "Select format: "
     :choices ("google-trace"))
    ("po" "Select an output path for the profiling data"
     "--profiling-output="
     :prompt "Select profiling output: "
     :reader transient-read-file)
    ]
   ]
  ["Misc"
   ("-f" "Configure a fresh build tree, removing any existing cache file"
    "--fresh")
   ("-n" "View mode only" "-N")
   ("-u" "Warn about uninitialized values"
    "--warn-uninitialized")
   ("l" " Set the verbosity of message from CMake files."
    "--log-level="
    :prompt "Select log level: "
    :choices ("ERROR" "WARNING" "NOTICE" "STATUS" "VERBOSE" "DEBUG" "TRACE"))
   ]
  [
   ["Do"
    ("xx" "Execute current" teamake-configure--configure-current)
    ("xp" teamake-configure--configure-preset)
    ]
   ["Navigate"
    ("C" teamake-project--teamake-cmake-navigate)
    ("P" teamake-cmake--teamake-project)
    ]
   ]
  ["Manage"
   [:description
    (lambda ()
      (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name))
    ("xc" "Save"    teamake-transient-save-current-values :transient t)
    ("xa" "Save as" teamake-transient-save-current-as :transient t)
    ("xl" "Load"    teamake-transient-load)
    ("xd" "Delete"  teamake-transient-delete :transient t)
    ]
   ["Templates"
    ("gc" "Save"    teamake-transient-save-current-values :transient t)
    ("ga" "Save as" teamake-transient-save-current-as :transient t)
    ("gl" "Load"    teamake-transient-load)
    ("gd" "Delete"  teamake-transient-delete :transient t)
    ]
   ]
  (interactive
   (list (teamake-project-from-source-dir
          (or (teamake--find-root default-directory "CMakeLists.txt")
              default-directory))))
  (teamake-setup-transient 'teamake-configure project))


(provide 'teamake-configure)
;;; teamake-configure.el ends here
