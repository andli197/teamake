
;;; Code:

(require 'transient)
(require 'teamake-base)
(require 'teamake-cache)
(require 'teamake-cmake-help)
(require 'teamake-preset)

(defun teamake-configure--list-generators ()
  "List all generators supported by CMake binary."
  (let* ((output (teamake-cmake-shell-command-to-string '() "--help"))
         (generators-part (substring output (string-match "Generators" output)))
         (generator-name-expression "[* ]+\\([ -a-zA-Z0-9=]+?\\)[ \n]?+=")
         (generators '()))
    (save-match-data
      (let ((pos 0))
        (while (string-match generator-name-expression generators-part pos)
          (push (match-string 1 generators-part) generators)
          (setq pos (match-end 0))))
      (setq generators (reverse generators)))))

(defun teamake-configure--do-configure ()
  "Execute the currently configured Teamake command."
  (interactive)
  (let ((command (transient-args transient-current-command))
        (source-path (teamake-source-dir))
        (build-path (teamake-build-dir)))

    ;; (apply #'teamake-expand-known-macros
    ;;        source-path
    ;;        command)

    (apply #'teamake-process-invoke-cmake
           default-directory
           "-S"
           source-path
           command)))

(defun teamake-configure--cache-variables-as-switches (cache-variables-plist)
  "Create statements like <key>=<val> from CACHE-VARIABLES-PLIST."
  (let* ((values '())
         (counter 0)
         (key (nth counter cache-variables-plist))
         (value (plist-get cache-variables-plist key)))
    (while key
      (add-to-list 'values (format "%s=%s" (teamake--string-from-key key) value))
      (setq counter (+ counter 2)
            key (nth counter cache-variables-plist)
            value (plist-get cache-variables-plist key)))
    values))

(defun teamake-configure--preset-to-values (preset)
  "Parse all values from PRESET to CMake flags to use as values."
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

(defun teamake-configure--select-preset ()
  (interactive)
  (call-interactively 'teamake-preset-select-configuration-preset (teamake-source-dir))
  (transient-setup 'teamake-configure '() '()
                   :value (teamake-configure--preset-to-values
                           teamake-preset--selected-configuration)))

(defun teamake-configure--describe ()
  "Create a description of the current configuration."
  (concat "CMake Configuration "
          (propertize (teamake-project-name) 'face 'teamake-name)
          " ("
          (propertize (teamake-source-dir) 'face 'teamake-path)
          ")\n"))

(transient-define-prefix teamake-configure ()
  "Invoke a Teamake configuration step."
  :value '("-Wdev" "-Wno-error=dev" "-Wdeprecated" "-Wno-error=deprecated"
           "-B=${buildDir}" "--install-prefix=${installDir}")
  [:description
   (lambda () (teamake-configure--describe))
   ["Warnings"
    ("-ww" "Enable developer warnings"
     "-Wdev")
    ("-wW" "Suppress developer warnings"
     "-Wno-dev")
    ("-we" "Make developer warnings errors"
     "-Werror=dev")
    ("-wE" "Make developer warnings not errors"
     "-Wno-error=dev")
    ("-wd" "Enable deprecation warnings"
     "-Wdeprecated")
    ("-wD" "Suppress deprecation warnings"
     "-Wno-deprecated")
    ("-wm" "Make deprecated macro and function warnings errors"
     "-Werror=deprecated")
    ("-wM" "Make deprecated macro and function warnings not errors"
     "-Wno-error=deprecated")
    ]
   ["Debug"
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
    ("-df" " Put cmake in a debug mode"
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
    ]]
  [["Trace"
    ("-tr" "Put cmake in trace mode" "--trace")
    ("-te" "Put cmake in trace mode with variable expansion"
     "--trace-expand")
    ("tf" " Set the output format of the trace"
     "--trace-format="
     :prompt "Format: "
     :choices ("human" "json-v1"))
    ("tts" "Trace only this CMake file/module"
     "--trace-source="
     :prompt "CMake file/module: "
     :multi-value repeat
     :reader transient-read-file)
    ("tr" " Redirect trace output to a file instead of stderr"
     "--trace-redirect="
     :prompt "Trace output: "
     :reader transient-read-file)
    ]
   ["Misc"
    ("-f" "Configure a fresh build tree, removing any existing cache file"
     "--fresh")
    ("-n" "View mode only" "-N")
    ("-u" "Warn about uninitialized values"
     "--warn-uninitialized")
    ]]
  ["Options"
   ("b" " Build path" "-B="
    :prompt "Build path: "
    :reader transient-read-directory)
   ;; -D <var>[:<type>]=<value>    = Create or update a cmake cache entry.
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
   ("po" "Output format for profiling CMake scripts"
    "--profiling-format="
    :prompt "Select format: "
    :choices ("google-trace"))
   ("pf" "Select an output path for the profiling data"
    "--profiling-output="
    :prompt "Select profiling output: "
    :reader transient-read-file)
   ("pr" teamake-configure--select-preset
    :description "Read configuration from preset")
   ("gr" "Generate graphviz of dependencies"
    "--graphviz="
    :prompt "Graphviz output: "
    :reader transient-read-file)
   ("l" " Set the verbosity of message from CMake files."
    "--log-level="
    :prompt "Select log level: "
    :choices ("ERROR" "WARNING" "NOTICE" "STATUS" "VERBOSE" "DEBUG" "TRACE"))
   ]
  ["Do"
   ("xx" "Execute configuration" teamake-configure--do-configure)
   ("xp" "Execute preset" teamake-preset-select-and-execute-configuration-preset)]
  ["Help"
   ("h" "CMake help menu" teamake-cmake-help)]
  (interactive)
  (transient-setup 'teamake-configure '() '()))

(provide 'teamake-configure)
;;; teamake-configure.el ends here
