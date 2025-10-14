
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

(defun teamake-configure-execute ()
  "Execute the currently configured Teamake command."
  (interactive)
  (let ((command (transient-args transient-current-command))
        (source-path (transient-scope transient-current-command)))
    (setq command (seq-map (lambda (arg) (string-replace "=" " " arg)) command))
    (apply #'teamake-process-invoke-cmake
           default-directory
           "-S"
           source-path
           command)))

(defun teamake-configure--build-menu ()
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (build-tree (transient-arg-value "-B=" args)))
    (unless build-tree
      (user-error "No build tree specified"))
    (message "build-tree=%s" build-tree)
    ;; (transient-setup 'teamake-build '() '() :scope build-tree)
    (teamake-build build-tree)
    ))

(defun teamake-configure--select-preset ()
  (interactive)
  (let ((code-path (transient-scope))
        (values '()))
    (call-interactively 'teamake-preset-select-configuration-preset code-path)
    (let ((warnings (plist-get teamake-preset--selected-configuration :warnings))
          (errors (plist-get teamake-preset--selected-configuration :errors))
          (debug (plist-get teamake-preset--selected-configuration :debug))
          (trace (plist-get teamake-preset--selected-configuration :trace)))
      (if warnings
          (progn
            (if (plist-member warnings :dev)
                (add-to-list 'values (if (eq (plist-get warnings :dev) :json-false)
                                         "-Wno-dev"
                                       "-Wdev")))
            (if (plist-member warnings :deprecated)
                (add-to-list 'values (if (eq (plist-get warnings :deprecated) :json-false)
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
            (if (plist-member trace :mode) (cond ((string= (plist-get trace :mode) "on")
                                                  (add-to-list 'values "--trace"))
                                                 ((string= (plist-get trace :mode) "off") ;; nop
                                                  )
                                                 ((string= (plist-get trace :mode) "expand")
                                                  (add-to-list 'values "--trace-expand"))))
            (if (plist-member trace :format) (cond ((string= (plist-get trace :format) "human")
                                                    (add-to-list 'values "--trace-format=human"))
                                                   ((string= (plist-get trace :format) "json-v1")
                                                    (add-to-list 'values "--trace-format=json-v1"))))
            (if (plist-member trace :source) (seq-do
                                              (lambda (file)
                                                (add-to-list 'values (format "--trace-source=%s" file)))
                                              (teamake-preset--get-property-as-list trace :source)))
            (if (plist-member trace :redirect) (add-to-list 'values (format "--trace-redirect=%s" (plist-get trace :redirect))))
            )
        )
      (if (plist-member teamake-preset--selected-configuration :generator)
          (add-to-list 'values (format "-G=%s" (plist-get teamake-preset--selected-configuration :generator))))
      (if (plist-member teamake-preset--selected-configuration :toolchainFile)
          (add-to-list 'values (format "--toolchain=%s" (plist-get teamake-preset--selected-configuration :toolchainFile))))
      (if (plist-member teamake-preset--selected-configuration :graphviz)
          (add-to-list 'values (format "--graphviz=%s" (plist-get teamake-preset--selected-configuration :graphviz))))
      (if (plist-member teamake-preset--selected-configuration :binaryDir)
          (add-to-list 'values (format "-B=%s" (plist-get teamake-preset--selected-configuration :binaryDir))))
      (if (plist-member teamake-preset--selected-configuration :installDir)
          (add-to-list 'values (format "--install-prefix=%s" (plist-get teamake-preset--selected-configuration :installDir))))
      (if (plist-member teamake-preset--selected-configuration :architecture)
          (add-to-list 'values (format "-A=%s" (plist-get teamake-preset--selected-configuration :architecture))))
      (if (plist-member teamake-preset--selected-configuration :toolset)
          (add-to-list 'values (format "-T=%s" (plist-get teamake-preset--selected-configuration :toolset))))
      )
    (message "%s" values)
    (transient-setup
     'teamake-configure '() '()
     :scope code-path
     :value values)
    ))

(transient-define-prefix teamake-configure (code-path)
  "Invoke a Teamake configuration step."
  :value '("-Wdev" "-Wno-error=dev" "-Wdeprecated" "-Wno-error=deprecated")
  [:description (lambda ()
                  (concat (teamake-heading "Configure " (transient-scope) 'teamake-code-tree-p)
                          "\n\n"
                          (teamake-heading "Warnings" '() 'teamake-code-tree-p)))
   ("-ww" "Enable developer warnings" "-Wdev")
   ("-wW" "Suppress developer warnings" "-Wno-dev")
   ("-we" "Make developer warnings errors" "-Werror=dev")
   ("-wE" "Make developer warnings not errors" "-Wno-error=dev")
   ("-wd" "Enable deprecation warnings" "-Wdeprecated")
   ("-wD" "Suppress deprecation warnings" "-Wno-deprecated")
   ("-wm" "Make deprecated macro and function warnings errors" "-Werror=deprecated")
   ("-wM" "Make deprecated macro and function warnings not errors" "-Wno-error=deprecated")
   ]
  ["Misc"
   ("-f" "Configure a fresh build tree, removing any existing cache file" "--fresh")
   ("-n" "View mode only" "-N")
   ("-u" "Warn about uninitialized values" "--warn-uninitialized")
   ]
  ["Debug"
   ("-cli" "Don't warn about command line options" "--no-warn-unused-cli")
   ("-csv" "Find problems with variable usage in system files" "--check-system-vars")
   ("-cne" "Compile no warnings as error" "--compile-no-warning-as-error")
   ("-lc" " Prepend log messages with context, if given" "--log-context")
   ("-dt" " Do not delete the try_compile build tree" "--debug-trycompile")
   ("-do" " Put cmake in a debug mode" "--debug-output")
   ("-df" " Put cmake in a debug mode" "--debug-find")
   ("dfp" "Limit cmake debug-find to the comma-separated list of packages"
    "--debug-find-pkg="
    :prompt "Packages (comma separated): ")
   ("dfv" "Limit cmake debug-find to the comma-separated list of result variables"
    "--debug-find-var="
    :prompt "Variables (comma separated): ")
   ("dsi" "Dump information about this system" "--system-information="
    :prompt "Select system dump file: "
    :reader transient-read-file)
   ]
  ["Trace"
   ("-tr" "Put cmake in trace mode" "--trace")
   ("-te" "Put cmake in trace mode with variable expansion" "--trace-expand")
   ("tf" "Set the output format of the trace" "--trace-format="
    :prompt "Format: "
    :choices ("human" "json-v1"))
   ("tts" "Trace only this CMake file/module" "--trace-source="
    :prompt "CMake file/module: "
    :multi-value repeat)
   ("tr" "Redirect trace output to a file instead of stderr" "--trace-redirect="
    :prompt "Trace output: "
    :reader transient-read-file)
   ]
  ["Options"
   ("b" " Build path" "-B="
    :prompt "Build path: "
    :reader transient-read-directory)
   ;; -D <var>[:<type>]=<value>    = Create or update a cmake cache entry.
   ("D" " Create or update a cmake cache entry." "-D "
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
   ("po" "Output format for profiling CMake scripts" "--profiling-format="
    :prompt "Select format: "
    :choices ("google-trace"))
   ("pf" "Select an output path for the profiling data" "--profiling-output="
    :prompt "Select profiling output: "
    :reader transient-read-file)
   ("pr" teamake-configure--select-preset
    :description "Read configuration from preset")
   ("gr" "Generate graphviz of dependencies" "--graphviz="
    :prompt "Graphviz output: "
    :reader transient-read-file)
   ("l" " Set the verbosity of message from CMake files." "--log-level="
    :prompt "Select log level: "
    :choices ("ERROR" "WARNING" "NOTICE" "STATUS" "VERBOSE" "DEBUG" "TRACE"))
   ]
  ["Do"
   ("C" teamake-configure-execute
    :description "Run configuration")]
  ["Help"
   ("h" "CMake help menu" teamake-cmake-help)]
  (interactive (list (teamake-code-root default-directory)))
  (transient-setup 'teamake-configure '() '() :scope code-path))

(provide 'teamake-configure)
;;; teamake-configure.el ends here
