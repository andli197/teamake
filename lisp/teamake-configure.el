
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


;; -C <initial-cache>           = Pre-load a script to populate the cache.
;; -D <var>[:<type>]=<value>    = Create or update a cmake cache entry.
;; -U <globbing_expr>           = Remove matching entries from CMake cache.

;; --graphviz=<file>            = Generate graphviz of dependencies, see
;;                                CMakeGraphVizOptions.cmake for more.
;; --log-level=<ERROR|WARNING|NOTICE|STATUS|VERBOSE|DEBUG|TRACE>
;;                              = Set the verbosity of messages from CMake
;;                                files.  --loglevel is also accepted for
;;                                backward compatibility reasons.
;; --log-context                = Prepend log messages with context, if given
;; --debug-trycompile           = Do not delete the try_compile build tree.
;;                                Only useful on one try_compile at a time.
;; --debug-output               = Put cmake in a debug mode.
;; --debug-find                 = Put cmake find in a debug mode.
;; --debug-find-pkg=<pkg-name>[,...]
;;                              = Limit cmake debug-find to the
;;                                comma-separated list of packages
;; --debug-find-var=<var-name>[,...]
;;                              = Limit cmake debug-find to the
;;                                comma-separated list of result variables
;; --trace                      = Put cmake in trace mode.
;; --trace-expand               = Put cmake in trace mode with variable
;;                                expansion.
;; --trace-format=<human|json-v1>
;;                              = Set the output format of the trace.
;; --trace-source=<file>        = Trace only this CMake file/module.  Multiple
;;                                options allowed.
;; --trace-redirect=<file>      = Redirect trace output to a file instead of
;;                                stderr.
;; --warn-uninitialized         = Warn about uninitialized values.
;; --no-warn-unused-cli         = Don't warn about command line options.
;; --check-system-vars          = Find problems with variable usage in system
;;                                files.
;; --compile-no-warning-as-error= Ignore COMPILE_WARNING_AS_ERROR property and
;;                                CMAKE_COMPILE_WARNING_AS_ERROR variable.
;; --profiling-format=<fmt>     = Output data for profiling CMake scripts.
;;                                Supported formats: google-trace
;; --profiling-output=<file>    = Select an output path for the profiling data
;;                                enabled through --profiling-format.

(defvar teamake-configure--flag--fresh '() "Variable representing --fresh flag to CMake")
(transient-define-infix teamake-configure-fresh ()
  :class 'transient-lisp-variable
  :variable 'teamake-configure--flag--fresh
  :description "Create a fresh build tree, remove any pre-existing cache file (--fresh)"
  :shortarg "--fresh")

(transient-define-prefix teamake-configure (code-path)
  "Invoke a Teamake configuration step."
  :value '("-Wdev" "-Wno-error=dev" "-Wdeprecated" "-Wno-error=deprecated")
  [:if (lambda () (teamake-code-tree-p (transient-scope)))
   :description
   (lambda ()
     (concat (teamake-heading "Configure " (transient-scope) 'teamake-code-tree-p)
             "\n\n"
             (propertize "Flags and switches" 'face 'teamake-heading)))
    ("f" teamake-configure-fresh)
    ("-f" "Create a fresh build tree, remove any pre-existing cache file" "--fresh")
    ("-w" "Enable developer warnings" "-Wdev")
    ("-W" "Suppress developer warnings" "-Wno-dev")
    ("-e" "Make developer warnings errors." "-Werror=dev")
    ("-E" "Make developer warnings not errors." "-Wno-error=dev")
    ("-d" "Enable deprecation warnings" "-Wdeprecated")
    ("-D" "Suppress deprecation warnings" "-Wno-deprecated")
    ("-c" "Make deprecated macro and function warnings errors" "-Werror=deprecated")
    ("-C" "Make deprecated macro and function warnings not errors" "-Wno-error=deprecated")
    ("--debug" "Put CMake in a debug mode." "--debug-output")
    ]
  ["Settings"
   ("b" "Build path" "-B="
    :prompt "Build path"
    :reader transient-read-directory)
   ("i" "Installation path" "--install-prefix="
    :prompt "Install"
    :reader transient-read-directory)
   ("g" "Generator" "-G="
    :prompt "Generator"
    :choices (lambda () (teamake-configure--list-generators)))
   ("p" "Preset" "--preset="
    :prompt "Select preset"
    :choices (lambda () (teamake-presets-get-configuration-presets
                         (transient-arg-value "-S=" (transient-args transient-current-command)))))
   ("T" "Toolset" "-T=" :prompt "Toolset")
   ("a" "Platform" "-A=" :prompt "Platform")
   ("t" "Toolchain file" "--toolchain="
     :prompt "Toolchain"
     :reader transient-read-file)
   ("c" "Pre-load a script to populate the cache" "-C="
    :prompt "Select cache varmup script "
    :reader transient-read-file)
   ("d" "Create or update a CMake cache entry." "-D="
    :prompt "Entries (<var>[:<type>]=<value>): ")
;; -U <globbing_expr>           = Remove matching entries from CMake cache.

   ("x" teamake-configure-execute
    :description "Configure using current configuration")
   ("X" teamake-configure--build-menu
    :description "Open build menu for configured build tree")
   ]
  ["Help"
    ("h" "CMake help menu" teamake-cmake-help)]
  (interactive (list (teamake-code-root default-directory)))
  (transient-setup 'teamake-configure '() '() :scope code-path)
  )
  

(provide 'teamake-configure)
;;; teamake-configure.el ends here
