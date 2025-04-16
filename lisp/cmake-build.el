
;;; Code:

(require 'cmake-base)
(require 'cmake-process)

(defvar cmake-build-path ""
  "The current build tree.")

(defvar cmake-build-target ""
  "The current build target.")

(defvar cmake-build-parallel ""
  "The amount of parallel compilations.")

(defvar cmake-build-config ""
  "The configuration, only valid for multi-configuration generators.")

(defun cmake-build-is-valid-build-tree (path)
  "Determine if PATH is a vanlid build tree."
  (file-exists-p (file-name-concat path "CMakeCache.txt")))

(defun cmake-build-set-build-path (path)
  "Set `cmake-build-path' to PATH."
  (interactive
   (list (read-directory-name "Build tree: " cmake-build-path '() t)))

  (unless (cmake-build-is-valid-build-tree path)
    (user-error "Selected path does not seem to be a configured build tree"))

  (setq cmake-build-path path))

(defun cmake-build--read-build-targets ()
  "Read build targets from `cmake-build-path' using target 'help'.

Assuming the generator can provide available targets using the 'help'
target in the build tree."
  (let ((cmake-command (format "--build \"%s\" --target help" cmake-build-path)))
    (cmake-process-invoke cmake-command)))

(defun cmake-build--filter-targets (targets)
  "Filter through all TARGETS and only show lines with valid targets."
  (seq-filter
   (lambda (line)
     (re-seq (re-seq "\\(.\\): .+" line)))
   targets))

(defun cmake-build-set-build-target (target)
  "Set `cmake-build-target' to TARGET."
  (interactive
   (let ((targets (split-string (cmake-build--read-build-targets) "\n")))
     (list (completing-read "Target: " targets '()))))
  (setq cmake-build-target target))

(defun cmake-build-set-parallel (amount)
  "Set `cmake-build-parallel' to AMOUNT."
  (interactive "nParallel compilations: ")
  (unless (number-or-marker-p amount)
    (user-error "Expected a number, was given \"%s\"" amount))
  (setq cmake-build-parallel amount))

(defun cmake-build-set-config (config)
  "Set `cmake-build-config' to CONFIG."
  (interactive "sConfig: ")
  (set cmake-build-config config))

(defun cmake-build-execute-build ()
  "Invoke compilation using the current configuration."
  (interactive)
  (message "Args: %s" (transient-args transient-current-command))
  (error "Not yet implemented!"))

(defun cmake-build--describe-build-path ()
  "Describe the build path."
  (format "Build(%s)"
          (propertize (cmake-return-value-or-default cmake-build-path "<Unset>")
                      'face 'transient-value)))

(defun cmake-build--describe-build-target ()
  "Describe the build path."
  (format "Target (%s)"
          (propertize (cmake-return-value-or-default cmake-build-target "<all>")
                      'face 'transient-value)))

(defun cmake-build--describe-parallel ()
  "Describe the build path."
  (if (or (string= cmake-build-parallel "") (eq cmake-build-parallel '()))
      (format "Parallel (%s)" (propertize "<$env{CMAKE_BUILD_PARALLEL_LEVEL}>" 'face 'transient-value))
    (format "Parallel (%s)" (propertize cmake-build-parallel 'face 'transient-value))))

(defun cmake-build--describe-config ()
  "Describe the build path."
  (format "Config (%s)"
          (propertize (cmake-return-value-or-default cmake-build-config "<Unset>")
                      'face 'transient-value)))

(defun cmake-build--describe-execute-build ()
  "Describe execution."
  (format "Invoke CMake with the current configuration"))

(transient-define-prefix cmake-build ()
  "Invoke a build command on an already existing configuration."
  ["Flags and switches"
   ;; ("-r" "Read available build targets" "-r")
   ("-c" "Build clean before actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")
   ("-a" "test infix" "--choice=" :choices (foo bar baz))
   ("-A" "switch with shortarg" ("-w" "--switch-short"))
   ]
  ["CMake build"
   ("b" cmake-build-set-build-path :transient t
    :description cmake-build--describe-build-path)
   ("c" cmake-build-set-config :transient t
    :description cmake-build--describe-config)
   ("t" cmake-build-set-build-target :transient t
    :description cmake-build--describe-build-target)
   ("p" cmake-build-set-parallel :transient t
    :description cmake-build--describe-parallel)
   ]
  ["Execute"
   ("x" cmake-build-execute-build
    :description cmake-build--describe-execute-build)
   ])

(setq debug-on-error '())

(defun force-debug (func &rest args)
  (condition-case e
      (apply func args)
    ((debug error) (signal (car e) (cdr e)))))

(advice-add #'vertico--exhibit :around #'force-debug)


(provide 'cmake-build.el)
;;; cmake-build.el ends here
