
;;; Code:

(require 'teamake-base)
(require 'teamake-process)

(defvar teamake-build-path ""
  "The current build tree.")

(defvar teamake-build-target ""
  "The current build target.")

(defvar teamake-build-parallel '()
  "The amount of parallel compilations.")

(defvar teamake-build-config ""
  "The configuration, only valid for multi-configuration generators.")

(defun teamake-build-is-valid-build-tree (path)
  "Determine if PATH is a vanlid build tree."
  (file-exists-p (file-name-concat path "TeamakeCache.txt")))

(defun teamake-build-set-build-path (path)
  "Set `teamake-build-path' to PATH."
  (interactive
   (list (read-directory-name "Build tree: " teamake-build-path '() t)))

  (unless (teamake-build-is-valid-build-tree path)
    (user-error "Selected path does not seem to be a configured build tree"))

  (setq teamake-build-path path))

(defun teamake-build--read-build-targets ()
  "Read build targets from `teamake-build-path' using target 'help'.

Assuming the generator can provide available targets using the 'help'
target in the build tree."
  (let ((teamake-command (format "--build \"%s\" --target help" teamake-build-path)))
    (teamake-process-invoke teamake-command)))

(defun teamake-build--filter-targets (targets)
  "Filter through all TARGETS and only show lines with valid targets."
  (seq-filter
   (lambda (line)
     (re-seq (re-seq "\\(.\\): .+" line)))
   targets))

(defun teamake-build-set-build-target (target)
  "Set `teamake-build-target' to TARGET."
  (interactive
   (let ((targets (split-string (teamake-build--read-build-targets) "\n")))
     (list (completing-read "Target: " targets '()))))
  (setq teamake-build-target target))

(defun teamake-build-set-parallel (amount)
  "Set `teamake-build-parallel' to AMOUNT."
  (interactive "nParallel compilations: ")
  (unless (number-or-marker-p amount)
    (user-error "Expected a number, was given \"%s\"" amount))
  (setq teamake-build-parallel amount))

(defun teamake-build-set-config (config)
  "Set `teamake-build-config' to CONFIG."
  (interactive "sConfig: ")
  (set teamake-build-config config))

(defun teamake-build-execute-build (build-root)
  "Invoke compilation using the current configuration."
  (interactive (list (teamake-project-build-root default-directory)))
  (message "Args: %s" (transient-args transient-current-command))
  (teamake-process-invoke-teamake-in-build-root
   build-root
   "--build"
   build-root
   "--target"
   "help"))

(defun teamake-build--describe-build-path ()
  "Describe the build path."
  (format "Build(%s)"
          (propertize (teamake-return-value-or-default teamake-build-path "<Unset>")
                      'face 'transient-value)))

(defun teamake-build--describe-build-target ()
  "Describe the build path."
  (format "Target (%s)"
          (propertize (teamake-return-value-or-default teamake-build-target "<all>")
                      'face 'transient-value)))

(defun teamake-build--describe-parallel ()
  "Describe the build path."
  (if (eq teamake-build-parallel '())
      (format "Parallel (%s)" (propertize "<$env{TEAMAKE_BUILD_PARALLEL_LEVEL}>" 'face 'transient-value))
    (format "Parallel (%s)" (propertize (number-to-string teamake-build-parallel) 'face 'transient-value))))

(defun teamake-build--describe-config ()
  "Describe the build path."
  (format "Config (%s)"
          (propertize (teamake-return-value-or-default teamake-build-config "<Unset>")
                      'face 'transient-value)))

(defun teamake-build--describe-execute-build ()
  "Describe execution."
  (format "Invoke Teamake with the current configuration"))

(transient-define-prefix teamake-build (build-path)
  "Invoke a build command on an already existing configuration."
  [:description
   (lambda ()
     (concat (teamake-project--build-tree-heading "Build" (transient-scope))
             "\n\n"
             (propertize "Flags and switches" 'face 'teamake-project-heading)))
   ("-r" "Read available build targets" "-r")
   ("-c" "Build clean before actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")
   ]
  ["Teamake build"
   ("b" teamake-build-set-build-path :transient t
    :description teamake-build--describe-build-path)
   ("c" teamake-build-set-config :transient t
    :description teamake-build--describe-config)
   ("t" teamake-build-set-build-target :transient t
    :description teamake-build--describe-build-target)
   ("p" teamake-build-set-parallel :transient t
    :description teamake-build--describe-parallel)
   ]
  ["Execute"
   ("x" teamake-build-execute-build
    :description teamake-build--describe-execute-build)
   ]
  (interactive (list (teamake-project-build-root default-directory)))
  (transient-setup 'teamake-build '() '() :scope build-path)
  )

(setq debug-on-error '())

(provide 'teamake-build.el)
;;; teamake-build.el ends here
