
;;; Code:

(require 'cmake-base)

(defvar cmake-build-path ""
  "The current build tree.")

(defvar cmake-build-target ""
  "The current build target.")

(defun cmake-build-is-valid-build-tree (path)
  "Determine if PATH is a vanlid build tree."
  (file-exists-p (file-name-concat path "CMakeCache.txt")))

(defun cmake-build-set-build-path (path)
  "Set `cmake-build-path' to PATH."
  (interactive
   (list (read-directory-name "Build tree: " cmake-build-path '() t)))

  (unless (cmake-build-is-valid-build-tree path)
    (user-error "Selected path does not seem to be a configured build tree"))

  (setq cmake-build-path path)
  (message "Build path updated to %s" cmake-build-path))

(defun cmake-build-set-build-target (target)
  "Set `cmake-build-target' to TARGET."
  (interactive
   (list (read-string "Target: " cmake-build-target)))

  (setq cmake-build-target target)
  (message "Build target updated to %s" cmake-build-target))

(defun cmake-build--describe-build-path ()
  "Describe the build path."
  (format "Build path (%s)"
          (propertize (cmake-return-value-or-default cmake-build-path "<Unset>")
                      'face 'transient-value)))

(defun cmake-build--describe-build-target ()
  "Describe the build path."
  (format "Target (%s)"
          (propertize (cmake-return-value-or-default cmake-build-target "<All>")
                      'face 'transient-value)))

(transient-define-prefix cmake-build ()
  "Invoke a build command on an already existing configuration."
  ["Flags and switches"
   ("-r" "Read available build targets" "-r")]
  ["CMake build"
   ("-B" cmake-build-set-build-path :transient t
    :description cmake-build--describe-build-path)
   ("-T" cmake-build-set-build-target :transient t
    :description cmake-build--describe-build-target)
   ])

(provide 'cmake-build.el)
;;; cmake-build.el ends here
