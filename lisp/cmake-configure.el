
;;; Code:

(require 'transient)
(require 'cmake-base)

(defvar cmake-configure-source-path ""
  "The current source path.")

(defvar cmake-configure-build-path ""
  "The current build path.")

(defvar cmake-configure-update-or-create-cache-entries '()
  "Cache entries to create or update.")

(defvar cmake-configure-remove-cache-entries ""
  "Cache entries to remove.")

(defvar cmake-configure-fresh-build-tree '()
  "Configure a fresh build tree, removing any pre-existing cashe file.")

(defvar cmake-configure-generator ""
  "Generator to use.")

(defvar cmake-configure-toolset ""
  "Toolset to use.")

(defvar cmake-configure-platform ""
  "Platform to use.")

(defvar cmake-configure-toolchain-file ""
  "Toolchain file to use.")

(defvar cmake-configure-install-prefix ""
  "Where to install.")

;; (transient-define-argument cmake-configure--configuration ()
;;   :class 'transient-switches
;;   :argument-format "-DCMAKE_BUILD_TYPE=%s"
;;   ;; :argument-regexp "\\(DCMAKE_BUILD_TYPE\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
;;   :choices '("Debug" "Release" "RelWithDebInfo"))

(defun cmake-configure-set-source-path (path)
  "Set `cmake-configure-source-path' to PATH."
  (interactive
   (list (call-interactively 'cmake-project-root)))

  (if (not (file-exists-p (file-name-concat path "CMakeLists.txt")))
      (user-error "Selected path does not contain a CMakeLists.txt file and cannot be used as source path"))

  (setq cmake-configure-source-path (directory-file-name path))
  (message "Source path updated to %s" cmake-configure-source-path))

(defun cmake-configure-set-build-path (path)
  "Set `cmake-configure-build-path' to PATH."
  (interactive
   (list (read-directory-name "Select build path: "
                              (or cmake-configure-source-path default-directory) '() t)))

  (setq cmake-configure-build-path path)
  (message "Build path updated to %s" path))

(defun cmake-configure-set-generator (generator)
  "Set `cmake-configure-generator' to GENERATOR."
  (interactive
   (list (read-string "Generator: " cmake-configure-generator)))

  (setq cmake-configure-generator generator)
  (message "Generator updated to %s" generator))

(defun cmake-configure-set-toolset (toolset)
  "Set `cmake-configure-toolset' to TOOLSET."
  (interactive
   (list (read-string "Toolset: " cmake-configure-toolset)))

  (setq cmake-configure-toolset toolset)
  (message "Toolset updated to %s" toolset))

(defun cmake-configure-set-platform (platform)
  "Set `cmake-configure-platform' to PLATFORM."
  (interactive
   (list (read-string "Platform: " cmake-configure-platform)))

  (setq cmake-configure-platform platform)
  (message "Platform updated to %s" platform))

(defun cmake-configure-set-toolchain-file (toolchain-file)
  "Set `cmake-configure-toolchain-file' to TOOLCHAIN-FILE."
  (interactive
   (let ((toolchain-file (read-file-name "Toolchain file: " cmake-configure-source-path "toolchain.cmake" t)))
     (list toolchain-file)))

  (setq cmake-configure-toolchain-file toolchain-file)
  (message "Toolchain updated to %s" toolchain-file))

(defun cmake-configure-set-install-prefix (install-path)
  "Set `cmake-configure-install-prefix' to INSTALL-PATH."
  (interactive
   (list (read-directory-name "Install path: " cmake-configure-install-prefix)))

  (setq cmake-configure-install-prefix install-path)
  (message "Install prefix updated to %s" cmake-configure-install-prefix))

(defun cmake-configure--describe-source-path ()
  "Return `cmake-configure-source-path' as protertized transient-variable."
  (format "Source path (%s)"
          (propertize (format "-S=%s" cmake-configure-source-path)
                      'face 'transient-value)))

(defun cmake-configure--describe-build-path ()
  "Return `cmake-configure-build-path' as protertized transient-variable."
  (format "Build path (%s)"
          (propertize (format "-B=%s" cmake-configure-build-path)
                      'face 'transient-value)))

(defun cmake-configure--describe-generator ()
  "Return `cmake-configure-generator' as protertized transient-variable."
  (format "Generator (%s)"
          (propertize (format "-G=%s" cmake-configure-generator)
                      'face 'transient-value)))

(defun cmake-configure--describe-toolset ()
  "Return `cmake-configure-toolset' as protertized transient-variable."
  (format "Toolset (%s)"
          (propertize (format "-T=%s" cmake-configure-toolset)
                      'face 'transient-value)))

(defun cmake-configure--describe-platform ()
  "Return `cmake-configure-platform' as protertized transient-variable."
  (format "Platform (%s)"
          (propertize (format "-A=%s" cmake-configure-platform)
                      'face 'transient-value)))

(defun cmake-configure--describe-toolchain-file ()
  "Return `cmake-configure-toolchain-file' as protertized transient-variable."
  (format "Toolchain (%s)"
          (propertize (format "--toolchain-file=%s" cmake-configure-toolchain-file)
                      'face 'transient-value)))

(defun cmake-configure--describe-install-prefix ()
  "Return `cmake-configure-install-prefix' as protertized transient-variable."
  (format "Install (%s)"
          (propertize (format "-DCMAKE_INSTALL_PREFIX=%s" (cmake-return-value-or-default cmake-configure-install-prefix "<Unset>"))
                      'face 'transient-value)))

(defun cmake-configure-execute ()
  "Execute the currently configured CMake command."
  (interactive)
  (message "args: %s" (transient-args transient-current-command)))


(transient-define-prefix cmake-configure (code-path)
  "Invoke a CMake configuration step."
  [:description
   (lambda ()
     (cmake-project-heading "Configure " (transient-scope)))
   ("b" cmake-configure-set-build-path :transient t
    :description cmake-configure--describe-build-path)
   ;; ("-D" cmake-configure--set-cache-entris :transient t
   ;;  :description cmake-configure--describe-cache-entries)
   ;; ("-U" cmake-configure--set-remove-cache-entries :transient t
   ;;  :description cmake-configure--describe-remove-cache-entries)
   ("g" cmake-configure-set-generator :transient t
    :description cmake-configure--describe-generator)
   ("T" cmake-configure-set-toolset :transient t
    :description cmake-configure--describe-toolset)
   ("a" cmake-configure-set-platform :transient t
    :description cmake-configure--describe-platform)
   ("t" cmake-configure-set-toolchain-file :transient t
    :description cmake-configure--describe-toolchain-file)
   ("i" cmake-configure-set-install-prefix :transient t
    :description cmake-configure--describe-install-prefix)
   ("x" cmake-configure-execute
    :description "Execute the current configuration")
   ]
  ["Flags\n"
   ("-f" "Create a fresh build tree, remove any pre-existing cache file" "--fresh")
   ("-w" "Enable developer warnings" "-Wdev")
   ("-W" "Suppress developer warnings" "-Wno-dev")
   ("-e" "Make developer warnings errors." "-Werror=dev")
   ("-E" "Make developer warnings not errors." "-Wno-error=dev")
   ("-d" "Enable deprecation warnings" "-Wdeprecated")
   ("-D" "Suppress deprecation warnings" "-Wno-deprecated")
   ("-c" "Make deprecated macro and function warnings errors" "-Werror=deprecated")
   ("-C" "Make deprecated macro and function warnings not errors" "-Wno-error=deprecated")
   ]

  (interactive (list (cmake-project-root default-directory)))
  (transient-setup 'cmake-configure '() '() :scope code-path)
  )

(provide 'cmake-configure)
;;; cmake-configure.el ends here
