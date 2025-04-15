
;;; Code:

(require 'transient)
(require 'cmake-base)
(require 'cmake-presets)

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

(defvar cmake-configure-preset ""
  "Current selected preset.")

(defun cmake-configure-set-source-path (path)
  "Set `cmake-configure-source-path' to PATH."
  (interactive
   (let ((path (read-directory-name "Select source path: " cmake-configure-source-path '() t)))
     (list path)))

  (if (not (file-exists-p (file-name-concat path "CMakeLists.txt")))
      (user-error "Selected path does not contain a CMakeLists.txt file and cannot be used as source path"))

  (setq cmake-configure-source-path (directory-file-name path))
  (message "Source path updated to %s" cmake-configure-source-path))

(defun cmake-configure-set-build-path (path)
  "Set `cmake-configure-build-path' to PATH."
  (interactive
   (let ((path (read-directory-name "Select build path: " cmake-configure-source-path '() t)))
     (list path)))

  (setq cmake-configure-build-path path)
  (message "Build path updated to %s" path))

(defun cmake-configure-set-generator (generator)
  "Set `cmake-configure-generator' to GENERATOR."
  (interactive
   (let ((generator (read-string "Generator: " cmake-configure-generator)))
     (list generator)))

  (setq cmake-configure-generator generator)
  (message "Generator updated to %s" generator))

(defun cmake-configure-set-toolset (toolset)
  "Set `cmake-configure-toolset' to TOOLSET."
  (interactive
   (let ((toolset (read-string "Toolset: " cmake-configure-toolset)))
     (list toolset)))

  (setq cmake-configure-toolset toolset)
  (message "Toolset updated to %s" toolset))

(defun cmake-configure-set-platform (platform)
  "Set `cmake-configure-platform' to PLATFORM."
  (interactive
   (let ((platform (read-string "Platform: " cmake-configure-toolset)))
     (list platform)))

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

(defun cmake-configure-set-preset (preset-name)
  "Set `cmake-configure-preset' to PRESET-NAME."
  (interactive
   (list (completing-read "Preset: " (cmake-presets-get-configuration-presets cmake-configure-source-path) '() t cmake-configure-preset)))

  (setq cmake-configure-preset preset-name)
  (message "Preset updated to %s" cmake-configure-preset))

(defun cmake-configure--describe-source-path ()
  "Return `cmake-configure-source-path' as protertized transient-variable."
  (format "Source path (%s)"
          (propertize cmake-configure-source-path 'face 'transient-value)))

(defun cmake-configure--describe-build-path ()
  "Return `cmake-configure-build-path' as protertized transient-variable."
  (format "Build path (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-build-path "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-generator ()
  "Return `cmake-configure-generator' as protertized transient-variable."
  (format "Generator (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-generator "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-toolset ()
  "Return `cmake-configure-toolset' as protertized transient-variable."
  (format "Toolset (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-toolset "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-platform ()
  "Return `cmake-configure-platform' as protertized transient-variable."
  (format "Platform (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-platform "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-toolchain-file ()
  "Return `cmake-configure-toolchain-file' as protertized transient-variable."
  (format "Toolchain (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-toolchain-file "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-install-prefix ()
  "Return `cmake-configure-install-prefix' as protertized transient-variable."
  (format "Install (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-install-prefix "<Unset>")
                      'face 'transient-value)))

(defun cmake-configure--describe-preset ()
  "Return `cmake-configure-preset' as protertized transient-variable."
  (format "Preset (%s)"
          (propertize (cmake-return-value-or-default cmake-configure-preset "<Unset>")
                      'face 'transient-value)))

;; (defvar cmake-project-cmake-prefix-command "\"C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvarsall.bat\" x64"
;;   "Command to prefix the call to cmake with.
;; For instance \"<path>vcvarsall.bat x64\" or \"module load cmake\".")

;; (defvar cmake-preferred-shell "c:/Program Files/Emacs/emacs-29.3_2/libexec/emacs/29.3/x86_64-w64-mingw32/cmdproxy.exe"
;;   "Preferred shell to execute commands in.")

(transient-define-prefix cmake-configure ()
  "Invoke a CMake configuration step."
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
  ;; ["Presets\n"
  ;;  ("-P" cmake-configure--set-preset
  ;;   :description cmake-configure--describe-preset)]
  ["Manual configuration\n"
   ("-S" cmake-configure-set-source-path :transient t
    :description cmake-configure--describe-source-path)
   ("-B" cmake-configure-set-build-path :transient t
    :description cmake-configure--describe-build-path)
   ;; ("-D" cmake-configure--set-cache-entris :transient t
   ;;  :description cmake-configure--describe-cache-entries)
   ;; ("-U" cmake-configure--set-remove-cache-entries :transient t
   ;;  :description cmake-configure--describe-remove-cache-entries)
   ("-G" cmake-configure-set-generator :transient t
    :description cmake-configure--describe-generator)
   ("-T" cmake-configure-set-toolset :transient t
    :description cmake-configure--describe-toolset)
   ("-A" cmake-configure-set-platform :transient t
    :description cmake-configure--describe-platform)
   ("-t" cmake-configure-set-toolchain-file :transient t
    :description cmake-configure--describe-toolchain-file)
   ("-i" cmake-configure-set-install-prefix :transient t
    :description cmake-configure--describe-install-prefix)
   ]
  ["Presets\n"
   ("-P" cmake-configure-set-preset :transient t
    :description cmake-configure--describe-preset)]
  )

(provide 'cmake-configure.el)
;;; cmake-configure.el ends here
