
;;; Code:

(require 'transient)
(require 'teamake-base)

(defvar teamake-configure-source-path ""
  "The current source path.")

(defvar teamake-configure-build-path ""
  "The current build path.")

(defvar teamake-configure-update-or-create-cache-entries '()
  "Cache entries to create or update.")

(defvar teamake-configure-remove-cache-entries ""
  "Cache entries to remove.")

(defvar teamake-configure-fresh-build-tree '()
  "Configure a fresh build tree, removing any pre-existing cashe file.")

(defvar teamake-configure-generator ""
  "Generator to use.")

(defvar teamake-configure-toolset ""
  "Toolset to use.")

(defvar teamake-configure-platform ""
  "Platform to use.")

(defvar teamake-configure-toolchain-file ""
  "Toolchain file to use.")

(defvar teamake-configure-install-prefix ""
  "Where to install.")

;; (transient-define-argument teamake-configure--configuration ()
;;   :class 'transient-switches
;;   :argument-format "-DTEAMAKE_BUILD_TYPE=%s"
;;   ;; :argument-regexp "\\(DTEAMAKE_BUILD_TYPE\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
;;   :choices '("Debug" "Release" "RelWithDebInfo"))

(defun teamake-configure-set-source-path (path)
  "Set `teamake-configure-source-path' to PATH."
  (interactive
   (list (call-interactively 'teamake-project-code-root)))

  (if (not (file-exists-p (file-name-concat path "TeamakeLists.txt")))
      (user-error "Selected path does not contain a TeamakeLists.txt file and cannot be used as source path"))

  (setq teamake-configure-source-path (directory-file-name path))
  (message "Source path updated to %s" teamake-configure-source-path))

(defun teamake-configure-set-build-path (path)
  "Set `teamake-configure-build-path' to PATH."
  (interactive
   (list (read-directory-name "Select build path: "
                              (or teamake-configure-source-path default-directory) '() t)))

  (setq teamake-configure-build-path path)
  (message "Build path updated to %s" path))

(defun teamake-configure-set-generator (generator)
  "Set `teamake-configure-generator' to GENERATOR."
  (interactive
   (list (read-string "Generator: " teamake-configure-generator)))

  (setq teamake-configure-generator generator)
  (message "Generator updated to %s" generator))

(defun teamake-configure-set-toolset (toolset)
  "Set `teamake-configure-toolset' to TOOLSET."
  (interactive
   (list (read-string "Toolset: " teamake-configure-toolset)))

  (setq teamake-configure-toolset toolset)
  (message "Toolset updated to %s" toolset))

(defun teamake-configure-set-platform (platform)
  "Set `teamake-configure-platform' to PLATFORM."
  (interactive
   (list (read-string "Platform: " teamake-configure-platform)))

  (setq teamake-configure-platform platform)
  (message "Platform updated to %s" platform))

(defun teamake-configure-set-toolchain-file (toolchain-file)
  "Set `teamake-configure-toolchain-file' to TOOLCHAIN-FILE."
  (interactive
   (let ((toolchain-file (read-file-name "Toolchain file: " teamake-configure-source-path "toolchain.teamake" t)))
     (list toolchain-file)))

  (setq teamake-configure-toolchain-file toolchain-file)
  (message "Toolchain updated to %s" toolchain-file))

(defun teamake-configure-set-install-prefix (install-path)
  "Set `teamake-configure-install-prefix' to INSTALL-PATH."
  (interactive
   (list (read-directory-name "Install path: " teamake-configure-install-prefix)))

  (setq teamake-configure-install-prefix install-path)
  (message "Install prefix updated to %s" teamake-configure-install-prefix))

(defun teamake-configure--describe-source-path ()
  "Return `teamake-configure-source-path' as protertized transient-variable."
  (format "Source path (%s)"
          (propertize (format "-S=%s" teamake-configure-source-path)
                      'face 'transient-value)))

(defun teamake-configure--describe-build-path ()
  "Return `teamake-configure-build-path' as protertized transient-variable."
  (format "Build path (%s)"
          (propertize (format "-B=%s" teamake-configure-build-path)
                      'face 'transient-value)))

(defun teamake-configure--describe-generator ()
  "Return `teamake-configure-generator' as protertized transient-variable."
  (format "Generator (%s)"
          (propertize (format "-G=%s" teamake-configure-generator)
                      'face 'transient-value)))

(defun teamake-configure--describe-toolset ()
  "Return `teamake-configure-toolset' as protertized transient-variable."
  (format "Toolset (%s)"
          (propertize (format "-T=%s" teamake-configure-toolset)
                      'face 'transient-value)))

(defun teamake-configure--describe-platform ()
  "Return `teamake-configure-platform' as protertized transient-variable."
  (format "Platform (%s)"
          (propertize (format "-A=%s" teamake-configure-platform)
                      'face 'transient-value)))

(defun teamake-configure--describe-toolchain-file ()
  "Return `teamake-configure-toolchain-file' as protertized transient-variable."
  (format "Toolchain (%s)"
          (propertize (format "--toolchain-file=%s" teamake-configure-toolchain-file)
                      'face 'transient-value)))

(defun teamake-configure--describe-install-prefix ()
  "Return `teamake-configure-install-prefix' as protertized transient-variable."
  (format "Install (%s)"
          (propertize (format "-DTEAMAKE_INSTALL_PREFIX=%s" (teamake-return-value-or-default teamake-configure-install-prefix "<Unset>"))
                      'face 'transient-value)))

(defun teamake-configure-execute ()
  "Execute the currently configured Teamake command."
  (interactive)
  (message "args: %s" (transient-args transient-current-command)))



(transient-define-prefix teamake-configure-manage-cache-variables (code-path &optional build-path)
  [[:description
    "Hu hu hu hu"
    ("b" "bbbbbb" "--bb")
   ]]
  )

(transient-define-prefix teamake-configure (code-path)
  "Invoke a Teamake configuration step."
  [:description
   (lambda ()
     (teamake-project--code-tree-heading "Configure " (transient-scope)))
   ("b" teamake-configure-set-build-path :transient t
    :description teamake-configure--describe-build-path)
   ("c" teamake-configure-manage-cache-variables
    :description "Configure cace variables")
    
   ;; ("-D" teamake-configure--set-cache-entris :transient t
   ;;  :description teamake-configure--describe-cache-entries)
   ;; ("-U" teamake-configure--set-remove-cache-entries :transient t
   ;;  :description teamake-configure--describe-remove-cache-entries)
   ("g" teamake-configure-set-generator :transient t
    :description teamake-configure--describe-generator)
   ("T" teamake-configure-set-toolset :transient t
    :description teamake-configure--describe-toolset)
   ("a" teamake-configure-set-platform :transient t
    :description teamake-configure--describe-platform)
   ("t" teamake-configure-set-toolchain-file :transient t
    :description teamake-configure--describe-toolchain-file)
   ("i" teamake-configure-set-install-prefix :transient t
    :description teamake-configure--describe-install-prefix)
   ("x" teamake-configure-execute
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

  (interactive (list (teamake-project-code-root)))
  (transient-setup 'teamake-configure '() '() :scope code-path)
  )

(provide 'teamake-configure)
;;; teamake-configure.el ends here
