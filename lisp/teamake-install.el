;;; teamake-install --- CMake install for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-build)

;; (defun teamake-install--list-all-components (binary-dir)
;;   "Read CMakeCache.txt in BINARY-DIR and deduce all components."
;;   )

(defun teamake-install--possible (project)
  "Determine if PROJECT contains enough information for setup `teamake-install'."
  (teamake-get-current-values 'teamake-install project))

(defun teamake-install--setup (project)
  "Setup `teamake-install' from PROJECT."
  (let ((current-values (teamake-get-current-values 'teamake-install project)))
    (if current-values
        (transient-setup 'teamake-install '() '()
                         :scope (plist-get current-values :scope)
                         :value (plist-get current-values :value)))))

(defun teamake-install--setup-transient-from-path (binary-dir)
  "Setup `teamake-install' transient from BINARY-DIR."
  ;;; error state when binary-dir is not a configure cmake binary-dir!!!
  (let* ((project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (current-values (teamake-get-current-values 'teamake-install project)))
    (if current-values
        (transient-setup 'teamake-install '() '()
                         :scope (plist-get current-values :scope)
                         :value (plist-get current-values :value))
      (transient-setup 'teamake-install '() '()
                       :scope binary-dir))))

(defun teamake-install-set-current (binary-dir values)
  "Set current values in matched project for BINARY-DIR to VALUES."
  (let ((project (teamake-cmake-cache--project-from-binary-dir binary-dir)))
    (unless project
      (user-error "Unable to match binary-dir '%s' to any project thus unable save values" binary-dir))
    (teamake-set-current-values 'teamake-install project (list :scope binary-dir
                                                               :value values))))

(transient-define-suffix teamake-install--do-install-current ()
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (values (teamake-get-current-values 'teamake-install project)))
    (apply #'teamake-process-invoke-cmake
           project
           "--install"
           binary-dir
           (plist-get values :value))))

(transient-define-suffix teamake-install--install-current ()
  :description "Install current"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (value (transient-args 'teamake-install)))
    (teamake-install-set-current binary-dir value)
    (teamake-install--do-install-current)))

(transient-define-prefix teamake-install (binary-dir)
  [:description
   (lambda () (teamake-binary-dir-heading "CMake install" (transient-scope)))
   ("cfg" "For multi configuration tools" "--config="
    :prompt "Configuration: "
    :choices ("Release" "Debug" "RelWithDebInfo"))
   ("co" "Component-based install. Only install component" "--component="
    :prompt "Component: ")
   ("pe" "Default directory install permissions" "--default-directory-permissions"
    :prompt "Permissions in format <u=rwx,g=rx,o=rx>: ")
   ("pa" "Specifies an alternative installation prefix" "--prefix="
    :prompt "Installation: "
    :reader transient-read-directory)
   ("j" "Install in parallel using the given number of jobs" "--parallel="
    :prompt "Select jobs: "
    :reader transient-read-number-N+)
   ]
  ["Flags"
   ("-s" "Strip before installing" "--strip")
   ("-v" "Enable verbose output" "--verbose")
   ]
  [["Do"
    ("xx" teamake-install--install-current)]
   ["Manage"
    ("xsc" "Save" "--save")
    ("xsa" "Save as" "--save-as")
    ("xl" " Load" "--load")]
   ]
  (interactive
   (list (teamake--select-binary-dir default-directory)))
  (teamake-install--setup-transient-from-path binary-dir)
  )

(provide 'teamake-install)
;;; teamake-install.el ends here
