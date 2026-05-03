;;; teamake-install --- CMake install for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)

;; (defun teamake-install--list-all-components (binary-dir)
;;   "Read CMakeCache.txt in BINARY-DIR and deduce all components."
;;   )

(defun teamake-install--possible (project)
  "Determine if PROJECT contain enough information for `teamake-install'."
  (teamake-project-has-valid-binary-dir-p project))

(defun teamake-install--setup (project)
  "Setup `teamake-install' from PROJECT."
  (unless (teamake-install--possible project)
    (user-error "Project not correctly configured with binary dir"))
  (teamake-setup-transient 'teamake-install project))

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
    (teamake-set-current-values 'teamake-install project value)
    (teamake-install--do-install-current)))

(transient-define-prefix teamake-install (project)
  [:description
   (lambda ()
     (format "%s %s\n\n%s\n"
             (propertize "CMake Install" 'face 'teamake-heading)
             (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)
             (propertize (format "cmake --install %s\n      <options>"
                                 (plist-get (transient-scope) :binary-dir))))
     )
  ["Options"
   ("cfg" teamake-transient--configuration)
   ;; Make this a suffix in order to have it autocomplete from read components for cmake version >= 4.0
   ;; reading the global target "list_install_components". For versions lower than 4.0 we can try
   ;; to parse the cmake_install.cmake to fetch all components.
   ("co" "Component-based install. Only install selected component(s)" "--component="
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
  ]
  ["Flags"
   ("-s" "Strip before installing" "--strip")
   ("-v" "Enable verbose output" "--verbose")
   ]
  [
   ["Do"
    ("xx" teamake-install--install-current)
    ]
   ["Manage"
    ("xc" "Save" teamake-transient-save-current-values :transient t)
    ("xa" "Save as" teamake-transient-save-current-as :transient t)
    ("xl" "Load" teamake-transient-load)
    ("xd" "Delete" teamake-transient-delete :transient t)
    ]
   ["Navigate"
    ("C" teamake-project--teamake-cmake-navigate)
    ("P" teamake-cmake--teamake-project)
    ]
   ]
  (interactive
   (let* ((binary-dir (teamake-select-binary-dir default-directory))
          (source-dir (teamake-cmake-cache--get-source-dir binary-dir)))
     (list (teamake-project-from-source-dir-or-create source-dir))))
  (teamake-setup-transient 'teamake-install project)

  )

(provide 'teamake-install)
;;; teamake-install.el ends here
