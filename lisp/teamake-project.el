;;; teamake-project --- Project configuration for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-cmake)
(require 'teamake-core)
(require 'teamake-configure)
(require 'teamake-build)
(require 'teamake-install)
(require 'teamake-preset)

;; Display and select project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun teamake-project-display (project)
  "Display the PROJECT for user."
  (if (not project)
      "No project"
    (format "%s (%s)"
            (or (plist-get project :name) "")
            (or (plist-get project :source-dir) ""))))

(defun teamake-project-display-propertized (project)
  "Display the PROJECT for user."
  (if (not project)
      (propertize "No project" 'face 'teamake-project-name)
    (format "%s (%s)"
            (propertize (or (plist-get project :name) "")
                        'face 'teamake-project-name)
            (propertize (or (plist-get project :source-dir) "")
                        'face 'teamake-path))))

(defun teamake-project-from-display (display)
  "Return PROJECT from DISPLAY."
  (seq-find
   (lambda (proj)
     (string= (teamake-project-display proj) display))
   teamake-project-configurations))

(defun teamake-project-prompt-user-select-project ()
  "Prompt user for selecting project."
  (interactive)
  (completing-read "Select project: "
                   (seq-map 'teamake-project-display
                            teamake-project-configurations)
                   '() t))

;; Project accessors
;;;;;;;;;;;;;;;;;;;;
(defun teamake-project--get-source-dir-name-from-project (project)
  (teamake-directory-name (plist-get project :source-dir)))

(defun teamake-project--get-parent-dir-from-project (project)
  (file-name-directory (directory-file-name (plist-get project :source-dir))))

(defun teamake-project--host-system-name ()
  "Return host system name as simple string as CMake usually use."
  (cond ((string= system-type "gnu/linux") "Linux")
        ((string= system-type "windows-nt") "Windows")
        ((string= system-type "darwin") "Darwin")
        (t system-type)))

(defun teamake-project--get-current-values (project transient)
  "Return current values for PROJECT in the parts defined in TRANSIENT.

I.e. getting the current `teamake-configure' values for reading
configuration values."
  (let ((current (plist-get project :current)))
    (alist-get transient current)))

(defun teamake-project--get-current-configured-generator (project)
  "Return executed teamake-configure command from PROJECT and return the generator."
  (let ((configuration-values (teamake-project--get-current-values project 'teamake-configure)))
    (save-match-data
      (match-string 1 (seq-find (lambda (p) (string-match "-G=\\(.+\\)" p)) configuration-values)))))

(defun teamake-project--list-variables (project)
  "Return an association list with  all variables for PROJECT and TEAMAKE."
  (append (plist-get project :variables)
          (list (cons "${project}" (plist-get project :name))
                (cons "${sourceDir}" (plist-get project :source-dir))
                (cons "${sourceDirName}" (teamake-project--get-source-dir-name-from-project project))
                (cons "${sourceParentDir}" (teamake-project--get-parent-dir-from-project project))
                (cons "${hostSystemName}" (teamake-project--host-system-name))
                (cons "${generator}" (teamake-project--get-current-configured-generator project))
                )))

(defun teamake-project--human-readable-from-project-variable (variable)
  "Return human readable from VARIABLE."
  (format "%s (%s)"
          (propertize (car variable) 'face 'transient-value)
          (cdr variable)))


(defun teamake-project--project-variable-from-human-readable (human-readable)
  "Return variable from HUMAN-READABLE."
  (save-match-data
    (string-match "\\(.+\\)\\ \\((.+)\\)" human-readable)
    (match-string 1 human-readable)))  

(defun teamake-project--insert-variable-at-point (project)
  "Prompt user for selection of PROJECT variable and insert the variable at point."
  (let* ((variables (teamake-project--list-variables project))
         (listing (seq-map 'teamake-project--human-readable-from-project-variable variables))
         (selection (completing-read "Variable: " listing '() t))
         (variable (teamake-project--project-variable-from-human-readable selection)))
    (insert variable)))

(defun teamake-project--expand-variables-from-project (project str)
  (let ((variables (teamake-project--list-variables project))
        (replaced str))
    (save-match-data
      (while (string-match "${.+?}" replaced)
        (let* ((variable (match-string 0 replaced))
               (match (assoc variable variables 'string=)))
          (unless match
            (user-error "Found variable expression '%s' in '%s' but was unable to match it in current project"
                        variable str))
          
          (setq replaced (string-replace variable (cdr match) replaced)))))
    replaced))

(defun teamake--name-from-project (project)
  "Return :name from PROJECT or default to `teamake-undetermined-project-name'"
  (or (plist-get project :name)
      teamake-undetermined-project-name))

(defun teamake--configuration-from-project (project)
  "Return :configuration from PROJECT or default value 'Not specified'."
  (or (plist-get project :configuration)
      "Not specified"))

(transient-define-suffix teamake-project--project-name ()
  :transient 'transient--do-recurse
  :description
  (lambda () (format "Project name (%s)"
                     (propertize
                      (teamake--name-from-project (transient-scope))
                      'face 'transient-value)))
  (interactive)
  (let* ((project (transient-scope))
         (name (read-string "Name: " (teamake--name-from-project project))))
    (plist-put project :name name)
    (transient-setup 'teamake-project '() '() :scope project)))

(transient-define-suffix teamake-project--project-source-dir ()
  :transient 'transient--do-recurse
  :description
  (lambda () (format "Source dir (%s)"
                     (propertize
                      (plist-get (transient-scope) :source-dir)
                      'face 'transient-value)))
  (interactive)
  (let* ((project (transient-scope))
         (source-dir (teamake--select-source-dir (plist-get project :source-dir))))
    (plist-put project :source-dir source-dir)
    (transient-setup 'teamake-project '() '() :scope project)))


(transient-define-suffix teamake-project--visit-source-dir ()
  :if (lambda () (plist-get (transient-scope) :source-dir))
  :description
  (lambda () (format "Visit source-dir (%s)"
                     (propertize (plist-get (transient-scope) :source-dir)
                                 'face 'teamake-path)))
  (interactive)
  (dired (plist-get (transient-scope) :source-dir)))

(transient-define-suffix teamake-project--visit-binary-dir ()
  :if (lambda () (plist-get (transient-scope) :binary-dir))
  :description
  (lambda () (format "Visit binary-dir (%s)"
                     (propertize (plist-get (transient-scope) :binary-dir)
                                 'face 'teamake-path)))
  (interactive)
  (dired (plist-get (transient-scope) :binary-dir)))

(defun teamake-project--unique-human-readable (project)
  "Display the PROJECT plist."
  (format "%s (%s)"
          (plist-get project :name)
          (plist-get project :source-dir)))

(defun teamake-project--get-project-from-unique-human-readable (display)
  "Return the project matching the DISPLAY."
  (seq-find
   (lambda (proj)
     (string= (teamake-project-display proj) display))
   teamake-project-configurations))

(defun teamake-project--matching-project-p (p1 p2)
  "Return if project P1 is a match to project P2."
  (string= (teamake-project--unique-human-readable p1)
           (teamake-project--unique-human-readable p2)))

(defun teamake-project--save-project ()
  "Save the project in transient-scope to the `teamake-project-configurations'."
  (interactive)
  (teamake-save-project (transient-scope)))

(transient-define-suffix teamake-project--load-project ()
  "Show a list of all known projects and prompt for one to select."
  :transient 'transient--do-replace
  (interactive)
  (let* ((projects-as-human-readable
          (seq-map
           (lambda (p) (teamake-project--unique-human-readable p))
           teamake-project-configurations))
         (project-to-load--human-readable
          (completing-read "Select project: " projects-as-human-readable '() t))
         (project (teamake-project--get-project-from-unique-human-readable
                   project-to-load--human-readable)))
    (teamake-setup-transient 'teamake-project project)
    ;; (transient-setup transient-current-command '() '() :scope project)
    ))

(defun teamake-project--delete-project ()
  "Show a list of all known projects and prompt for one to delete."
  (interactive)
  (let* ((projects-as-human-readable
          (seq-map
           (lambda (p) (teamake-project--unique-human-readable p))
           teamake-project-configurations))
         (project-to-remove--human-readable
          (completing-read "Delete project: " projects-as-human-readable '() t))
         (project (teamake-project--get-project-from-unique-human-readable
                             project-to-remove--human-readable)))
    (setq teamake-project-configurations
          (seq-remove
           (lambda (p)
             (teamake-project--matching-project-p p project))
           teamake-project-configurations)))
  (teamake-save-project-configurations))

(defun teamake-project--new-project (source-dir)
  "Create new project from SOURCE-DIR."
  (list :name (or (teamake-project--read-project-name-from-cmakelists source-dir)
                  "New project")
        :source-dir source-dir))

(transient-define-suffix teamake-project--create-project ()
  "Prompt for path to code of new project."
  (interactive)
  (let* ((source-dir (teamake-select-source-dir))
         (project (teamake-project--new-project source-dir)))
    (add-to-list 'teamake-project-configurations project)
    (transient-setup 'teamake-project '() '() :scope project)))

(transient-define-suffix teamake-project--teamake-cmake-navigate ()
  :description "CMake"
  :transient 'transient--do-replace
  :if (lambda () (transient-scope))
  (interactive)
  (transient-setup 'teamake-cmake-navigation '() '() :scope (transient-scope)))

(transient-define-prefix teamake-project (project)
  "Manage `teamake-project' settings."
  [:if
   (lambda () (transient-scope))
   "Project configuration"
   ("n" teamake-project--project-name)
   ("s" teamake-project--project-source-dir)
   ("c" teamake-transient--configuration)
   ]
  [:if
   (lambda () (transient-scope))
   "Visit"
   ("vs" teamake-project--visit-source-dir)
   ("vb" teamake-project--visit-binary-dir)
   ]
  ["Project"
   ("N" "New" teamake-project--create-project)
   ("S" "Save" teamake-project--save-project)
   ("L" "Load" teamake-project--load-project)
   ("D" "Delete" teamake-project--delete-project)
   ]
  ["Navigate"
   ("C" teamake-project--teamake-cmake-navigate)
   ]
  (interactive
   (list (teamake-get-or-create-project-from-source-dir default-directory)))
  (transient-setup 'teamake-project '() '() :scope project)
  )

(transient-define-suffix teamake-project--teamake-configure ()
  :description "Configure"
  :transient 'transient--do-recurse
  :if (lambda () (teamake-configure--possible (transient-scope)))
  (interactive)
  (teamake-configure--setup (transient-scope)))

(transient-define-suffix teamake-project--teamake-build ()
  :description "Build"
  :transient 'transient--do-recurse
  :if (lambda () (teamake-build--possible (transient-scope)))
  (interactive)
  (teamake-build--setup (transient-scope)))

(transient-define-suffix teamake-project--teamake-install ()
  :description "Install"
  :transient 'transient--do-recurse
  :if (lambda () (teamake-install--possible (transient-scope)))
  (interactive)
  (teamake-install--setup (transient-scope)))

(transient-define-suffix teamake-project--teamake-test ()
  :description "Test"
  :transient 'transient--do-recurse
  :if (lambda () (teamake-test--possible (transient-scope)))
  (interactive)
  (teamake-test--setup (transient-scope)))

(transient-define-suffix teamake-project--teamake-preset ()
  :description "Preset handling"
  :transient 'transient--do-recurse
  :if (lambda () (teamake-preset--possible (transient-scope)))
  (interactive)
  (teamake-preset--setup (transient-scope)))

(transient-define-suffix teamake-cmake--teamake-project ()
  :description "Project configuration"
  :transient 'transient--do-replace
  (interactive)
  (transient-setup 'teamake-project '() '() :scope (transient-scope)))

(transient-define-prefix teamake-cmake-navigation (project)
  [:if
   (lambda () (transient-scope) "")
   :description
   (lambda () (format "%s %s %s\n"
                      (propertize "CMake" 'face 'teamake-heading)
                      (propertize (plist-get (transient-scope) :configuration) 'face 'teamake-heading)
                      (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)
                      ))
   ("c" teamake-project--teamake-configure)
   ("b" teamake-project--teamake-build)
   ("i" teamake-project--teamake-install)
   ("t" teamake-project--teamake-test)
   ("pr" teamake-project--teamake-preset)
   ;; ("pa" teamake-project--teamake-package)
   ;; ("w" teamake-project--teamake-workflow)
   ]
  ["Navigate"
   ("P" teamake-cmake--teamake-project)
   ]
  (interactive
   (list (teamake-get-or-create-project-from-source-dir default-directory)))
  (transient-setup 'teamake-cmake-navigation '() '() :scope project)
  )

(provide 'teamake-project)
;;; teamake-project.el ends here
