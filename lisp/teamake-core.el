;;; teamake-core --- Shared core functions that is used by different modules
;;; Commentary:
;;; Code:

(require 'transient)

(defconst teamake-version "0.0.1")

;;=========================
;; Customization and groups
;;=========================

(defgroup teamake '()
  "CMake integration in Emacs using `transient'."
  :group 'tools)

(defgroup teamake-faces '()
  "Faces used by teamake."
  :group 'teamake)

(defgroup teamake-process '()
  "Processing setup for teamake."
  :group 'teamake)

(defgroup teamake-misc '()
  "Misc setup for teamake."
  :group 'teamake)

(defface teamake-heading
  '((t :inherit font-lock-keyword-face))
  "Face for teamake headings."
  :group 'teamake-faces)

(defface teamake-variable-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake variable names."
  :group 'teamake-faces)

(defface teamake-variable-value
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake variable values."
  :group 'teamake-faces)

(defface teamake-project-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake project name."
  :group 'teamake-faces)

(defface teamake-path
  '((((class color) (background light)) :foreground "Medium aquamarine")
    (((class color) (background  dark)) :foreground "Medium aquamarine"))
  "Face for teamake project path."
  :group 'teamake-faces)

(defcustom teamake-process-preferred-shell shell-file-name
  "User preferred shell for executing commands in.

Defaults to `shell-file-name'"
  :package-version '(teamake . "0.0.1")
  :group 'teamake-process
  :type 'string)

(defcustom teamake-process-cmake-tool-path
  (locate-file "cmake" exec-path exec-suffixes)
  "Location of the cmake executables.  Used in calls to cmake, ctest, and cpack.
If available on PATH this needs not to be set."
  :package-version '(teamake . "0.0.1")
  :group 'teamake-process
  :type 'string)

(defcustom teamake-process-buffer-base-name "TEAMake"
  "Base name of process buffers created by `teamake'."
  :package-version '(teamake . "0.0.1")
  :group 'teamake-process
  :type 'string)

(defcustom teamake-project-configurations-file
  (locate-user-emacs-file "teamake-project-configurations.el")
  "File in which to save all project configurations."
  :package-version '(teamake . "0.0.1")
  :group 'teamake-misc
  :type 'file)

(defcustom teamake-undetermined-project-name "Undetermined project"
  "Name to present as project name when no project was available."
  :package-version '(teamake . "0.0.1")
  :group 'teamake-misc
  :type 'string)

(defcustom teamake-autosave-new-projects '()
  "Set to non-nil to autosave newly created projects to file for persistance."
  :package-version '(teamake . "0.0.1")
  :group 'teamake-misc
  :type 'boolean)


;;===================
;; Project management
;;===================

(defvar teamake-project-configurations '()
  "Available configured projects to use.")

;;;###autoload
(defun teamake-load-project-configurations ()
  "Initialize project configurations from save file.

Load contents of the file specified by `teamake-project-configurations-file'
into the variable `teamake-project-configurations'."
  (interactive)
  (if (file-exists-p teamake-project-configurations-file)
      (setq teamake-project-configurations
            (with-temp-buffer
              (insert-file-contents teamake-project-configurations-file)
              (read (current-buffer))))))

;;;###autoload
(defun teamake-save-project-configurations ()
  "Save project configurations to file.

Write the contents of the variable `teamake-project-configurations' to the
file specified by `teamake-project-configurations-file'."
  (interactive)
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length '())
          (print-level '()))
      (pp teamake-project-configurations (current-buffer)))
    (write-region '() '() teamake-project-configurations-file '() 'silent)))

(defun teamake--human-readable (project)
  "Display PROJECT as a unique identifier."
  (format "%s (%s)"
          (plist-get project :name)
          (plist-get project :source-dir)))

(defun teamake--matching-project-p (p1 p2)
  "Return if project P1 is determined to match P2."
  (string= (teamake--human-readable p1) (teamake--human-readable p2)))

(defun teamake-save-project (project)
  "Save PROJECT to `teamake-project-configurations'.

If the PROJECT matches an existing entry in `teamake-project-configurations'
it will be updated, otherwise it will be added."
  (let* ((hr (teamake--human-readable project))
         (match (seq-find
                 (lambda (p) (teamake--matching-project-p p project))
                 teamake-project-configurations)))
    (if match
        (progn
          (seq-do
           (lambda (p)
             (if (teamake--matching-project-p project p)
                 (setq p project)))
           teamake-project-configurations)
          (message "Project %s updated!" (teamake--human-readable project)))
      (progn
        (push project teamake-project-configurations)
        (message "Project %s added!" (teamake--human-readable project))))))

(defun teamake-create-project-from-source-dir (source-dir)
  "Create a new project from SOURCE-DIR.

The created project is added to `teamake-project-configurations' and if
`teamake-autosave-new-projects' is set all project configurations are
automatically saved to file, specified by `teamake-project-configurations-file'."
  (let ((project
         (list :name (teamake-project-name-from-source-dir source-dir)
               :source-dir source-dir)))
    (add-to-list 'teamake-project-configurations project)
    (if teamake-autosave-new-projects (teamake-save-project-configurations))
    project))

(defun teamake-project-from-source-dir (path)
  "Locate project associated with PATH as source-dir."
  (seq-find
   (lambda (p)
     (teamake--is-same-source-dir-p p path))
   teamake-project-configurations))

(defun teamake-project-from-binary-dir (path)
  "Return the project associated with PATH."
  (seq-find
   (lambda (proj)
     (teamake--is-same-binary-dir-p proj path))
   teamake-project-configurations))

(defun teamake--is-same-dir-p (p1 p2)
  "Determine if path P1 and P2 are the same."
  (string= (directory-file-name p1)
           (directory-file-name p2)))

(defun teamake--is-same-source-dir-p (project source-dir)
  "Determine if PROJECT :source-dir matches SOURCE-DIR."
  (teamake--is-same-dir-p (plist-get project :source-dir) source-dir))

(defun teamake--is-same-binary-dir-p (project binary-dir)
  "Determine if PROJECT :source-dir matches BINARY-DIR."
  (teamake--is-same-dir-p (plist-get project :binary-dir) binary-dir))

(defun teamake-get-or-create-project-from-source-dir (path)
  "Fetch the project associated with PATH or create one.

PATH must be from within the code tree, otherwise return nil."
  (interactive (teamake-select-source-dir))
  (let ((source-dir (teamake--find-root path "CMakeLists.txt")))
    (if source-dir
        (let ((project (teamake-project-from-source-dir path)))
          (unless project
            (setq project (teamake-create-project-from-source-dir source-dir)))
          project))))

(defalias 'teamake-project-from-source-dir-or-create 'teamake-get-or-create-project-from-source-dir)

;;; TODO: Remove?
(defun teamake-project-heading (text project)
  "Return a propertized PROJECT heading with TEXT.

Project name, source dir and binary dir are also shown."
  (concat (format "%s %s\n"
                  (propertize text 'face 'teamake-heading)
                  (propertize
                   (or (plist-get project :name) teamake-undetermined-project-name)
                   'face 'teamake-project-name))
          (if (plist-member project :source-dir) (format "  source-dir: %s\n"
                                                     (propertize (plist-get project :source-dir)
                                                                 'face 'teamake-path)))
          (if (plist-member project :binary-dir) (format "  binary-dir: %s\n"
                                                     (propertize (plist-get project :binary-dir)
                                                                 'face 'teamake-path)))))

;;===========================
;; CMake directories handling
;;===========================

(defun teamake--find-root (path filename)
  "Look for the dominating FILENAME in PATH.

Look backward for FILENAME files and return the path to the topmost
file.  From the selected PATH first locate the dominating FILENAME,
then look for FILENAME files in parent directories."
  (let* ((start-path path)
         (topmost-directory (locate-dominating-file start-path filename))
         (candidate-file (file-name-concat topmost-directory filename)))
    (while (file-exists-p candidate-file)
      (setq topmost-directory (file-name-directory candidate-file)
            candidate-file (file-name-concat
                            (file-name-parent-directory
                             (file-name-directory candidate-file))
                            filename)))
    (if topmost-directory
        (directory-file-name topmost-directory)
      '())))

(defun teamake-select-source-dir (&optional initial)
  "Return a correct CMake source-dir.

If INITIAL is given, check first if that is valid.  If not valid source-dir
prompt user for input.  A correct source-dir must contain a CMakeLists.txt file."
  (interactive)
  (let ((source-dir (or initial (read-directory-name "CMake source dir: " default-directory '() t))))
    (while (not (teamake--find-root source-dir "CMakeLists.txt"))
      (setq source-dir (read-directory-name
                        "Invalid CMake source dir, select new (must contain CMakeLists.txt): " '() '() t)))
    (directory-file-name source-dir)))

(defun teamake-select-binary-dir (&optional initial)
  "Return a correct CMake binary-dir.

If INITIAL is given, check first if that is valid.  If not valid binary-dir
prompt user for input.  A correct binary-dir must contain a CMakeCache.txt file."
  (interactive)
  (let ((binary-dir (or initial (read-directory-name "CMake binary dir: " default-directory '() t))))
    (while (not (teamake--find-root binary-dir "CMakeCache.txt"))
      (setq binary-dir (read-directory-name
                        "Invalid CMake binary dir, select new (must contain CMakeCache.txt): " '() '() t)))
    (directory-file-name binary-dir)))

(defun teamake-project-has-valid-source-dir-p (project)
  "Determine if PROJECT has a valid configured source-dir."
  (and (plist-member project :source-dir)
       (file-exists-p (plist-get project :source-dir))
       (teamake--find-root (plist-get project :source-dir) "CMakeLists.txt")))

(defun teamake-project-has-valid-binary-dir-p (project)
  "Determine if PROJECT has a valid configured binary-dir."
  (and (plist-member project :binary-dir)
       (file-exists-p (plist-get project :binary-dir))
       (teamake--find-root (plist-get project :binary-dir) "CMakeCache.txt")))

;;===========================================
;; Transient and current/save values handling
;;===========================================
(defun teamake-setup-transient (transient project)
  "Setup TRANSIENT with current values from PROJECT."
  (transient-setup transient '() '()
                   :scope project
                   :value (teamake-get-current-values transient project)))

(defun teamake-get-current-values (transient project)
  "Get current value in PROJECT for TRANSIENT."
  (alist-get transient (plist-get project :current)))

(defun teamake-set-current-values (transient project value)
  "Set current value in PROJECT for TRANSIENT to VALUE."
  (unless (plist-member project :current)
    (plist-put project :current '()))
  (let* ((current (plist-get project :current))
         (existing (alist-get transient current)))
    (if existing
        (setf (alist-get transient current) value)
      (push (cons transient value) (plist-get project :current)))))

(defun teamake-get-save-value (transient project name)
  "Return specific saved value with NAME in PROJECT for TRANSIENT."
  (cdr
   (seq-find
    (lambda (pair)
      (string= (car pair) name))
    (teamake-get-save-values transient project))))

(defun teamake-get-save-values (transient project)
  "Return all saved values in PROJECT for TRANSIENT."
  (alist-get transient (plist-get project :save)))

(defun teamake-get-save-names (transient project)
  "Return name of all saved values in PROJECT for TRANSIENT."
  (interactive)
  (seq-map (lambda (s)
             (car s))
           (teamake-get-save-values transient project)))

(defun teamake-set-save-values (transient project name values)
  "Set a save record in PROJECT for TRANSIENT to NAME containing VALUES."
  (unless (plist-member project :save)
    (plist-put project :save '()))
  (let ((pair (cons name values))
        (existing (alist-get transient (plist-get project :save))))
    (if (alist-get name existing '() '() 'string=)
        (setf (alist-get name existing '() '() 'string=)
              values)
      (setf (alist-get transient (plist-get project :save))
            (if existing
                (push pair existing)
              (list pair))))))

(defun teamake-delete-save-value (transient project name)
  "Remove save record NAME in PROJECT for TRANSIENT."
  (let* ((save-values (alist-get transient (plist-get project :save)))
         (to-remove (assoc name save-values 'string=)))
    (setf (alist-get transient (plist-get project :save))
          (seq-keep (lambda (pair)
                      (unless (string= (car pair) (car to-remove))
                        pair))
                    save-values))))

(transient-define-suffix teamake-transient-save-current-values ()
  "Save current values for `transient-current-command' into active project."
  (interactive)
  (let* ((project (transient-scope))
         (values (transient-args transient-current-command)))
    (teamake-set-current-values transient-current-prefix project values)
    (teamake-save-project project)))

(transient-define-suffix teamake-transient-save-current-as ()
  "Prompt user for a name to save current values for `transient-current-command'."
  (interactive)
  (let* ((project (transient-scope))
         (values (transient-args transient-current-command))
         (existing-names (teamake-get-save-names transient-current-command project))
         (name (completing-read "Name (match will be overwritten): "
                                existing-names)))
    (teamake-set-save-values transient-current-command project name values)))

(transient-define-suffix teamake-transient-load ()
  "Prompt user for selecting among saved settings for `transient-current-command'."
  (interactive)
  (let* ((project (transient-scope))
         (existing-names (teamake-get-save-names transient-current-command project))
         (name (completing-read "Load: " existing-names '() t))
         (value (teamake-get-save-value transient-current-command project name)))

    (teamake-set-current-values transient-current-command project value)
    (teamake-setup-transient transient-current-command project)))

(transient-define-suffix teamake-transient-delete ()
  "Prompt user for selecting saved settings for `transient-current-command' to delete."
  :transient 'transient--do-recurse
  (interactive)
  (let* ((project (transient-scope))
         (existing-names (teamake-get-save-names transient-current-command project))
         (name (completing-read "Delete: " existing-names '() t)))
    (teamake-delete-save-value transient-current-command project name)))

;;=======================
;; Misc utility functions
;;=======================
(transient-define-suffix teamake-transient--configuration ()
  :description
  (lambda ()
    (let* ((project (transient-scope))
           (text "Configuration (for multi-configuration tools)")
           (option "--config=")
           (value (plist-get project :configuration)))
      (format "%s (%s)" text (if value
                                 (propertize (format "%s%s" option value)
                                             'face 'transient-value)
                               option))))
  (interactive)
  (let* ((project (transient-scope))
         (values (transient-args transient-current-command))
         (configuration (completing-read "Configuration: " '("Debug" "RelWithDebInfo" "Release") '() t)))
    (plist-put project :configuration configuration)
    (teamake-set-current-values transient-current-command project values)
    (teamake-setup-transient transient-current-command project)))

(defun teamake-directory-name (path)
  "Return the directory name of the PATH."
  (let* ((source-parent-dir (file-name-directory (directory-file-name path))))
    (substring (directory-file-name path)
               (length source-parent-dir))))

(defun teamake-source-parent-dir (path)
  "Return the source parent dir from PATH."
  (directory-file-name (file-name-directory (directory-file-name path))))

(defun teamake-host-system-name ()
  "Return host system name as simple string as CMake usually use."
  (cond ((string= system-type "gnu/linux") "Linux")
        ((string= system-type "windows-nt") "Windows")
        ((string= system-type "darwin") "Darwin")
        (t system-type)))

(defun teamake-expand-regular (text source-dir)
  "Expand TEXT with known replacements from SOURCE-DIR."
  (cond ((string= text "${sourceDir}")
         source-dir)
        ((string= text "${sourceParentDir}")
         (teamake-source-parent-dir source-dir))
        ((string= text "${sourceDirName}")
         (teamake-directory-name source-dir))
        ((string= text "${hostSystemName}")
         (teamake-host-system-name))
        (t text)))

(defun teamake-expand-expression (expression source-dir &optional expansion-fn)
  "Find all occurances of type ${NAME} in EXPRESSION and try to expand them."
  (let ((expansion-fn (or expansion-fn 'teamake-expand-regular)))
    (save-match-data
      (if (not (string-match "${[a-zA-Z-0-9]+}?" expression))
          expression
        (let* ((macro (match-string 0 expression))
               (expansion-step (string-replace macro (funcall expansion-fn macro source-dir) expression)))
          (if (string= expansion-step expression)
              (error "No known definition for '%s' found!" macro))
          (teamake-expand-expression expansion-step source-dir expansion-fn))))))

(defun teamake--get-cmake-tool (&optional tool)
  "Use the `teamake-process-cmake-tool-path' for locating TOOL.

Locate the TOOL from cmake tool suite (cmake, ctest, cpack)
using the configured `teamake-process-cmake-tool-path' or
default configured PATH as a fallback."
  (if (file-exists-p tool)
      tool
    (let* ((program (or tool "cmake"))
           (location program)
           (locations exec-path))
      (add-to-list 'locations (file-name-directory teamake-process-cmake-tool-path))
      (setq location (locate-file program locations exec-suffixes t))
      (unless (file-exists-p location)
        (user-error "Unable to locate program %s" program))
      location)))

(defun teamake--cmake-version ()
  "Return version of CMake."
  (let* ((shell-file-name teamake-process-preferred-shell)
         (cmd (list (teamake--get-cmake-tool "cmake") "--version"))
         (output (shell-command-to-string (mapconcat 'shell-quote-argument cmd " "))))
    (save-match-data
      (if (string-match "cmake version \\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)" output)
          (list (string-to-number (match-string 1 output))
                (string-to-number (match-string 2 output))
                (string-to-number (match-string 3 output)))))))

(defun teamake--version-min (major &optional minor patch)
  "Return wether the found cmake version is at least the version.

Version is divided into MAJOR, MINOR and PATCH and matched using Emacs
`version-list-<=' comparision."
  (let ((minor (or minor 0))
        (patch (or patch 0))
        (version (teamake--cmake-version)))
    (version-list-<= (list major minor patch) version)))


;; (read-multiple-choice
;;  "Continue connecting?"
;;  '((?a "always" "Accept certificate for this and future sessions.")
;;    (?s "session only" "Accept certificate this session only.")
;;    (?n "no" "Refuse to use certificate, close connection."))
;;  "Longer help string here"
;;  t)
;; (defun teamake--completing-read (prompt)
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html
;;   )

(provide 'teamake-core)
;;; teamake-core.el ends here
