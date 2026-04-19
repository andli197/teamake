;;; teamake-core --- Shared core functions that is used by different modules
;;; Commentary:
;;; Code:

(require 'transient)

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

(defcustom teamake-process-preferred-shell
  shell-file-name
  "User preferred shell for executing commands in.

Defaults to `shell-file-name'"
  :type 'string
  :group 'teamake-process)

(defcustom teamake-process-cmake-tool-path
  (locate-file "cmake" exec-path exec-suffixes)
  "Location of the cmake executable.

Used in calls to cmake, ctest, cpack, etc."
  :type 'string
  :group 'teamake-process)

(defcustom teamake-process-buffer-base-name
  "TEAMake"
  "Base name of process buffers created by `teamake'."
  :type 'string
  :group 'teamake-process)

(defcustom teamake-project-configurations-file
  (locate-user-emacs-file "teamake-project-configurations.el")
  "File in which to save all project configurations."
  :type 'file
  :group 'teamake-misc)


(defvar teamake-project-configurations '()
  "Available configured projects to use.")

(if '()
    (transient-setup 'teamake-configure '() '()
                     :scope (caddr teamake-project-configurations)
                     :value (alist-get 'teamake-configure (plist-get (caddr teamake-project-configurations) :current)))
    )


;; Save and load projects
;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun teamake-setup-transient (transient project)
  "Setup TRANSIENT with values from PROJECT."
  (interactive)
  (let ((current (teamake-get-current-values transient project)))
    (transient-setup transient '() '()
                     :scope project
                     :value current)))

(defun teamake-set-current-values (transient project value)
  "Set current value in PROJECT for TRANSIENT to VALUE."
  (unless (plist-member project :current)
    (plist-put project :current '()))
  (let* ((current (plist-get project :current))
         (existing (alist-get transient current)))
    (if existing
        (setf (alist-get transient current) value)
      (push (cons transient value) (plist-get project :current)))))

;; (defun teamake-get-current-values (transient project)
(transient-define-suffix teamake-get-current-values (transient project)
  "Get current value in PROJECT for TRANSIENT."
  (interactive)
  (alist-get transient (plist-get project :current)))

(defun teamake-set-save-values (transient project name values)
  "Set a save record in PROJECT for TRANSIENT to NAME containing VALUES."
  (unless (plist-member project :save)
    (plist-put project :save '()))
  (let ((pair (cons name values))
        (existing (alist-get transient (plist-get project :save))))
    (if (not existing)
        (setf (alist-get transient (plist-get project :save)) (list pair))
      (push pair (alist-get transient (plist-get project :save))))))

(defun teamake-get-save-value (transient project name)
  "Return specific saved value with NAME in PROJECT for TRANSIENT."
  (cdr
   (seq-find
    (lambda (pair)
      (string= (car pair) name))
    (teamake-get-save-values transient project))))

(defun teamake-get-save-values (transient project)
  "Return name of all saved values in PROJECT for TRANSIENT."
  (alist-get transient (plist-get project :save)))

(defun teamake-get-save-names (transient project)
  "Return name of all saved values in PROJECT for TRANSIENT."
  (interactive)
  (seq-map (lambda (s)
             (car s))
           (teamake-get-save-values transient project)))

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

(defun teamake--path-matches-p (val path)
  "Determine if any of the VAL match for PATH.

VAL can be either a single argument or a list of arguments."
  (cond ((stringp val) (string-match val path))
        ((listp val) (> (seq-count (lambda (item) (string-match item path)) val) 0))
        (t '())))

(defun teamake--project-contains-p (project path)
  "Determine if PROJECT is matching PATH.

Either the PROJECT :source-dir,
or current `teamake-build' ,
or current `teamake-configure'
can mention PATH in any way for it to be considered a match."
  (or (string= (directory-file-name (plist-get project :source-dir))
               (directory-file-name path))
      (teamake--path-matches-p (teamake-get-current-values 'teamake-build project) path)
      (teamake--path-matches-p (teamake-get-current-values 'teamake-configure project) path)))

(defun teamake--project-from-path (path)
  "Return the project associated with PATH.

Check if any configured project have a setting pointing out PATH exactly
in any way."
  (seq-find
   (lambda (proj)
     (teamake--project-contains-p proj (directory-file-name path)))
   teamake-project-configurations))

(defun teamake--select-source-dir (&optional initial)
  "Prompt user for source directory starting at optional INITIAL.

Ensure it is a cmake project (containing CMakeLists.txt) and
make sure the path does not end in directory separator."
  (let* ((initial (or initial default-directory))
         (source-dir (read-directory-name "CMake source dir: " initial '() t)))
    (while (not (teamake--find-root source-dir "CMakeLists.txt"))
      (setq source-dir (read-directory-name
                         "Invalid CMake project, select new (must contain CMakeLists.txt): " '() '() t)))
    (directory-file-name source-dir)))

(defun teamake--select-binary-dir (&optional initial)
  "Prompt user for binary directory starting at optional INITIAL.

Ensure it is a cmake binary dir (containing CMakeCache.txt) and
make sure the path does not end in directory separator."
  (let* ((initial (or initial default-directory))
         (binary-dir (read-directory-name "CMake binary dir: " initial '() t)))
    (while (not (teamake--find-root binary-dir "CMakeCache.txt"))
      (setq binary-dir (read-directory-name
                         "Invalid CMake binary dir, select new (must contain CMakeCache.txt): " '() '() t)))
    (directory-file-name binary-dir)))

;; (read-multiple-choice
;;  "Continue connecting?"
;;  '((?a "always" "Accept certificate for this and future sessions.")
;;    (?s "session only" "Accept certificate this session only.")
;;    (?n "no" "Refuse to use certificate, close connection."))
;;  "Build a "
;;  t)
;; (defun teamake--completing-read (prompt)
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html
;;   )

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

(provide 'teamake-core)
;;; teamake-core.el ends here
