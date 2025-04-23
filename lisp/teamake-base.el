
;;; Code:


(defgroup teamake-faces '()
  "Faces used by teamake project."
  :group 'teamake-project)

(defface teamake-project-heading
  '((t :inherit font-lock-keyword-face))
  "Face for teamake project headings."
  :group 'teamake-faces)

(defface teamake-project-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake project name."
  :group 'teamake-faces)

(defface teamake-project-path
  '((((class color) (background light)) :foreground "Medium aquamarine")
    (((class color) (background  dark)) :foreground "Medium aquamarine"))
  "Face for teamake project path."
  :group 'teamake-faces)

(defun teamake-return-value-or-default (variable default-value)
  "Return the value of VARIABLE if it is non empty, otherwise DEFAULT-VALUE."
  (if (teamake-variable-not-set variable)
      default-value
    variable))

(defun teamake-variable-not-set (variable)
  "Determine if the VARIABLE is set or not."
  (or (eq variable '()) (string= variable "")))

(defun teamake-project--find-root (path filename)
  "Look for the dominating FILENAME in PATH.

Look backward for FILENAME files and return the path to the topmost
file.  From the selected PATH first locate the dominating FILENAME
file, then look for FILENAME files in parent directories.

If no source-path is provided `default-directory' is used and returned."
  (let* ((start-path (or path default-directory))
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
      "")))

(defun teamake-project-build-root (&optional build-path)
  "Find the dominating topmost TeamakeCache.txt file in BUILD-PATH."
  (interactive (list (read-directory-name "Select path within build tree: " default-directory '() t)))
  (teamake-project--find-root build-path "TeamakeCache.txt"))

(defun teamake-project-build-tree-p (path)
  "Determine if PATH is part of a build tree."
  (not (string= (teamake-project-build-root path) "")))

(defun teamake-project-code-root (&optional source-path)
  "Find the dominating topmost TeamakeLists.txt file in SOURCE-PATH."
  (interactive (list (read-directory-name "Select path within code tree: " default-directory '() t)))
  (teamake-project--find-root source-path "TeamakeLists.txt"))

(defun teamake-project-code-tree-p (path)
  "Determine if PATH is part of a code tree."
  (not (string= (teamake-project-code-root path) "")))

(defun teamake-project-get-name (path)
  "Return deduced project-name from PATH."
  (cond ((teamake-project-code-tree-p path)
         ;; TODO: Parse project() in TeamakeLists.txt
         (let ((directory (teamake-project-code-root path)))
           (file-relative-name directory (file-name-parent-directory directory))))
        ((teamake-project-build-tree-p path)
         ;; TODO: Parse TEAMAKE_PROJECT_NAME:STATIC= row in TeamakeCache.txt
         (let ((directory (teamake-project-build-root path)))
           (file-relative-name directory (file-name-parent-directory directory))))
        (t "<No project>")))

(defun teamake-project-get-root (path)
  "Return deduced root from PATH."
  (cond ((teamake-project-code-tree-p path)
         (teamake-project--find-root path "TeamakeLists.txt"))
        ((teamake-project-build-tree-p path)
         (teamake-project--find-root path "TeamakeCache.txt"))
        (t (concat "<default-directory: " path ">"))))

;;; TODO: Parse TeamakeLists.txt and check for project() to read project name
(defun teamake-project--name-from-code-tree (&optional code-path)
  "Return project name of the project within CODE-PATH."
  (if (teamake-project-code-tree-p code-path)
      (let ((directory (teamake-project-code-root code-path)))
        (file-relative-name directory (file-name-parent-directory directory)))
    "<No project>"))

;;; TODO: Parse TeamakeCache.txt and check for TEAMAKE_PROJECT_NAME:STATIC= to read project name
(defun teamake-project--name-from-build-tree (build-path)
  "Return project name of the project within BUILD-PATH."
  (if (teamake-project-build-tree-p build-path)
      (let ((directory (teamake-project-build-root build-path)))
        (file-relative-name directory (file-name-parent-directory directory)))
    "<No project>"))

(defun teamake-project-heading (text path)
  "Create a heading to use in transient with TEXT at PATH."
  (concat (format "%s "
                  (propertize text 'face 'teamake-project-heading))
          (propertize (teamake-project-get-name path)
                      'face 'teamake-project-name)
          (format " (%s)"
                  (propertize (teamake-project-get-root path)
                              'face 'teamake-project-path))))

(defun teamake-project--code-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'teamake-project-heading))
          (propertize (cond ((teamake-project-code-tree-p path) (teamake-project--name-from-code-tree path))
                            (t "<No code tree>"))
                      'face 'teamake-project-name)
          (format " (%s)"
                  (propertize (if (teamake-project-code-tree-p path)
                                  path
                                "<Empty>")
                              'face 'teamake-project-path))))

(defun teamake-project--build-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'teamake-project-heading))
          (propertize (cond ((teamake-project-build-tree-p path) (teamake-project--name-from-build-tree path))
                            (t "<No build tree>"))
                      'face 'teamake-project-name)
          (format " (%s)"
                  (propertize (if (teamake-project-build-tree-p path)
                                  path
                                "<Empty>")
                              'face 'teamake-project-path))))

(defun teamake-project--heading-code-tree (heading-text code-path)
  "Return a propertized heading from HEADING-TEXT and CODE-PATH as Teamake project."
  (concat (format "%s " (propertize heading-text 'face 'teamake-project-heading))
          (teamake-project--code-path-propertized code-path)
          "\n"))

(defun teamake-project--code-path-propertized (path)
  "Return a propertized information about PATH as Teamake project."
  (concat (propertize (teamake-project--name-from-code-tree path) 'face 'teamake-project-name)
          (format " (%s)" (propertize (if (string= path "") "<Empty>" path)
                                      'face 'teamake-project-path))))

(provide 'teamake-base)
;;; teamake-base.el ends here
