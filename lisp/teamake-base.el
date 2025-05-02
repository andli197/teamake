
;;; Code:

;; Group definitions
(defgroup teamake '()
  "Teamake integration in Emacs using 'transient."
  :group 'tools)

(defgroup teamake-misc '()
  "Misc Teamake project options."
  :group 'teamake)

(defgroup teamake-commands '()
  "Options for controlling the behavior of configurable commands."
  :group 'teamake)

(defgroup teamake-buffers '()
  "Options for teamake buffers."
  :group 'teamake)

;; Faces
(defgroup teamake-faces '()
  "Faces used by teamake project."
  :group 'teamake)

(defface teamake-heading
  '((t :inherit font-lock-keyword-face))
  "Face for teamake project headings."
  :group 'teamake-faces)

(defface teamake-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake project name."
  :group 'teamake-faces)

(defface teamake-path
  '((((class color) (background light)) :foreground "Medium aquamarine")
    (((class color) (background  dark)) :foreground "Medium aquamarine"))
  "Face for teamake project path."
  :group 'teamake-faces)

;; Utilities
(defun re-seq (regexp string)
  "Fetch all match in the REGEXP applied to the STRING and return it as a list."
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (reverse matches))))

(defun teamake-return-value-or-default (variable default-value)
  "Return the value of VARIABLE if it is non empty, otherwise DEFAULT-VALUE."
  (if (teamake-variable-not-set variable)
      default-value
    variable))

(defun teamake-variable-not-set (variable)
  "Determine if the VARIABLE is set or not."
  (or (eq variable '()) (string= variable "")))

(defun teamake--find-root (path filename)
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

(defun teamake-build-root (&optional build-path)
  "Find the dominating topmost CMakeCache.txt file in BUILD-PATH."
  (interactive (list (read-directory-name "Select path within build tree: " default-directory '() t)))
  (teamake--find-root build-path "CMakeCache.txt"))

(defun teamake-build-tree-p (path)
  "Determine if PATH is part of a build tree."
  (not (string= (teamake-build-root path) "")))

(defun teamake-code-root (&optional source-path)
  "Find the dominating topmost CMakeLists.txt file in SOURCE-PATH."
  (interactive (list (read-directory-name "Select path within code tree: " default-directory '() t)))
  (teamake--find-root source-path "CMakeLists.txt"))

(defun teamake-code-tree-p (path)
  "Determine if PATH is part of a code tree."
  (not (string= (teamake-code-root path) "")))

(defun teamake-get-name (path &optional predicate)
  "Return deduced project-name from PATH.

If PATH needs to be code-tree or build-tree set PREDICATE to either
`teamake-code-tree-p' or `teamake-build-tree-p'."
  (let* ((predicate (or predicate (lambda (path) t)))
         (prediction (funcall predicate path)))
    (cond ((and (teamake-code-tree-p path) prediction)
           (teamake--name-from-code-tree path))
          ((and (teamake-build-tree-p path) prediction)
           (teamake--name-from-build-tree path))
          (t "<No project name>"))))

(defun teamake-get-root (path &optional predicate)
  "Return deduced root from PATH.

If PATH needs to be code-tree or build-tree set PREDICATE to either
`teamake-code-tree-p' or `teamake-build-tree-p'."

  (let* ((predicate (or predicate (lambda (path) t)))
         (prediction (funcall predicate path)))
    (cond ((and (teamake-code-tree-p path) prediction)
           (teamake--find-root path "CMakeLists.txt"))
          ((and (teamake-build-tree-p path) prediction)
           (teamake--find-root path "CMakeCache.txt"))
          (t "<No project path>"))))

(defun teamake--name-from-code-tree (&optional code-path)
  "Return project name of the project within CODE-PATH."
  (if (teamake-code-tree-p code-path)
      (let* ((cmake-lists (file-name-concat (teamake-code-root code-path) "CMakeLists.txt"))
             (content (with-temp-buffer
                        (insert-file-contents cmake-lists)
                        (buffer-string))))
        (if (string-match "project(\\(.+\\))" content)
            (car (split-string (match-string 1 content) " " t))
          "<No project>"))))

(defun teamake--name-from-build-tree (build-path)
  "Return project name of the project within BUILD-PATH."
  (if (teamake-build-tree-p build-path)
      (let* ((cache-file (file-name-concat (teamake-build-root build-path) "CMakeCache.txt"))
             (content (with-temp-buffer
                        (insert-file-contents cache-file)
                        (buffer-string))))
        (if (string-match "CMAKE_PROJECT_NAME:STATIC=\\(.+\\)" content)
            (match-string 1 content)
          "<No project>"))))

(defun teamake-heading (text path &optional expected)
  "Create a heading to use in transient with TEXT at PATH.

EXPECTED can be one of `teamake-code-tree' or `teamake-build-tree'"
  (concat (format "%s "
                  (propertize text 'face 'teamake-heading))
          (propertize (teamake-get-name path expected)
                      'face 'teamake-name)
          (format " (%s)"
                  (propertize (teamake-get-root path expected)
                              'face 'teamake-path))))

(defun teamake--code-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'teamake-heading))
          (propertize (teamake--name-from-code-tree path)
                      'face 'teamake-name)
          (format " (%s)"
                  (propertize (teamake-get-root path)
                              'face 'teamake-path))))

(defun teamake--build-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'teamake-heading))
          (propertize (teamake--name-from-build-tree path)
                      'face 'teamake-name)
          (format " (%s)"
                  (propertize (teamake-build-root path)
                              'face 'teamake-path))))

(defun teamake--heading-code-tree (heading-text code-path)
  "Return a propertized heading from HEADING-TEXT and CODE-PATH as Teamake project."
  (concat (format "%s " (propertize heading-text 'face 'teamake-heading))
          (teamake--code-path-propertized code-path)
          "\n"))

(defun teamake--code-path-propertized (path)
  "Return a propertized information about PATH as Teamake project."
  (concat (propertize (teamake--name-from-code-tree path) 'face 'teamake-name)
          (format " (%s)" (propertize (if (string= path "") "<Empty>" path)
                                      'face 'teamake-path))))

(provide 'teamake-base)
;;; teamake-base.el ends here
