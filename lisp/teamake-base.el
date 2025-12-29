
;;; Code:

(require 'transient)
(require 's)

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

(defcustom teamake-project-configurations-file
  (locate-user-emacs-file "teamake-projects.el")
  "File in which to save all project configurations."
  :type 'file
  :group 'teamake)

(defvar teamake--project-configurations '()
  "List structure containing full configurations of known projects.")

;; Utilities
(defun re-seq (regexp string)
  "Fetch all match in the REGEXP applied to the STRING and return it as a list."
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (reverse matches))))

(defun teamake--read-project-configurations ()
  "Initialize `teamake--project-configurations' from contents of
`teamake-project-configurations-file'"
  (setq teamake--project-configurations
        (when (file-exists-p teamake-project-configurations-file)
          (with-temp-buffer
            (insert-file-contents teamake-project-configurations-file)
            (read (current-buffer))))))

(defun teamake--write-project-configurations ()
  "Save `teamake--project-configurations' in `teamake-project-configurations-file'"
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length '())
          (print-level '()))
      (pp teamake--project-configurations (current-buffer)))
    (write-region '() '() teamake-project-configurations-file '() 'silent)))

(defun teamake--string-from-key (key)
  "Remove the leading :-sign from the KEY."
  (let ((string-key (format "%s" key)))
    (substring string-key 1)))

(defun teamake--try-known-variables (expression)
  "Try fetching EXPRESSION from predefined variables."
  (transient-arg-value expression (transient-args 'teamake)))

(defun teamake--try-custom-variables (name)
  "Try fetching NAME from the additional variables set.

They need not to be specified as ${name}=value but may also be specified
as name=value and the transient value has also -V flag in front of it."
  (let ((flag "-V"))
    (or (transient-arg-value (format "%s%s=" flag name) (transient-args 'teamake))
        (transient-arg-value (format "%s${%s}=" flag name) (transient-args 'teamake)))))

(defun teamake--get-transient-variable (macro-expr)
  "Return the transient MACRO-EXPR value."
  (let ((suffix ""))
    (if (not (s-ends-with? "=" macro-expr))
        (setq suffix "="))
    (or (teamake--try-known-variables (concat macro-expr suffix))
        (teamake--try-custom-variables
         (substring macro-expr 2
                    (- (length macro-expr) 1))))))

(defun teamake--get-macro-value (macro-expr)
  "Extract the variable MACRO-EXPR from `teamake' transient or the known macros.

The known macros are:
 ${sourceParentDir} = Deduced from ${sourceDir}
 ${sourceDirName}   = Deduced from ${sourceDir}
 ${dollar}          = Litteral \"$\" sign
 ${hostSystemName}  = Translates to Linux/Windows/Darwin
 ${pathListSep}     = path-separator"
  (let ((source-dir (transient-arg-value "${sourceDir}=" (transient-args 'teamake))))
    (cond ((string= macro-expr "${sourceParentDir}")
           (file-name-directory (directory-file-name source-dir)))
          ((string= macro-expr "${sourceDirName}")
           (teamake-directory-name source-dir))
          ((string= macro-expr "${dollar}") "$")
          ((string= macro-expr "${hostSystemName}")
           (teamake-host-system-name))
          ((string= macro-expr "${pathListSep}")
           path-separator)
          (t (teamake--get-transient-variable macro-expr)))))

(defun teamake-get-variable-value (name)
  "Extract the variable NAME from `teamake' transient.

If name is not wrapped as a variable expression, wrap it
before fetching value."
  (let ((prefix "${")
        (suffix "}"))
    (if (s-starts-with? prefix name)
        (setq prefix ""))
    (if (s-ends-with? suffix name)
        (setq suffix ""))
    (or (teamake--get-macro-value
         (concat prefix name suffix))
        name)))

(defun teamake-expand-macro-expression (expression)
  "Fully expand all macros in EXPRESSION."
  (save-match-data
    (if (not (string-match "${[a-zA-Z-0-9]+}?" expression))
        expression
      (let ((macro (match-string 0 expression)))
        (setq expansion
              (string-replace macro
                              (teamake-get-variable-value macro)
                              expression))
        (if (string= expansion expression)
            (error "No known definition for '%s' found!" macro))
        (teamake-expand-macro-expression expansion)))))

(defun teamake-root ()
  "Fetch root directory.

Start from base directory deduced from scope (if invoked from transient)
or default-directory and either:
1. Locate topmost CMakeLists.txt file or
2. Locate topmost CMakeCache.txt file or
3. Locate .git directory or
4. Return base directory."
  (let ((dir (teamake-get-scope-or-default)))
    (or (teamake--find-root dir "CMakeLists.txt")
        (teamake--find-root dir "CMakeCache.txt")
        (teamake--find-root dir ".git")
        dir)))

(defun teamake-get-scope-or-default ()
  (or (transient-scope) default-directory))

(defun teamake-project-name ()
  "Return ${project} tag or deduced name from the current scope."
  (teamake-get-variable-value "${project}"))

(defun teamake-source-dir ()
  "Return current source directory."
  (teamake-get-variable-value "${sourceDir}"))

(defun teamake-source-dir-absolute ()
  "Return current source directory."
  (file-truename (teamake-expand-macro-expression
                  (teamake-source-dir))))

(defun teamake-build-dir ()
  "Return current build directory."
  (teamake-get-variable-value "${buildDir}"))

(defun teamake-build-dir-absolute ()
  "Return fully resolved ${buildDir}."
  (file-truename (teamake-expand-macro-expression
                  (teamake-build-dir))))

(defun teamake-directory-name (path)
  "Return the directory name of the PATH."
  (let* ((source-parent-dir (file-name-directory (directory-file-name path))))
    (substring (directory-file-name path)
               (length source-parent-dir))))

(defun teamake-host-system-name ()
  "Return host system name as simple string as CMake usually use."
  (cond ((string= system-type "gnu/linux") "Linux")
        ((string= system-type "windows-nt") "Windows")
        ((string= system-type "darwin") "Darwin")
        (t system-type)))

(defun teamake--find-root (path filename)
  "Look for the dominating FILENAME in PATH.

Look backward for FILENAME files and return the path to the topmost
file.  From the selected PATH first locate the dominating FILENAME,
then look for FILENAME files in parent directories."
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
      '())))

(defun teamake-build-root (&optional path)
  "Find the dominating topmost CMakeCache.txt file in BUILD-PATH."
  (interactive (list (read-directory-name "Select path within build tree: " default-directory '() t)))
  (teamake--find-root path "CMakeCache.txt"))

(defun teamake-build-tree-p (path)
  "Determine if PATH is part of a build tree."
  (not (string= (teamake-build-root path) "")))

(defun teamake-select-tree (prompt predicate initial mustmatch)
  "PROMPT for selection of tree."
  (let ((root (teamake-get-root (read-directory-name prompt initial '() mustmatch) predicate "")))
    (while (and (string= root "") (not mustmatch))
      (message "No root!")
      (setq root (teamake-get-root (read-directory-name prompt initial '() mustmatch) predicate "")))
    root))

(defun teamake-code-root (&optional source-path)
  "Find the dominating topmost CMakeLists.txt file in SOURCE-PATH."
  (interactive
   (list
    (read-directory-name "Select path within code tree: " default-directory '() t)))
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
    (cond (teamake-custom-project-name teamake-custom-project-name)
          ((and (teamake-code-tree-p path) prediction)
           (teamake--name-from-code-tree path))
          ((and (teamake-build-tree-p path) prediction)
           (teamake--name-from-build-tree path))
          (t "<No project name>"))))

(defun teamake-get-root (path &optional predicate default-value)
  "Return deduced root from PATH.

If PATH needs to be code-tree or build-tree set PREDICATE to either
`teamake-code-tree-p' or `teamake-build-tree-p'."

  (let* ((predicate (or predicate (lambda (path) t)))
         (prediction (funcall predicate path)))
    (cond ((and (teamake-code-tree-p path) prediction)
           (teamake--find-root path "CMakeLists.txt"))
          ((and (teamake-build-tree-p path) prediction)
           (teamake--find-root path "CMakeCache.txt"))
          (t (or default-value "<No project path>")))))

(defun teamake-tree-p (path &optional predicate)
  "Return if PATH is a recognized tree.

PREDICATE is testing for type of tree."

  (let* ((predicate (or predicate (lambda (path) t)))
         (prediction (funcall predicate path)))
    (cond ((and (teamake-code-tree-p path) prediction)
           (teamake-code-tree-p path))
          ((and (teamake-build-tree-p path) prediction)
           (teamake-build-tree-p path))
          (t '()))))

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

(defun teamake--name-from-build-tree (path)
  "Return project name of the project within BUILD-PATH."
  (if (teamake-build-tree-p path)
      (let* ((cache-file (file-name-concat (teamake-build-root path) "CMakeCache.txt"))
             (content (with-temp-buffer
                        (insert-file-contents cache-file)
                        (buffer-string))))
        (if (string-match "CMAKE_PROJECT_NAME:STATIC=\\(.+\\)" content)
            (match-string 1 content)
          "<No project>"))))

(defun teamake-heading (text &optional path predicate)
  "Create a heading to use in transient with TEXT at PATH.

PREDICATE can be one of `teamake-code-tree-p' or `teamake-build-tree-p'"
  (concat (format "%s " (propertize text 'face 'teamake-heading))
          (if path
              (concat (propertize (teamake-get-name path predicate)
                                  'face 'teamake-name)
                      (format " (%s)"
                              (propertize (teamake-get-root path predicate)
                                          'face 'teamake-path))))))

(defun teamake--format-as-variable-expression (variable)
  "Make VARIABLE as a valid variable expression"
  (let ((prefix "${")
        (suffix "}"))
    (if (s-starts-with? prefix variable) (setq prefix ""))
    (if (s-ends-with? suffix variable) (setq suffix ""))
    (format "%s%s%s" prefix variable suffix)))

(defun teamake--get-project-configuration-variable-expression (scope variable)
  "Return VARIABLE from project-configuration using SCOPE."
  (seq-find (lambda (val)
              (s-starts-with? (teamake--format-as-variable-expression variable) val))
            (teamake-get-project-configuration-data scope 'teamake)))

;; (setq teamake--project-configurations
;;       (list "scope" (list 'prefix '("${project}=\"Test project\""))))
;; (teamake--write-project-configurations)
;; (teamake--get-project-configuration-variable-expression "ased" "foobar")

(defun teamake--get-value-from-expression (expression)
  "Return the value of EXPRESSION.

EXPRESSION should be formatted as property=value, $property=value, ${property}=value."
  (let ((idx (string-match "=" expression)))
    (if idx (setq idx (+ idx 1)))
    (substring expression idx)))

(defun teamake--get-project-configurations-data (scope prefix)
  (teamake--read-project-configurations)
  (let ((existing (plist-get teamake--project-configurations scope 'string=)))
    (plist-get existing prefix)))

(defun teamake--set-project-configurations-data (scope prefix values)
  "Update the PREFIX data at SCOPE with VALUES."
  (if (not (plist-member teamake--project-configurations scope 'string=))
      (plist-put teamake--project-configurations scope (list prefix values))
    (let ((existing (plist-get teamake--project-configurations scope 'string=)))
      (plist-put existing prefix prefix values))))

(defun teamake-invoke-as-subprefix (scope prefix)
  "Save project configuration and invoke PREFIX at SCOPE."
  (teamake--write-project-configurations)
  (teamake-setup-from-scope scope prefix))

(defun teamake-setup-from-scope (scope prefix)
  "Read the project configurations file and initialize PREFIX at SCOPE."
  (transient-setup
   prefix '() '()
   :scope scope
   :value (teamake--get-project-configurations-data scope prefix)))



(provide 'teamake-base)
;;; teamake-base.el ends here
