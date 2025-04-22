
;;; Code:

(require 'cmake-faces)

(defun cmake-return-value-or-default (variable default-value)
  "Return the value of VARIABLE if it is non empty, otherwise DEFAULT-VALUE."
  (if (cmake-variable-not-set variable)
      default-value
    variable))

(defun cmake-variable-not-set (variable)
  "Determine if the VARIABLE is set or not."
  (or (eq variable '()) (string= variable "")))

(defun cmake-project--find-root (path filename)
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

(defun cmake-project-build-root (&optional build-path)
  "Find the dominating topmost CMakeCache.txt file in BUILD-PATH."
  (interactive (list (read-directory-name "Select path within build tree: " default-directory '() t)))
  (cmake-project--find-root build-path "CMakeCache.txt"))

(defun cmake-project-build-root-p (path)
  "Search upward for CMakeCache.txt files and see if PATH is root."
  (string= path (cmake-project-build-root path)))

(defun cmake-project-build-tree-p (path)
  "Determine if PATH is part of a build tree."
  (not (string= (cmake-project-build-root path) "")))

(defun cmake-project-code-root (&optional source-path)
  "Find the dominating topmost CMakeLists.txt file in SOURCE-PATH."
  (interactive (list (read-directory-name "Select path within code tree: " default-directory '() t)))
  (cmake-project--find-root source-path "CMakeLists.txt"))

(defun cmake-project-code-root-p (path)
  "Search upward for CMakeLists.txt files and see if PATH is root."
  (string= path (cmake-project-code-root path)))

(defun cmake-project-code-tree-p (path)
  "Determine if PATH is part of a code tree."
  (not (string= (cmake-project-code-root path) "")))

;;; TODO: Parse CMakeLists.txt and check for project() to read project name
(defun cmake-project--name-from-code-tree (&optional source-path)
  "Return directory name of the project root or SOURCE-PATH."
  (interactive
   (list (cmake-project-code-root default-directory)))

  (let ((directory (cmake-project-code-root source-path)))
    (if (string= directory "")
        "<No project>"
      (file-relative-name directory (file-name-parent-directory directory)))))


;;; TODO: Parse CMakeCache.txt and check for CMAKE_PROJECT_NAME:STATIC= to read project name
(defun cmake-project--name-from-build-tree (build-path)
  "Return project name from the build root of BUILD-PATH."
  (let ((directory (cmake-project-build-root build-path)))
    (if (string= directory "")
        "<No project>"
      (file-relative-name directory (file-name-parent-directory directory)))))



(defun cmake-project--code-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'cmake-project-heading))
          (propertize (cond ((cmake-project-code-tree-p path) (cmake-project--name-from-code-tree path))
                            (t "<No code tree>"))
                      'face 'cmake-project-name)
          (format " (%s)"
                  (propertize (if (cmake-project-code-tree-p path)
                                  path
                                "<Empty>")
                              'face 'cmake-project-path))))

(defun cmake-project--build-tree-heading (text path)
  "Create a heading for use in transient with TEXT for PATH."
  (concat (format "%s " (propertize text 'face 'cmake-project-heading))
          (propertize (cond ((cmake-project-build-tree-p path) (cmake-project--name-from-build-tree path))
                            (t "<No build tree>"))
                      'face 'cmake-project-name)
          (format " (%s)"
                  (propertize (if (cmake-project-build-tree-p path)
                                  path
                                "<Empty>")
                              'face 'cmake-project-path))))

(defun cmake-project--heading-code-tree (heading-text code-path)
  "Return a propertized heading from HEADING-TEXT and CODE-PATH as CMake project."
  (concat (format "%s " (propertize heading-text 'face 'cmake-project-heading))
          (cmake-project--code-path-propertized code-path)
          "\n"))

(defun cmake-project--code-path-propertized (path)
  "Return a propertized information about PATH as CMake project."
  (concat (propertize (cmake-project--name-from-code-tree path) 'face 'cmake-project-name)
          (format " (%s)" (propertize (if (string= path "") "<Empty>" path)
                                      'face 'cmake-project-path))))

(provide 'cmake-base)
;;; cmake-base.el ends here
