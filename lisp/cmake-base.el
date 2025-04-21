
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

(defun cmake-project-root (&optional source-path)
  "Return absolute path to project root of SOURCE-PATH.

Look backward for CMakeLists.txt files and return the path to the topmost
file.  From the selected SOURCE-PATH first locate the dominating CMakeLists.txt
file, then look for CMakeLists.txt files in parent directories.
This method is only working for projects containing CMakeLists-files throughout
the project hierarchy but that is a goo enough for now assumption.

If no source-path is provided `default-directory' is used."
  (interactive (read-directory-name "Select file within project: " default-directory '() t))

  (let* ((directory (or source-path default-directory))
         (topmost-cmake-file (locate-dominating-file directory "CMakeLists.txt"))
         (candidate topmost-cmake-file))
    (while (and candidate (file-exists-p candidate))
      (setq topmost-cmake-file candidate
            directory (file-name-parent-directory (file-name-directory topmost-cmake-file))
            candidate (file-name-concat directory "CMakeLists.txt")))

    (directory-file-name
     (if topmost-cmake-file
         (file-name-directory topmost-cmake-file)
       default-directory))))

(defun cmake-project-name (&optional source-path)
  "Return directory name of the project root or SOURCE-PATH."
  (interactive
   (list (cmake-project-root default-directory)))

  (let ((directory (cmake-project-root source-path)))
    (file-relative-name directory (file-name-parent-directory directory))))

(defun cmake-project-heading (heading-text source-path)
  "Return a propertized heading from HEADING-TEXT and SOURCE-PATH as CMake project."
  (concat (format "%s " (propertize heading-text 'face 'cmake-project-heading))
          (cmake-project-propertized source-path)
          "\n"))

(defun cmake-project-propertized (source-path)
  "Return a propertized information about SOURCE-PATH as CMake project."
  (concat (propertize (cmake-project-name source-path) 'face 'cmake-project-name)
          (format " (%s)" (propertize source-path 'face 'cmake-project-path))))

(provide 'cmake-base)
;;; cmake-base.el ends here
