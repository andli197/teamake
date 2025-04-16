
;;; Code:

(defun cmake-return-value-or-default (variable default-value)
  "Return the value of VARIABLE if it is non empty, otherwise DEFAULT-VALUE."
  (if (or (string= variable "") (eq variable '()))
      default-value
    variable))

(defun cmake-variable-not-set (variable)
  "Determine if the VARIABLE is set or not."
  (or (eq variable '()) (string= variable "")))

(defun cmake-code-root (&optional source-path)
  "Return absolute path to project root or SOURCE-PATH.

Look backward for CMakeLists.txt files and return the path to the topmost
file.  From the selected SOURCE-PATH first locate the dominating CMakeLists.txt
file, then look for CMakeLists.txt files in parent directories.
This method is not 100% working but good enough for now."
  (interactive
   (list (read-directory-name "Select project: " default-directory '() t)))
  
  (let* ((directory (or source-path default-directory))
         (topmost-cmake-file (locate-dominating-file directory "CMakeLists.txt"))
         (candidate topmost-cmake-file))
    (while (file-exists-p candidate)
      (setq topmost-cmake-file candidate
            directory (file-name-parent-directory directory)
            candidate (file-name-concat directory "CMakeLists.txt")))
    (directory-file-name directory)))

(defun cmake-project-name (&optional source-path)
  "Return directory name of the project root or SOURCE-PATH."
  (interactive
   (list (read-directory-name "Select project: " default-directory '() t)))

  (let ((directory (cmake-code-root source-path)))
    (file-relative-name directory (file-name-parent-directory directory))))

(provide 'cmake-base)
;;; cmake-base.el ends here
