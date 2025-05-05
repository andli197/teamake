
;;; Code:

(require 'teamake-base)
(require 'teamake-process)

(defvar teamake-cache--variable-match
  "^\\([-a-z0-9_]+\\):\\([a-z]+?\\)=\\(.?+\\)"
  "Regexp for matching cmake cache variable.")

(defun teamake-cache--set (path name &optional value type)
  "Set the cache variable NAME in PATH to VALUE.

If no value is provided, it is assumed that the variable should be unset."
  (if (not value)
      (teamake-cmake-process-file path path (concat "-U" name))
    (teamake-cmake-process-file path path (format "-D%s:%s=%s" name type value))))

(defun teamake-cache--list-variables (path)
  "Read variables from CMakeCache.txt file located under PATH."
  (if (not (teamake-build-tree-p path))
      (user-error "No build root"))

  (let* ((cache-file
          (file-name-concat (teamake-get-root path 'teamake-build-tree-p)
                            "CMakeCache.txt"))
         (contents (with-temp-buffer
                     (insert-file-contents cache-file)
                     (buffer-string))))
    (seq-map (lambda (line)
               (save-match-data
                 (string-match teamake-cache--variable-match line)
                 (list :name  (match-string 1 line)
                       :type  (match-string 2 line)
                       :value (match-string 3 line))))
             (re-seq teamake-cache--variable-match contents))))


(defvar teamake-cache--variable-types '("BOOL" "STRING" "PATH" "FILEPATH" "STATIC"))

(defun teamake-cache--prompt-user-for-value (type name &optional default)
  "Prompt user for valid value according to the selected TYPE.

NAME is used for prompting user for the value."
  (let ((prompt (format "%s " name)))
    (cond ((string= type "STRING") (read-string prompt default))
          ((string= type "BOOL") (completing-read prompt '("ON" "OFF") '() t default))
          ((string= type "PATH") (read-directory-name prompt (or default default-directory) '() t))
          ((string= type "FILEPATH") (read-file-name prompt '() default t))
          ((string= type "STATIC") (read-string (format "%s (should not be changed) " prompt) default))
          ((string= type "INTERNAL") (read-string (format "%s (CMAKE INTERNAL VARIABLE) " prompt) default))
          (t (user-error "Unknown input type \"%s\"" type)))))

(defun teamake-cache--add-variable (build-path name value &optional type)
  "Add the cache variable NAME to the CMakeCache in BUILD-PATH with VALUE.

If no type is provided, CMake will default it to \"UNINITIALIZED\"."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (name (read-string "Name " ))
          (type (completing-read "Type " teamake-cache--variable-types '() t '()
                                'teamake-cache--variable-types))
          (value (teamake-cache--prompt-user-for-value type name)))
     (list build-path name value type)))
  (teamake-cache--set build-path name value type))

(defun teamake-cache--remove-variable (build-path name)
  "Remove the cache variable NAME from the CMakeCache in BUILD-PATH."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (variables (teamake-cache--list-variables build-path))
          (names (seq-map (lambda (var) (plist-get var :name)) variables))
          (name (completing-read "Variable to remove " names '() t)))
     (list build-path name)))
  (teamake-cache--set build-path name))

(defun teamake-cache--modify-variable (build-path name)
  "Remove the cache variable NAME from the CMakeCache in BUILD-PATH."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (variables (teamake-cache--list-variables build-path))
          (names (seq-map (lambda (var) (plist-get var :name)) variables))
          (name (completing-read "Variable to change " names '() t)))
     (list build-path name)))

  (let* ((variables (teamake-cache--list-variables build-path))
         (variable (seq-find (lambda (var) (string= (plist-get var :name) name)) variables))
         )
    (if (not variable)
        (user-error "No cache variable \"%s\" found" name))
    
    (teamake-cache--set build-path name
                        (teamake-cache--prompt-user-for-value
                         (plist-get variable :type)
                         name
                         (plist-get variable :value)))))


(transient-define-prefix teamake-cache (build-path)
  "Manage CMake cache entries."
  [:description
   (lambda ()
     (concat (teamake-heading "CMake cache management"  build-path 'teamake-build-tree-p)
             "\n"))
   ["Variables"
    ("a" "Add"    teamake-cache--add-variable)
    ("m" "Modify" teamake-cache--modify-variable)
    ("x" "Remove" teamake-cache--remove-variable)
    ]
   ]
  (interactive (list (teamake-build-root default-directory)))
  (transient-setup 'teamake-cache '() '() :scope build-path)
  )

(provide 'teamake-cache)
;;; teamake-cache.el ends here
