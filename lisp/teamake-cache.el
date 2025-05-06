
;;; Code:

(require 'teamake-base)
(require 'teamake-process)
(require 'teamake-cmake-help)

(defvar teamake-cache--variable-match
  "^\\([-a-z0-9_]+\\):\\([a-z]+?\\)=\\(.?+\\)"
  "Regexp for matching cmake cache variable.")

(defun teamake-cache--set (path name &optional value previous type)
  "Set the cache variable NAME in PATH to VALUE.

If no value is provided, it is assumed that the variable should be unset."
  (let ((msg (format "CMake cache variable \"%s\"" name)))
    (cond (value
           (teamake-cmake-process-file path path (format "-D%s:%s=%s" name type value))
           (message "%s set to \"%s\"" msg value))
          (t
           (teamake-cmake-process-file path path (concat "-U" name))
           (message "%s removed" msg)))))

(defun teamake-cache--parse-variable-line (line)
  "Parse the CMakeCache LINE to a property list.

If the input type is a BOOL the value is converted to ON or OFF
where the values ON and TRUE in either capitalization is interpret
as ON, all other values are interpret as OFF."
  (save-match-data
    (string-match teamake-cache--variable-match line)
    (let ((name (match-string 1 line))
          (type (match-string 2 line))
          (value (match-string 3 line)))
    (list :name  name
          :type  type
          :value (cond ((string= type "BOOL")
                        (if (or (string= (upcase value) "ON")
                                (string= (upcase value) "TRUE"))
                            "ON"
                          "OFF"))
                       (t value))))))

(defun teamake-cache--list-cache-variables (path)
  "Read variables from CMakeCache.txt file located under PATH."
  (if (not (teamake-build-tree-p path))
      (user-error "No build root"))

  (let* ((cache-file
          (file-name-concat (teamake-get-root path 'teamake-build-tree-p)
                            "CMakeCache.txt"))
         (contents (with-temp-buffer
                     (insert-file-contents cache-file)
                     (buffer-string))))
    (seq-map 'teamake-cache--parse-variable-line
             (re-seq teamake-cache--variable-match contents))))

(defun teamake-cache--prompt-for-value (type name &optional default)
  "Prompt user for valid value according to the selected TYPE.

NAME is used for prompting user for the value."
  (let ((prompt (format "%s " name)))
    (cond ((string= type "STRING") (read-string prompt default))
          ((string= type "BOOL") (completing-read prompt '("ON" "OFF") '() t default))
          ((string= type "PATH") (read-directory-name prompt (or default default-directory) '() t))
          ((string= type "FILEPATH") (read-file-name prompt '() default t))
          ((string= type "STATIC") (read-string (format "%s (should not be changed) " prompt) default))
          ((string= type "INTERNAL") (read-string (format "%s (CMAKE INTERNAL VARIABLE - DO NOT TAMPER WITH) " prompt) default))
          (t (user-error "Unknown input type \"%s\"" type)))))

(defvar teamake-cache--user-creatable-variable-types '("BOOL" "STRING" "PATH" "FILEPATH"))
(defun teamake-cache--add-variable (build-path name value &optional type)
  "Add the cache variable NAME to the CMakeCache in BUILD-PATH with VALUE.

If no type is provided, CMake will default it to \"UNINITIALIZED\"."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (name (read-string "Name " ))
          (type (completing-read "Type " teamake-cache--user-creatable-variable-types '() t '()
                                'teamake-cache--user-creatable-variable-types))
          (value (teamake-cache--prompt-for-value type name)))
     (list build-path name value type)))
  (teamake-cache--set build-path name value '() type))

(defun teamake-cache--remove-variable (build-path name)
  "Remove the cache variable NAME from the CMakeCache in BUILD-PATH."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (variables (teamake-cache--list-cache-variables build-path))
          (names (seq-map 'teamake-cache--display-cmake-cache-variable variables))
          (name (teamake-cache--get-name-from-display (completing-read "Variable to remove " names '() t))))
     (list build-path name)))
  (teamake-cache--set build-path name))

(defun teamake-cache--display-cmake-cache-variable (variable)
  "Display VARIABLE according to settings."
  (if (teamake-cache--list-details-p)
      (format "%s (%s:%s)"
              (plist-get variable :name)
              (plist-get variable :type)
              (plist-get variable :value))
    (format "%s" (plist-get variable :name))))

(defun teamake-cache--get-name-from-display (display-variable)
  "Get name from DISPLAY-VARIABLE according to settings.

Used in combination with `teamake-cache--display-cmake-cache-variable' for
when prompting user for selecting a variable to transform back the selection
to the name of the variable."
  (if (teamake-cache--list-details-p)
      (seq-first (split-string display-variable " " '() t))
    display-variable))

(defun teamake-cache--modify-variable (build-path name)
  "Remove the cache variable NAME from the CMakeCache in BUILD-PATH."
  (interactive
   (let* ((build-path (teamake-get-root default-directory 'teamake-build-tree-p))
          (variables (teamake-cache--list-cache-variables build-path))
          (names (seq-map 'teamake-cache--display-cmake-cache-variable variables))
          (name (teamake-cache--get-name-from-display (completing-read "Variable to change " names '() t))))
     (list build-path name)))

  (let* ((variables (teamake-cache--list-cache-variables build-path))
         (variable (seq-find (lambda (var) (string= (plist-get var :name) name)) variables))
         (previous (plist-get variable :value))
         )
    (if (not variable)
        (user-error "No cache variable \"%s\" found" name))
    
    (teamake-cache--set build-path name previous
                        (teamake-cache--prompt-for-value
                         (plist-get variable :type)
                         name
                         previous))))

(defun teamake-cache--help-variables (variable)
  "Call for the cmake help for VARIABLE."
  (interactive
   (let* ((variables (teamake-cmake-shell-command-to-lines '() "--help-variable-list"))
          (variable (completing-read "CMake Variable " variables '() t)))
     (list variable)))
  (teamake-process-invoke-cmake-in-root
   default-directory
   "--help-variable"
   variable
  ))

(defun teamake-cache--arguments ()
  (transient-args 'teamake-cache))

(defun teamake-cache--list-details-p ()
  (interactive)
  (let ((args (teamake-cache--arguments)))
    (seq-find (lambda (s) (string= s "--list-details")) args)))

(transient-define-prefix teamake-cache (build-path)
  "Manage CMake cache entries."
  [:description
   (lambda ()
     (concat (teamake-heading "CMake cache management"  (transient-scope) 'teamake-build-tree-p)
             "\n"))
   ["Flags"
    ("-d" "List details about variables" ("-d" "--list-details"))]
   ]
  ["Variables"
   ("a" "Add"    teamake-cache--add-variable :transient t)
   ("m" "Modify" teamake-cache--modify-variable :transient t)
   ("x" "Remove" teamake-cache--remove-variable :transient t)
   ]
   ["Help"
    ("h" "CMake variable" teamake-cmake-help--variable)]
  (interactive (list (teamake-build-root default-directory)))
  (transient-setup 'teamake-cache '() '() :scope build-path)
  )

(provide 'teamake-cache)
;;; teamake-cache.el ends here
