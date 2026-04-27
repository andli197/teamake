;;; teamake-cmake-cache --- CMake operations for handling CMakeCache.txt and binary dir
;;; Commentary:
;;; Code:

(require 'teamake-core)

(defvar teamake-cmake-cache--variable-regexp
  "^\\([-a-zA-Z0-9_]+\\):?\\([a-zA-Z]?+\\)=\\(.?+\\)"
  "Regexp for matching cmake cache variable.")

(defun teamake-cmake-cache--parse-variable-line (line)
  "Parse a CMakeCache line.

If the type is a BOOL the value is converted to ON or OFF
where the values ON and TRUE in either capitalization or lowercase
is interpret as ON, all other values are interpret as OFF."
  (save-match-data
    (string-match teamake-cmake-cache--variable-regexp line)
    (let ((name (match-string 1 line))
          (type (match-string 2 line))
          (value (match-string 3 line)))
      (list :name name
            :type type
            :value (cond ((string= type "BOOL")
                          (if (or (string= (upcase value) "ON")
                                  (string= (upcase value) "TRUE"))
                              "ON"
                            "OFF"))
                         (t value))))))

(defun teamake-cmake-cache--parse-variables (binary-dir)
  "Parse all options in the form <NAME>(:[TYPE])=<VALUE> from BINARY-DIR.

Return the options as an property list."
  (let* ((cache-file (file-name-concat binary-dir "CMakeCache.txt"))
         (contents (and (file-exists-p cache-file)
                        (with-temp-buffer
                          (insert-file-contents cache-file)
                          (buffer-string))))
         result '())
    (if contents
        (seq-do
         (lambda (line)
           (if (string-match teamake-cmake-cache--variable-regexp line)
               (add-to-list 'result (teamake-cmake-cache--parse-variable-line line))))
         (split-string contents "\n")))
    result))

(defun teamake-cmake-cache--get-variable-value (parsed-cache name)
  "Extract the cache object value from PARSED-CACHE with :name NAME."
  (plist-get (seq-find
              (lambda (c)
                (string= (plist-get c :name) name))
              parsed-cache)
             :value))

(defun teamake-cmake-cache--project-name-from-cache (cache)
  "Return the value of cache variable CMAKE_PROJECT_NAME among CACHE."
  (teamake-cmake-cache--get-variable-value
   cache "CMAKE_PROJECT_NAME"))

(defun teamake-cmake-cache--source-dir-from-cache (cache cmake-project-name)
  "Return the value of cache variable <CMAKE-PROJECT-NAME>_SOURCE_DIR among CACHE."
  (teamake-cmake-cache--get-variable-value
   cache (format "%s_SOURCE_DIR" cmake-project-name)))

(defun teamake-cmake-cache--get-project-name (binary-dir &optional cache)
  "Read CMAKE_PROJECT_NAME cache variable from BINARY-DIR."
  (teamake-cmake-cache--project-name-from-cache
   (or cache (teamake-cmake-cache--parse-variables binary-dir))))

(defun teamake-cmake-cache--get-source-dir (binary-dir &optional cache)
  "Read the <PROJECT_NAME>_SOURCE_DIR cache variable from BINARY-DIR."
  (let ((cache (or cache (teamake-cmake-cache--parse-variables binary-dir))))
    (teamake-cmake-cache--source-dir-from-cache
     cache (teamake-cmake-cache--project-name-from-cache cache))))

(defun teamake-cmake-cache--project-from-binary-dir (binary-dir)
  "Try to read source-dir from BINARY-DIR and fetch project for that."
  (interactive)
  (let ((source-dir (teamake-cmake-cache--get-source-dir binary-dir)))
    (unless source-dir
      (user-error "'%s' does not seem to be a cmake binary-dir" binary-dir))
    (teamake--project-from-path source-dir)))

(defun teamake-deduce-project-name (binary-dir)
  "Deduce name of project from BINARY-DIR.

Search order:
1. Locate project matching the cache <PROJECT_NAME>_SOURCE_DIR and return :name.
2. Parse CMAKE_PROJECT_NAME from CMakeCache.txt under BINARY-DIR.
3. Return `teamake-undetermined-project-name'"
  (interactive)
  (or (plist-get (teamake-cmake-cache--project-from-binary-dir binary-dir) :name)
      (teamake-cmake-cache--get-project-name binary-dir)
      teamake-undetermined-project-name))

(defun teamake-binary-dir-heading (text binary-dir)
  "Return a propertized heading with TEXT for BINARY-DIR."
  (format "%s %s (%s)"
          (propertize text 'face 'teamake-heading)
          (propertize (teamake-deduce-project-name binary-dir) 'face 'teamake-project-name)
          (propertize binary-dir 'face 'teamake-path)))

(provide 'teamake-cmake-cache)
;;; teamake-cmake-cache.el ends here
