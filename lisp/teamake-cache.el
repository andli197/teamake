
;;; Code:

(require 'teamake-base)
(require 'teamake-process)

(defvar teamake-cache--variable-match
  "\\([a-zA-Z0-9_]+\\):\\([A-Z]+\\)=\\(.+\\)"
  "Regexp for matching cmake cache variable.")

(defvar teamake-cache--extended-variable-match
  (concat "\\(.+\\)" (regexp-quote "\n") teamake-cache--variable-match)
  "Regexpt for matching cmake cache variable with help.")

(defvar teamake-cache--current-cache '()
  "List of currently configured cache variables.")

(defun teamake-cache--parse-existing (path)
  "Read cache variables from PATH as build root."
  (let ((output (teamake-cmake-shell-command-to-string (format "%s " path))))
    (re-seq teamake-cache--variable-match output)))

(defun teamake-cache--set (path name &optional value type)
  "Set the cache variable NAME in PATH to VALUE."
  (if (not value)
      (teamake-cmake-process-file path path (concat "-U" name))
    (teamake-cmake-process-file path path (format "-D%s:%s=%s" name type value))))

(defun teamake-cache--read-all (path)
  "Read exinst cache variables from PATH."
  (let ((output (teamake-cmake-shell-command-to-string
                 path "-LAH" "-N")))
    (seq-map
     'teamake-cache--to-plist
     (re-seq teamake-cache--extended-variable-match output))))

(defun teamake-cache--get (path name)
  "Read the value of CMake cache variable NAME from PATH."
  (let ((output (teamake-cache--read-all path)))
    (seq-find (lambda (l) (string= (plist-get l :name) name)) output)))

;; (teamake-cache--read-all "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/")
;; (teamake-cache--set "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/" "FOOBAR" "ON" "BOOL")
;; (teamake-cache--set "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/" "FOOBAR")
;; (teamake-cache--get "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/" "CMAKE_INSTALL_PREFIX")

(transient-define-infix 

(defun teamake-cache--select-name (ccache-entries)
  "Select the name from the CCACHE-ENTRIES."
  (seq-map (lambda (line)
             (string-match teamake-cache--variable-match line)
             (match-string 1 line))
           ccache-entries))

(defun teamake-cache--to-plist (entry)
  "Transform ENTRY from CMakeCache line to a property list.

The entry must follow naming pattern // Heading text\n<NAME>:<TYPE>=<VALUE>.
If entry cannot be matched an empty list is returned."
  (if (string-match teamake-cache--extended-variable-match entry)
      (list :description (substring (match-string 1 entry) 3)
            :name (match-string 2 entry)
            :type (match-string 3 entry)
            :value (match-string 4 entry))
    '()))


(defun teamake-cache--select-variable (path)
  "Parse cmake cache variables from PATH and present user with selection."
  (let* ((cmake-cache-entries (teamake-cache--parse-existing path))
         (variable-name
          (completing-read "Cache variable: " (teamake-cache--select-name cmake-cache-entries) '() t))
         ((match (seq-find (lambda (line) (string-match variable-name line)) cmake-cache-entries '()))))
    match))

(defun teamake-cache--list-variables (path)
  "Read all variables from PATH and list them."
  (interactive)
  (let ((cmake-cache-entries (teamake-cache--parse-existing path)))
    (setq teamake-cache--current-cache (seq-map 'teamake-cache--to-plist cmake-cache-entries))))

;; (teamake-cache--select-variable "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/")
;; (teamake-cache--list-variables "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/")
;; (teamake-cache--parse-existing "c:/Arbetsfiler/Andreas/project/chess/chess-build/MultiNinja/")
  
(transient-define-prefix teamake-cache (build-path)
  "Manage CMake cache entries."
  [:description
   (lambda ()
     (teamake--build-tree-heading "Manage cache for " (transient-scope)))
   ("b" "bbbbbbb" "--b")

   ("x" "Remove variable")
   ]
  [:description
   (lambda ()
     (propertize "Variables" 'face 'teamake-heading))
   ;; ("a" "Add variable" teamake-cache--add-variable)
   ("m" "Modify variable" teamake-cache--modify-variable)
   ("x" "Remove variable" teamake-cache--remove-variable)
   ]
  (interactive (list (teamake-build-root default-directory)))
  (transient-setup 'teamake-cache '() '() :scope build-path)
  )

(provide 'teamake-cache)
;;; teamake-cache.el ends here
