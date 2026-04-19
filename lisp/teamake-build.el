;;; teamake-build --- CMake build for teamake
;;;
;;; Commentary:
;;;
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-preset)

(defvar teamake-build--cache-variable-regexp
  "^\\([-a-zA-Z0-9_]+\\):?\\([a-zA-Z]?+\\)=\\(.?+\\)"
  "Regexp for matching cmake cache variable.")

(defun teamake-build--preset-to-values (preset)
  "Parse all values from current build PRESET."
  (let ((values '()))
    (if (plist-member preset :binaryDir)
        (add-to-list 'values (format "-B=%s" (plist-get preset :binaryDir))))
    (if (eq (plist-get preset :cleanFirst) t)
        (add-to-list 'values "--clean-first"))
    (if (eq (plist-get preset :verbose) t)
        (add-to-list 'values "--verbose"))
    (if (plist-member preset :jobs)
        (add-to-list 'values (format "--parallel=%i"
                                     (plist-get preset :jobs))))
    (if (plist-member preset :resolvePackageReferences)
        (add-to-list 'values (format "--resolve-package-references=%s"
                                     (plist-get preset :resolvePackageReferences))))
    (if (plist-member preset :targets)
        (seq-do
         (lambda (target)
           (add-to-list 'values (format "--target=%s" target)))
         (teamake-preset--get-property-as-list preset :targets)))
    (if (plist-member preset :configuration)
        (add-to-list 'values (format "--config=%s"
                                     (plist-get preset :configuration))))
    (if (plist-member preset :nativeToolOptions)
        (warn "NativeToolOptions not yet supported for build presets.  Settings '%s' are ignored"
              (plist-get preset :nativeToolOptions)))
    values))

(transient-define-suffix teamake-build--select-preset ()
  :description "Read from preset"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-build--project-from-binary-dir binary-dir))
         (preset (teamake-preset-select-from-project project :buildPresets))
         (values (teamake-build--preset-to-values preset)))
    (teamake-set-current-values 'teamake-build project values)
    (transient-setup 'teamake-build '() '()
                     :scope binary-dir
                     :value values)))

(defun teamake-build--project-contains-p (project build-tree)
  "Determine if PROJECT is matching BUILD-TREE."
  (or (teamake-build--build-path-matches-p
       (teamake-get-current-values 'teamake-build project) build-tree)
      (teamake-build--build-path-matches-p
       (teamake-get-current-values 'teamake-configure project) build-tree)))

(defun teamake-build--parse-variable-line (line)
  "Parse a CMakeCache line.

If the type is a BOOL the value is converted to ON or OFF
where the values ON and TRUE in either capitalization or lowercase
is interpret as ON, all other values are interpret as OFF."
  (save-match-data
    (string-match teamake-build--cache-variable-regexp line)
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

(defun teamake-build--parse-cmake-cache (build-tree)
  "Parse all options in the form <NAME>(:[TYPE])=<VALUE> from BUILD-TREE.

Return the options as an property list."
  (let* ((cache-file (file-name-concat build-tree "CMakeCache.txt"))
         (contents (and (file-exists-p cache-file)
                        (with-temp-buffer
                          (insert-file-contents cache-file)
                          (buffer-string))))
         result '())
    (if contents
        (seq-do
         (lambda (line)
           (if (string-match teamake-build--cache-variable-regexp line)
               (add-to-list 'result (teamake-build--parse-variable-line line))))
         (split-string contents "\n")))
    result))

(defun teamake-build--cache-objects--get-variable (parsed-cache name)
  "Extract the cache object value from PARSED-CACHE with :name NAME."
  (plist-get (seq-find
              (lambda (c)
                (string= (plist-get c :name) name))
              parsed-cache)
             :value))

(defun teamake-build--read-source-dir-from-cmake-cache (build-tree &optional cache)
  "Read the <PROJECT_NAME>_SOURCE_DIR cache variable from BUILD-TREE."
  (let* ((cache (or cache (teamake-build--parse-cmake-cache build-tree)))
         (project-name (teamake-build--cache-objects--get-variable cache "CMAKE_PROJECT_NAME")))
    (teamake-build--cache-objects--get-variable cache (format "%s_SOURCE_DIR" project-name))))

(defun teamake-build--read-project-name-from-cmake-cache (build-tree)
  "Read CMAKE_PROJECT_NAME cache variable from BUILD-TREE."
  (let ((cache (teamake-build--parse-cmake-cache build-tree)))
    (teamake-build--cache-objects--get-variable cache "CMAKE_PROJECT_NAME")))

(defun teamake-build--project-name-from-build-tree (build-tree)
  "Present name of BUILD-TREE.

Fetch project name among projects if any project has an association to
BUILD-TREE.  If no such association exists, try and parse project name
from CMakeCache.txt.  If no CMakeCache.txt could be located, return a
default project name."
  (or (plist-get (teamake--project-from-path build-tree) :name)
      (teamake-build--read-project-name-from-cmake-cache build-tree)
      "Undetermined project"))

(defun teamake-build--read-build-targets (build-path)
  "Read available build targets from BUILD-PATH.

Assuming the generator can provide available targets using the 'help'
target in the build tree."
  (let ((targets '()))
    (seq-do
     (lambda (line)
       (save-match-data
         (if (string-match "\\(.+\\):.*" line)
             (add-to-list 'targets (match-string 1 line)))))
     (apply #'teamake-cmake-command-to-lines
            (list "--build" build-path "--target help")))
    targets))

(transient-define-suffix teamake-build--do-build-current ()
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-build--project-from-binary-dir binary-dir))
         ((values (teamake-get-current-values 'teamake-build project)))
         (expanded-values (teamake-preset-expand-macros-from-project project values)))
    (teamake-process-invoke-cmake project
                                  "-B"
                                  binary-dir
                                  "--build"
                                  expanded-values)))

(transient-define-suffix teamake-build--do-build-preset ()
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-build--project-from-binary-dir binary-dir))
         (preset (teamake-preset--get-current project :buildPresets)))
    (teamake-process-invoke-cmake project
                                  "--build"
                                  (format "--preset=%s" (plist-get preset :name))
                                  )))

(defun teamake-build--get-cache-value-from-name (cache name)
  "Return the :value from CACHE where :name matches NAME."
  (plist-get
   (seq-find
    (lambda (item) (string= (plist-get item :name) name))
    cache)
   :value))

(defun teamake-build--values-from-cache (path)
  "If PATH is determined to be a CMake build tree, parse the cache values."
  (let* ((cache (teamake-build--parse-cmake-cache path))
         (project-name (teamake-build--get-cache-value-from-name cache "CMAKE_PROJECT_NAME"))
         (source-dir (teamake-build--get-cache-value-from-name cache (format "%s_SOURCE_DIR" project-name)))
         (value '()))
    (if source-dir
        (add-to-list 'source-dir (format "-S=%s" source-dir)))
    value))

(defun teamake-build--project-from-binary-dir (binary-dir)
  "Try to read source-dir from BINARY-DIR and fetch project for that."
  (interactive)
  (let* ((source-dir (teamake-build--read-source-dir-from-cmake-cache binary-dir))
         (project (teamake--project-from-path source-dir)))
    (unless project
      (user-error "No matching project for binary dir '%s' found" binary-dir))
    project))

(defun teamake-build--setup-transient-from-path (binary-dir)
  "Setup `teamake-build' transient from BINARY-DIR."
  (interactive)
  (let* ((source-dir (teamake-build--read-source-dir-from-cmake-cache binary-dir))
         (matched-project (teamake--project-from-path source-dir)))
    (transient-setup 'teamake-build '() '()
                     :scope binary-dir
                     :value (teamake-get-current-values 'teamake-build matched-project))))

(defun teamake-build--project-name-p (entry)
  (string= (plist-get entry :name) "CMAKE_PROJECT_NAME"))

(defun teamake-build--source-dir-p (name entry)
  (string= (plist-get entry :name) (format "%s_SOURCE_DIR" (plist-get name :value))))

(defun teamake-build--source-dir-from-build-tree (build-tree)
  "Try to resolve code-dir from BUILD-TREE.

Either the BUILD-TREE contains a CMakeCache variable specifying
code-tree, or a configured project has a reference to BUILD-TREE."
  (let* ((cache (teamake-build--parse-cmake-cache build-tree))
         (name (teamake-build--get-cache-value-from-name cache "CMAKE_PROJECT_NAME"))
         (code (teamake-build--get-cache-value-from-name cache (format "%s_SOURCE_DIR" name))))
    (and code (file-exists-p code) code)))

(transient-define-suffix teamake-build--build-current ()
  :description "Build current"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-build--project-from-binary-dir binary-dir))
         (values (transient-args 'teamake-build)))
    (teamake-set-current-values 'teamake-build project values)
    (teamake-build--do-build-current)))

(transient-define-suffix teamake-build--build-preset ()
  :description "Build preset"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-build--project-from-binary-dir binary-dir)))
    (teamake-preset-select-from-project project :buildPresets)
    (teamake-build--do-build-preset)))

(transient-define-suffix teamake-build--get-code-dir ()
  (interactive)
  (let ((binary-dir (transient-scope)))
    (teamake-build--read-source-dir-from-cmake-cache binary-dir)))

(transient-define-suffix teamake-build--binary-dir ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (format "Binary dir (%s)" (propertize (transient-scope) 'face 'transient-value)))
  (interactive)
  (let* ((current-binary-dir (transient-scope))
         (binary-dir (teamake--select-binary-dir current-binary-dir))
         (source-dir (teamake-build--source-dir-from-build-tree binary-dir))
         (project (teamake--project-from-path source-dir))
         (current-values (teamake-get-current-values 'cmake-build project))
         (new-values (list (format "-S=%s" source-dir))))
    (seq-do
     (lambda (value)
       (if (not (string-match "-S=.+" value))
           (add-to-list 'new-values value)))
     current-values)
    (teamake-set-current-values 'cmake-build project new-values)
    (transient-setup 'teamake-build '() '()
                     :scope binary-dir
                     :value new-values)
    ))

(transient-define-suffix teamake-build--source-dir ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (format "Source dir (%s)"
            (propertize (or (teamake-build--get-code-dir) "")
                        'face 'transient-value)))
  (interactive)
  (let ((values (transient-args 'teamake-build))
        (source-dir (teamake--select-source-dir (transient-arg-value "-S=" values))))
    (transient-setup 'teamake-build '() '()
                     :scope (transient-scope)
                     :value (seq-map
                             (lambda (value)
                               (if (string-match "-S=.+")
                                   (format "-S=" source-dir)
                                 value))
                             values))))



;;;###autoload
(transient-define-prefix teamake-build (build-tree)
  [:description
   ;; "CMake Build"
   (lambda () (format "%s %s (%s)\n"
                      (propertize "CMake Build" 'face 'teamake-heading)
                      (propertize (or (teamake-build--project-name-from-build-tree (transient-scope))
                                      "Undetermined project")
                                  'face 'teamake-project-name)
                      (propertize (transient-scope) 'face 'teamake-path)))

   ("S" teamake-build--source-dir)
   ]
  ["Flags"
   ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")
   ]
  ["Options"
   ("pr" teamake-build--select-preset)
   ("pa" "Parallel builds, using this amount of jobs" "--parallel="
    :prompt "Parallel builds: "
    :reader transient-read-number-N+)
   ("t" "Build target instead of default targets" "--target="
    :prompt "Targets: "
    :choices (lambda () (teamake-build--read-build-targets (transient-scope)))
    :multi-value repeat)
   ("c" "For multi configuration tools" "--config="
    :prompt "Configuration: "
    :choices ("Release" "Debug" "RelWithDebInfo"))
   ("r" "Restore/resolve package references during build"
    "--resolve-package-references="
    :prompt "Select package restore/resolve: "
    :choices ("on" "only" "off"))
   ;; ("n" "Native tool option" "--"
   ;;  :class transient-option
   ;;  :prompt "Options: ")
   ]
  ["Do"
   ("xx" teamake-build--build-current)
   ("xp" teamake-build--build-preset)
   ]
  (interactive
   (list (or (teamake--find-root default-directory "CMakeCache.txt")
             (directory-file-name default-directory))))
  (teamake-build--setup-transient-from-path build-tree))



(provide 'teamake-build)
;;; teamake-build.el ends here
