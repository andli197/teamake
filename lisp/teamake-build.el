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

(defun teamake-build--read-project-name-from-cmake-cache (build-tree)
  "Read CMAKE_PROJECT_NAME cache variable from BUILD-TREE."
  (let ((cache (teamake-build--parse-cmake-cache build-tree)))
    (plist-get
     (seq-find (lambda (p)
                 (string= (plist-get p :name) "CMAKE_PROJECT_NAME"))
               cache)
     :value)))

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
         (values '()))
    (let ((v (teamake-build--get-cache-value-from-name cache (format "%s_SOURCE_DIR" project-name))))
      (if v (add-to-list 'values (format "-S=%s" v))))

    (add-to-list 'values (format "-B=%s" path))
    values))
    

(defun teamake-build--setup-transient-from-path (path)
  "Setup `teamake-build' transient from PATH.

Matching project using PATH for build to set all values."
  (interactive)

  (transient-setup 'teamake-build '() '()
                   :scope path
                   :value (or ;;(teamake-build--values-from-project path)
                              (teamake-build--values-from-cache path))))

(defun teamake-build--project-name-p (entry)
  (string= (plist-get entry :name) "CMAKE_PROJECT_NAME"))

(defun teamake-build--source-dir-p (name entry)
  (string= (plist-get entry :name) (format "%s_SOURCE_DIR" (plist-get name :value))))

(defun teamake-build--source-dir-from-build-tree (build-tree)
  "Try to resolve code-dir from BUILD-TREE.

Either the BUILD-TREE contains a CMakeCache variable specifying
code-tree, or a configured project has a reference to BUILD-TREE."
  (let* ((cache (teamake-build--parse-cmake-cache build-tree))
         (name-prop (seq-find 'teamake-build--project-name-p cache))
         (code-prop (seq-find (lambda (entry) (teamake-build--source-dir-p name-prop entry)) cache))
         (code (plist-get code-prop :value))
         (project (teamake--project-from-path build-tree)))
    (cond ((and code (file-exists-p code)) code)
          (project (plist-get project :source-dir))
          (t '()))))

(defun teamake-build--get-source-dir-from-values (values)
  (let ((source-dir '()))
    (seq-do
     (lambda (val)
       (save-match-data
         (if (string-match "-S=\\(.+\\)" val)
             (setq source-dir (match-string 1 val)))))
     values)
    source-dir
    )
  )

(transient-define-suffix teamake-build--can-deduce-code-dir ()
  (interactive)
  (or (teamake-build--source-dir-from-build-tree (transient-scope))
      teamake-build--source-dir))

(transient-define-suffix teamake-build--to-configure ()
  :description "To configuration"
  :if 'teamake-build--can-deduce-code-dir
  (interactive)

  (let* ((source-dir (teamake-build--can-deduce-code-dir))
         (project (teamake--project-from-path (directory-file-name source-dir))))
    (message "source-dir=%s" source-dir)
    (message "project=%s" project)
    (teamake-setup-transient
     'teamake-configure
     project)))

(transient-define-suffix teamake-build--build-current ()
  :description "Build current"
  :transient 'transient--do-recurse
  (interactive)
  (message "Building current...")
  )

(transient-define-suffix teamake-build--build-preset ()
  :description "Build preset"
  :transient 'transient--do-recurse
  (interactive)
  (message "Select preset and build...")
  )

(defvar teamake-build--source-dir '() "Current code directory")

(transient-define-infix teamake-build--source-dir-infix ()
  :transient 'transient--do-recurse
  :class transient-lisp-variable
  :prompt "Select source dir: "
  :reader 'transient-read-directory
  :variable 'teamake-build--source-dir)

(transient-define-suffix teamake-build--source-dir-suffix ()
  :transient 'transient--do-recurse
  :description (lambda () (format "Source dir (%s)"
                                  (propertize teamake-build--source-dir 'face 'transient-value)))
  (interactive)
  
  )

;;;###autoload
(transient-define-prefix teamake-build (build-tree)
  [:description
   ;; "CMake Build"
   (lambda () (format "%s %s\n"
                      (propertize "CMake Build" 'face 'teamake-heading)
                      (propertize (or (teamake-build--project-name-from-build-tree (transient-scope))
                                      "Undetermined project")
                                  'face 'teamake-project-name
                                  )))

   ("B" "Build tree" "-B="
    :always-read t
    :reader transient-read-directory)
   ("S" teamake-build--source-dir-suffix)
   ;; ("S" "Source dir" teamake-build--source-dir-infix)
   ;; ("C" " Code tree" "-S="
   ;;  :always-read t
   ;;  :reader transient-read-directory)
   ]
  ["Flags"
   ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")
   ]
  ["Options"
   ;; ("preset" teamake-build--select-preset
   ;;  :description "Read settings from a build preset"
   ;;  :transient t)
   ("p" "Parallel builds, using this amount of jobs" "--parallel="
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
   ("n" "Native tool option" "-- "
    :class transient-option
    :prompt "Options: ")
   ]
  ["Do"
   ("xx" teamake-build--build-current)
   ("xp" teamake-build--build-preset)
   ]
  ["Navigate"
   ("gc" teamake-build--to-configure)
   ("gt" "Test" "--to-test")
   ("gp" "Pack" "--to-pack")
   ]
  (interactive
   (list (or (teamake--find-root default-directory "CMakeCache.txt")
             (directory-file-name default-directory))))
  (teamake-build--setup-transient-from-path build-tree))



(provide 'teamake-build)
;;; teamake-build.el ends here
