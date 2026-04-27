;;; teamake-build --- CMake build for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-cmake-cache)
(require 'teamake-preset)
(require 'teamake-process)

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
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (preset (teamake-preset-select-from-project project :buildPresets))
         (values (teamake-build--preset-to-values preset)))
    (teamake-set-current-values 'teamake-build project values)
    (transient-setup 'teamake-build '() '()
                     :scope binary-dir
                     :value values)))

(defun teamake-build--project-contains-p (project binary-dir)
  "Determine if PROJECT is matching BINARY-DIR."
  (or (teamake-build--build-path-matches-p
       (teamake-get-current-values 'teamake-build project) binary-dir)
      (teamake-build--build-path-matches-p
       (teamake-get-current-values 'teamake-configure project) binary-dir)))

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
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (values (teamake-get-current-values 'teamake-build project))
         (expanded-values (teamake-preset-expand-macros-from-project project values)))
    (apply #'teamake-process-invoke-cmake
           project
           "--build"
           binary-dir
           expanded-values)))

(transient-define-suffix teamake-build--do-build-preset ()
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (preset (teamake-preset--get-current project :buildPresets)))
    (teamake-process-invoke-cmake project
                                  "--build"
                                  (format "--preset=%s" (plist-get preset :name))
                                  )))

(defun teamake-build-can-be-setup (project)
  "Predicate function to determine if `teamake-build' can be setup from PROJECT."
  (interactive)
  (teamake-get-current-values 'teamake-build project))

(defun teamake-build--source-dir-from-binary-dir (binary-dir)
  "Try to resolve code-dir from BINARY-DIR.

Either the BINARY-DIR contains a CMakeCache variable specifying
code-tree, or a configured project has a reference to BINARY-DIR."
  (let* ((cache (teamake-cmake-cache--parse-variables binary-dir))
         (name (teamake-cmake-cache--get-variable-value cache "CMAKE_PROJECT_NAME"))
         (code (teamake-cmake-cache--get-variable-value cache (format "%s_SOURCE_DIR" name))))
    (and code (file-exists-p code) code)))

(transient-define-suffix teamake-build--build-current ()
  :description "Build current"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (value (transient-args 'teamake-build)))
    (teamake-set-current-values 'teamake-build project value)
    (teamake-build--do-build-current)))

(transient-define-suffix teamake-build--build-preset ()
  :description "Build preset"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir)))
    (teamake-preset-select-from-project project :buildPresets)
    (teamake-build--do-build-preset)))

(transient-define-suffix teamake-build--get-code-dir ()
  (interactive)
  (let ((binary-dir (transient-scope)))
    (teamake-cmake-cache--get-source-dir binary-dir)))

(transient-define-suffix teamake-build--binary-dir ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (format "Binary dir (%s)" (propertize (transient-scope) 'face 'transient-value)))
  (interactive)
  (let* ((current-binary-dir (transient-scope))
         (binary-dir (teamake--select-binary-dir current-binary-dir))
         (source-dir (teamake-build--source-dir-from-binary-dir binary-dir))
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

(defun teamake-build--set-current (project binary-dir values)
  "Set VALUES for current BINARY-DIR in PROJECT."
  (interactive)
  (teamake-set-current-values
   'teamake-build project
   (list :scope binary-dir :value values)))

(transient-define-suffix teamake-build--save-current ()
  "Save current build settings."
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (values (transient-args 'teamake-build)))
    (teamake-build--set-current project binary-dir values)))

(transient-define-suffix teamake-build--save-current-as ()
  "Save the current build settings with a name."
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (values (transient-args 'teamake-build))
         (existing-names (teamake-get-save-names 'teamake-build project))
         (name (completing-read "Build name (match will be overwritten): "
                                existing-names)))
    (teamake-set-save-values 'teamake-build project name
                             (list :scope binary-dir :value values))))

(transient-define-suffix teamake-build--load ()
  "Load a previously saved setting."
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (existing-names (teamake-get-save-names 'teamake-build project))
         (name-to-load (completing-read "Build: " existing-names '() t))
         (value (teamake-get-save-value 'teamake-build project name-to-load)))

    (transient-setup 'teamake-build '() '()
                     :scope (plist-get value :scope)
                     :value (plist-get value :value)
                     )))

(defun teamake-build--possible (project)
  "Determine if PROJECT contain enough information for `teamake-build'."
  (teamake-get-current-values 'teamake-build project))

(defun teamake-build--setup (project)
  "Setup `teamake-build' from PROJECT."
  (let ((current-values (teamake-get-current-values 'teamake-build project)))
    (if current-values
        (transient-setup 'teamake-build '() '()
                         :scope (plist-get current-values :scope)
                         :value (plist-get current-values :value))
      (transient-setup 'teamake-build '() '()
                       :scope binary-dir
                       :value (list (format "-S=%s" (plist-get project :source-dir)))))))

(defun teamake-build--setup-transient-from-path (binary-dir)
  "Setup `teamake-build' transient from BINARY-DIR."
  (let* ((project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (current-values (teamake-get-current-values 'teamake-build project)))
    (if current-values
        (transient-setup 'teamake-build '() '()
                         :scope (plist-get current-values :scope)
                         :value (plist-get current-values :value))
      (transient-setup 'teamake-build '() '()
                       :scope binary-dir))))

(transient-define-suffix teamake-build--install-menu ()
  :description "Install menu"
  (interactive)
  (teamake-install--setup-transient-from-path (transient-scope)))

;;;###autoload
(transient-define-prefix teamake-build (binary-dir)
  [:description
   (lambda () (teamake-binary-dir-heading "CMake build" (transient-scope)))
   ("S" teamake-build--source-dir)
   ]
  ["Flags"
   ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")]
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
  [["Do"
    ("xx" teamake-build--build-current)
    ("xp" teamake-build--build-preset)
    ("xi" teamake-build--install-menu :transient t)]
   ["Manage"
    ("xsc" "Save" teamake-build--save-current :transient t)
    ("xsa" "Save as" teamake-build--save-current-as :transient t)
    ("xl" " Load" teamake-build--load)]]
  (interactive
   (list (teamake--select-binary-dir default-directory)))
  (teamake-build--setup-transient-from-path binary-dir)
  )



(provide 'teamake-build)
;;; teamake-build.el ends here
