;;; teamake-build --- CMake build for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-cmake)
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
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-from-project project :buildPresets))
         (values (teamake-build--preset-to-values preset)))
    (teamake-set-current-values 'teamake-build project values)
    (transient-setup 'teamake-build '() '()
                     :scope binary-dir
                     :value values)))

(transient-define-suffix teamake-build--do-build-current ()
  (interactive)
  (let* ((project (transient-scope))
         (values (teamake-get-current-values 'teamake-build project))
         (expanded-values (teamake-cmake-expand-macros-from-project project values)))
    (apply #'teamake-process-invoke-cmake
           project
           "--build"
           binary-dir
           expanded-values)))

(transient-define-suffix teamake-build--do-build-preset ()
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset--get-current project :buildPresets)))
    (teamake-process-invoke-cmake project
                                  "--build"
                                  (format "--preset=%s" (plist-get preset :name))
                                  )))

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

(transient-define-suffix teamake-build--build-current ()
  :description "Build current"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir))
         (value (transient-args 'teamake-build)))
    (teamake-set-current-values 'teamake-build project values)
    (teamake-build--do-build-current)))

(transient-define-suffix teamake-build--build-preset ()
  :description "Build preset"
  (interactive)
  (let* ((binary-dir (transient-scope))
         (project (teamake-cmake-cache--project-from-binary-dir binary-dir)))
    (teamake-preset-select-from-project project :buildPresets)
    (teamake-build--do-build-preset)))

(transient-define-suffix teamake-binary-dir--configuration ()
  :description
  (lambda ()
    (let* ((project (transient-scope))
           (text "For multi configuration tools")
           (value (plist-get project :configuration)))
      (if value
          (format "%s (%s)"
                  text
                  (propertize (format "--config=%s" value) 'face 'transient-value))
        (format "%s (--config=)" text))))
  (interactive)
  (let* ((project (transient-scope))
         (values (transient-args transient-current-command))
         (configuration (completing-read "Configuration: " '("Debug" "RelWithDebInfo" "Release") '() t)))
    (plist-put project :configuration configuration)
    (teamake-set-current-values transient-current-command project values)
    (teamake-setup-transient transient-current-command project)))
  
  
(transient-define-suffix teamake-build--save-current ()
  "Save current build settings."
  (interactive)
  (teamake-transient-save-current-values)
  )

(transient-define-suffix teamake-build--save-current-as ()
  "Save the current build settings with a name."
  (interactive)
  (teamake-transient-save-current-as)
  )

(transient-define-suffix teamake-build--load ()
  "Load a previously saved setting."
  (interactive)
  (teamake-transient-load))

(defun teamake-build--possible (project)
  "Determine if PROJECT contain enough information for `teamake-build'."
  (and (plist-member project :binary-dir)
       (teamake--find-root (plist-get project :binary-dir)
                           "CMakeCache.txt")))

(defun teamake-build--setup (project)
  "Setup `teamake-build' from PROJECT."
  (unless (teamake-build--possible project)
    (user-error "Project not correctly configured with binary dir"))
  (teamake-setup-transient 'teamake-build project))

;;;###autoload
(transient-define-prefix teamake-build (project)
  [:description
   (lambda () (teamake-project-heading (transient-scope) "CMake build"))
  ["Flags"
   ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")]
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

   ;; ("c" "For multi configuration tools" "--config="
   ;;  :prompt "Configuration: "
   ;;  :choices ("Release" "Debug" "RelWithDebInfo"))

   ("C" teamake-binary-dir--configuration)

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
    ]
   ["Manage"
    ("xc" "Save" teamake-transient-save-current-values :transient t)
    ("xa" "Save as" teamake-transient-save-current-as :transient t)
    ("xl" "Load" teamake-transient-load)
    ("xd" "Delete" teamake-transient-delete :transient t)
    ]
   ]
  (interactive
   (let* ((binary-dir (teamake-select-binary-dir default-directory))
          (source-dir (teamake-cmake-cache--get-source-dir binary-dir)))
     (list (teamake-project-from-source-dir-or-create source-dir))))
  (teamake-setup-transient 'teamake-build project)
  )

(provide 'teamake-build)
;;; teamake-build.el ends here
