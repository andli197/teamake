;;; teamake-build --- CMake build for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-cmake)
(require 'teamake-preset)
(require 'teamake-process)

(defun teamake-build--read-build-targets (project)
  "Read available build targets from PROJECT.

Assuming the generator can provide available targets using the 'help'
target in the binary dir."
  (unless (plist-member project :binary-dir)
    (user-error "Unable to read build targets since no binary dir specified"))
  (let ((targets '()))
    (seq-do
     (lambda (line)
       (save-match-data
         (if (string-match "\\(.+\\):.*" line)
             (add-to-list 'targets (match-string 1 line)))))
     (apply #'teamake-cmake-command-to-lines
            (list "--build" (plist-get project :binary-dir) "--target help")))
    targets))

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
         (preset (teamake-preset-select-build project))
         (values (teamake-build--preset-to-values preset)))
    (teamake-set-current-values 'teamake-build project values)
    (transient-setup 'teamake-build '() '()
                     :scope project
                     :value values)))

(transient-define-suffix teamake-build--execute-current ()
  (interactive)
  (let* ((project (transient-scope))
         (current-args (transient-args 'teamake-build))
         (expanded-args (seq-map
                         (lambda (value)
                           (teamake-build--expand-macro-in-current-value value project))
                         current-args)))
    (teamake-set-current-values 'teamake-build project current-args)
    (apply #'teamake-process-invoke-cmake
           project
           "--build"
           (plist-get project :binary-dir)
           expanded-args)))

(transient-define-suffix teamake-build--execute-preset ()
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-build project)))
    (apply #'teamake-process-invoke-cmake
           project
           "--build"
           (plist-get project :binary-dir)
           (format "--preset=%s" (plist-get preset :name)))))

(transient-define-suffix teamake-build--native-tool-option ()
  :description
  (lambda ()
    (let ((text "Native tool option")
          (option "--")
          (value '()))
      (format "%s (%s)"
              text
              (if value (propertize (format "%s%s" option value) 'face 'transient-value)
                option))))
  (interactive)
  (let ((project (transient-scope)))
    )
  )

(defun teamake-build--possible (project)
  "Determine if PROJECT contain enough information for `teamake-build'."
  (teamake--project-has-valid-binary-dir-p project))

(defun teamake-build--setup (project)
  "Setup `teamake-build' from PROJECT."
  (unless (teamake-build--possible project)
    (user-error "Project not correctly configured with binary dir"))
  (teamake-setup-transient 'teamake-build project))

;;;###autoload
(transient-define-prefix teamake-build (project)
  [:description
   (lambda ()
     (format "%s %s\n\n%s\n"
             (propertize "CMake Build" 'face 'teamake-heading)
             (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)
             (propertize (format "cmake --build %s\n      <options>"
                                 (plist-get (transient-scope) :binary-dir))))
     )
   ["Options"
    ("cfg" teamake-transient--configuration)
    ("pr" teamake-build--select-preset)
    ("pa" "Parallel builds, using this amount of jobs" "--parallel="
     :prompt "Parallel builds: "
     :reader transient-read-number-N+)
    ("t" "Build target instead of default targets" "--target="
     :prompt "Targets: "
     :choices (lambda () (teamake-build--read-build-targets (transient-scope)))
     :multi-value repeat)
    ("r" "Restore/resolve package references during build"
     "--resolve-package-references="
     :prompt "Select package restore/resolve: "
     :choices ("on" "only" "off"))
    ;; ("n" "Native tool option" "--"
    ;;  :class transient-option
    ;;  :prompt "Options: ")
    ]
   ]
  ["Flags"
   ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
   ("-v" "Verbose output" "--verbose")]
  [
   ["Build"
    ("xx" "Current "teamake-build--execute-current)
    ("xp" "Preset" teamake-build--execute-preset)
    ]
   ["Manage"
    ("xc" "Save" teamake-transient-save-current-values :transient t)
    ("xa" "Save as" teamake-transient-save-current-as :transient t)
    ("xl" "Load" teamake-transient-load)
    ("xd" "Delete" teamake-transient-delete :transient t)
    ]
   ["Navigate"
    ("C" teamake-project--teamake-cmake-navigate)
    ("P" teamake-cmake--teamake-project)
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
