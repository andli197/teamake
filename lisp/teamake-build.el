;;; teamake-build --- CMake build for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-cmake)
(require 'teamake-preset)
(require 'teamake-process)

(defun teamake-build--string (project)
  "Display current build command for PROJECT."
  (propertize
   (format "cmake --build %s %s"
           (plist-get project :binary-dir)
           "<options>")
   'face
   'teamake-cmake-command))

(defun teamake-cmake-build-current (project)
  "Run build with current values for PROJECT."
  (unless (teamake--project-has-valid-binary-dir-p project)
    (user-error "Unable to build.  No valid binary dir was specified"))
  (apply #'teamake-process-invoke-cmake
         project
         (append (list "--build" (plist-get project :binary-dir))
                 (teamake-get-current-values 'teamake-build project))))

(defun teamake-cmake-build-preset (project)
  "Run build with current preset for PROJECT."
  (let* ((preset (teamake-preset--get-current-build-preset project))
         (preset-name (plist-get preset :name)))
    (unless preset-name
      (error "Unable to build.  No preset was provided"))
    (teamake-process-invoke-cmake project
                                  "--build"
                                  (format "--preset=%s" preset-name))))

(defun teamake-build--read-build-targets (project)
  "Read available build targets from PROJECT.

Assuming the generator can provide available targets using the 'help'
target in the binary dir."
  (unless (teamake--project-has-valid-binary-dir-p project)
    (user-error "Unable to build.  No valid binary dir was specified"))
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

(defun teamake-build--expand-macro-in-current-value (value project)
  "Replace any macro expressions in VALUE for PROJECT.

Use current build preset as base for preset specific expansions."
  (teamake-expand-expression
   value
   (plist-get project :source-dir)
   (lambda (text source-dir)
     (teamake-preset--expand-macro
      text (teamake-preset--get-current-build-preset project) source-dir))))

(transient-define-suffix teamake-build--select-preset ()
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-build project))
         (raw-values (teamake-build--preset-to-values preset))
         (values (seq-map (lambda (value)
                            (teamake-build--expand-macro-in-current-value value project))
                          raw-values)))
    (teamake-set-current-values 'teamake-build project values)
    (teamake-setup-transient 'teamake-build project)))

(transient-define-suffix teamake-build--execute-current ()
  (interactive)
  (let* ((project (transient-scope))
         (current-args (transient-args 'teamake-build)))
    (teamake-set-current-values 'teamake-build project current-args)
    (teamake-cmake-build-current project)))

(transient-define-suffix teamake-build--execute-preset ()
  (interactive)
  (let ((project (transient-scope)))
    (teamake-preset-select-build project)
    (teamake-cmake-build-preset project)))

;; (transient-define-suffix teamake-build--native-tool-option ()
;;   :description
;;   (lambda ()
;;     (let ((text "Native tool option")
;;           (option "--")
;;           (value '()))
;;       (format "%s (%s)"
;;               text
;;               (if value (propertize (format "%s%s" option value) 'face 'transient-value)
;;                 option))))
;;   (interactive)
;;   (let ((project (transient-scope)))
;;     )
;;   )

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
             (teamake-build--string (transient-scope))))
   ["Options"
    ("cfg" teamake-transient--configuration)
    ("pr" "Read from preset" teamake-build--select-preset)
    ("pa" "Parallel builds, using this amount of jobs" "--parallel="
     :prompt "Parallel builds: "
     :reader transient-read-number-N+)
    ("r" "Restore/resolve package references during build"
     "--resolve-package-references="
     :prompt "Select package restore/resolve: "
     :choices ("on" "only" "off"))
    ("t" "Build target instead of default targets" "--target="
     :prompt "Targets: "
     :choices (lambda () (teamake-build--read-build-targets (transient-scope)))
     :multi-value repeat)
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
