
;;; Code:

(require 'teamake-base)
(require 'teamake-cache)
(require 'teamake-process)
(require 'teamake-cmake-help)

(defun teamake-build--read-build-targets (build-path)
  "Read build targets from `teamake-build-path' using target 'help'.

Assuming the generator can provide available targets using the 'help'
target in the build tree."
  (interactive (list (teamake-build-root default-directory)))
  (teamake-cmake-shell-command-to-lines
   build-path
   "--build"
   build-path
   "--target"
   "help"))

(defun teamake-build--do-build ()
  "Invoke compilation using the current configuration at BUILD-PATH."
  (interactive)
  (apply #'teamake-process-invoke-cmake
            (teamake-build-dir)
            "--build"
            (teamake-build-dir)
            (seq-map (lambda (cmd)
                       (teamake-expand-macro-expression cmd))
                     (transient-args 'teamake-build))))

(defun teamake-build--do-build-preset ()
  "Prompt for build preset and run build from selection."
  (interactive)
  (apply #'teamake-process-invoke-cmake
         (teamake-build-dir)
         "--build"
         (teamake-build-dir)
         "--preset=dafdsfadsf"))

(defun teamake-build--preset-to-values (preset)
  "Parse all values from PRESET to CMake build flags."
  (let ((values '()))
    ;; (if (plist-member preset :binaryDir)
    ;;     ;; TODO: Set ${buildDir}
    ;;     )
    (if (plist-member preset :cleanFirst)
        (add-to-list 'values "--clean-first"))
    (if (plist-member preset :verbose)
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
    ;; TODO handle preset :nativeToolOptions
    values
    )
  )

(defun teamake-build--select-preset ()
  (interactive)
  (teamake-preset-select-build-preset (teamake-source-dir))
  (transient-setup 'teamake-build '() '()
                   :value (teamake-build--preset-to-values
                           teamake-preset--selected-build)))

(transient-define-suffix teamake--invoke-cache ()
  (interactive)
  (transient-setup 'teamake-cache '() '() :scope (teamake-build-dir)))

(defun teamake-build--describe ()
  "Create a description of the current build."
  (let ((build-dir (teamake-build-dir)))
    (concat (propertize "CMake Build " 'face 'teamake-heading)
            (propertize (teamake-expand-macro-expression
                         (teamake-project-name))
                        'face 'teamake-name)
            "\n"
            (propertize (format "${buildDir}=%s (%s)"
                                build-dir
                                (file-truename
                                 (teamake-expand-macro-expression build-dir)))
                        'face 'teamake-path)
            "\n")))

(transient-define-prefix teamake-build ()
  "Invoke a build command on an already existing configuration."
  :value '("--verbose")
  [:description
   (lambda () (teamake-build--describe))
   ["Flags"
    ("-c" "Build target 'clean' first, then build actual target" "--clean-first")
    ("-v" "Verbose output" "--verbose")
    ]
   ["Options"
    ("pr" teamake-build--select-preset
     :description "Read settings from a build preset"
     :transient t)
    ("pa" "Parallel builds, using this amount of jobs" "--parallel="
     :prompt "Parallel builds: "
     :reader transient-read-number-N+)
    ("ta" "Build target instead of default targets" "--target="
     :prompt "Targets: "
     :choices (lambda () (teamake-build--read-build-targets (teamake-build-dir)))
     :multi-value repeat)
    ("cf" "For multi configuration tools" "--target="
     :prompt "Configuration: "
     :choices ("Release" "Debug" "RelWithDebInfo"))
    ("rp" "Restore/resolve package references during build"
     "--resolve-package-references="
     :prompt "Select package restore/resolve: "
     :choices ("on" "only" "off"))]
   ]
  ["Do"
   ("xx" "Build current tree" teamake-build--do-build)
   ("xp" "Build using preset" teamake-build--do-build-preset)
   ]
  ["Help"
   ("h" "CMake help menu" teamake-cmake-help)
   ]
  (interactive)
  (transient-setup 'teamake-build))

(provide 'teamake-build)
;;; teamake-build.el ends here
