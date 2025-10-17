
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
            (transient-args transient-current-command)))

(defun teamake-build--do-build-preset ()
  "Prompt for build preset and run build from selection."
  (interactive)
  (apply #'teamake-process-invoke-cmake
         (teamake-build-dir)
         "--build"
         (teamake-build-dir)
         "--preset=dafdsfadsf"))

(transient-define-suffix teamake--invoke-cache ()
  (interactive)
  (transient-setup 'teamake-cache '() '() :scope (transient-scope)))

(defun teamake-build--describe ()
  "Create a description of the current build."
  (concat "CMake Build "
          (propertize (teamake-project-name) 'face 'teamake-name)
          " ("
          (propertize (teamake-build-dir) 'face 'teamake-path)
          ")\n"))

;; export function buildArgs(preset: BuildPreset, tempOverrideArgs?: string[], tempOverrideBuildToolArgs?: string[]): string[] {
;;     const result: string[] = [];

;;     preset.__binaryDir && result.push('--build', preset.__binaryDir);
;;     preset.jobs && result.push('--parallel', preset.jobs.toString());
;;     preset.configuration && result.push('--config', preset.configuration);
;;     preset.cleanFirst && result.push('--clean-first');
;;     preset.verbose && result.push('--verbose');

;;     if (util.isString(preset.__targets)) {
;;         result.push('--target', preset.__targets);
;;     } else if (util.isArrayOfString(preset.__targets)) {
;;         result.push('--target', ...preset.__targets);
;;     }

;;     tempOverrideArgs && result.push(...tempOverrideArgs);
;;     if (preset.nativeToolOptions || tempOverrideBuildToolArgs) {
;;         result.push('--');
;;         preset.nativeToolOptions && result.push(...preset.nativeToolOptions);
;;         tempOverrideBuildToolArgs && result.push(...tempOverrideBuildToolArgs);
;;     }

;;     return result;
;; }

(defun teamake-build--get-build-path ()
  "Get build-path from either deduced ${buildPath} or `default-directory'"
  (if transient-current-prefix
      (transient-arg-value "${buildDir}=" (transient-args 'teamake))
    (teamake-get-root default-directory 'teamake-build-tree-p)))

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
    ("pr" "Read settings from a build preset" "--preset="
     :prompt "Select preset: ")
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
