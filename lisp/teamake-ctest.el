

(require 'teamake-base)
(require 'teamake-process)

(defun teamake-ctest--run ()
  "Run CTest with current settings."
  (interactive)
  (apply #'teamake-process-invoke-ctest
         (transient-scope)
         (transient-args transient-current-command)))
  

(defun teamake-ctest--describe ()
  "Create a description of the current build."
  (concat "CTest  "
          (propertize (teamake-project-name) 'face 'teamake-name)
          " ("
          (propertize (transient-scope) 'face 'teamake-path)
          ")\n"))

(defun teamake-ctest--get-build-path ()
  "Get build-path from either deduced ${buildPath} or `default-directory'"
  (if transient-current-prefix
      (transient-arg-value "${buildDir}=" (transient-args 'teamake))
    (teamake-get-root default-directory 'teamake-build-tree-p)))

;; export function testArgs(preset: TestPreset): string[] {
;;     const result: string[] = [];

;;     preset.configuration && result.push('--build-config', preset.configuration);
;;     if (preset.overwriteConfigurationFile) {
;;         for (const config of preset.overwriteConfigurationFile) {
;;             result.push('--overwrite', config);
;;         }
;;     }

;;     // Output
;;     if (preset.output) {
;;         preset.output.shortProgress && result.push('--progress');
;;         preset.output.verbosity === 'verbose' && result.push('--verbose');
;;         preset.output.verbosity === 'extra' && result.push('--extra-verbose');
;;         preset.output.debug && result.push('--debug');
;;         preset.output.outputOnFailure && result.push('--output-on-failure');
;;         preset.output.quiet && result.push('--quiet');
;;         preset.output.outputLogFile && result.push('--output-log', preset.output.outputLogFile);
;;         preset.output.outputJUnitFile && result.push('--output-junit', preset.output.outputJUnitFile);
;;         preset.output.labelSummary === false && result.push('--no-label-summary');
;;         preset.output.subprojectSummary === false && result.push('--no-subproject-summary');
;;         preset.output.maxPassedTestOutputSize && result.push('--test-output-size-passed', preset.output.maxPassedTestOutputSize.toString());
;;         preset.output.maxFailedTestOutputSize && result.push('--test-output-size-failed', preset.output.maxFailedTestOutputSize.toString());
;;         preset.output.testOutputTruncation && result.push('--test-output-truncation', preset.output.testOutputTruncation.toString());
;;         preset.output.maxTestNameWidth && result.push('--max-width', preset.output.maxTestNameWidth.toString());
;;     }

;;     // Filter
;;     if (preset.filter?.include) {
;;         preset.filter.include.name && result.push('--tests-regex', preset.filter.include.name);
;;         preset.filter.include.label && result.push('--label-regex', preset.filter.include.label);
;;         preset.filter.include.useUnion && result.push('--union');
;;         if (preset.filter.include.index) {
;;             if (util.isString(preset.filter.include.index)) {
;;                 result.push('--tests-information', preset.filter.include.index);
;;             } else {
;;                 const start = preset.filter.include.index.start || '';
;;                 const end = preset.filter.include.index.end || '';
;;                 const stride = preset.filter.include.index.stride || '';
;;                 const specificTests = preset.filter.include.index.specificTests ? `,${preset.filter.include.index.specificTests.join(',')}` : '';
;;                 result.push(`--tests-information ${start},${end},${stride}${specificTests}`);
;;             }
;;         }
;;     }
;;     if (preset.filter?.exclude) {
;;         preset.filter.exclude.name && result.push('--exclude-regex', preset.filter.exclude.name);
;;         preset.filter.exclude.label && result.push('--label-exclude', preset.filter.exclude.label);
;;         preset.filter.exclude.fixtures?.any && result.push('--fixture-exclude-any', preset.filter.exclude.fixtures.any);
;;         preset.filter.exclude.fixtures?.setup && result.push('--fixture-exclude-setup', preset.filter.exclude.fixtures.setup);
;;         preset.filter.exclude.fixtures?.cleanup && result.push('--fixture-exclude-cleanup', preset.filter.exclude.fixtures.cleanup);
;;     }
;;     if (preset.execution) {
;;         preset.execution.stopOnFailure && result.push('--stop-on-failure');
;;         preset.execution.enableFailover && result.push('-F');
;;         preset.execution.jobs && result.push('--parallel', preset.execution.jobs.toString());
;;         preset.execution.resourceSpecFile && result.push('--resource-spec-file', preset.execution.resourceSpecFile);
;;         preset.execution.testLoad && result.push('--test-load', preset.execution.testLoad.toString());
;;         preset.execution.showOnly && result.push('--show-only', preset.execution.showOnly);
;;         preset.execution.repeat && result.push('--repeat', `${preset.execution.repeat.mode}:${preset.execution.repeat.count}`);
;;         preset.execution.interactiveDebugging && result.push('--interactive-debug-mode 1');
;;         preset.execution.interactiveDebugging === false && result.push('--interactive-debug-mode 0');
;;         preset.execution.scheduleRandom && result.push('--schedule-random');
;;         preset.execution.timeout && result.push('--timeout', preset.execution.timeout.toString());
;;         preset.execution.noTestsAction && preset.execution.noTestsAction !== 'default' && result.push('--no-tests=' + preset.execution.noTestsAction);
;;     }

;;     return result;
;; }

(transient-define-prefix teamake-ctest (build-path)
  :value '("--output-on-failure" "--timeout=60")
  [:description
   (lambda () (teamake-ctest--describe))
    "Flags and switches"
    ("-d" "Displaying more verbose internals of CTest"
     "--debug")
    ("-p" "Enable short progress output from tests"
     "--progress")
    ("-v" "Enable verbose output from tests"
     "--verbose")
    ("-V" "Enable more verbose output from tests"
     "--extra-verbose")
    ("-o" "Output anything outputted by the test program if the test should fail."
     "--output-on-failure")
    ("-s" "Stop running the tests after one has failed"
     "--stop-on-failure")
    ("-q" "Make ctest quiet"
     "--quiet")
    ("-N" "Disable actual execution of tests"
     "--show-only")
    ("-r" "Run only the tests that failed previously"
     "--rerun-failed")
    ("-n" "Disable timing summary information for labels"
     "--no-label-summary")
    ("-n" "Disable timing summary information for subprojects"
     "--no-subproject-summary")
    ("-f" "Run child CTest instances as new processes"
     "--force-new-ctest-process")
    ("-r" "Use a random order for scheduling tests"
     "--schedule-random")
    ("-p" "Print all available test labels"
     "--print-labels")
    ("-t" "Set the default test timeout"
     "--timeout="
     :prompt "Time in seconds: "
     :reader transient-read-number-N+)
    ]
  [:description
    "Commands"
    ("la" "Run tests with labels matching regular expression"
     "--label-regex="
     :prompt "Labels include regex: ")
    ("le" "Exclude tests with labels matching regular expression"
     "--label-exclude="
     :prompt "Label exclude regex: ")
    ("ta" "Run tests matching regular expression"
     "--tests-regex="
     :prompt "Test include regex: ")
    ("te" "Exclude tests matching regular expression"
     "--exclude-regex="
     :prompt "Test exclude regex: ")
    ("uf" "Require each test to run <n> times without failing in order to pass"
     "--repeat until-fail="
     :prompt "Repeat until fail: "
     :reader transient-read-number-N+)
    ("up" "Allow each test to run up to <n> times in order to pass"
     "--repeat until-pass="
     :prompt "Allow repeat: "
     :reader transient-read-number-N+)
    ]
  [:description
   "Do"
   ("xx" "Run tests" teamake-ctest--run)
   ;; ("xp" "Execute test preset" teamake-ctest--run-preset)
   ]
  [:description
   "Help"
   ("h" "CMake help menu" teamake-cmake-help)]
  (interactive (list (teamake-get-root default-directory)))
  (transient-setup 'teamake-ctest '() '()
                   :scope (teamake-ctest--get-build-path))
  )
(provide 'teamake-ctest)
;;; teamake-ctest.el ends here
