;;; teamake-test --- CTest commands for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-process)
(require 'teamake-preset)

(transient-define-suffix teamake-test--do-run-tests ()
  "Execute the currently test command."
  (interactive)
  (let* ((project (transient-scope))
         (current-args (transient-args 'teamake-test))
         (source-dir (plist-get project :source-dir))
         (expanded-args
          (seq-map
           (lambda (value)
             (teamake-test--expand-macro-in-current-value value project))
           current-args)))
    (teamake-set-current-values 'teamake-test project current-args)
    (apply #'teamake-process-invoke-ctest
           project
           "--test-dir"
           source-dir
           expanded-args)))

(transient-define-suffix teamake-test--select-and-execute-preset ()
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-from-project project :testPresets)))
    (teamake-process-invoke-ctest
     project
     (format "--preset=%s" (plist-get preset :name)))))


(defun teamake-test--preset-to-values (preset)
  "Parse all values from PRESET into values for `teamake-test'."
  (let ((values '()))
    (let ((output (plist-get preset :output))
          (filter (plist-get preset :filter)))
      (if output
          (progn
            (if (eq (plist-get output :shortProgress) t)
                (add-to-list 'values "--progress"))
            (if (plist-member output :verbosity)
                (cond ((string= (plist-get output :verbosity) "verbose") (add-to-list 'values "--verbose"))
                      ((string= (plist-get output :verbosity) "extra") (add-to-list 'values "--extra-verbose"))))
            (if (not (eq (plist-get output :debug) :json-false))
                (add-to-list 'values "--debug"))
            (if (eq (plist-get output :outputOnFailure) t)
                (add-to-list 'values "--output-on-failure"))
            (if (eq (plist-get output :quiet) t)
                (add-to-list 'values "--quiet"))
            (if (plist-member output :outputLogFile)
                (add-to-list 'values (format "--output-log %s" (plist-get output :outputLogFile))))
            (if (eq (plist-get output :labelSummary) :json-false)
                (add-to-list 'values "--no-label-summary"))
            (if (eq (plist-get output :subprojectSummary) :json-false)
                (add-to-list 'values "--no-subproject-summary"))
            (if (plist-member output :maxPassedTestOutputSize)
                (add-to-list 'values (format "--test-output-size-passed %s"
                                             (plist-get output :maxPassedTestOutputSize))))
            (if (plist-member output :maxFailedTestOutputSize)
                (add-to-list 'values (format "--test-output-size-failed %s"
                                             (plist-get output :maxFailedTestOutputSize))))
            (if (plist-member output :maxTestNameWidth)
                (add-to-list 'values (format "--max-width %s"
                                             (plist-get output :maxTestNameWidth))))
            ))
      (if filter
          (let ((include (plist-get filter :include))
                (exclude (plist-get filter :exclude)))

            )
        )
      )
    values))


(defun teamake-test--expand-macro-in-current-value (value project)
  "Replace any macro expressions in VALUE for PROJECT.

Use current test preset as base for preset specific expansions."
  (teamake-expand-expression
   value
   (plist-get project :source-dir)
   (lambda (text source-dir)
     (teamake-preset--expand-macro
      text (teamake-preset--get-current project :testPresets) source-dir))))

(defun teamake-test--possible (project)
  "Determine if PROJECT contain enough information for `teamake-test'."

  ;; TODO: Check if presets has test presets or if current binary dir contains any tests
  
  (or (teamake-get-current-values 'teamake-test project)
      (transient-arg-value "-B=" (teamake-get-current-values 'teamake-configure project))))

(defun teamake-test--setup (project)
  "Setup `teamake-test' from PROJECT."
  (teamake-setup-transient 'teamake-test project))

(transient-define-prefix teamake-test (project)
  [:description
   (lambda () (teamake-project-heading "CTest" (transient-scope)))
   ["Commands"
    ("cfg" teamake-transient--configuration)
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
   ]
   ["Options"
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
   [
    ["Do"
     ("xx" "Execute current" teamake-test--do-run-tests)
     ("xp" "Execute preset" teamake-test--select-and-execute-preset)
     ]
    ["Manage"
     ("xsc" "Save" "--xsc")
     ("xsa" "Save as" "--xsa")
     ("xl" " Load" "--xl")
     ]
    ]
   (interactive
    (let* ((binary-dir (teamake-select-binary-dir default-directory))
           (source-dir (teamake-cmake-cache--get-source-dir binary-dir)))
      (list (teamake-project-from-source-dir-or-create source-dir))))
   (teamake-setup-transient 'teamake-test project))


  ;; --preset <preset>, --preset=<preset>
  ;;                              = Read arguments from a test preset.
  ;; --list-presets               = List available test presets.
  ;; -C <cfg>, --build-config <cfg>
  ;;                              = Choose configuration to test.
  ;; --progress                   = Enable short progress output from tests.
  ;; -V,--verbose                 = Enable verbose output from tests.
  ;; -VV,--extra-verbose          = Enable more verbose output from tests.
  ;; --debug                      = Displaying more verbose internals of CTest.
  ;; --output-on-failure          = Output anything outputted by the test
  ;;                                program if the test should fail.
  ;; --stop-on-failure            = Stop running the tests after one has failed.
  ;; --test-output-size-passed <size>
  ;;                              = Limit the output for passed tests to <size>
  ;;                                bytes
  ;; --test-output-size-failed <size>
  ;;                              = Limit the output for failed tests to <size>
  ;;                                bytes
  ;; --test-output-truncation <mode>
  ;;                              = Truncate 'tail' (default), 'middle' or
  ;;                                'head' of test output once maximum output
  ;;                                size is reached
  ;; -F                           = Enable failover.
  ;; -j [<level>], --parallel [<level>]
  ;;                              = Run tests in parallel, optionally limited to
  ;;                                a given level of parallelism.
  ;; -Q,--quiet                   = Make ctest quiet.
  ;; -O <file>, --output-log <file>
  ;;                              = Output to log file
  ;; --output-junit <file>        = Output test results to JUnit XML file.
  ;; -N,--show-only[=format]      = Disable actual execution of tests.  The
  ;;                                optional 'format' defines the format of the
  ;;                                test information and can be 'human' for the
  ;;                                current text format or 'json-v1' for json
  ;;                                format.  Defaults to 'human'.
  ;; -L <regex>, --label-regex <regex>
  ;;                              = Run tests with labels matching regular
  ;;                                expression.  With multiple -L, run tests
  ;;                                where each regular expression matches at
  ;;                                least one label.
  ;; -R <regex>, --tests-regex <regex>
  ;;                              = Run tests matching regular expression.
  ;; -E <regex>, --exclude-regex <regex>
  ;;                              = Exclude tests matching regular expression.
  ;; -LE <regex>, --label-exclude <regex>
  ;;                              = Exclude tests with labels matching regular
  ;;                                expression.  With multiple -LE, exclude
  ;;                                tests where each regular expression matches
  ;;                                at least one label.
  ;; -FA <regex>, --fixture-exclude-any <regex>
  ;;                              = Do not automatically add any tests for
  ;;                                fixtures matching regular expression.
  ;; -FS <regex>, --fixture-exclude-setup <regex>
  ;;                              = Do not automatically add setup tests for
  ;;                                fixtures matching regular expression.
  ;; -FC <regex>, --fixture-exclude-cleanup <regex>
  ;;                              = Do not automatically add cleanup tests for
  ;;                                fixtures matching regular expression.
  ;; -D <dashboard>, --dashboard <dashboard>
  ;;                              = Execute dashboard test
  ;; -D <var>:<type>=<value>      = Define a variable for script mode
  ;; -M <model>, --test-model <model>
  ;;                              = Sets the model for a dashboard
  ;; -T <action>, --test-action <action>
  ;;                              = Sets the dashboard action to perform
  ;; --group <group>              = Specify what build group on the dashboard
  ;;                                you'd like to submit results to.
  ;; -S <script>, --script <script>
  ;;                              = Execute a dashboard for a configuration
  ;; -SP <script>, --script-new-process <script>
  ;;                              = Execute a dashboard for a configuration
  ;; -A <file>, --add-notes <file>= Add a notes file with submission
  ;; -I [Start,End,Stride,test#,test#|Test file], --tests-information
  ;;                              = Run a specific number of tests by number.
  ;; -U, --union                  = Take the Union of -I and -R
  ;; --rerun-failed               = Run only the tests that failed previously
  ;; --tests-from-file <file>     = Run the tests listed in the given file
  ;; --exclude-from-file <file>   = Run tests except those listed in the given
  ;;                                file
  ;; --repeat until-fail:<n>, --repeat-until-fail <n>
  ;;                              = Require each test to run <n> times without
  ;;                                failing in order to pass
  ;; --repeat until-pass:<n>      = Allow each test to run up to <n> times in
  ;;                                order to pass
  ;; --repeat after-timeout:<n>   = Allow each test to run up to <n> times if it
  ;;                                times out
  ;; --max-width <width>          = Set the max width for a test name to output
  ;; --interactive-debug-mode [0|1]
  ;;                              = Set the interactive mode to 0 or 1.
  ;; --resource-spec-file <file>  = Set the resource spec file to use.
  ;; --no-label-summary           = Disable timing summary information for
  ;;                                labels.
  ;; --no-subproject-summary      = Disable timing summary information for
  ;;                                subprojects.
  ;; --test-dir <dir>             = Specify the directory in which to look for
  ;;                                tests.
  ;; --build-and-test             = Configure, build and run a test.
  ;; --build-target               = Specify a specific target to build.
  ;; --build-nocmake              = Run the build without running cmake first.
  ;; --build-run-dir              = Specify directory to run programs from.
  ;; --build-two-config           = Run CMake twice
  ;; --build-exe-dir              = Specify the directory for the executable.
  ;; --build-generator            = Specify the generator to use.
  ;; --build-generator-platform   = Specify the generator-specific platform.
  ;; --build-generator-toolset    = Specify the generator-specific toolset.
  ;; --build-project              = Specify the name of the project to build.
  ;; --build-makeprogram          = Specify the make program to use.
  ;; --build-noclean              = Skip the make clean step.
  ;; --build-config-sample        = A sample executable to use to determine the
  ;;                                configuration
  ;; --build-options              = Add extra options to the build step.
  ;; --test-command               = The test to run with the --build-and-test
  ;;                                option.
  ;; --test-timeout               = The time limit in seconds, internal use
  ;;                                only.
  ;; --test-load                  = CPU load threshold for starting new parallel
  ;;                                tests.
  ;; --tomorrow-tag               = Nightly or experimental starts with next day
  ;;                                tag.
  ;; --overwrite                  = Overwrite CTest configuration option.
  ;; --extra-submit <file>[;<file>]
  ;;                              = Submit extra files to the dashboard.
  ;; --http-header <header>       = Append HTTP header when submitting
  ;; --force-new-ctest-process    = Run child CTest instances as new processes
  ;; --schedule-random            = Use a random order for scheduling tests
  ;; --submit-index               = Submit individual dashboard tests with
  ;;                                specific index
  ;; --timeout <seconds>          = Set the default test timeout.
  ;; --stop-time <time>           = Set a time at which all tests should stop
  ;;                                running.
  ;; --http1.0                    = Submit using HTTP 1.0.
  ;; --no-compress-output         = Do not compress test output when submitting.
  ;; --print-labels               = Print all available test labels.
  ;; --no-tests=<[error|ignore]>  = Regard no tests found either as 'error' or
  ;;                                'ignore' it.

(provide 'teamake-test)
;;; teamake-test.el ends here
