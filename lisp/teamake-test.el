;;; teamake-test --- CTest commands for teamake
;;; Commentary:
;;; Code:

(require 'transient)
(require 'teamake-core)
(require 'teamake-process)
(require 'teamake-preset)

(defun teamake-test--string (project)
  "Display current test command for PROJECT."
  (propertize
   (format "ctest --test-dir %s %s"
           (plist-get project :binary-dir)
           "<options>"
           )
   'face 'teamake-cmake-command))

(defun teamake-cmake-test-current (project)
  "Run test with current values for PROJECT."
  (unless (teamake--project-has-valid-binary-dir-p project)
    (user-error "Unable to test.  No valid binary dir was specified"))
  (apply #'teamake-process-invoke-ctest
         project
         (append (list "--test-dir" (plist-get project :binary-dir)
                       "--config")
                 (teamake-get-current-values 'teamake-test project))))

(defun teamake-cmake-test-preset (project)
  "Run test with current preset for PROJECT."
  (let* ((preset (teamake-preset--get-current-test-preset project))
         (preset-name (plist-get preset :name)))
    (unless preset-name
      (user-error "Unable to test.  No preset was provided"))
    (teamake-process-invoke-ctest project
                                  (format "--preset=%s" preset-name))))

(transient-define-suffix teamake-test--execute-current ()
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
    (teamake-cmake-test-current project)))

(transient-define-suffix teamake-test--execute-preset ()
  (interactive)
  (let* ((project (transient-scope)))
    (teamake-preset-select-test project)
    (teamake-cmake-test-preset project)))

(defun teamake-test--preset-to-values (preset)
  "Parse all values from PRESET into values for `teamake-test'."
  (let ((values '()))
    (if (plist-member preset :overwriteConfigurationFile)
        (seq-do
         (lambda (kvp)
           (add-to-list 'values (format "--overwrite %s" kvp)))
         (plist-get preset :overwriteConfigurationFile)))
    (if (plist-member preset :output)
        (let ((output (plist-get preset :output)))
          (if (eq (plist-get output :shortProgress) t)
                (add-to-list 'values "--progress"))
            (if (plist-member output :verbosity)
                (cond ((string= (plist-get output :verbosity) "verbose") (add-to-list 'values "--verbose"))
                      ((string= (plist-get output :verbosity) "extra") (add-to-list 'values "--extra-verbose"))))
            (if (and (plist-member output :debug)
                     (not (eq (plist-get output :debug) :json-false)))
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
            )
      )
    (if (plist-member preset :filter)
        (let ((filter (plist-get preset :filter)))
          (if (plist-member filter :include)
              (let ((include (plist-get filter :include)))
                (if (plist-member include :name)
                    (add-to-list 'values (format "--tests-regex %s"
                                                 (plist-get include :name))))
                (if (plist-member include :label)
                    (add-to-list 'values (format "--label-regex %s"
                                                 (plist-get include :label))))
                (if (eq (plist-member include :useUnion) t)
                    (add-to-list 'values "--union"))
                )
            )
          (if (plist-member filter :exclude)
              (let ((exclude (plist-get filter :exclude)))
                (if (plist-member exclude :name)
                    (add-to-list 'values (format "--exclude-regex %s"
                                                 (plist-get exclude :name))))
                (if (plist-member exclude :label)
                    (add-to-list 'values (format "--label-exclude %s"
                                                 (plist-get exclude :label))))
                (if (plist-member exclude :fixtures)
                    (let ((fixtures (plist-get exclude :fixtures)))
                      (if (plist-member fixtures :any)
                          (add-to-list 'values (format "--fixture-exclude-any %s"
                                                       (plist-get fixtures :any))))
                      (if (plist-member fixtures :setup)
                          (add-to-list 'values (format "--fixture-exclude-setup %s"
                                                       (plist-get fixtures :setup))))
                      (if (plist-member fixtures :cleanup)
                          (add-to-list 'values (format "--fixture-exclude-cleanup %s"
                                                       (plist-get fixtures :cleanup))))
                      )
                  )
                )
            )
          )
      )
    (if (plist-member preset :execution)
        (let ((execution (plist-get preset :execution)))
          (if (eq (plist-get execution :stopOnFailure) t)
              (add-to-list 'values "--stop-on-failure"))
          (if (eq (plist-get execution :enableFailover) t)
              (add-to-list 'values "-F"))
          (if (plist-member execution :jobs)
              (add-to-list 'values (format "--parallel %s"
                                           (plist-get execution :jobs))))
          (if (plist-member execution :resourceSpecFile)
              (add-to-list 'values (format "--resource-spec-file %s"
                                           (plist-get execution :resourceSpecFile))))
          (if (plist-member execution :testLoad)
              (add-to-list 'values (format "--test-load %s"
                                           (plist-get execution :testLoad))))
          (if (plist-member execution :showOnly)
              (add-to-list 'values (format "--show-only %s"
                                           (plist-get execution :showOnly))))
          (if (plist-member execution :repeat)
              (let ((repeat (plist-get execution :repeat)))
                (if (and (plist-member repeat :mode)
                         (plist-member repeat :count))
                    (add-to-list 'values (format "--repeat %s:%s"
                                                 (plist-get repeat :mode)
                                                 (plist-get repeat :count))))
                )
            )
          (if (plist-member execution :interactiveDebugging)
              (add-to-list 'values (format "--interactive-debug-mode %s"
                                           (if (eq (plist-get execution :interactiveDebugging) t)
                                               "1" "0"))))
          (if (eq (plist-get execution :scheduleRandom) t)
              (add-to-list 'values "--schedule-random"))
          (if (plist-member execution :timeout)
              (add-to-list 'values (format "--timeout %s" (plist-get execution :timeout))))
          (if (and (plist-member execution :noTestsAction)
                   (not (string= (plist-get execution :noTestsAction) "default")))
              (add-to-list 'values (format "--no-tests=%s" (plist-get execution :noTestsAction))))
          ))
    values))

(transient-define-suffix teamake-test--select-preset ()
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-test project))
         (raw-values (teamake-test--preset-to-values preset))
         (values (seq-map
                  (lambda (value)
                    (teamake-test--expand-macro-in-current-value value project))
                  raw-values)))
    (teamake-set-current-values 'teamake-test project values)
    (teamake-setup-transient 'teamake-test project)))

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
  (teamake--project-has-valid-binary-dir-p project))

(defun teamake-test--setup (project)
  "Setup `teamake-test' from PROJECT."
  (unless (teamake-install--possible project)
    (user-error "Project not correctly configured with binary dir"))
  (teamake-setup-transient 'teamake-test project))

(defun teamake-test--find-repeat (values)
  "Find the value associated with \"--repeat\" among VALUES."
  (let ((value (seq-find (lambda (e) (string-match "--repeat" e)) values)))
    (if value
        (save-match-data
          (string-match "--repeat \\(.+\\):\\(.+\\)" value)
          (list :mode (match-string 1 value)
                :amount (match-string 2 value))))))

(transient-define-suffix teamake-test--repeat ()
  "Select how tests are to be repeated"
  :description
  (lambda ()
    (let* ((values (teamake-get-current-values 'teamake-test (transient-scope)))
           (text "Repeat")
           (option "--repeat")
           (value (teamake-test--find-repeat values)))
      (format "%s (%s)" text (if value
                                 (propertize (format "%s %s:%s"
                                                     option
                                                     (plist-get value :mode)
                                                     (plist-get value :amount))
                                             'face 'transient-value)
                               option))
      ))
  (interactive)
  (let* ((project (transient-scope))
         (mode (completing-read "Mode: " '("until-pass" "until-fail" "after-timeout") '() t))
         (amount (read-number "Times: "))
         (current (teamake-get-current-values 'teamake-test project)))

    (message "before: %s" current)
    ;; quick and dirty way, keep all values
    (add-to-list 'current (format "--repeat %s:%s" mode amount))
    (message "after: %s" current)
    (teamake-set-current-values 'teamake-test project current)
    (teamake-setup-transient 'teamake-test project)
  ))

(transient-define-prefix teamake-test (project)
  [:description
   (lambda ()
     (format "%s %s\n\n%s\n"
             (propertize "CMake Test" 'face 'teamake-heading)
             (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)
             (teamake-test--string (transient-scope)))
     )
   ["Misc"
    ("cfg" teamake-transient--configuration)
    ("pr" "Read from preset" teamake-test--select-preset)
    ]
   ]
  ["Filter"
   ["Include"
    ("tr" "Run tests matching regular expression"
     "--tests-regex="
     :prompt "Test include regex: ")
    ("lr" "Run tests with labels matching regular expression"
     "--label-regex="
     :prompt "Labels include regex: ")
    ("u" "Take the union of --tests-information and --tests-regex" "--union")]
   ["Exclude"
    ("te" "Exclude tests matching regular expression"
     "--exclude-regex="
     :prompt "Test exclude regex: ")
    ("le" "Exclude tests with labels matching regular expression"
     "--label-exclude="
     :prompt "Label exclude regex: ")
    ("fa" "Exclude fixtures matching" "--fixture-exclude-any"
     :class transient-option
     :prompt "Exclude fixtured matching regex: ")
    ("fs" "Exclude setup matching" "--fixture-exclude-setup"
     :class transient-option
     :prompt "Exclude setup matching regex: ")
    ("fc" "Exclude cleanup matching" "--fixture-exclude-cleanup"
     :class transient-option
     :prompt "Exclude cleanup matching regex: ")
    ]
   ]
  ["Execution"
   ("sof" "Stop running the tests when the first failure happens" "--stop-on-failure")
   ("F" "Enable failover" "-F")
   ("pa" "Run tests in parallel (jobs)" ("-j" "--parallel")
    :class transient-option
    :prompt "Parallel jobs: "
    :reader transient-read-number-N+)
   ("rsf" "Run CTest with resource allocation enabled" "--resource-spec-file"
    :class transient-option
    :prompt "Resource specification file: "
    :reader transient-read-file)
   ("tl" "CPU threshold for when executing in parallel" "--test-load"
    :class transient-option
    :prompt "Load theshold: "
    :reader transient-read-number-N+)
   ("show" "Do not execute tests, only show" "--show-only"
    :class transient-option
    :prompt "Select format: "
    :choices '("human" "json-v1"))
   ("rep" teamake-test--repeat)
   ("i" "Interactive debug mode" "--interactive-debug-mode")
   ("s" "Use a random order for scheduling tests" "--schedule-random")
   ("to" "Set the default test timeout" "--timeout"
    :class transient-option
    :prompt "Timeout (s): "
    :reader transient-read-number-N+)
   ]
  ["Output"
    ("-p" "Enable short progress output from tests"
     "--progress")
    ("-v" "Enable verbose output from tests"
     "--verbose")
    ("-V" "Enable more verbose output from tests"
     "--extra-verbose")
    ("-d" "Displaying more verbose internals of CTest"
    "--debug")
    ("-o" "Output anything outputted by the test program if the test should fail."
     "--output-on-failure")
    ("-s" "Stop running the tests after one has failed"
     "--stop-on-failure")
    ("-q" "Make ctest quiet"
     "--quiet")
    ("o" "Output to log file" ("-o" "--output-log")
     :class transient-option
     :prompt "Log file: "
     :reader transient-read-file)
    ("-n" "Disable timing summary information for labels"
     "--no-label-summary")
    ("-n" "Disable timing summary information for subprojects"
     "--no-subproject-summary")
    ("top" "Test output size passed" "--test-output-size-passed"
     :class transient-option
     :prompt "Byte size to limit output for passed tests: "
     :reader transient-read-number-N+)
    ("tof" "Test output size failed" "--test-output-size-failed"
     :class transient-option
     :prompt "Byte size to limit output for failed tests: "
     :reader transient-read-number-N+)
    ("mw" "Set the max width for a test name to output" "--max-width"
     :class transient-option
     :prompt "Max width: "
     :reader transient-read-number-N+)
    ]
  [
   ["Test"
    ("xx" "Current" teamake-test--execute-current)
    ("xp" "Preset" teamake-test--execute-preset)
    ]
   ["Manage project"
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
  (teamake-setup-transient 'teamake-test project))

(provide 'teamake-test)
;;; teamake-test.el ends here
