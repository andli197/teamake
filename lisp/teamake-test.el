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
  t)

(defun teamake-test--setup (project)
  "Setup `teamake-test' from PROJECT."
  (teamake-setup-transient 'teamake-test project))

(transient-define-prefix teamake-test (project)
  [:description
   (lambda () (format "%s %s\n"
                      (propertize "CTest" 'face 'teamake-heading)
                      (teamake-project-display-propertized (transient-scope))))
   ["Commands"
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
     :reader transient-read-number-N+)]
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
     :reader transient-read-number-N+)]
   [["Do"
     ("xx" "Execute current" teamake-test--do-run-tests)
     ("xp" "Execute preset" teamake-test--select-and-execute-preset)
     ]
    ["Manage"
     ("xsc" "Save" "--xsc")
     ("xsa" "Save as" "--xsa")
     ("xl" " Load" "--xl")]
    ]
   (interactive
    (let ((source-dir (teamake--find-root default-directory "CMakeLists.txt"))
          (no-project (list :name "No CMake project" :source-dir default-directory)))
      (list (if source-dir (teamake--project-from-path source-dir)
              no-project))))
   (teamake-setup-transient 'teamake-test project))

(provide 'teamake-test)
;;; teamake-test.el ends here
