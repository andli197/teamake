

(require 'teamake-base)
(require 'teamake-process)

(defun teamake-ctest--run-preset (preset)
  "Execute ctest with PRESET."
  (interactive
   (let* ((presets (teamake-ctest-shell-command-to-string default-directory "--list-presets"))
          (preset (completing-read "Preset " presets '() t)))
     (list preset)))
  
  )

(transient-define-prefix teamake-ctest (path)
  :value '("--output-on-failure" "--timeout=60")
  [[:if (lambda () (teamake-build-tree-p (transient-scope)))
    :description "Flags and switches"
    ("-d" "Displaying more verbose internals of CTest"    "--debug")
    ("-p" "Enable short progress output from tests"       "--progress")
    ("-v" "Enable verbose output from tests"              "--verbose")
    ("-V" "Enable more verbose output from tests"         "--extra-verbose")
    ("-o" "Output anything outputted by the test program
    if the test should fail." "--output-on-failure")
    ("-s" "Stop running the tests after one has failed"   "--stop-on-failure")
    ("-q" "Make ctest quiet"                              "--quiet")
    ("-N" "Disable actual execution of tests"             "--show-only")
    ("-r" "Run only the tests that failed previously"     "--rerun-failed")
    ("-n" "Disable timing summary information for labels" "--no-label-summary")
    ("-n" "Disable timing summary information for subprojects"
     "--no-subproject-summary")
    ("-f" "Run child CTest instances as new processes"
     "--force-new-ctest-process")
    ("-r" "Use a random order for scheduling tests"      "--schedule-random")
    ("-p" "Print all available test labels"              "--print-labels")
    ("-t" "Set the default test timeout" "--timeout="
     :prompt "Time in seconds: " :reader transient-read-number-N+)
    ]]
  [[:if (lambda () (teamake-build-tree-p (transient-scope)))
    :description "Commands"
    ("LA" "Run tests with labels matching regular expression" "--label-regex=" :prompt "Expression: ")
    ("RE" "Run tests matching regular expression" "--tests-regex=" :prompt "Expression: ")
    ("EE" "Exclude tests matching regular expression" "--exclude-regex=" :prompt "Expression: ")
    ("LE" "Exclude tests with labels matching regular expression" "--label-exclude=" :prompt "Expression: ")
    ("RUF" "Require each test to run <n> times without failing in order to pass" "--repeat until-fail=" :prompt "Attempts: ")
    ("RUP" "Allow each test to run up to <n> times in order to pass" "--repeat until-pass=" :prompt "Attempts: ")
    ]]
  [[:if (lambda () (teamake-code-tree-p (transient-scope)))
    :description "Commands"
     ("p" "Execute test preset" teamake-ctest--run-preset)
    ]]
  (interactive (list (teamake-get-root default-directory)))
  (transient-setup 'teamake-ctest '() '() :scope path)
  )

(provide 'teamake-ctest)
;;; teamake-ctest.el ends here
