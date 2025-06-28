
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

(defun teamake-build-execute-build (build-path)
  "Invoke compilation using the current configuration at BUILD-PATH."
  (interactive (list (teamake-build-root default-directory)))
  (apply #'teamake-process-invoke-cmake
            build-path
            "--build"
            build-path
            (transient-args transient-current-command)))

(transient-define-suffix teamake--invoke-cache ()
  (interactive)
  (transient-setup 'teamake-cache '() '() :scope (transient-scope)))

(transient-define-prefix teamake-build (build-path)
  "Invoke a build command on an already existing configuration."
  :value '("--verbose")
  [:if (lambda () (teamake-build-tree-p (transient-scope)))
   :description
   (lambda ()
     (concat
      (teamake-heading "Build" (transient-scope) 'teamake-build-tree-p)
      "\n\n"
      (propertize "Flags" 'face 'teamake-heading)))
   ("-c" "Build target 'clean' first, then build" "--clean-first")
   ("-v" "Verbose output" "--verbose")
   ]
  [:if (lambda () (teamake-build-tree-p (transient-scope)))
   :description "Build"
   ("c" "For multi-configuration tools" "--config="
    :prompt "Configuration "
    :choices ("Debug" "RelWithDebInfo" "Release"))
   ("t" "Build <target> instead of default targets" "--target="
    :prompt "Target "
    :choices (lambda () (teamake-build--read-build-targets (transient-scope))))
   ("p" "Build in parallel using the given number of jobs" "--parallel="
    :prompt "Amount "
    :reader transient-read-number-N+)
   ("d" "Modify cache" teamake--invoke-cache
    :transient transient--do-recurse)
   ]
  [:if (lambda () (teamake-build-tree-p (transient-scope)))
   :description "Execute"
   ("x" teamake-build-execute-build
    :description "Build current tree")
   ]
  ["Help"
   ("h" "CMake help menu" teamake-cmake-help)
   ]
  (interactive (list (teamake-build-root default-directory)))
  (transient-setup 'teamake-build '() '() :scope build-path)
  )

(provide 'teamake-build)
;;; teamake-build.el ends here
