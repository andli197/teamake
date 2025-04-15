
;;; Code:

(require 'transient)
(require 'cmake-configure)
(require 'cmake-process)
(require 'cmake-build)

(transient-define-prefix cmake-dispatch ()
  "Invoke a CMake command from the list of available commands."
  ["Transient and dwim commands\nThe commands validity is specified in parentheses."
   ("B" "Build     (existing)"                         cmake-build)
   ("C" "Configure (new/existing)"                     cmake-configuration)
   ;; ("P" "Package installation using CPack (existing) " cmake-package)
   ;; ("T" "Run tests invoking CTest (existing)"          cmake-test)
   ])

(provide 'cmake.el)
;;; cmake.el ends here
