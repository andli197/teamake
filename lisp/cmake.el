
;;; Code:

(require 'transient)
(require 'cmake-base)
(require 'cmake-configure)
(require 'cmake-preset)
(require 'cmake-build)

(transient-define-prefix cmake-dispatch (source-path)
  "Invoke a CMake command from the list of available commands."
  [:description
   (lambda ()
     (cmake-project--heading-code-tree "CMake commands" (transient-scope)))
   ("c" "Configure (new/existing)"      cmake-configure)
   ("b" "Build     (existing)"          cmake-build)
   ("p" "Preset    (new/existing)"      cmake-preset)
   ;; ("P" "Package installation using CPack (existing) " cmake-package)
   ;; ("T" "Run tests invoking CTest (existing)"          cmake-test)
   ]
  (interactive (list (cmake-project-code-root default-directory)))
  (transient-setup 'cmake-dispatch '() '() :scope source-path)
  )

(provide 'cmake.el)
;;; cmake.el ends here
