
;;; Code:

(require 'transient)
(require 'teamake-base)
(require 'teamake-configure)
(require 'teamake-preset)
(require 'teamake-build)

(transient-define-prefix teamake-dispatch (path)
  "Invoke a Teamake command from the list of available commands."
  
  ["Teamake dwim commands\n"
   [:if (lambda () (teamake-project-code-tree-p (transient-scope)))
    :description (lambda () (teamake-project--code-tree-heading "Code" (transient-scope)))
    ("c" "Configuration"   teamake-configure)
    ("p" "Preset execution"      teamake-preset)
    ;; ("c" "command c" "-c")
    ]
   [:if (lambda () (teamake-project-build-tree-p (transient-scope)))
    :description (lambda () (teamake-project-heading "Build" (transient-scope)))
    ("b" "Build" teamake-build)
    ]
   ]
  ;; [:description
  ;;  (lambda ()
  ;;    (teamake-project--heading-code-tree "Teamake commands" (transient-scope)))
   
  ;;  ("c" "Configure (new/existing)"      teamake-configure)
  ;;  ("b" "Build     (existing)"          teamake-build)
  ;;  ("p" "Preset    (new/existing)"      teamake-preset)
  ;;  ;; ("P" "Package installation using CPack (existing) " teamake-package)
  ;;  ;; ("T" "Run tests invoking CTest (existing)"          teamake-test)
  ;;  ]
  (interactive (list (cond ((teamake-project-build-tree-p default-directory)
                            (teamake-project-build-root default-directory))
                           ((teamake-project-code-tree-p default-directory)
                            (teamake-project-code-root default-directory))
                           (t default-directory))))
  (transient-setup 'teamake-dispatch '() '() :scope path)
  )

(provide 'teamake-dispatch)
;;; teamake-dispatch.el ends here
