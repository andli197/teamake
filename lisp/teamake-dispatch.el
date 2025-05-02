
;;; Code:

(require 'transient)
(require 'teamake-base)
(require 'teamake-configure)
(require 'teamake-preset)
(require 'teamake-build)

(transient-define-prefix teamake-dispatch (path)
  "Invoke a Teamake command from the list of available commands."
  ["Teamake dwim commands\n"
   [:if (lambda () (teamake-code-tree-p (transient-scope)))
    :description (lambda () (teamake-heading "Code" (transient-scope) 'teamake-code-tree))
    ("c" "Configuration"         teamake-configure)
    ("p" "Preset execution"      teamake-preset)
    ]
   [:if (lambda () (teamake-build-tree-p (transient-scope)))
    :description (lambda () (teamake-heading "Build" (transient-scope) 'teamake-build-tree))
    ("b" "Build" teamake-build)
    ]
   ]
  (interactive
   (let ((path (teamake-get-root default-directory)))
     (list path)))
  (transient-setup 'teamake-dispatch '() '() :scope path)
  )

(provide 'teamake-dispatch)
;;; teamake-dispatch.el ends here
