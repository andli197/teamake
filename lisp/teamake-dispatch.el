
;;; Code:

(require 'transient)
(require 'teamake-base)
(require 'teamake-configure)
(require 'teamake-build)
(require 'teamake-ctest)
(require 'teamake-preset)
(require 'teamake-cmake-help)

(transient-define-suffix teamake-invoke-configure (prefix-arg)
  :transient 'transient--do-recurse
  (interactive "P")
  (teamake-configure (transient-arg-value "-S=" (transient-args transient-current-command))))

(transient-define-suffix teamake-invoke-build (prefix-arg)
  :transient 'transient--do-recurse
  (interactive "P")
  (teamake-build (transient-arg-value "-B=" (transient-args transient-current-command))))

(transient-define-suffix teamake-invoke-ctest (prefix-arg)
  :transient 'transient--do-recurse
  (interactive "P")
  (teamake-ctest (transient-arg-value "-B=" (transient-args transient-current-command))))

(transient-define-suffix teamake-invoke-preset (prefix-arg)
  :transient 'transient--do-recurse
  (interactive "P")
  (teamake-preset (transient-arg-value "-S=" (transient-args transient-current-command))))

;;;###autoload
(transient-define-prefix teamake-dispatch (path)
  "Invoke a Teamake command from the list of available commands."
  [[:description (lambda () (teamake-heading "CMake project configuration"))
                 ("n" "Project name" "--project-name="
                  :prompt "Project name: "
                  :always-read t)
                 ("s" "Code tree" "-S="
                  :prompt "Code tree: "
                  :reader transient-read-directory)
                 ("b" "Build tree" "-B="
                  :prompt "Build tree: "
                  :reader transient-read-directory)
                 ]]
  [[:description (lambda () (teamake-heading "Execute"))
    ("C" "Configuration"        teamake-invoke-configure)
    ("B" "Build"                teamake-invoke-build)
    ("T" "Test"                 teamake-invoke-ctest)
    ("P" "Preset"               teamake-invoke-preset)
    ;; ("p" "Package"              teamake-cpack)
    ;; ("w" "Workflow"             teamake-workflow)
    ]]
  [[:description (lambda () (teamake-heading "Experimental"))
    ("E" teamake-experimental)
    ]]
   [["Help"
    ("h" "CMake help menu" teamake-cmake-help)]]
  (interactive
   (let ((path (teamake-get-root default-directory)))
     (list path)))
  (transient-setup 'teamake-dispatch '() '() :scope path)
  )

(defvar teamake-experimental-variable (list :name "Empty"))
(transient-define-infix teamake-experimental ()
  :class 'transient-lisp-variable
  :prompt "Enter a property list: "
  :variable 'teamake-experimental-variable)

(provide 'teamake-dispatch)
;;; teamake-dispatch.el ends here
