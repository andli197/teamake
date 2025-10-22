
;;; Code:

(require 'json)
(require 'transient)
(require 'teamake-base)
(require 'teamake-configure)
(require 'teamake-build)
(require 'teamake-ctest)
(require 'teamake-preset)

(defun experimental-describe ()
  (transient-scope))

(defvar experimental-data '())

(defun experimental-setup-from-scope (scope)
  (transient-setup 'experimental '() '()
                   :scope scope
                   :value (experimental-transient-scope-data-get
                           scope
                           'experimental)))

(defun experimental-transient-scope-data-get (scope prefix)
  (let ((existing (plist-get experimental-data scope)))
    (plist-get existing prefix)))

(defun experimental-transient-scope-data-update-specific (scope prefix property new-value)
  (let* ((existing-scope (plist-get experimental-data scope 'string=))
         (existing-values (plist-get existing-scope prefix))
         (suffix "="))
    (if (s-ends-with? suffix property)
        (setq suffix ""))
    (plist-put existing-scope prefix
               (seq-map
                (lambda (opt)
                  (if (s-starts-with? property opt)
                      (concat property suffix new-value)
                    opt))
                existing-values)
               )))

(transient-define-suffix experimental-set-values ()
  (interactive)

  (experimental-transient-scope-data-update-specific
   (transient-arg-value "--scope=" (transient-args 'experimental-child))
   'experimental
   (transient-arg-value "--parameter=" (transient-args 'experimental-child))
   (transient-arg-value "--value=" (transient-args 'experimental-child))))

(transient-define-prefix experimental-child ()
  ["Experimental child"
   ("s" "Set scope" "--scope="
    :prompt "Scope: "
    :reader transient-read-directory)
   ("p" "Parameter" "--parameter="
    :prompt "Parameter: ")
   ("v" "Value" "--value="
    :prompt "Value")
   ("x" experimental-set-values
    :description "Execute")])

(defun experimental-transient-scope-data-update ()
  (interactive)
  (if (not (plist-member experimental-data (transient-scope)))
      (if (not experimental-data)
          (setq experimental-data
                (list (transient-scope)
                      (list transient-current-command '())))
        (plist-put experimental-data
                   (transient-scope)
                   (list transient-current-command '()))))

  (let ((existing-scope (plist-get experimental-data (transient-scope))))
    (plist-put existing-scope
               transient-current-command
               (transient-args transient-current-command))))



(transient-define-prefix experimental (scope)
  [:description (lambda () (experimental-describe))
   ("p" "project" "${project}="
    :prompt "project: ")
   ("s" experimental-transient-scope-data-update
    :description "Save values"
    :transient t)
   ("c" experimental-child
    :description "Child")]
  (interactive (list default-directory))
  (experimental-setup-from-scope scope)
  
  )


(defun teamake-help ()
  "Open help pages about `teamake'."
  (interactive)
  )

(transient-define-prefix teamake ()
  ["Teamake - Transient Emacs CMake integration\n\nProject configuration"
   ("p" "Project name" "${project}="
    :prompt "Name of project: ")
   ("s" "Source dir" "${sourceDir}="
    :prompt "Source dir: "
    :reader transient-read-directory)
   ("b" "Build dir" "${buildDir}="
    :prompt "Build dir ")
   ("i" "Install dir" "${installDir}="
    :prompt "Install dir: ")
   ("v" "Additional variables" "-V"
    :class transient-option
    :prompt "Variables (var1=value1, var2=value2, ...): "
    :multi-value repeat)
   ("h" teamake-help :description "Help")
   ]
  ["CMake Tools"
   ("cc" teamake-configure :description "Configure")
   ("cb" teamake-build :description "Build")
   ("ct" teamake-ctest :description "Test")
   ("ch" teamake-cmake-help :description "Help")
   ]
  )

(provide 'teamake)
