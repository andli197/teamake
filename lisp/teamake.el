
;;; Code:

(require 'json)
(require 'transient)
(require 'teamake-base)
(require 'teamake-configure)
(require 'teamake-build)
(require 'teamake-ctest)
(require 'teamake-preset)

(defun teamake-help ()
  "Open help pages about `teamake'."
  (interactive)
  )

(defun teamake-descibe ()
  (let ((main-heading
         (propertize "Teamake - Transient Emacs CMake integration"
                     'face 'teamake-heading))
        (scope (format "(%s)"
                       (propertize (transient-scope) 'face 'teamake-path)))
        (configuration-heading
         (propertize "Project configuration" 'face 'teamake-heading)))
    (concat main-heading "\n" scope "\n\n" configuration-heading)))

(transient-define-suffix teamake-invoke-configure ()
  (interactive)
  (teamake--set-project-configurations-data (transient-scope)
                                            'teamake
                                            (transient-args 'teamake))
  (teamake-invoke-as-subprefix (transient-scope) 'teamake-configure))

(transient-define-suffix teamake-invoke-build ()
  (interactive)
  (teamake--set-project-configurations-data (transient-scope)
                                            'teamake
                                            (transient-args 'teamake))
  (teamake-invoke-as-subprefix (transient-scope) 'teamake-build))

(transient-define-suffix teamake-invoke-ctest ()
  (interactive)
  (teamake--set-project-configurations-data (transient-scope)
                                            'teamake
                                            (transient-args 'teamake))
  (teamake-invoke-as-subprefix (transient-scope) 'teamake-ctest))

(transient-define-prefix teamake (scope)
  [:description (lambda () (teamake-descibe))
   ("p" "Project name" "${project}="
    :prompt "Name of project: ")
   ("s" "Source dir" "${sourceDir}="
    :prompt "Source dir: "
    :reader transient-read-directory)
   ("b" "Build dir" "${buildDir}="
    :prompt "Build dir "
    :reader transient-read-directory)
   ("i" "Install dir" "${installDir}="
    :prompt "Install dir: "
    :reader transient-read-directory)
   ("v" "Additional variables to be used in project" "-V"
    :class transient-option
    :prompt "Variables (var1=value1, var2=value2, ...): "
    :multi-value repeat)
   ;; ("h" teamake-help :description "Help")
   ]
  ["CMake Tools"
   ("cc" teamake-invoke-configure :description "Configure")
   ("cb" teamake-invoke-build :description "Build")
   ("ct" teamake-invoke-ctest :description "Test")
   ("ch" teamake-cmake-help :description "Help")
   ]

  (interactive (list (teamake-root)))
    (transient-setup 'teamake '() '() :scope scope))

(provide 'teamake)
