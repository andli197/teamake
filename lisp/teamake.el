
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

(transient-define-prefix teamake ()
  :value '("${project}=chess" "${sourceDir}=C:/Arbetsfiler/Andreas/project/chess/chess" "${buildDir}=${sourceDir}/../${project}-build" "${installDir}=${sourceDir}/../${project}-install" "-Vfoo=${bar}" "-Vbar=${baz}" "-V${foobar}=${baz}")
  ["Teamake - Transient Emacs CMake integration\n\nProject configuration"
   ("p" "Project name" "${project}="
    :prompt "Name of project: ")
   ("s" "Source code" "${sourceDir}="
    :prompt "Code tree): "
    :reader transient-read-directory)
   ("b" "Build tree" "${buildDir}="
    :prompt "Build tree: ")
   ("i" "Install directory" "${installDir}="
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
