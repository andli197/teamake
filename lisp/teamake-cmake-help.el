
(require 'teamake-process)
(require 'transient)


(defun teamake-cmake-help--prompt-for-type (full-listing prompt)
  (let* ((listings (teamake-cmake-shell-command-to-lines '() full-listing))
         (selection (completing-read prompt listings '() t)))
    selection))

(defun teamake-cmake-help--command (command)
  "Show help for CMake COMMAND."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-command-list" "CMake command")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-command"
   command))

(defun teamake-cmake-help--manual (manual)
  "Show help for CMake MANUAL."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-manual-list" "CMake manual")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-manual"
   manual))

(defun teamake-cmake-help--module (module)
  "Show help for CMake MODULE."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-module-list" "CMake module")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-module"
   module))

(defun teamake-cmake-help--policy (policy)
  "Show help for CMake POLICY."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-policy-list" "CMake policy")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-policy"
   policy))

(defun teamake-cmake-help--property (property)
  "Show help for CMake PROPERTY."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-property-list" "CMake property")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-property"
   property))

(defun teamake-cmake-help--variable (variable)
  "Show help for CMake VARIABLE."
  (interactive
   (list (teamake-cmake-help--prompt-for-type
          "--help-variable-list" "CMake variable")))
  (teamake-process-invoke-cmake
   "CMake help"
   "--help-variable"
   variable))

(transient-define-prefix teamake-cmake-help ()
  ["CMake help"
   ("c" "Command"   teamake-cmake-help--command)
   ("m" "Manual"    teamake-cmake-help--manual)
   ("M" "Module"    teamake-cmake-help--module)
   ("p" "Policy"    teamake-cmake-help--policy)
   ("P" "Property"  teamake-cmake-help--property)
   ("p" "Variable"  teamake-cmake-help--variable)
   ]
  )

(provide 'teamake-cmake-help)
;;; teamake-cmake-help.el ends here
