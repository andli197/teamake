
;;; Code:

(require 'transient)
(require 'teamake-base)
(require 'teamake-cache)

(defun teamake-configure--list-generators ()
  "List all generators supported by CMake binary."
  (let* ((output (teamake-cmake-shell-command-to-string '() "--help"))
         (generators-part (substring output (string-match "Generators" output)))
         (generator-name-expression "[* ]+\\([ -a-zA-Z0-9=]+?\\)[ \n]?+=")
         (generators '()))
    (save-match-data
      (let ((pos 0))
        (while (string-match generator-name-expression generators-part pos)
          (push (match-string 1 generators-part) generators)
          (setq pos (match-end 0))))
      (setq generators (reverse generators)))))

(defun teamake-configure-execute ()
  "Execute the currently configured Teamake command."
  (interactive)
  (let ((command (transient-args transient-current-command))
        (source-path (transient-scope transient-current-command)))
    (setq command (seq-map (lambda (arg) (string-replace "=" " " arg)) command))
    (apply #'teamake-process-invoke-cmake
           default-directory
           "-S"
           source-path
           command)))

(transient-define-prefix teamake-configure (code-path)
  "Invoke a Teamake configuration step."
  :value '("-Wdev" "-Wdeprecated" "-Wno-error=deprecated")
  [:description
   (lambda ()
     (teamake-heading "Configure " (transient-scope) 'teamake-code-tree-p))
   ["Flags"
    ("-f" "Create a fresh build tree, remove any pre-existing cache file" "--fresh")
    ("-w" "Enable developer warnings" "-Wdev")
    ("-W" "Suppress developer warnings" "-Wno-dev")
    ("-e" "Make developer warnings errors." "-Werror=dev")
    ("-E" "Make developer warnings not errors." "-Wno-error=dev")
    ("-d" "Enable deprecation warnings" "-Wdeprecated")
    ("-D" "Suppress deprecation warnings" "-Wno-deprecated")
    ("-c" "Make deprecated macro and function warnings errors" "-Werror=deprecated")
    ("-C" "Make deprecated macro and function warnings not errors" "-Wno-error=deprecated")
    ("--debug" "Put CMake in a debug mode." "--debug-output")
    ]
   ["Settings"
    ("b" "Build path" "-B="
     :prompt "Build path"
     :reader transient-read-directory)
    ("i" "Installation path" "--install-prefix="
     :prompt "Install"
     :reader transient-read-directory)
    ("g" "Generator" "-G="
     :prompt "Generator"
     :choices (lambda () (teamake-configure--list-generators)))
    (5"T" "Toolset" "-T=" :prompt "Toolset")
    (5"a" "Platform" "-A=" :prompt "Platform")
    (6"t" "Toolchain file" "--toolchain="
      :prompt "Toolchain"
      :reader transient-read-file)
    ("x" teamake-configure-execute
     :description "Execute the current configuration")
    ]
   ]
  (interactive (list (teamake-code-root default-directory)))
  (transient-setup 'teamake-configure '() '() :scope code-path))
  

(provide 'teamake-configure)
;;; teamake-configure.el ends here
