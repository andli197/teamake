
;;; Code:

(require 'cmake-custom)
(require 'cmake-base)

(defcustom cmake-process-preferred-shell
  shell-file-name
  "User preferred shell for executing CMake commands in."
  :type 'string
  :group 'cmake-project-commands)

(defcustom cmake-process-cmake-executable
  ""
  "Location of the cmake executable.

This is typically used for invoking CMake commands that produces an
output that some transient are ment to present the result of to the user.
If it is available on PATH this variable is not required to be set."
  :type 'string
  :group 'cmake-project-commands)

(defcustom cmake-process-setup-build-environment
  ""
  "Command to setup build environment before calling CMake.

Typically a custom command per project for setting up third party dependencies,
location of compilers and other resources or, using MSVC, to call the
`vcvars.bat' before executing configuration or build commands.
If setting this option to locate `vcvars.bat' you typically also want to
set `cmake-process-preferred-shell' to where cmdproxy.exe is located in
your emacs installation."
  :type 'string
  :group 'cmake-project-commands)

(defcustom cmake-process-buffer-base-name
  "CMake project"
  "Base name of buffers created by `cmake-project'.

This is used along with the determined project name as process buffer name."
  :type 'string
  :group 'cmake-project-buffers)

(defun cmake-process-start-process (program &optional path &rest args)
  "Start a process of PROGRAM at PATH with ARGS as a process."
  (pcase-let* ((shell-file-name cmake-process-preferred-shell)
               (default-directory (or path default-directory))
               (process-buf (cmake-process--get-buffer default-directory))
               (process (apply #'start-file-process
                               (file-name-nondirectory program)
                               process-buf
                               program
                               args)))
    (set-process-buffer process process-buf)
    (with-current-buffer process-buf
      (goto-char (point-max))
      (set-marker (process-mark process) (point)))
    (pop-to-buffer process-buf)
    process)
  )

(defvar cmake-process-command-history '())

(defun cmake-process--get-cmake-executable ()
  "Return the cmake executable path if it is set.

If the path contains spaces, quotation marks will be padded in front and back."
  (if (not (string= cmake-process-cmake-executable ""))
      (if (string-match-p (regexp-quote " ") cmake-process-cmake-executable)
          (format "\"%s\"" cmake-process-cmake-executable)
        cmake-process-cmake-executable)
    "cmake"))

(defun cmake-process-get-output (command &optional path)
  "Invoke CMake at the project root of PATH with the given COMMAND.

This command is only ment for lightweight CMake invokation to process
output from the command.  Not to be used to perform configuration,
or any build process since it will only invoke cmake by calling
`cmake-process-cmake-executable'.
If building or other actions are to be performed, please use
`cmake-process-invoke-cmake' for those types of actions since it
will setup the shell using `cmake-process-setup-build-environment'."
  (let ((shell-file-name cmake-process-preferred-shell)
        (default-directory (cmake-project-root (or path default-directory)))
        (output (shell-command-to-string
                 (format "%s %s"
                         (cmake-process--get-cmake-executable)
                         command))))
    (setq output (string-replace "" "" output))
    output
    ))

(defun cmake-process--build-command (command)
  "Append call to `cmake-process-setup-build-environment' before COMMAND.

Create an execution string of `cmake-process-setup-build-environment' and
COMMAND.  If no build environment setup is configured it will be empty."
  command)

(defun cmake-process-invoke-cmake (&optional path &rest args)
  "Start processing a CMake command in PATH with ARGS."
  (apply #'cmake-process-start-process
         (cmake-process--build-command cmake-process-cmake-executable)
         path
         args))

(defun cmake-process-invoke-cmake-in-root (&optional path &rest args)
  "Start processing a CMake command in project root for PATH with ARGS."
  (apply #'cmake-process-invoke-cmake
         (cmake-project-root path)
         args))

(defun cmake-process-invoke-command (command &optional path &rest args)
  "Start processing COMMAND in PATH with ARGS."
  (apply #'cmake-process-start-process
         (cmake-process--build-command command)
         source-path
         args))

(defun cmake-process-invoke-command-in-root (command &optional path &rest args)
  "Start processing COMMAND in project root for PATH with ARGS."
  (apply #'cmake-process-invoke-command
         command
         (cmake-project-root path)
         args))

;; (defun cmake-process-read-shell-command (&optional initial-input)
;;   "Prompt for command to be executed with INITIAL-INPUT as default command."
;;   (let ((root (cmake-process-root)))
;;   (while (cmake-process-root)
;;     (call-interactively 'cmake-process-set-source-path))

;;   (let ((default-directory (cmake-process-root)))
;;     (read-shell-command "Async shell command: " initial-input 'cmake-process-command-history))))

(defun cmake-process--get-buffer (source-path)
  "Create or return the existing process buffer for SOURCE-PATH."
  (interactive)
  (let ((buffer (get-buffer-create
                 (format "*%s: %s*"
                         cmake-process-buffer-base-name
                         (cmake-project-name source-path)))))
    (with-current-buffer buffer
      (compilation-mode)
      (read-only-mode t))
    buffer))

(provide 'cmake-process)
;;; cmake-process.el ends here
