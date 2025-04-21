
;;; Code:

(require 'cmake-custom)
(require 'cmake-base)

(defcustom cmake-process-preferred-shell
  shell-file-name
  "User preferred shell for executing CMake commands in.

Specifically useful if running Emacs on Windows and having another
`shell-file-name' set than cmdproxy.exe and using VC as compiler since
it is most useful to call the `vcvars.bat' for setting up the paths.

Defaults to `shell-file-name'"
  :type 'string
  :group 'cmake-project-commands)

(defcustom cmake-process-cmake-executable
  (locate-file "cmake" exec-path exec-suffixes)
  "Location of the cmake executable.

If it is available on PATH this variable is not required to be set."
  :type 'string
  :group 'cmake-project-commands)

(defcustom cmake-process-verbose
  '()
  "If set to true, the input prompts and commands will be more verbose.

This does not affect the flags send to the cmake command itself but only
for the Emacs user interface."
  :type 'boolean
  :group 'cmake-project-commands)

(defcustom cmake-process-buffer-base-name
  "CMake project"
  "Base name of buffers created by `cmake-project'.

This is used along with the determined project name as process buffer name."
  :type 'string
  :group 'cmake-project-buffers)

(defun cmake-process--start-process (program &optional name path &rest args)
  "Start a process of PROGRAM with NAME at PATH with ARGS as a process."
  (pcase-let* ((shell-file-name cmake-process-preferred-shell)
               (default-directory (or path default-directory))
               (process-buf (cmake-process--get-buffer default-directory))
               (process (apply #'start-file-process
                               name
                               process-buf
                               program
                               args)))
    (set-process-buffer process process-buf)
    (with-current-buffer process-buf
      (goto-char (point-max))
      (set-marker (process-mark process) (point)))
    (pop-to-buffer process-buf)
    process))

(defun cmake-process--get-cmake-executable ()
  "Return the cmake executable path."
  (let ((cmake-executable "cmake"))
    (setq cmake-executable
          (cmake-return-value-or-default cmake-process-cmake-executable cmake-executable))

    (if (not (file-exists-p cmake-executable))
        (setq cmake-executable (locate-file "cmake" exec-path exec-suffixes t)))

    (unless (and cmake-executable (file-exists-p cmake-executable))
      (user-error "Unable to locate program cmake"))

    cmake-executable))

(defun quote-if-needed (path)
  "Return PATH quoted if it contain spaces."
  (if (string-match-p (regexp-quote " ") path)
      (format "\"%s\"" path)
    path))

(defun cmake-process-get-output (command &optional path)
  "Invoke CMake at the project root of PATH with the given COMMAND.

This command is only ment for lightweight CMake invokation to process
output from the command.  Not to be used to perform configuration,
or any build process since it will only invoke cmake by calling
`cmake-process-cmake-executable'.
If building or other actions are to be performed, please use
`cmake-process-invoke-cmake' for those types, since output is
longer."
  (let ((shell-file-name cmake-process-preferred-shell)
        (default-directory (cmake-project-root (or path default-directory)))
        (output (shell-command-to-string
                 (format "%s %s"
                         (quote-if-needed
                          (cmake-process--get-cmake-executable))
                         command))))
    (string-replace "" "" output)))


(defvar cmake-process--cmake-command-history '("--help"))

(defun cmake-process-invoke-cmake (&optional path &rest args)
  "Start processing a CMake command in PATH with ARGS."
  (interactive
   (let* ((path default-directory)
          (cmake-arguments (cmake-process--prompt-user-for-command
                            "cmake " path (car cmake-process--cmake-command-history))))
     (seq-concatenate 'list (list path) cmake-arguments)))

  (let ((cmake-executable (cmake-process--get-cmake-executable)))
    (apply #'cmake-process--start-process
           cmake-executable
           (file-name-nondirectory cmake-executable)
           path
           args)))

(defun cmake-process-invoke-cmake-in-root (&optional path &rest args)
  "Start processing a CMake command in project root for PATH with ARGS."
  (interactive
   (let* ((path (cmake-project-root default-directory))
          (cmake-arguments (cmake-process--prompt-user-for-command
                            "cmake " path (car cmake-process--cmake-command-history))))
     (seq-concatenate 'list (list path) cmake-arguments)))

  (apply #'cmake-process-invoke-cmake
         (cmake-project-root path)
         args)
  )

(defvar cmake-process--user-command-history '(""))

(defun cmake-process-invoke-command (command &optional path &rest args)
  "Start processing COMMAND in PATH with ARGS."
  (interactive
   (let* ((path default-directory)
          (command (read-string "Command: " (car cmake-process--user-command-history) 'cmake-process--user-command-history))
          (arguments (cmake-process--prompt-user-for-command
                      (file-name-nondirectory command) path 'cmake-process--user-command-history)))
     (seq-concatenate 'list (list path) command)))
  
  (apply #'cmake-process--start-process
         (file-name-nondirectory command)
         (quote-if-needed command)
         source-path
         args))

(defun cmake-process-invoke-command-in-root (command &optional path &rest args)
  "Start processing COMMAND in project root for PATH with ARGS."
  (apply #'cmake-process-invoke-command
         (cmake-project-root path)
         args))

(defun cmake-process--prompt-user-for-command (prompt &optional path initial-input)
  "PROMPT for command to be executed at PATH with INITIAL-INPUT as default command.

The output from this command is splitting the user input on \" \" since it is designed
to be used as input for commands as &rest args types of input.  This means that user
cannot input \" \"."
  (let ((full-prompt
         (if cmake-process-verbose
             (format "(%s) %s " path (string-trim prompt))
           prompt)))
    (split-string (read-string full-prompt initial-input 'cmake-process--cmake-command-history) " " t)))


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
