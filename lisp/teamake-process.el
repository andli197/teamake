
;;; Code:

(require 'teamake-custom)
(require 'teamake-base)

(defcustom teamake-process-preferred-shell
  shell-file-name
  "User preferred shell for executing Teamake commands in.

Specifically useful if running Emacs on Windows and having another
`shell-file-name' set than cmdproxy.exe and using VC as compiler since
it is most useful to call the `vcvars.bat' for setting up the paths.

Defaults to `shell-file-name'"
  :type 'string
  :group 'teamake-commands)

(defcustom teamake-process-teamake-executable
  (locate-file "teamake" exec-path exec-suffixes)
  "Location of the teamake executable.

If it is available on PATH this variable is not required to be set."
  :type 'string
  :group 'teamake-commands)

(defcustom teamake-process-verbose
  '()
  "If set to true, the input prompts and commands will be more verbose.

This does not affect the flags send to the teamake command itself but only
for the Emacs user interface."
  :type 'boolean
  :group 'teamake-commands)

(defcustom teamake-process-buffer-base-name
  "TEAMake"
  "Base name of buffers created by `teamake'.

This is used along with the determined project name as process buffer name."
  :type 'string
  :group 'teamake-buffers)

(defun teamake-process--start-process (program &optional name path &rest args)
  "Start a process of PROGRAM with NAME at PATH with ARGS as a process."
  (pcase-let* ((shell-file-name teamake-process-preferred-shell)
               (default-directory (or path default-directory))
               (process-buf (teamake-process--get-buffer default-directory))
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

(defun teamake-process--get-teamake-executable ()
  "Return the teamake executable path."
  (let ((teamake-executable "cmake"))
    (setq teamake-executable
          (teamake-return-value-or-default teamake-process-teamake-executable teamake-executable))

    (if (not (file-exists-p teamake-executable))
        (setq teamake-executable (locate-file "cmake" exec-path exec-suffixes t)))

    (unless (and teamake-executable (file-exists-p teamake-executable))
      (user-error "Unable to locate program cmake"))

    teamake-executable))

(defun quote-if-needed (path)
  "Return PATH quoted if it contain spaces."
  (if (string-match-p (regexp-quote " ") path)
      (format "\"%s\"" path)
    path))

(defun teamake-process-get-output (command &optional path)
  "Invoke Teamake at the project root of PATH with the given COMMAND.

This command is only ment for lightweight Teamake invokation to process
output from the command.  Not to be used to perform configuration,
or any build process since it will only invoke teamake by calling
`teamake-process-teamake-executable'.
If building or other actions are to be performed, please use
`teamake-process-invoke-teamake' for those types, since output is
longer."
  (let ((shell-file-name teamake-process-preferred-shell)
        (default-directory (teamake-code-root (or path default-directory)))
        (output (shell-command-to-string
                 (format "%s %s"
                         (quote-if-needed
                          (teamake-process--get-teamake-executable))
                         command))))
    (string-replace "" "" output)))


(defvar teamake-process--teamake-command-history '("--help"))

(defun teamake-process-invoke-teamake (&optional path &rest args)
  "Start processing a Teamake command in PATH with ARGS."
  (interactive
   (let* ((path default-directory)
          (teamake-arguments (teamake-process--prompt-user-for-command
                            "teamake " path 'teamake-process--teamake-command-history)))
     (seq-concatenate 'list (list path) teamake-arguments)))

  (let ((teamake-executable (teamake-process--get-teamake-executable)))
    (apply #'teamake-process--start-process
           teamake-executable
           (file-name-nondirectory teamake-executable)
           path
           args)))

(defun teamake-process-invoke-teamake-in-root (&optional path &rest args)
  "Start processing a Teamake command in code root for PATH with ARGS."
  (interactive
   (let* ((path (teamake-code-root default-directory))
          (teamake-arguments (teamake-process--prompt-user-for-command
                            "teamake " path 'teamake-process--teamake-command-history)))
     (seq-concatenate 'list (list path) teamake-arguments)))

  (apply #'teamake-process-invoke-teamake
         (teamake-code-root path)
         args)
  )

(defun teamake-process-invoke-teamake-in-build-root (&optional path &rest args)
  "Start processing a Teamake command in build root for PATH with ARGS."
  (interactive
   (let* ((path (teamake-build-root default-directory))
          (teamake-arguments (teamake-process--prompt-user-for-command
                            "teamake " path 'teamake-process--teamake-command-history)))
     (seq-concatenate 'list (list path) teamake-arguments)))

  (apply #'teamake-process-invoke-teamake
         (teamake-build-root path)
         args)
  )

(defvar teamake-process--user-command-history '())
(defvar teamake-process--user-command-arguments-history '())

(defun teamake-process-invoke-command (command &optional path &rest args)
  "Start processing COMMAND in PATH with ARGS."
  (interactive
   (let* ((path default-directory)
          (command (teamake-process--prompt-user-for-command
                    "Command" path 'teamake-process--user-command-history t))
          (arguments (teamake-process--prompt-user-for-command
                      (file-name-nondirectory command)
                      path
                      'teamake-process--user-command-arguments-history)))
     (seq-concatenate 'list (list command path) arguments)))
  
  (apply #'teamake-process--start-process
         command
         (file-name-nondirectory command)
         path
         args))

(defun teamake-process-invoke-command-in-root (command &optional path &rest args)
  "Start processing COMMAND in project root for PATH with ARGS."
  (interactive
   (let* ((path (teamake-code-root default-directory))
          (command (teamake-process--prompt-user-for-command
                    "Command" path 'teamake-process--user-command-history t))
          (arguments (teamake-process--prompt-user-for-command
                      (file-name-nondirectory command)
                      path
                      'teamake-process--user-command-arguments-history)))
     (seq-concatenate 'list (list command path) arguments)))

  (apply #'teamake-process-invoke-command
         command
         (teamake-code-root path)
         args))

(defun teamake-process--prompt-user-for-command (prompt &optional path history single-output)
  "PROMPT for command to be executed at PATH with HISTORY.

If SINGLE-OUTPUT is used, only the first output before \" \" is output.
Otherwise the result is split on \" \" and returned as a list."
  (let* ((full-prompt
          (if teamake-process-verbose
              (format "%s: %s " path (string-trim prompt))
            (format "%s " (string-trim prompt))))
         (result (read-string full-prompt (or (car (eval history)) "") history)))
    (if single-output
        result
      (split-string result " " t))))

(defun teamake-process--get-buffer (path)
  "Create or return the existing process buffer for PATH."
  (interactive)
  (let ((buffer (get-buffer-create
                 (format "*%s: %s*"
                         teamake-process-buffer-base-name
                         (teamake-get-name path)))))
    (with-current-buffer buffer
      (compilation-mode)
      (read-only-mode t))
    buffer))

(provide 'teamake-process)
;;; teamake-process.el ends here
