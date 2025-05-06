
;;; Code:

(require 'teamake-base)

(defcustom teamake-process-preferred-shell
  shell-file-name
  "User preferred shell for executing commands in.

Defaults to `shell-file-name'"
  :type 'string
  :group 'teamake-commands)

(defcustom teamake-process-cmake-tool-path
  (file-name-directory (locate-file "cmake" exec-path exec-suffixes))
  "Location of the cmake tools executables.

Used in calls to cmake, ctest, cpack, etc.
If they are available on PATH this variable is not required to be set."
  :type 'string
  :group 'teamake-commands)

(defcustom teamake-process-verbose
  '()
  "If set to true, the input prompts and commands will be more verbose.

This does not affect the flags send to the cmake command itself but only
for the Emacs user interface."
  :type 'boolean
  :group 'teamake-commands)

(defcustom teamake-process-buffer-base-name
  "TEAMake"
  "Base name of buffers created by `teamake'.

This is used along with the determined project name as process buffer name."
  :type 'string
  :group 'teamake-buffers)

(defun teamake-process--start-process (program path-or-buffer-name &rest args)
  "Start an asynchronous process of PROGRAM with ARGS.

Output will be displayed in the process buffer specified by PATH-OR-BUFFER-NAME.
If it is set to an existing path it will be interpret as such and a root will
be atempted to be deduced.  Otherwise it is assumed to be the name."
  (pcase-let* ((shell-file-name teamake-process-preferred-shell)
               (default-directory
                (teamake-get-root
                 (if (file-exists-p path-or-buffer-name)
                     path-or-buffer-name
                   default-directory)
                 '()
                 default-directory))
               (process-buf (teamake-process--get-buffer path-or-buffer-name))
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
    process))

(defun teamake-process--get-cmake-tool (&optional tool)
  "Use the `teamake-process-cmake-tool-path' for locating TOOL.

Locate the TOOL from cmake tool suite (cmake, ctest, cpack)
using the configured `teamake-process-cmake-tool-path' or
default configured PATH as a fallback."
  (let* ((program (or tool "cmake"))
        (location program))
    (setq location
          (teamake-return-value-or-default
           (file-name-concat teamake-process-cmake-tool-path program)
           program))

    (if (not (file-exists-p location))
        (setq location (locate-file program exec-path exec-suffixes t)))

    (unless (and program (file-exists-p location))
      (user-error "Unable to locate program %s" program))

    location))

(defun teamake-process-invoke-cmake (path-or-buffer-name &rest args)
  "Start processing a CMake command with ARGS.

PATH-OR-BUFFER-NAME if passed along to the
`teamake-process--start-process' command."
  (apply #'teamake-process--start-process
         (teamake-process--get-cmake-tool "cmake")
         path-or-buffer-name
         args))

(defun teamake-process-invoke-ctest (&optional path-or-buffer-name &rest args)
  "Start processing a CTest command in PATH with ARGS."
  (let ((executable (teamake-process--get-cmake-tool "ctest")))
    (apply #'teamake-process--start-process
           executable
           path-or-buffer-name
           args)))

(defun teamake-shell-command-to-string (&optional path &rest args)
  "Call `shell-command-to-string' with ARGS as the command.

The ARGS are concatenated with \" \" and all arguments are shell-quoted
by `shell-quote-argument'.
Optional PATH is used to set `default-directory' for the processing and
the `shell-file-name' is specified by `teamake-process-preferred-shell'."
  (let ((shell-file-name teamake-process-preferred-shell)
        (default-directory (or path default-directory)))

    ;; (shell-command-to-string (mapconcat 'shell-quote-argument args " "))))
    (shell-command-to-string (mapconcat 'shell-quote-argument args " "))))

(defun teamake-cmake-shell-command-to-string (&optional path &rest args)
  "Call `teamake-shell-command-to-string' with ARGS passed to CMake.

Optional PATH is used to set `default-directory' for the processing."
  (apply #'teamake-shell-command-to-string
         (or path default-directory)
         (teamake-process--get-cmake-tool "cmake")
         args))

(defun teamake-cmake-shell-command-to-lines (&optional path &rest args)
  "Call `teamake-cmake-shell-command-to-string' with ARGS passed to CMake.

Optional PATH is used to set `default-directory' for the processing.
Return all lines as a list."
  (split-string
   (apply #'teamake-cmake-shell-command-to-string
          (or path default-directory)
          args)))

(defun teamake-ctest-shell-command-to-string (&optional path &rest args)
    "Call `teamake-shell-command-to-string' with ARGS passed to CTest.

Optional PATH is used to set `default-directory' for the processing."
  (apply #'teamake-shell-command-to-string
         path
         (teamake-process--get-cmake-tool "ctest")
         args))

(defun teamake-process-file (program &optional path &rest args)
  "Apply `process-file' in project PATH with PROGRAM and ARGS as input."
  (let* ((default-directory (or path default-directory))
         (buffer (teamake-process--get-buffer default-directory))
         (inhibit-read-only t))
    (apply #'process-file program '() buffer t args)))

(defun teamake-cmake-process-file (&optional path &rest args)
  "Apply `teamake-process-file' with tool cmake as program."
  (apply #'teamake-process-file
         (teamake-process--get-cmake-tool "cmake")
         path
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

(defun teamake-process--get-buffer (path-or-buffer-name)
  "Create or return the existing process buffer.

If PATH-OR-BUFFER-NAME is an existing path, apply
`teamake-get-name' to deduce name, otherwise use
the value as name."
  (let* ((name (if (file-exists-p path-or-buffer-name)
                   (teamake-get-name path-or-buffer-name)
                 path-or-buffer-name))
         (buffer (get-buffer-create
                  (format "*%s: %s*" teamake-process-buffer-base-name name))))
    (with-current-buffer buffer
      (compilation-mode)
      (read-only-mode t))
    buffer))

(provide 'teamake-process)
;;; teamake-process.el ends here
