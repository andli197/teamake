
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

(defun teamake-process--start-process (program &optional name path &rest args)
  "Start an asynchronous process of PROGRAM with NAME at PATH with ARGS.

Output will be displayed in the process buffer."
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

(defvar teamake-process--cmake-command-history '("--help"))
(defun teamake-process-invoke-cmake (&optional path &rest args)
  "Start processing a CMake command in PATH with ARGS."
  (interactive
   (let* ((path default-directory)
          (teamake-arguments (teamake-process--prompt-user-for-command
                            "cmake " path 'teamake-process--cmake-command-history)))
     (seq-concatenate 'list (list path) teamake-arguments)))

  (let ((cmake-executable (teamake-process--get-cmake-tool "cmake")))
    (apply #'teamake-process--start-process
           cmake-executable
           (file-name-nondirectory cmake-executable)
           path
           args)))

(defun teamake-process-invoke-cmake-in-root (&optional path &rest args)
  "Start processing a Teamake command in code root for PATH with ARGS."
  (interactive
   (let* ((path (teamake-code-root default-directory))
          (cmake-arguments (teamake-process--prompt-user-for-command
                            "cmake " path 'teamake-process--cmake-command-history)))
     (seq-concatenate 'list (list path) cmake-arguments)))

  (apply #'teamake-process-invoke-cmake
         (teamake-code-root path)
         args))

(defun teamake-process-invoke-cmake-in-build-root (&optional path &rest args)
  "Start processing a Teamake command in build root for PATH with ARGS."
  (interactive
   (let* ((path (teamake-build-root default-directory))
          (cmake-arguments (teamake-process--prompt-user-for-command
                            "cmake " path 'teamake-process--cmake-command-history)))
     (seq-concatenate 'list (list path) cmake-arguments)))

  (apply #'teamake-process-invoke-cmake
         (teamake-build-root path)
         args))

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

(defun teamake-ctest-shell-command-to-string (&optional path &rest args)
    "Call `teamake-shell-command-to-string' with ARGS passed to CTest.

Optional PATH is used to set `default-directory' for the processing."
  (apply #'teamake-shell-command-to-string
         path
         (teamake-process--get-cmake-tool "ctest")
         args))

(defun teamake-cpack-shell-command-to-string (&optional path &rest args)
    "Call `teamake-shell-command-to-string' with ARGS passed to CPack.

Optional PATH is used to set `default-directory' for the processing."
  (apply #'teamake-shell-command-to-string
         path
         (teamake-process--get-cmake-tool "cpack")
         args))

(defun teamake-process-file (program &optional path &rest args)
  "Apply `process-file' in project PATH with PROGRAM and ARGS as input.

The DISPLAY parameter is passed along to the process file."
  (let* ((default-directory (or path default-directory))
         (buffer (teamake-process--get-buffer default-directory))
         (inhibit-read-only t))
    (apply #'process-file program '() buffer '() args)))

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
