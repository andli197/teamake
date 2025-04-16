
;;; Code:

(defvar cmake-process-preferred-shell
  ;; shell-file-name
  "c:/Program Files/Emacs/emacs-29.3_2/libexec/emacs/29.3/x86_64-w64-mingw32/cmdproxy.exe"
  "User preferred shell to use for executing commands in.")

(defvar cmake-process-cmake-executable
  "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/CMake/CMake/bin/cmake.exe"
  "Location of the cmake executable.
If it is available on PATH this variable is not required to be set.")

(defun cmake-process-start-process (program &rest args)
  "Start the a process from PROGRAM with ARGS as a process and attach properties."
  (pcase-let* ((process-buf (cmake-process-process-buffer))
               (process (apply #'start-file-process
                               (file-name-nondirectory program)
                               process-buf
                               program
                               args)))
    (set-process-buffer process process-buf)
    (process-put process 'default-dir default-directory)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (pop-to-buffer process-buf)
    process))

(defvar cmake-process-command-history '())

(defun cmake-process-invoke (command)
  "Invoke CMake with the given COMMAND and return the output."
  (interactive
   (list (read-string "CMake command: " (or (car cmake-process-command-history) "") 'cmake-process-command-history)))
  (let ((shell-file-name cmake-process-preferred-shell))
    (let ((output (string-replace "" "\n"
                                  (shell-command-to-string
                                   (format "\"%s\" %s"
                                           cmake-process-cmake-executable command)))))
      (message "Output: %s" output))))

(defun cmake-process-start-cmake (&rest args)
  "Start cmake with ARGS and return process object."
  (apply #'cmake-process-start-process
         cmake-process-cmake-executable
         args))

(defun cmake-process-shell-command (command)
  "Execute COMMAND asyncronously and show the output."
  (interactive
   (list (cmake-process-read-shell-command)))

  (let ((default-directory cmake-process-source-path))
    (cmake-process-start-process shell-file-name
                                 shell-command-switch
                                 command)))

(defun cmake-process-read-shell-command (&optional initial-input)
  "Prompt for command to be executed with INITIAL-INPUT as default command."
  (let ((root (cmake-process-root)))
  (while (cmake-process-root)
    (call-interactively 'cmake-process-set-source-path))

  (let ((default-directory (cmake-process-root)))
    (read-shell-command "Async shell command: " initial-input 'cmake-process-command-history))))

(defun cmake-process-process-buffer ()
  "Return the current process buffer."
  (interactive)
  (let ((buffer (get-buffer-create
                 (format "*CMake Process: %s*"
                         (cmake-process-project-name)))))
    (with-current-buffer buffer
        (read-only-mode t))
    buffer))

(provide 'cmake-process)
;;; cmake-process.el ends here
