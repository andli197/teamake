
;;; Code:

(defvar cmake-project-cmake-executable
  "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/CMake/CMake/bin/cmake.exe"
  "Location of the cmake executable.
If it is available on PATH this variable is not required to be set.")

(defvar cmake-project-command-history '())

(defun cmake-project-project-name ()
  "Return absolute path to cmake-project-source-path."
  (let ((source-path (cmake-project-root)))
    (file-relative-name source-path (file-name-parent-directory source-path))))

(defun cmake-project-start-process (program &rest args)
  "Start the a process from PROGRAM with ARGS as a process and attach properties."
  (pcase-let* ((process-buf (cmake-project-process-buffer))
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

(defun cmake-project-start-cmake (&rest args)
  "Start cmake with ARGS and return process object."
  (apply #'cmake-project-start-process
         cmake-project-cmake-executable
         args))

(defun cmake-project-shell-command (command)
  "Execute COMMAND asyncronously and show the output."
  (interactive
   (list (cmake-project-read-shell-command)))

  (let ((default-directory cmake-project-source-path))
    (cmake-project-start-process shell-file-name
                                 shell-command-switch
                                 command)))

(defun cmake-project-root ()
  "Return absolute path to cmake-project-source-path."
  (while (cmake-project--variable-not-set cmake-project-source-path)
    (call-interactively 'cmake-project-set-source-path))
  cmake-project-source-path)

(defun cmake-project--variable-not-set (variable)
  "Determine if the VARIABLE is set or not."
  (or (eq variable '()) (string= variable "")))

(defun cmake-project-read-shell-command (&optional initial-input)
  "Prompt for command to be executed with INITIAL-INPUT as default command."
  (while (cmake-project--variable-not-set cmake-project-source-path)
    (call-interactively 'cmake-project-set-source-path))

  (let ((default-directory (cmake-project-root)))
    (read-shell-command "Async shell command: " initial-input 'cmake-project-command-history)))


(defun cmake-project-process-buffer ()
  "Return the current process buffer."
  (interactive)
  (let ((buffer (get-buffer-create
                 (format "*CMake Process: %s*"
                         (cmake-project-project-name)))))
    (with-current-buffer buffer
        (read-only-mode t))
    buffer))

(provide 'cmake-process)
;;; cmake-process.el ends here
