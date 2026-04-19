;;; teamake-process --- Base processing functionality for teamake
;;; Commentary:
;;; Code:

(require 'teamake-core)

(defun teamake-process--start-process (project program &rest args)
  "Start an asynchronous process of PROGRAM with ARGS for PROJECT."
  (pcase-let* ((shell-file-name teamake-process-preferred-shell)
               (default-directory (or (plist-get project :source-dir) default-directory))
               (process-buf (teamake-process--get-buffer project))
               (process (apply #'start-file-process
                               (file-name-nondirectory program)
                               process-buf
                               program
                                args)))
    (set-process-buffer process process-buf)
    (with-current-buffer process-buf
      (goto-char (point-max))
      (set-marker (process-mark process) (point)))
    ;; (pop-to-buffer process-buf)
    process))

(defun teamake-process--get-cmake-tool (&optional tool)
  "Use the `teamake-process-cmake-tool-path' for locating TOOL.

Locate the TOOL from cmake tool suite (cmake, ctest, cpack)
using the configured `teamake-process-cmake-tool-path' or
default configured PATH as a fallback."
  (if (file-exists-p tool)
      tool
    (let* ((program (or tool "cmake"))
           (location program)
           (locations exec-path))
      (add-to-list 'locations (file-name-directory teamake-process-cmake-tool-path))
      (setq location (locate-file program locations exec-suffixes t))
      (unless (file-exists-p location)
        (user-error "Unable to locate program %s" program))
      location)))

(defun teamake-process-invoke-cmake (project &rest args)
  "Start processing a CMake command with ARGS using BUFFER-NAME as output."
  (let ((executable (teamake-process--get-cmake-tool "cmake")))
    (apply #'teamake-process--start-process
           project
           executable
           args)))

(defun teamake-process-invoke-ctest (project &rest args)
  "Start processing a CTest command with ARGS using BUFFER-NAME as output."
  (let ((executable (teamake-process--get-cmake-tool "ctest")))
    (apply #'teamake-process--start-process
           project
           executable
           args)))

(defun teamake-process-invoke-cpack (project &rest args)
  "Start processing a CPack command with ARGS using BUFFER-NAME as output."
  (let ((executable (teamake-process--get-cmake-tool "cpack")))
    (apply #'teamake-process--start-process
           project
           executable
           args)))

(defun teamake-process--get-buffer (project)
  "Create or return the existing process buffer for PROJECT."
  (let* ((name (or (plist-get project :name)
                   (plist-get project :source-dir)
                   "Unknown project"))
         (buffer (get-buffer-create (format "*%s: %s*" teamake-process-buffer-base-name name))))
    (with-current-buffer buffer
      (compilation-mode)
      (read-only-mode t))
    buffer))

(defun teamake-command-to-string (&rest args)
  "Call `shell-command-to-string' with ARGS as the command.

The ARGS are concatenated with \" \" and all arguments are shell-quoted
by `shell-quote-argument'.
PROJECTs source-dir is used to set `default-directory' for the processing and
the `shell-file-name' is specified by `teamake-process-preferred-shell'."
  (let ((shell-file-name teamake-process-preferred-shell))
    (shell-command-to-string (mapconcat 'shell-quote-argument args " "))))

(defun teamake-cmake-command-to-string (&rest args)
  "Invoke cmake with ARGS and return the output.

No output to the projects process buffer."
  (apply #'teamake-command-to-string
         (teamake-process--get-cmake-tool "cmake")
         args))

(defun teamake-cmake-command-to-lines (&rest args)
  "Return a list of all lilnes after invocation of CMake.

Call `teamake-cmake-command-to-string' with ARGS and split output
on newlines."
  (split-string (apply #'teamake-cmake-command-to-string
                       args)
                "\n"))

(provide 'teamake-process)
;;; teamake-process.el ends here
