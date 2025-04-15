
;;; Code:

(require 'transient)

(defvar cmake-project '()
  "Current selected CMake project.")

(defun cmake--project-describe ()
  "Show the current `cmake-project' if set."
  (format " (%s)"
          (propertize
           (if cmake-project
               (file-name-directory cmake-project)
             "No cmake project selected.")
           'face 'transient-value)))

(defun cmake-set-project-root (project)
  "Set CMake project root to PROJECT."
  (interactive
   (let ((d (read-directory-name "Select CMake project directory: " '() '() '() cmake-project)))
     (list d)))
  (if (not (file-exists-p (file-name-concat project "CMakeLists.txt")))
      (user-error "Selected cmake project %s does not contain a CMakeLists.txt file!" project))

  (setq cmake-project project)
  (message "CMake project updated"))



(transient-define-prefix cmake-project ()
  "CMake project menu."
  ["CMake\n"
   ("C" cmake-set-project-root :transient t
    :description cmake--project-describe)]
  ["Major uses\n"
   ;; ("P" cmake-presets :description "Presets")
   ;; ("M" cmake-manual-configuration :description "Manual configuration")
   ])

(defvar cmake-configure-preset '()
  "Currently selected CMake configure preset.")

(defun cmake--configure-preset-describe ()
  ""
  (format " (%s)"
          (propertize
           (if cmake-configure-preset
               cmake-configure-preset
             "No preset selected"))))

(defvar cmake-prefix-command "\"C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvarsall.bat\" x64"
  "Command to prefix shell command before calling cmake commands.")

(defvar cmake-preferred-shell "c:/Program Files/Emacs/emacs-29.3_2/libexec/emacs/29.3/x86_64-w64-mingw32/cmdproxy.exe"
  "Preferred shell to execute commands in.")

(defun cmake--execute (command)
  "Execute COMMAND in the preferred shell."
  (if (not cmake-preferred-shell)
      (setq cmake-preferred-shell shell-file-name))
  (let ((shell-file-name cmake-preferred-shell)
        (prefix-command (if cmake-prefix-command (concat cmake-prefix-command " && ") "")))
    (shell-command-to-string (format "%s%s" prefix-command command))))

(defun cmake--list-configure-presets ()
  "List all configure presets in `cmake-project' root."
  (cmake--execute (format "cmake -S \"%s\" --list-presets" cmake-project)))

;; (cmake--list-configure-presets)

(defun cmake--select-configure-preset (preset)
  "Set configure preset to PRESET."
  (interactive
   (let ((p (completing-read "Select preset: " (cmake--list-configure-presets) '() t cmake-configure-preset)))
     (list p)))
  (setq cmake-configure-preset preset)
  (message "CMake configure preset %s selected" preset))

(defun cmake--preset-available (filename)
  "Return \"Available\" if FILENAME is available, otherwise \"Unavailable\""
  (if (file-exists-p (file-name-concat cmake-project filename))
      "Available"
    "Unavailable"))

(defun cmake--presets-available ()
  "Format string if presets are available or not."
  (format " Project: %s User: %s"
          (cmake--preset-available "CMakePresets.json")
          (cmake--preset-available "CMakeUserPresets.json")))



(transient-define-prefix cmake-presets ()
  "CMake presets menu."
  ["Presets:"
   (:info #'cmake--presets-available :format " %d")
   ("C" cmake--select-configure-preset :transient t
    :description cmake--configure-preset-describe)
   ("B" cmake--select-build-preset :transient t
    :description cmake--build-preset-describe)])

;;; (cmake-project)


;; (defvar tsc-creativity-subjective "I Just press buttons on my gen-AI"
;;   "An unverifiable statement about the user's creativity.")

;; (defvar tsc-creativity-objective 30
;;   "User's creativity percentile as assesed by our oracle")

;; (defun tsc-creativity-subjective-update (creativity)
;;   "Update the users creativity assessment subjectively."
;;   (interactive (list (read-string "User subjective creativity: "
;;                                   tsc-creativity-subjective)))
;;   (setq tsc-creativity-subjective creativity)
;;   (message "Subjective creativity updated: %s" tsc-creativity-subjective))

;; (defun tsc-creativity-objective-update (creativity)
;;   "Update the users creativity assessment objectively."
;;   (interactive (list (read-number "User objective creativity: "
;;                             tsc-creativity-objective)))
;;   (if (and (integerp creativity) (>= 100 creativity -1))
;;       (progn (setq tsc-creativity-objective creativity)
;;              (message "Objective creativity updated: %d"
;;                       tsc-creativity-objective))
;;     (user-error "Only integers between 0 and 100 allowed")))


;; (defun tsc--creativity-subjective-describe ()
;;   "Describe command and display current subjective state."
;;   (format "subjective: %s" (propertize tsc-creativity-subjective
;;                                        'face 'transient-value)))

;; (defun tsc--creativity-objective-describe ()
;;   "Describe command and display current objective state."
;;   (format "objective: %s"
;;           (propertize (number-to-string tsc-creativity-objective)
;;                       'face 'transient-value)))

;; ;; When we can't jsut use a symbol for what we want to display, write a function
;; (defun tsc--creativity-display ()
;;   "Returns a formatted assessment of the users value as a human being."
;;   (format "User creativity score of %s self-assesses: %s"
;;           (propertize tsc-creativity-subjective 'face 'transient-value)
;;           (propertize (number-to-string tsc-creativity-objective)
;;                       'face 'transient-value)))

;; (transient-define-prefix tsc-defvar-settings ()
;;   "A prefix demonstrating file-based ad-hoc persistence."
;;   ;; Note the sharpquote (#') used to distinguish a symbol from just a function
;;   ["Creativity\n"
;;    (:info #'tsc--creativity-display :format " %d")
;;    " "
;;    ("d" tsc-creativity-subjective-update :transient t
;;     :description tsc--creativity-subjective-describe)
;;    ("o" tsc-creativity-objective-update :transient t
;;     :description tsc--creativity-objective-describe)])

;; ;; (tsc-defvar-settings)



;; (provide 'cmake-menu)
;;; cmake-menu.el ends here
