
;;; Code:

(require 'json)
(require 'cmake-base)
(require 'cmake-configure)

;; Variables

(defvar cmake-preset-source-path ""
  "Selected source path.")

(defvar cmake-preset-configuration ""
  "Selected configuration preset.")

(defvar cmake-preset-build ""
  "Selected build preset.")

;; Utility functions

(defun cmake-presets-get-configuration-preset-objects (source-path)
  "Return the configuration preset objects from SOURCE-PATH."
  (cmake-presets--get-visible-preset-type-objects source-path "configurePresets"))

(defun cmake-presets-get-configuration-presets (source-path)
  "Return the configuration presets from SOURCE-PATH."
  (seq-map
   (lambda (json-object) (cmake-presets--node-name-selector json-object "name"))
   (cmake-presets-get-configuration-preset-objects source-path)
   ))

(defun cmake-presets-get-names (json-array)
  "Return a list with the name for each of the elements JSON-ARRAY."
  (seq-map
   (lambda (json-object) (cmake-presets--node-name-selector json-object "name"))
   json-array))

(defun cmake-presets-get-build-preset-objects (source-path &optional configure-preset)
  "Return the build presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (cmake-presets--get-visible-preset-type-objects source-path "buildPresets" configure-preset))

(defun cmake-presets-get-build-presets (source-path &optional configure-preset)
  "Return the build presets from SOURCE-PATH matching CONFIGURE-PRESET.

If a configure-preset is specified the field \"configurePreset\" in the
build preset must match. If the field is not specified it is considered
non matching."
  (seq-map
   (lambda (json-object) (cmake-presets--node-name-selector json-object "name"))
   (cmake-presets-get-build-preset-objects source-path configure-preset)))

(defun cmake-presets-get-test-preset-objects (source-path &optional configure-preset)
  "Return the test presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (cmake-presets--get-visible-preset-type-objects source-path "testPresets" configure-preset))


(defun cmake-presets-get-test-presets (source-path &optional configure-preset)
  "Return the test presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (seq-map
   (lambda (json-object) (cmake-presets--node-name-selector json-object "name"))
   (cmake-presets-get-test-preset-objects source-path configure-preset)))

(defun cmake-presets--parse-preset-file (preset-file)
  "Parse the PRESET-FILE as a json object and return it."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-file preset-file)))

(defun cmake-presets--get-visible-preset-type-objects (source-path type &optional configure-preset)
  "Return TYPE node from presets found in SOURCE-PATH filtered by CONFIGURE-PRESET."
  (let ((filter-fn
         (lambda (build-preset)
           (and (not (cmake-presets--node-name-selector build-preset "hidden" t))
                (if (not configure-preset) t
                  (string= (cmake-presets--node-name-selector build-preset "configurePreset" t)
                           configure-preset))))))
   (seq-filter
    filter-fn
    (cmake-presets--get-presets source-path type))))

(defun cmake-presets--get-presets (source-path node)
  "Read the NODE from the CMakePresets.json located under SOURCE-PATH."
  (let* ((json-object (cmake-presets--parse-preset-file
                       (file-name-concat source-path "CMakePresets.json")))
         (preset (assoc node json-object 'string=)))
    (if preset
        (cdr preset)
      '()
      )))

(defun cmake-presets--node-name-selector (json-object name &optional no-error)
  "Pick the contents of NAME from JSON-OBJECT."
  (let ((match (assoc name json-object 'string=)))
    (if match (cdr match)
      (if (not no-error)
          (user-error "Missing \"%s\" element in json object" name)))))

;; (setq source-path "c:/Arbetsfiler/Andreas/project/chess/chess/")
;; (cmake-presets-get-configuration-preset-objects source-path)
;; (cmake-presets-get-configuration-presets source-path)
;; (cmake-presets-get-build-preset-objects source-path)
;; (cmake-presets-get-build-presets source-path)
;; (cmake-presets--get-preset-type-objects source-path "buildPresets" "First-configure-preset")
;; (cmake-presets-get-build-preset-objects source-path "First-configure-preset")
;; (cmake-presets-get-build-presets source-path "First-configure-preset")
;; (cmake-presets-get-test-presets source-path)
;; (cmake-presets--get-preset-type source-path "configurePresets")

;; Setters

(defun cmake-preset-set-source-path (source-path)
  "Set the source path to read presets from."
  (interactive
   (list (call-interactively 'cmake-project-root default-directory)))
  (setq cmake-preset-source-path source-path))

(defun cmake-preset-set-configuration-preset (preset &optional source-path)
  "Set the configuration preset to PRESET."
  (interactive
   (list (completing-read "Configuration preset: "
                          (cmake-presets-get-configuration-presets
                           cmake-preset-source-path)
                          '() t)))
  (setq cmake-preset-configuration preset))

(defun cmake-preset-set-build-preset (preset &optional source-path)
  "Set the build preset to PRESET."
  (interactive
   (list (completing-read "Build preset: "
                          (cmake-presets-get-build-presets
                           cmake-preset-source-path
                           cmake-preset-configuration)
                          '() t)))
  (setq cmake-preset-build preset))

;; Descriptions

(defun cmake-preset--describe-source-path ()
  "Set the source path to read presets from."
  (format "Source path (%s)"
          (propertize cmake-preset-source-path 'face 'transient-value)))

(defun cmake-preset--describe-configuration-preset ()
  (format "Configuration (%s)"
          (propertize cmake-preset-configuration 'face 'transient-value)))

(defun cmake-preset--describe-build-preset ()
  (format "Build (%s)"
          (propertize cmake-preset-build 'face 'transient-value)))


;; Executions
(defun cmake-preset-execute-configuration-preset ()
  (interactive)
  (cmake-process-invoke
   (format "-S=%s --preset=%s"
           cmake-preset-source-path
           cmake-preset-configuration)))

(defun cmake-preset-execute-build-preset ()
  (interactive)
  (cmake-process-invoke
   (format "-S=%s --build --preset=%s"
           cmake-preset-source-path
           cmake-preset-build)))

(transient-define-prefix cmake-preset () ;; source-path)
  "Handle presets for CMake project."
  [["Source path"
   ("s" cmake-preset-set-source-path :transient t
     :description cmake-preset--describe-source-path)
   ]]
  [["Configuration"
    ("c" cmake-preset-set-configuration-preset :transient t
     :description cmake-preset--describe-configuration-preset)
    ("C" cmake-preset-execute-configuration-preset :transient t
     :description "Execute the configuration preset")
    ]]
   [["Build"
    ("b" cmake-preset-set-build-preset :transient t
    :description cmake-preset--describe-build-preset)
    ]]
   
  ;; (interactive (list (cmake-project-root)))
  ;; (transient-setup 'cmake-preset '() '() :scope source-path)
  )
  

(provide 'cmake-preset)
;;; cmake-preset.el ends here
