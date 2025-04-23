
;;; Code:

(require 'json)
(require 'teamake-base)
(require 'teamake-process)

;; Variables

(defvar teamake-preset-configuration ""
  "Selected configuration preset.")

(defvar teamake-preset-build ""
  "Selected build preset.")

(defvar teamake-preset-test ""
  "Selected test preset.")

;; Utility functions

(defun teamake-presets-get-configuration-preset-objects (source-path)
  "Return the configuration preset objects from SOURCE-PATH."
  (teamake-presets--get-visible-preset-type-objects source-path "configurePresets"))

(defun teamake-presets-get-configuration-presets (source-path)
  "Return the configuration presets from SOURCE-PATH."
  (seq-map
   (lambda (json-object) (teamake-presets--node-name-selector json-object "name"))
   (teamake-presets-get-configuration-preset-objects source-path)
   ))

(defun teamake-presets-get-names (json-array)
  "Return a list with the name for each of the elements JSON-ARRAY."
  (seq-map
   (lambda (json-object) (teamake-presets--node-name-selector json-object "name"))
   json-array))

(defun teamake-presets-get-build-preset-objects (source-path &optional configure-preset)
  "Return the build presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (teamake-presets--get-visible-preset-type-objects source-path "buildPresets" configure-preset))

(defun teamake-presets-get-build-presets (source-path &optional configure-preset)
  "Return the build presets from SOURCE-PATH matching CONFIGURE-PRESET.

If a configure-preset is specified the field \"configurePreset\" in the
build preset must match. If the field is not specified it is considered
non matching."
  (seq-map
   (lambda (json-object) (teamake-presets--node-name-selector json-object "name"))
   (teamake-presets-get-build-preset-objects source-path configure-preset)))

(defun teamake-presets-get-test-preset-objects (source-path &optional configure-preset)
  "Return the test presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (teamake-presets--get-visible-preset-type-objects source-path "testPresets" configure-preset))


(defun teamake-presets-get-test-presets (source-path &optional configure-preset)
  "Return the test presets from SOURCE-PATH matching CONFIGURE-PRESET."
  (seq-map
   (lambda (json-object) (teamake-presets--node-name-selector json-object "name"))
   (teamake-presets-get-test-preset-objects source-path configure-preset)))

(defun teamake-presets--parse-preset-file (preset-file)
  "Parse the PRESET-FILE as a json object and return it."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-file preset-file)))

(defun teamake-presets--get-visible-preset-type-objects (source-path type &optional configure-preset)
  "Return TYPE node from presets found in SOURCE-PATH filtered by CONFIGURE-PRESET."
  (let ((filter-fn
         (lambda (build-preset)
           (and (not (teamake-presets--node-name-selector build-preset "hidden" t))
                (if (not configure-preset) t
                  (string= (teamake-presets--node-name-selector build-preset "configurePreset" t)
                           configure-preset))))))
   (seq-filter
    filter-fn
    (teamake-presets--get-presets source-path type))))

(defun teamake-presets--get-presets (source-path node)
  "Read the NODE from the TeamakePresets.json located under SOURCE-PATH."
  (let* ((json-object (teamake-presets--parse-preset-file
                       (file-name-concat source-path "TeamakePresets.json")))
         (preset (assoc node json-object 'string=)))
    (if preset
        (cdr preset)
      '()
      )))

(defun teamake-presets--node-name-selector (json-object name &optional no-error)
  "Pick the contents of NAME from JSON-OBJECT."
  (let ((match (assoc name json-object 'string=)))
    (if match (cdr match)
      (if (not no-error)
          (user-error "Missing \"%s\" element in json object" name)))))

;; Setters

(defun teamake-preset-set-configuration-preset (source-path preset)
  "Set the configuration preset to PRESET."
  (interactive
   (let* ((source-path (teamake-code-root default-directory))
          (preset (completing-read "Configuration preset: "
                                   (teamake-presets-get-configuration-presets source-path)
                                   '()
                                   t)))
     (list source-path preset)))
  (setq teamake-preset-configuration preset)
  (let ((args (teamake-preset-arguments)))
    (if (seq-contains-p args "-x" 'string=)
        (teamake-preset--execute-configuration source-path))))

(defun teamake-preset-set-build-preset (source-path preset)
  "Set the build preset to PRESET."
  (interactive
   (let* ((source-path (teamake-code-root default-directory))
          (preset (completing-read "Build preset: "
                                   (teamake-presets-get-build-presets
                                    source-path
                                    teamake-preset-configuration)
                                   '()
                                   t)))
     (list source-path preset)))
  (setq teamake-preset-build preset)
  (let ((args (teamake-preset-arguments)))
    (if (seq-contains-p args "-x" 'string=)
        (teamake-preset--execute-build source-path))))

(defun teamake-preset-set-test-preset (source-path preset)
  "Set the test preset to PRESET."
  (interactive
   (let* ((source-path (teamake-code-root default-directory))
          (preset (completing-read "Test preset: "
                                   (teamake-presets-get-test-presets
                                    source-path
                                    teamake-preset-configuration)
                                   '()
                                   t)))
     (list source-path preset)))

  (setq teamake-preset-test preset)
  (let ((args (teamake-preset-arguments)))
    (if (seq-contains-p args "-x" 'string=)
        (teamake-preset--execute-test source-path))))

;; Descriptions

(defun teamake-preset--describe-configuration-preset ()
  (format "Configuration (%s)"
          (propertize teamake-preset-configuration 'face 'transient-value)))

(defun teamake-preset--describe-build-preset ()
  (format "Build (%s)"
          (propertize teamake-preset-build 'face 'transient-value)))

(defun teamake-preset--describe-test-preset ()
  (format "Test (%s)"
          (propertize teamake-preset-test 'face 'transient-value)))

;; Executions
(defun teamake-preset--execute-configuration (source-path)
  (interactive
   (let ((source-path (teamake-code-root default-directory)))
     (list source-path)))
  (teamake-process-invoke-teamake-in-root
   source-path
   (format "-S=%s" source-path)
   (format "--preset=%s" teamake-preset-configuration)))

(defun teamake-preset--execute-build (source-path)
  (interactive
   (let ((source-path (teamake-code-root default-directory)))
     (list source-path)))
  (teamake-process-invoke-teamake-in-root
   source-path
   "--build"
   (format "--preset=%s" teamake-preset-build)))

(defun teamake-preset--execute-test (source-path)
  (interactive
   (let ((source-path (teamake-code-root default-directory)))
     (list source-path)))
  (teamake-process-invoke-command-in-root
   "ctest"
   source-path
   (format "--preset=%s" teamake-preset-test)))

(defun teamake-preset-arguments ()
  (transient-args 'teamake-preset))

(transient-define-prefix teamake-preset (source-path)
  "Handle presets for Teamake project."
  [:description
   (lambda ()
     (teamake--code-tree-heading "Presets for" (transient-scope)))
     ("c" teamake-preset-set-configuration-preset :transient t
      :description teamake-preset--describe-configuration-preset)
     ("b" teamake-preset-set-build-preset :transient t
      :description teamake-preset--describe-build-preset)
     ("t" teamake-preset-set-test-preset :transient t
      :description teamake-preset--describe-test-preset)]
  ["Execute\n"
   ("C" teamake-preset--execute-configuration :transient t
    :description
    (lambda ()
      (format "Configure %s"
              (propertize teamake-preset-configuration 'face 'transient-value))))
   ("B" teamake-preset--execute-build :transient t
    :description
    (lambda ()
      (format "Build %s"
              (propertize teamake-preset-build 'face 'transient-value))))
   ("T" teamake-preset--execute-test :transient t
    :description
    (lambda ()
      (format "Test %s"
              (propertize teamake-preset-test 'face 'transient-value))))
   ]
  ["Flags"
   ("x" "Execute preset after selection" "-x")]
  (interactive (list (teamake-code-root default-directory)))
  (transient-setup 'teamake-preset '() '() :scope source-path)
  )



(provide 'teamake-preset)
;;; teamake-preset.el ends here
