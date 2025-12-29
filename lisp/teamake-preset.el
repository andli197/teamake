;;; Code:

(require 'json)
(require 'transient)
(require 'teamake-base)
(require 'teamake-process)

;; Variables
(defvar teamake-preset--selected-configuration '()
  "Currently selected configuration preset.")

(defvar teamake-preset--selected-build '()
  "Currently selected build preset.")

;; Utility functions


(defun teamake-preset--get-property-as-list (preset property)
  "Return PROPERTY from PRESET as a list.

If the property does not exist, return empty list."
  (let ((value (plist-get preset property)))
    (if (stringp value)
        (list value)
      value)))

(defun teamake-preset--get-inheritance-list (preset)
  "Return property :inherits from PRESET as a list."
  (teamake-preset--get-property-as-list preset :inherits))

(defun teamake-preset--get-referenced-file (reference origin)
  "Return the deduced filepath to REFERENCE from ORIGIN."
  (if (not (file-name-absolute-p reference))
      (file-name-concat (file-name-directory origin) reference)
    reference)
  )

(defun teamake-preset--parse-file (filename &optional include)
  "Parse each preset from FILENAME and the optional INCLUDE file."
  (let* ((presets '())
         (file-contents (teamake-preset--read-json-file filename))
         (includes (plist-get file-contents :includes))
         (reachable-presets '()))
    (if include (add-to-list 'includes include t))

    (seq-do (lambda (included-file)
              (seq-do
               (lambda (p)
                 (add-to-list 'reachable-presets p t))
               (teamake-preset--parse-file
                (teamake-preset--get-referenced-file included-file filename))
               ))
            includes)
    (seq-do
     (lambda (category)
       (seq-do
        (lambda (preset)
          (plist-put preset :file filename)
          (plist-put preset :category category)
          (add-to-list 'reachable-presets preset t)

          (seq-do
           (lambda (inheritance)
             (unless (seq-find
                      (lambda (p)
                        (and (string= (plist-get p :name) inheritance)
                             (eq (plist-get p :category) category)))
                      reachable-presets)
               (error "Unable to resolve inheritance '%s' from '%s' (category '%s') in file '%s'"
                      inheritance
                      (plist-get preset :name)
                      category
                      filename)
               )
             )
           (teamake-preset--get-inheritance-list preset))
            
          )
        (plist-get file-contents category)))
     '(:configurePresets :buildPresets :testPresets :packagePresets :workflowPresets))

    (seq-do
     (lambda (preset)
       (let ((name (plist-get preset :name))
             (category (plist-get preset :category)))
         (if (seq-find (lambda (p)
                         (and (string= name (plist-get p :name))
                              (eq category (plist-get p :category))))
                       presets)
             (error "Duplicate presets '%s' in category '%s' found!"
                    name
                    category)))
       (add-to-list 'presets preset))
     reachable-presets)
    presets)
  )

(defun teamake-preset--fuse-preset (fused-preset preset)
  "Fuse PRESET with FUSED-PRESET."
  (seq-do
   (lambda (prop-key-val)
     (let* ((property (car prop-key-val))
            (value (cadr prop-key-val))
            (pre-existing (plist-get fused-preset property)))
       ;; Name and file are not relevant when fusing
       (if (not (seq-find (lambda (p) (eq property p)) '(:name :file)))
           (if (plist-member fused-preset property)
               (cond ((eq property :cacheVariables)
                      (seq-do
                       (lambda (cache_expression)
                         ;; possibly merge the two property lists here instead...
                         ;; This will cause all values to be added but using plist-get
                         ;; will only return the first match. Since appending to the
                         ;; list here these inherited values will effectively be overridden.
                         (add-to-list 'pre-existing cache_expression t))
                       value)
                      (plist-put fused-preset property pre-existing)
                      ))
             (plist-put fused-preset property value)))))
   (seq-partition preset 2))
  fused-preset)

(defun teamake-preset--fuse (preset presets)
  "Fuse all properties to a new full preset starting with PRESET.

Fuse all properties in the inheritance list to a resulting preset.
Also find full configurePreset if available for current PRESET.
Available presets are specified by PRESETS."

  (let ((fused-preset (list :name (plist-get preset :name))))
    (teamake-preset--fuse-preset fused-preset preset)
    (seq-do
     (lambda (name)
       (let ((inherit-preset
              (seq-find (lambda (candidate)
                          (and (string= name (plist-get candidate :name))
                               (eq (plist-get fused-preset :category)
                                   (plist-get candidate :category))))
                        presets)))
         (teamake-preset--fuse-preset
          fused-preset
          (teamake-preset--fuse inherit-preset presets))))
     (teamake-preset--get-inheritance-list preset))
    (if (plist-member preset :configurePreset)
        (let* ((configure-name (plist-get preset :configurePreset))
               (configure-preset (seq-find (lambda (candidate)
                                              (and (string= configure-name (plist-get candidate :name))
                                                   (eq (plist-get candidate :category) :configurePresets)))
                                           presets)))
          (setq fused-configuration (teamake-preset--fuse configure-preset presets))
          (plist-put fused-preset :fusedConfiguration fused-configuration)))
    fused-preset
    ))

(defun teamake-preset--read-json-file (preset-file)
  "Read the file specified by PRESET-FILE as a CMakePresets.json file.

Name must not match CMakePresets.json but format must be that of a preset file."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (json-read-file preset-file)))

(defun teamake-preset-parse-presets (source-path)
  "Parse presets from SOURCE-PATH."
  (let ((cmake-user-presets-file (file-name-concat source-path "CMakeUserPresets.json"))
        (cmake-presets-file (file-name-concat source-path "CMakePresets.json")))
    (if (file-exists-p cmake-user-presets-file)
        (teamake-preset--parse-file cmake-user-presets-file "CMakePresets.json")
      (teamake-preset--parse-file cmake-presets-file))
    ))

(defun teamake-preset--get-name-from-preset (preset)
  "Extract displayName from PRESET, if it does not exists use name."
  (cond ((plist-member preset :displayName)
         (plist-get preset :displayName))
        ((plist-member preset :name)
         (plist-get preset :name))
        (t "")))
    
(defun teamake-preset--get-description-from-preset (preset)
  "Extract description from PRESET or return nil."
  (if (plist-member preset :description)
      (plist-get preset :description)
    '()))

(defun teamake-preset--get-preset-from-name (name presets)
  "Find preset with displayName or name as NAME among PRESETS."
  (seq-find
   (lambda (preset)
     (or (string= name (plist-get preset :name))
         (string= name (plist-get preset :displayName))
         ))
   presets))

(defun teamake-preset--get-locally-defined-specific (info property predicate)
  "Fetch PROPERTY defined in current INFO with NAME."
  (seq-find
   (lambda (preset)
     (funcall predicate preset))
   (teamake-preset--get-locally-defined info property)))

(defun teamake-preset--get-locally-defined (info property)
  "Fetch all PROPERTY defined in current INFO."
  (let ((contents (plist-get info :contents))
        (property-values '()))
    (if (plist-member contents property)
        (seq-do
         (lambda (p)
           (add-to-list 'property-values p))
         (plist-get contents property)))
    property-values))

(defun teamake-preset--get-all-reachable (info property)
  "Fetch all properties of type PROPERTY starting with INFO.

Recurse over all includes and find all properties of type PROPERTY
and return a flat list with matched objects."
  (let ((property-values (teamake-preset--get-locally-defined info property)))
    (if (plist-member info :includes)
        (seq-do
         (lambda (include)
           (seq-do
            (lambda (p)
              (add-to-list 'property-values p))
            (teamake-preset--get-locally-defined include property)
            ))
         (plist-get info :includes)))
    property-values))

(defun teamake-preset--get-all (info property)
  "Fetch all properties of type PROPERTY starting with INFO.

Recurse over all includes and find all properties of type PROPERTY
and return a flat list with matched objects."
  (let ((contents (plist-get info :contents))
        (property-values '()))
    (if (plist-member contents property)
        (seq-do
         (lambda (p)
           (add-to-list 'property-values p))
         (plist-get contents property)))
    (if (plist-member info :includes)
        (seq-do
         (lambda (include-info)
           (seq-do
            (lambda (p)
              (add-to-list 'property-values p t))
            (teamake-preset--get-all include-info property)))
         (plist-get info :includes)))
    property-values))

(defun teamake-preset--get-configure-presets (info)
  "Return a list of all configure presets starting at INFO."
  (teamake-preset--get-all info :configurePresets))

(defun teamake-preset--get-build-presets (info)
  "Return a list of all build presets starting at INFO."
  (teamake-preset--get-all info :buildPresets))

(defun teamake-preset--get-test-presets (info)
  "Return a list of all test presets starting at INFO."
  (teamake-preset--get-all info :testPresets))

(defun teamake-preset--get-workflow-presets (info)
  "Return a list of all test presets starting at INFO."
  (teamake-preset--get-all info :workflowPresets))

(defun teamake-preset--get-presets-matching (info property predicate)
  "Fetch all presets of PROPERTY reachable from INFO matching PREDICATE."
  (seq-filter
   (lambda (preset)
     (funcall predicate preset))
   (teamake-preset--get-all info property)))

(defun teamake-preset--is-visible (preset)
  "Evaluate :hidden property in PRESET.  Return the opposite."
  (not (plist-get preset :hidden)))

(defun teamake-preset--environment-variable-p (text)
  (string= (substring text 0 4) "$env" ))

(defun teamake-preset--get-env-var-name (text)
  (save-match-data
    (string-match "$env{\\(.+\\)}" text)
    (match-string 1 text)))

(defun teamake-preset--expand-macro (text preset)
  (cond ((string= text "${sourceDir}")
         "<Not yet implemented>")
        ((string= text "${sourceParentDir}")
         "<Not yet implemented>")
        ((string= text "${sourceDirName}")
         "<Not yet implemented>")
        ((string= text "${hostSystemName}")
         (cond ((string-equal system-type "windows-nt") "Windows")
               ((string-equal system-type "gnu/linux") "Linux")
               (t "<Unhandled OS>")))
        ((string= text "${presetName}") (plist-get preset :name))
        ((string= text "${generator}") (plist-get preset :generator))
        (t text)))

(defun teamake-preset--parse-condition-text (text)
  "Parse the TEXT contents from a condition node."
  (cond ((teamake-preset--environment-variable-p text)
         (teamake-preset--get-env-var-name text))
        (t (teamake-preset--expand-macro text))))

(defun teamake-preset--condition-equals (lhs rhs preset)
  (string= (teamake-preset--expand-macro lhs preset)
           (teamake-preset--expand-macro rhs preset)))

(defun teamake-preset--condition-not-equals (lhs rhs preset)
  (not (teamake-preset--condition-equals lhs rhs preset)))


(defun teamake-preset--condition-in-list (string list preset)
  (seq-find
   (lambda (item)
     (teamake-preset--condition-equals string item preset))
   list))

(defun teamake-preset--condition-not-in-list (string list preset)
  (not (teamake-preset--condition-in-list preset)))

(defun teamake-preset--condition-matches (string regex preset)
  (string-match-p regex (teamake-preset--expand-macro string preset)))

(defun teamake-preset--condition-not-matches (string regex preset)
  (not (teamake-preset--condition-matches string regex preset)))

(defun teamake-preset--is-condition-active-recursively (preset presets)
  "Resolve all the inheritance and determine if conditions are valid all the way.

Validate the condition for each inheritance from PRESET and upwards among the
collection of PRESETS."
  (let ((is-active (teamake-preset--is-condition-active preset)))
    (if is-active
        (seq-do
         (lambda (name)
           (let ((inherited-preset
                  (seq-find (lambda (candidate)
                              (and (string= name (plist-get candidate :name))
                                   (eq (plist-get preset :category)
                                       (plist-get candidate :category))))
                            presets)))
             (setq is-active
                   (and is-active
                        (teamake-preset--is-condition-active-recursively inherited-preset presets)))
             ))
         (teamake-preset--get-inheritance-list preset))
        )
    is-active
    )
  )

(defun teamake-preset--is-condition-active (preset)
  "Evaluate :condition property and return if PRESET should be enabled."
  (if (not (plist-member preset :condition))
      t
    (let ((condition (plist-get preset :condition)))
      (cond ((stringp condition)
             ;; type, value => static value, everything but
             ;; litteral string 'false' is considered true
             (not (string= condition "false")))
            ((listp condition)
             (let ((type (plist-get condition :type)))
               (cond ((string= type "equals")    ;; lhs, rhs
                      (teamake-preset--condition-equals
                       (plist-get condition :lhs)
                       (plist-get condition :rhs)
                       preset))
                     ((string= type "notEquals") ;; lhs, rhs
                      (teamake-preset--condition-not-equals
                       (plist-get condition :lhs)
                       (plist-get condition :rhs)
                       preset))
                     ((string= type "inList")    ;; string, list
                      (teamake-preset--condition-in-list
                       (plist-get condition :string)
                       (plist-get condition :list)
                       preset))
                     ((string= type "notInList") ;; string, list
                      (teamake-preset--condition-not-in-list
                       (plist-get condition :string)
                       (plist-get condition :list)
                       preset))
                     ((string= type "matches")    ;; string, regex
                      (teamake-preset--condition-matches
                       (plist condition :string)
                       (plist condition :regex)
                       preset))
                     ((string= type "notMatches")  ;; string, regex
                      (teamake-preset--condition-not-matches
                       (plist condition :string)
                       (plist condition :regex)
                       preset))
                     (t t))
               ))
            (t t))
      )))

(defun teamake-preset--invoke-cmake-preset (source-path preset)
  (teamake-process-invoke-cmake
   source-path
   (format "-S=%s" source-path)
   (format "--preset=%s" (plist-get preset :name))))

(defun teamake-preset--invoke-cmake-build-preset (source-path preset)
  (teamake-process-invoke-cmake
   source-path
   "--build"
   (format "--preset=%s" (plist-get preset :name))))

(defun teamake-preset--cmake-list-build-targets (source-path)
  (let ((targets (teamake-cmake-shell-command-to-lines
                  source-path
                  "--build"
                  (format "--preset=%s" (plist-get teamake-preset--selected-build :name))
                  "--target=help")))
    (seq-map
     (lambda (line)
       (save-match-data
         (string-match "\\(.+\\): .+" line)
         (match-string 1 line)))
     (seq-filter
      (lambda (line)
        (string-search ": phony" line))
      targets))))

(defun teamake-completing-read (prompt collection obj-to-text text-to-obj)
  (let ((mapped-collection (seq-map (lambda (o) (funcall obj-to-text o)) collection)))
    (let ((selection (completing-read prompt mapped-collection '() t)))
      (funcall text-to-obj selection collection))))

(defun teamake-preset--get-user-selectable-configure-presets (presets)
  "Filter out user selectable configuration presets from PRESETS."
  (seq-filter
   (lambda (preset)
     (and (eq (plist-get preset :category) :configurePresets)
          (teamake-preset--is-visible preset)
          (teamake-preset--is-condition-active-recursively preset presets)))
   presets))

(defun teamake-preset--get-user-selectable-build-presets (presets)
  "Filter out user selectable build presets from PRESETS."
  (seq-filter
   (lambda (preset)
     (and (eq (plist-get preset :category) :buildPresets)
          (string= (plist-get preset :configurePreset)
                   (plist-get teamake-preset--selected-configuration :name))
          (teamake-preset--is-visible preset)
          (teamake-preset--is-condition-active-recursively preset presets)))
   presets))

(defun teamake-preset-select-configuration-preset (source-path &optional preset)
  "Set `teamake-preset--selected-configuration' to PRESET.

In interactive mode all presets are pared from SOURCE-PATH and
user is interactively prompted to select one."
  (interactive
   (let ((source-path (teamake-code-root default-directory)))
     (list source-path)))

  (let ((presets (teamake-preset-parse-presets (teamake-code-root source-path))))
    (setq teamake-preset--selected-configuration
          (teamake-preset--fuse
           (if preset preset
             (teamake-completing-read
              "Configuration preset: "
              (teamake-preset--get-user-selectable-configure-presets presets)
              'teamake-preset--get-name-from-preset
              'teamake-preset--get-preset-from-name))
           presets))))

(defun teamake-preset-select-and-execute-configuration-preset (source-path &optional preset)
  (interactive (list (teamake-code-root default-directory)))
  (teamake-preset-select-configuration-preset source-path preset)
  (teamake-preset--invoke-cmake-preset
   source-path
   teamake-preset--selected-configuration))

(defun teamake-preset-select-build-preset (source-path &optional preset)
  "Set `teamake-preset--selected-build' to PRESET.

In interactive mode all presets are pared from SOURCE-PATH and
user is interactively prompted to select one."
  (interactive
   (let ((source-path (teamake-code-root default-directory)))
     (list source-path)))

  (let* ((presets (teamake-preset-parse-presets (teamake-code-root source-path))))
    (setq teamake-preset--selected-build
          (teamake-preset--fuse
           (if preset preset
             (teamake-completing-read
              "Build preset: "
              (teamake-preset--get-user-selectable-build-presets presets)
              'teamake-preset--get-name-from-preset
              'teamake-preset--get-preset-from-name))
           presets))))

(defun teamake-preset-select-and-execute-build-preset (source-path &optional preset)
  (interactive
   (list (teamake-code-root default-directory)))

  (teamake-preset-select-build-preset source-path preset)
  (teamake-preset--invoke-cmake-build-preset
   source-path
   teamake-preset--selected-build))

(defun teamake-preset-describe ()
  (let ((main-heading (propertize "Teamake preset" 'face 'teamake-heading))
        (scope (format "(%s)" (propertize (transient-scope) 'face 'teamake-path))))
    (concat main-heading "\n" scope)))

(defun teamake-preset--describe-configuration ()
  (concat "Configuration preset "
          (if teamake-preset--selected-configuration
              (concat "("
                      (propertize (teamake-preset--get-name-from-preset
                                   teamake-preset--selected-configuration)
                                  'face 'transient-value)
                      ")"))))

(defun teamake-preset--describe-build ()
  (concat "Build preset "
          (if teamake-preset--selected-build
              (concat "("
                      (propertize (teamake-preset--get-name-from-preset
                                   teamake-preset--selected-build)
                                  'face 'transient-value)
                      ")"))))

(transient-define-prefix teamake-preset (scope)
  [:description
   (lambda () (teamake-preset-describe))
   ("c" teamake-preset-select-configuration-preset
    :description teamake-preset--describe-configuration
    :transient t)
   ("b" teamake-preset-select-build-preset
    :description teamake-preset--describe-build
    :transient t)
   ]
  (interactive (list (teamake-root)))
  (transient-setup 'teamake-preset '() '() :scope scope)
  )



(provide 'teamake-preset)
;;; teamake-preset.el ends here
