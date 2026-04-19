;;; teamake-preset --- Cmake preset handling for teamake
;;;
;;; Commentary:
;;;
;;; Code:

(require 'json)
(require 'teamake-core)

(defun teamake-preset--read-json-file (preset-file)
  "Read the file specified by PRESET-FILE as a CMakePresets.json file.

Name must not match CMakePresets.json but format must be that of a preset file."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (json-read-file preset-file)))

(defun teamake-preset--get-referenced-file (reference origin)
  "Return the deduced filepath to REFERENCE from ORIGIN."
  (if (not (file-name-absolute-p reference))
      (file-name-concat (file-name-directory origin) reference)
    reference)
  )

(defun teamake-preset--get-property-as-list (preset property)
  "Return PROPERTY from PRESET as a list.

Property may either be a string, list or empty.
If the property does not exist, return empty list."
  (let ((value (plist-get preset property)))
    (if (stringp value)
        (list value)
      value)))

(defun teamake-preset--get-inheritance-list (preset)
  "Return property :inherits from PRESET as a list."
  (teamake-preset--get-property-as-list preset :inherits))

(defun teamake-preset--is-category (preset category)
  "Determine if PRESET is of CATEGORY.

The CMakePresets is naming categories to configurePresets but it is
benificial to have it in singular instead.  Hence need to convert to string "
  )

(defun teamake-preset--parse-file (filename &optional include)
  "Parse each preset from FILENAME and the optional INCLUDE file.

Reson for INCLUDE being optional is that for CMakeUserPresets.json the
CMakePresets.json is not necessary explicitly included."
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
             (unless (seq-find (lambda (p) (and (string= (plist-get p :name) inheritance)
                                                (eq (plist-get p :category) category)))
                               reachable-presets)
               (error "Unable to resolve inheritance '%s' from '%s' (category '%s') in file '%s'"
                      inheritance (plist-get preset :name) category filename)))
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

(defun teamake-preset-parse-presets (source-path)
  "Parse presets from SOURCE-PATH."
  (let ((cmake-user-presets-file (file-name-concat source-path "CMakeUserPresets.json"))
        (teamake-presets-file (file-name-concat source-path "CMakePresets.json")))
    (if (file-exists-p cmake-user-presets-file)
        (teamake-preset--parse-file cmake-user-presets-file "CMakePresets.json")
      (teamake-preset--parse-file teamake-presets-file))))


(defun teamake-preset--display-name-from-preset (preset)
  "Display name from PRESET.

Prioritize displayName, if it does not exist use preset name."
  (or (plist-get preset :displayName)
      (plist-get preset :name)))
  
(defun teamake-preset--get-description-from-preset (preset)
  "Description from PRESET.

If no description exists, return empty string."
  (or (plist-get preset :description)
      ""))

(defun teamake-preset--get-preset-from-name (name presets)
  "Find preset with displayName or name as NAME among PRESETS."
  (seq-find
   (lambda (preset)
     (or (string= name (plist-get preset :name))
         (string= name (plist-get preset :displayName))
         ))
   presets))

(defun teamake-preset--get-preset-matching (presets match-fn)
  "Find the first preset among PRESETS matching the result of applying MATCH-FN."
  (seq-find
   (lambda (p)
     (funcall match-fn p))
   presets))

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
  "Return a list of all workflow presets starting at INFO."
  (teamake-preset--get-all info :workflowPresets))

(defun teamake-preset--is-visible (preset)
  "Evaluate :hidden property in PRESET.  Return the opposite."
  (not (plist-get preset :hidden)))

(defun teamake-preset--expand-macro (text preset &optional source-dir)
  "Expand TEXT with macro replacement from PRESET."
  (let ((source-dir (or source-dir (teamake--find-root (plist-get preset :file) "CMakeLists.txt"))))
    (cond ((string= text "${presetName}") (plist-get preset :name))
          ((string= text "${generator}") (plist-get preset :generator))
          ((string= text "${dollar}") "$")
          ((string= text "${pathListSep}") path-separator)
          (t (teamake-expand-regular text source-dir)))))

(defun teamake-preset--condition-equals (lhs rhs preset)
  "Expand LHS and RHS macro from PRESET and evaluate equality."
  (string= (teamake-preset--expand-macro lhs preset)
           (teamake-preset--expand-macro rhs preset)))

(defun teamake-preset--condition-not-equals (lhs rhs preset)
  "Expand LHS and RHS macro from PRESET and evaluate inequality."
  (not (teamake-preset--condition-equals lhs rhs preset)))

(defun teamake-preset--condition-in-list (string list preset)
  "Expand STRING macro from PRESET and seach if any match exist in LIST."
  (seq-find
   (lambda (item)
     (teamake-preset--condition-equals string item preset))
   list))

(defun teamake-preset--condition-not-in-list (string list preset)
  "Expand STRING macro from PRESET and seach if no match exist in LIST."
  (not (teamake-preset--condition-in-list string list preset)))

(defun teamake-preset--condition-matches (string regex preset)
  "Expand STRING macro from PRESET and match against REGEX."
  (string-match-p regex (teamake-preset--expand-macro string preset)))

(defun teamake-preset--condition-not-matches (string regex preset)
  "Expand STRING macro from PRESET and match negatively against REGEX."
  (not (teamake-preset--condition-matches string regex preset)))

(defun teamake-preset--is-condition-active (preset)
  "Evaluate :condition property and return if PRESET should be enabled.

NOTE: Only the first condition will be evaluated!"
  (if (not (plist-member preset :condition))
      t
    (let ((condition (plist-get preset :condition)))
      ;; Since we represent a preset as a property list we only
      ;; can parse one condition on each preset for now.
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
            ((eq condition :json-false) '())
            (t t))
      )))

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



(defun teamake-preset--get-user-selectable-presets (presets category &optional configure-preset)
  "Filter out user selectable preset of type CATEGORY from PRESETS.

If optional value CONFIGURE-PRESET is given the preset must match that preset. Usable for other than configuration presets."
  (seq-filter
   (lambda (preset)
     (and (eq (plist-get preset :category) category)
          (if (not configure-preset)
              t
            (string= (plist-get preset :configurePreset) configure-preset))
          (teamake-preset--is-visible preset)
          (teamake-preset--is-condition-active-recursively preset presets)))
   presets))

(defun teamake-preset--is-condition-active-all-the-way (preset presets)
  "Determine if PRESET is active through its inheritance.

PRESETS is representing all presets and CATEGORY is the preset category to select from."
  (let* ((category (plist-get preset :category))
         (current preset)
         (category-presets (seq-filter (lambda (p) (eq (plist-get p :category) category)) presets))
         (inheritance (teamake-preset--get-inheritance-list current))
         (visited '())
         (is-condition-active (teamake-preset--is-condition-active current)))
    (while (and is-condition-active inheritance)
      (setq current (teamake-preset--get-preset-from-name (car inheritance) category-presets)
            inheritance (cdr inheritance))
      (unless (seq-find (lambda (name) (string= (plist-get current :name) name)) visited)
        (add-to-list 'visited (plist-get current :name))
        (setq is-condition-active
              (and is-condition-active (teamake-preset--is-condition-active current)))
        (seq-do
         (lambda (inheritance-name)
           (add-to-list 'inheritance inheritance-name))
         (teamake-preset--get-inheritance-list current))))
    is-condition-active
    ))

(defun teamake-preset--is-match-recursive (preset presets match-fn)
  ;; match-fn should take a preset and current value as input and produce next value
  (let* ((category (plist-get preset :category))
         (current preset)
         (category-presets (seq-filter (lambda (p) (eq (plist-get p :category) category)) presets))
         (inheritance (teamake-preset--get-inheritance-list current))
         (visited '())
         (result (funcall match-fn current t)))
    (while (and result inheritance)
      (setq current (teamake-preset--get-preset-matching
                     category-presets
                     (lambda (p) (string= (plist-get p :name) (car inheritance))))
            inheritance (cdr inheritance))
      (unless (seq-find (lambda (name) (string= (plist-get current :name) name)) visited)
        (add-to-list 'visited (plist-get current :name))

        (setq result (funcall match-fn current result))
        
        (seq-do
         (lambda (inheritance-name)
           (add-to-list 'inheritance inheritance-name))
         (teamake-preset--get-inheritance-list current))))
    result
    ))

(defun teamake-preset--selectable-presets (presets category &optional configure-preset)
  "Return all PRESETS matching CATEGORY and is available for selection.

Selection availability is determined from if the preset is visible and if
the condition thoughout the inheritance is true.  If the optional parameter
CONFIGURE-PRESET is provided (and the category is not :configurePresets) the
preset must match it to be selectable."
  (seq-filter
   (lambda (preset)
     (and (eq (plist-get preset :category) category)
          (teamake-preset--is-visible preset)
          ;; Check if current preset´s inheritance is matching configure-preset and
          ;; if it has an conditional that evaluates to true
          ;;   => preset is selectable
          (teamake-preset--is-match-recursive
           preset presets
           (lambda (inner value)
             (and value (teamake-preset--is-condition-active inner)
                  (if (and configure-preset (plist-member inner :configurePreset))
                      (string= (plist-get inner :configurePreset) configure-preset)
                    t))))))
   presets))

(defun teamake-preset--prompt-selection (selectable category)
  (let* ((names (seq-map (lambda (p) (teamake-preset--display-name-from-preset p)) selectable))
         (selected-name (completing-read (format "Select %s:" (substring (format "%s" category) 1)) names '() t)))
    (seq-find 
     (lambda (p)
       (string= (teamake-preset--display-name-from-preset p) selected-name))
     selectable)))

(transient-define-suffix teamake-preset-select-from-project (project category)
  "Select preset in PROJECT for the given CATEGORY and use as current."
  (interactive)
  (let ((preset (teamake-preset-select-from-path
                 (plist-get project :source-dir)
                 category
                 (teamake-preset--get-current-configuration-preset-name project))))
    (teamake-preset--set-current project category preset)
    preset))

(defun teamake-preset-select-from-path (source-dir category &optional configure-preset)
  "Read presets from SOURCE-DIR and prompt user selection from CATEGORY.

The preset that can be selected must not be hidden nor condition not met.
If the CATEGORY is anything other than :configurePresets, the optional argument
CONFIGURE-PRESET is used to discriminate against a preset not matching that
:configurePreset.  If all presets of CATEGORY are to be presented, no
CONFIGURE-PRESET should be provided."
  (let* ((presets (teamake-preset-parse-presets source-dir))
         (selectable (teamake-preset--selectable-presets presets category configure-preset))
         (preset (teamake-preset--prompt-selection selectable category)))
    (teamake-preset--fuse preset presets)
    ))

(defun teamake-preset-expand-macros-from-project (project values)
  "In PROJECT expand all macros for VALUES."
  (let ((source-dir (plist-get project :source-dir)))
    (seq-map (lambda (value) (teamake-expand-expression
                              value (plist-get project :source-dir)
                              (lambda (text source-dir)
                                (teamake-preset--expand-macro text preset source-dir))))
             values)))




(defun teamake-preset--get-user-selectable-configure-presets (presets)
  "Filter out user selectable configuration presets from PRESETS."
  (teamake-preset--get-user-selectable-presets presets :configurePresets))

(defun teamake-preset--get-user-selectable-build-presets (presets configure-preset)
  "Filter out user selectable build presets from PRESETS matching the CONFIGURE-PRESET."
  (teamake-preset--get-user-selectable-presets presets :buildPresets configure-preset))

(defun teamake-preset--get-user-selectable-test-presets (configure-preset presets)
  "Filter out user selectable test presets from PRESETS matching the CONFIGURE-PRESET."
  (teamake-preset--get-user-selectable-presets presets :testPresets configure-preset))

(defun teamake-preset--get-current (project category)
  "Return current CATEGORY from PROJECT."
  (interactive)
  (let ((presets (teamake-get-current-values 'teamake-preset project)))
    (message "(plist-get presets category)=%s" (plist-get presets category))
    (plist-get presets category)
    ))

(defun teamake-preset--set-current (project category preset)
  "Set current CATEGORY in PROJECT to PRESET."
  (interactive)
  (let ((presets (teamake-get-current-values 'teamake-preset project)))
     (setq presets (plist-put presets category preset))
    (teamake-set-current-values 'teamake-preset project presets)))




;; Specific
;;==================

(defun teamake-preset--get-current-configuration-preset (project)
  "Return current configuration preset from PROJECT."
  (interactive)
  (teamake-preset--get-current project :configurePresets))

(defun teamake-preset--get-current-build-preset (project)
  "Return current build preset for PROJECT."
  (interactive)
  (teamake-preset--get-current project :buildPresets))

(defun teamake-preset--get-current-test-preset (project)
  "Return current test preset for PROJECT."
  (teamake-preset--get-current project :testPresets))

(defun teamake-preset--get-current-configuration-preset-name (project)
  "Return current configuration preset name from PROJECT."
  (interactive)
  (plist-get (teamake-preset--get-current-configuration-preset project) :name))

(defun teamake-preset--set-current-configuration-preset (project preset)
  "Set current configuration preset in PROJECT to PRESET."
  (teamake-preset--set-current project :configurePresets preset))

(defun teamake-preset--set-current-build-preset (project preset)
  "Set current build PRESET in PROJECT."
  (teamake-preset--set-current project :buildPresets preset))

(defun teamake-preset--set-current-test-preset (project preset-name)
  "Set current test PRESET in PROJECT."
  (teamake-preset--set-current project :testPresets preset))

(defun teamake-preset-select-configuration (project)
  "Select a CMake configuration preset from PROJECT."
  (interactive)
  (let ((preset (teamake-preset-select-configuration-preset-from-path
                 (plist-get project :source-dir)))
        (current-presets (teamake-get-current-values 'teamake-preset project)))
    (plist-put current-presets preset :configurePresets)
    (teamake-set-current-values 'teamake-preset project current-presets)
    preset))

(defun teamake-preset-select-build (project)
  "Select a CMake build preset from PROJECT."
  (interactive)
  (let* ((presets (teamake-preset-parse-presets (plist-get project :source-dir)))
         (configuration-preset (teamake-preset--get-current-configuration-preset project))
         (selectable-build-presets
          (teamake-preset--get-user-selectable-build-presets
           presets (plist-get configuration-preset :name)))
         (selectable-names
          (seq-do (lambda (p) (teamake-preset--display-name-from-preset p))
                  selectable-build-presets))
         (build-preset-name (completing-read "Build preset: " selectable-names '() t))
         (build-preset (teamake-preset--get-preset-from-name build-preset-name selectable-build-presets))
         (fused-build-preset (teamake-preset--fuse (build-preset presets))))
    (teamake-preset--set-current-build-preset fused-build-preset)
    fused-build-preset))

(defun teamake-preset-select-configuration-preset-from-path (source-path)
  "Read all configuration presets from SOURCE-PATH and preset to user."
  ;; save presets in project???????? Needs some thinking, due to cache invalidation
  (let* ((presets (teamake-preset-parse-presets source-path))
         (user-selectable-presets
          (teamake-preset--get-user-selectable-configure-presets presets))
         (names
          (seq-map (lambda (p) (teamake-preset--display-name-from-preset p))
                   user-selectable-presets))
         (preset-name
          (completing-read "Configuration preset: "
                           names '() t)))
    (teamake-preset--fuse (teamake-preset--get-preset-from-name preset-name presets)
                          presets)))

(defun teamake-preset-select-build-preset (project)
  "Select a CMake build preset from PROJECT."
  (let* ((presets (teamake-preset-parse-presets (plist-get project :source-dir)))
         (config-preset (teamake-preset--get-current-configuration-preset-name project))
         (user-selectable-presets
          (teamake-preset--get-user-selectable-build-presets presets config-preset))

         (names
          (seq-map (lambda (p) (teamake-preset--display-name-from-preset p))
                   user-selectable-presets))
         (preset-name
          (completing-read "Build preset: "
                           names '() t)))
    (teamake-preset--get-preset-from-name preset-name presets)))

(defun teamake-preset-select-test-preset-from-project (project)
  "Select a CMake test preset from PROJECT."
  (let* ((presets (teamake-preset-parse-presets (plist-get project :source-dir)))
         (config-preset (teamake-preset--get-current-configuration-preset-name project))
         (user-selectable-presets
          (teamake-preset--get-user-selectable-test-presets config-preset presets))

         (names
          (seq-map (lambda (p) (teamake-preset--display-name-from-preset p))
                   user-selectable-presets))
         (preset-name
          (completing-read "Test preset: "
                           names '() t)))
    (teamake-preset--get-preset-from-name preset-name presets)))

(transient-define-suffix teamake-preset--configuration ()
  :description
  (lambda () (format "Configuration (%s)"
                     (propertize
                      (or (teamake-preset--get-current-configuration-preset-name (transient-scope))
                          "No configuration selected")
                      'face 'transient-value)))
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-configuration-preset project)))
    (teamake-preset--set-current-configuration-preset project preset)
    (transient-setup transient-current-command '() '() :scope project)))

(transient-define-suffix teamake-preset--build ()
  :description
  (lambda () (format "Build (%s)"
                     (propertize
                      (or (plist-get (teamake-preset--get-current-build-preset (transient-scope)) :name)
                          "No build selected")
                      'face 'transient-value)))
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-build-preset project)))
    (teamake-preset--set-current-build-preset project preset)
    (transient-setup transient-current-command '() '() :scope project)))

(transient-define-suffix teamake-preset--test ()
  :description
  (lambda () (format "Test (%s)"
                     (propertize
                      (or (teamake-preset--get-current-test-preset (transient-scope))
                          "No test selected")
                      'face 'transient-value)))
  (interactive)
  (let* ((project (transient-scope))
         (preset (teamake-preset-select-test-preset-from-project project)))
    (teamake-preset--set-current-test-preset project (plist-get preset :name))
    (transient-setup transient-current-command '() '() :scope project)))


(transient-define-prefix teamake-preset (project)
  "Handle cmake presets for PROJECT."
  [:description
   (lambda ()
     (format "CMake presets for %s"
             (propertize (plist-get (transient-scope) :name))))
   ("c" teamake-preset--configuration)
   ("b" teamake-preset--build)
   ("t" teamake-preset--test)
   ;; ("p" "Package" "--cc")
   ;; ("w" "Workflow" "--cc")
   ]
  ["Do"
   ;; ("C" teamake-preset--execute-configuration)
   ;; ("B" teamake-preset--execute-build)
   ;; ("T" teamake-preset--execute-test)
   ;; ("P" teamake-preset--execute-package)
   ;; ("W" teamake-preset--execute-workflow)
   ]
  (interactive (list (teamake-project-get-project-from-path
                      (teamake--find-root default-directory "CMakeLists.txt"))))
  (transient-setup 'teamake-preset '() '() :scope project)
  )

(provide 'teamake-preset)
;;; teamake-preset.el ends here
