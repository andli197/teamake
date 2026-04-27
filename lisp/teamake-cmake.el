;;; teamake-cmake --- Processing and raw parsing of CMakes files
;;; Commentary:
;;; Code:

(require 'json)
(require 'teamake-core)

;; TODO:
;; 1. Rename all function from prefix teamake-preset-- to teamake-cmake--
;; 2. Move functions from teamake-cmake-cache to here
;; 2.1 Rrename all prefix teamake-cmake-cache-- to teamake-cmake--

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
    reference))

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
          (plist-put fused-preset :fusedConfiguration (teamake-preset--fuse configure-preset presets))))
    fused-preset
    ))

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
                       (plist-get condition :string)
                       (plist-get condition :regex)
                       preset))
                     ((string= type "notMatches")  ;; string, regex
                      (teamake-preset--condition-not-matches
                       (plist-get condition :string)
                       (plist-get condition :regex)
                       preset))
                     (t t))
               ))
            ((eq condition :json-false) '())
            (t t))
      )))

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
         (selected-name (completing-read
                         (format "Select %s:" (substring (format "%s" category) 1)) names '() t)))
    (seq-find 
     (lambda (p)
       (string= (teamake-preset--display-name-from-preset p) selected-name))
     selectable)))

(defun teamake-cmake-select-preset-from-path (source-dir category &optional configure-preset)
  "Read presets from SOURCE-DIR and prompt user selection from CATEGORY.

The preset that can be selected must not be hidden nor condition not met.
If the CATEGORY is anything other than :configurePresets, the optional argument
CONFIGURE-PRESET is used to discriminate against a preset not matching that
:configurePreset.  If all presets of CATEGORY are to be presented, no
CONFIGURE-PRESET should be provided."
  (let* ((presets (teamake-cmake-parse-presets source-dir))
         (selectable (teamake-preset--selectable-presets presets category configure-preset))
         (preset (teamake-preset--prompt-selection selectable category)))
    (teamake-preset--fuse preset presets)))

(defun teamake-cmake-expand-macros-from-project (project values)
  "In PROJECT expand all macros for VALUES."
  (let ((source-dir (plist-get project :source-dir)))
    (seq-map (lambda (value) (teamake-expand-expression
                              value (plist-get project :source-dir)
                              (lambda (text source-dir)
                                (teamake-preset--expand-macro text preset source-dir))))
             values)))

(defun teamake-cmake-parse-presets (source-path)
  "Parse presets from SOURCE-PATH."
  (let ((cmake-user-presets-file (file-name-concat source-path "CMakeUserPresets.json"))
        (teamake-presets-file (file-name-concat source-path "CMakePresets.json")))
    (if (file-exists-p cmake-user-presets-file)
        (teamake-preset--parse-file cmake-user-presets-file "CMakePresets.json")
      (teamake-preset--parse-file teamake-presets-file))))


(provide 'teamake-cmake)
;;; teamake-cmake.el ends here
