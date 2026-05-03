;;; teamake-cmake --- Processing and raw parsing of CMakes files
;;; Commentary:
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

;; (setq json-schema (teamake-preset--read-json-file "c:/Arbetsfiler/Andreas/emacs/development/cmake-project/lisp/json-schema.org_draft-07_schema.json"))
;; (plist-get json-schema :definitions)

;; (defun teamake-preset-schema--get-properties (json-node version)
;;   "Return the :properties object for version."
;;   (seq-find
;;      (lambda (n)
;;        (= (plist-get (plist-get (plist-get n :properties) :version) :const)
;;           version))
;;      json-node))

;; (defun teamake-preset-parse-schema (schema-file version)
;;   "Parse the json SCHEMA-FILE for CMakePresets in VERSION.

;; This is used for parsing values and building the values from
;; presets to the transients."
;;   (let* ((schema (teamake-preset--read-json-file schema-file)))
;;     )
;;     )


;; (defun teamake-preset-schema--parse-node (json-node definitions)
;;   "Parse JSON-NODE according to read DEFINITIONS."
;;   (let ((node json-node)
;;         (key '())
;;         (value '()))
;;     (while node
;;       (setq key (car node)
;;             value (cadr node)
;;             node (cddr node))
;;       (cond ((eq key :$ref) (teamake-preset-schema--lookup-ref value definitions))
;;             ((eq key :properties) (
;;       ))
;;   )

;; (setq schema (teamake-preset--read-json-file
;;                 "c:/Arbetsfiler/Andreas/emacs/development/cmake-project/lisp/schema.json"))
;; (setq schema-v4-properties
;;       (plist-get (teamake-preset-schema--get-properties
;;                   (plist-get schema :oneOf) 4)
;;                  :properties)
;;       definitions (plist-get schema :definitions))
;; (let ((properties schema-v4-properties)
;;       (prop '())
;;       (val '()))
;;   (while properties
;;     (setq prop (car properties)
;;           val (cadr properties)
;;           properties (cddr properties))
    
;;     (message "%s: %s" prop (plist-get val :$ref))
;;     ))

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

(defun teamake-cmake-select-preset-from-project (project category &optional configure-preset)
  "Read presets from PROJECT and prompt user selection from CATEGORY.

The preset that can be selected must not be hidden nor condition not met.
If the CATEGORY is anything other than :configurePresets, the optional argument
CONFIGURE-PRESET is used to discriminate against a preset not matching that
:configurePreset.  If all presets of CATEGORY are to be presented, no
CONFIGURE-PRESET should be provided."
  ;; TODO: Perhaps cache the presets in project here?
  (teamake-cmake-select-preset-from-path (plist-get project :source-dir) category configure-preset))


(defun teamake-cmake-expand-macros-from-project (project values)
  "In PROJECT expand all macros for VALUES."
  (let ((source-dir (plist-get project :source-dir)))
    (seq-map
     (lambda (value)
       (teamake-expand-expression
        value
        (plist-get project :source-dir)
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

(defun teamake-project--read-project-name-from-cmakelists (source-dir)
  "Read project name from CMakeLists.txt located in SOURCE-DIR."
  (let* ((cmake-lists (file-name-concat source-dir "CMakeLists.txt"))
         (contents (with-temp-buffer
                     (insert-file-contents cmake-lists)
                     (buffer-string))))
    (save-match-data
      (if (string-match "project(\\(.+\\))" contents)
          (car (split-string (match-string 1 contents) " "))))))

(defun teamake-project-name-from-source-dir (source-tree)
  "Deduce project name from CMakeLists.txt from SOURCE-TREE."
  (let ((source-dir (teamake--find-root source-tree "CMakeLists.txt")))
    (if source-dir
        (let* ((cmake-lists (file-name-concat source-dir "CMakeLists.txt"))
               (contents (with-temp-buffer
                           (insert-file-contents cmake-lists)
                           (buffer-string))))
          (save-match-data
            (if (string-match "project(\\(.+\\))" contents)
                (car (split-string (match-string 1 contents) " ")))))
      teamake-undetermined-project-name)))


(defvar teamake-cmake-cache--variable-regexp
  "^\\([-a-zA-Z0-9_]+\\):?\\([a-zA-Z]?+\\)=\\(.?+\\)"
  "Regexp for matching cmake cache variable.")

(defun teamake-cmake-cache--parse-variable-line (line)
  "Parse a CMakeCache line.

If the type is a BOOL the value is converted to ON or OFF
where the values ON and TRUE in either capitalization or lowercase
is interpret as ON, all other values are interpret as OFF."
  (save-match-data
    (string-match teamake-cmake-cache--variable-regexp line)
    (let ((name (match-string 1 line))
          (type (match-string 2 line))
          (value (match-string 3 line)))
      (list :name name
            :type type
            :value (cond ((string= type "BOOL")
                          (if (or (string= (upcase value) "ON")
                                  (string= (upcase value) "TRUE"))
                              "ON"
                            "OFF"))
                         (t value))))))

(defun teamake-cmake-cache--parse-variables (binary-dir)
  "Parse all options in the form <NAME>(:[TYPE])=<VALUE> from BINARY-DIR.

Return the options as an property list."
  (let* ((cache-file (file-name-concat binary-dir "CMakeCache.txt"))
         (contents (and (file-exists-p cache-file)
                        (with-temp-buffer
                          (insert-file-contents cache-file)
                          (buffer-string))))
         result '())
    (if contents
        (seq-do
         (lambda (line)
           (if (string-match teamake-cmake-cache--variable-regexp line)
               (add-to-list 'result (teamake-cmake-cache--parse-variable-line line))))
         (split-string contents "\n")))
    result))

(defun teamake-cmake-cache--get-variable-value (parsed-cache name)
  "Extract the cache object value from PARSED-CACHE with :name NAME."
  (plist-get (seq-find
              (lambda (c)
                (string= (plist-get c :name) name))
              parsed-cache)
             :value))

(defun teamake-cmake-cache--project-name-from-cache (cache)
  "Return the value of cache variable CMAKE_PROJECT_NAME among CACHE."
  (teamake-cmake-cache--get-variable-value
   cache "CMAKE_PROJECT_NAME"))

(defun teamake-cmake-cache--source-dir-from-cache (cache cmake-project-name)
  "Return the value of cache variable <CMAKE-PROJECT-NAME>_SOURCE_DIR among CACHE."
  (teamake-cmake-cache--get-variable-value
   cache (format "%s_SOURCE_DIR" cmake-project-name)))

(defun teamake-cmake-cache--get-project-name (binary-dir &optional cache)
  "Read CMAKE_PROJECT_NAME cache variable from BINARY-DIR."
  (teamake-cmake-cache--project-name-from-cache
   (or cache (teamake-cmake-cache--parse-variables binary-dir))))

(defun teamake-cmake-cache--get-source-dir (binary-dir &optional cache)
  "Read the <PROJECT_NAME>_SOURCE_DIR cache variable from BINARY-DIR."
  (let ((cache (or cache (teamake-cmake-cache--parse-variables binary-dir))))
    (teamake-cmake-cache--source-dir-from-cache
     cache (teamake-cmake-cache--project-name-from-cache cache))))

(defun teamake-cmake-cache--project-from-binary-dir (binary-dir)
  "Try to read source-dir from BINARY-DIR and fetch project for that."
  (interactive)
  (let ((source-dir (teamake-cmake-cache--get-source-dir binary-dir)))
    (unless source-dir
      (user-error "'%s' does not seem to be a cmake binary-dir" binary-dir))
    (teamake-project-from-source-dir source-dir)))

(defun teamake-deduce-project-name (binary-dir)
  "Deduce name of project from BINARY-DIR.

Search order:
1. Locate project matching the cache <PROJECT_NAME>_SOURCE_DIR and return :name.
2. Parse CMAKE_PROJECT_NAME from CMakeCache.txt under BINARY-DIR.
3. Return `teamake-undetermined-project-name'"
  (interactive)
  (or (plist-get (teamake-cmake-cache--project-from-binary-dir binary-dir) :name)
      (teamake-cmake-cache--get-project-name binary-dir)
      teamake-undetermined-project-name))

(provide 'teamake-cmake)
;;; teamake-cmake.el ends here
