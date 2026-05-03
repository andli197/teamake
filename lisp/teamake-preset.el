;;; teamake-preset --- Cmake preset handling for teamake
;;; Commentary:
;;; Code:

(require 'json)
(require 'teamake-core)
(require 'teamake-cmake)

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
    is-condition-active))

(transient-define-suffix teamake-preset-select-from-project (project category)
  "Select preset in PROJECT for the given CATEGORY.

The selected preset is fused among the inheritance chain and saved as current preset
for the project, ready to be used."
  (interactive)
  (teamake-preset--set-current
   project category
   (teamake-cmake-select-preset-from-path
    (plist-get project :source-dir)
    category
    (teamake-preset--get-current-configuration-preset-name project))))

(defun teamake-preset--get-current (project category)
  "Return current CATEGORY from PROJECT."
  (interactive)
  (let ((presets (teamake-get-current-values 'teamake-preset project)))
    (plist-get presets category)))

(defun teamake-preset--set-current (project category preset)
  "Set current CATEGORY in PROJECT to PRESET."
  (interactive)
  (let ((presets (teamake-get-current-values 'teamake-preset project)))
     (setq presets (plist-put presets category preset))
     (teamake-set-current-values 'teamake-preset project presets))
  preset)

;; Specific accessors for presets
;;===============================
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

(defun teamake-preset--get-current-package-preset (project)
  "Return current test preset for PROJECT."
  (teamake-preset--get-current project :packagePresets))

(defun teamake-preset--get-current-workflow-preset (project)
  "Return current test preset for PROJECT."
  (teamake-preset--get-current project :workflowPresets))

(defun teamake-preset--get-current-configuration-preset-name (project)
  "Return current configuration preset name from PROJECT."
  (interactive)
  (plist-get (teamake-preset--get-current-configuration-preset project) :name))

;; Specific manipulators for presets
;;==================================
(defun teamake-preset--set-current-configuration-preset (project preset)
  "Set current configuration preset in PROJECT to PRESET."
  (teamake-preset--set-current project :configurePresets preset))

(defun teamake-preset--set-current-build-preset (project preset)
  "Set current build PRESET in PROJECT."
  (teamake-preset--set-current project :buildPresets preset))

(defun teamake-preset--set-current-test-preset (project preset)
  "Set current test PRESET in PROJECT."
  (teamake-preset--set-current project :testPresets preset))

(defun teamake-preset--set-current-package-preset (project preset)
  "Set current package PRESET in PROJECT."
  (teamake-preset--set-current project :packagePresets preset))

(defun teamake-preset--set-current-workflow-preset (project preset)
  "Set current workflow PRESET in PROJECT."
  (teamake-preset--set-current project :workflowPresets preset))

;; Specific selectors for presets
;;===============================
(defun teamake-preset-select-configuration (project)
  "Select a CMake configuration preset from PROJECT."
  (teamake-preset-select-from-project project :configurePresets))

(defun teamake-preset-select-build (project)
  "Select a CMake build preset from PROJECT."
  (interactive)
  (teamake-preset-select-from-project project :buildPresets))

(defun teamake-preset-select-test (project)
  "Select a CMake build preset from PROJECT."
  (interactive)
  (teamake-preset-select-from-project project :testPresets))

(defun teamake-preset-select-package (project)
  "Select a CMake build preset from PROJECT."
  (interactive)
  (teamake-preset-select-from-project project :packagePresets))

(defun teamake-preset-select-workflow (project)
  "Select a CMake build preset from PROJECT."
  (interactive)
  (teamake-preset-select-from-project project :packagePresets))

(defun teamake-preset--description (text project category)
  "Create description for preset CATEGORY.

Construct description from TEXT fetching the name of the current CATEGORY
preset from PROJECT."
  (format "%s (%s)" text
          (propertize (or (plist-get
                           (teamake-preset--get-current project category)
                           :name)
                          "No preset selected")
                      'face 'transient-value)))

(transient-define-suffix teamake-preset--configuration ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (teamake-preset--description "Configure" (transient-scope) :configurePresets))
  (interactive)
  (teamake-preset-select-from-project (transient-scope) :configurePresets))

(transient-define-suffix teamake-preset--build ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (teamake-preset--description "Build" (transient-scope) :buildPresets))
  (interactive)
  (teamake-preset-select-from-project (transient-scope) :buildPresets))

(transient-define-suffix teamake-preset--test ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (teamake-preset--description "Test" (transient-scope) :testPresets))
  (interactive)
  (teamake-preset-select-from-project (transient-scope) :testPresets))

(transient-define-suffix teamake-preset--package ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (teamake-preset--description "Package" (transient-scope) :packagePresets))
  (interactive)
  (teamake-preset-select-from-project (transient-scope) :packagePresets))

(transient-define-suffix teamake-preset--workflow ()
  :transient 'transient--do-recurse
  :description
  (lambda ()
    (teamake-preset--description "Workflow" (transient-scope) :workflowPresets))
  (interactive)
  (teamake-preset-select-from-project (transient-scope) :workflowPresets))

(defun teamake-preset--possible (project)
  "Determine if PROJECT contain enough information for `teamake-preset'."
  (and (teamake-project-has-valid-source-dir-p project)
       (file-exists-p (file-name-concat (plist-get project :source-dir) "CMakePresets.json"))))

(defun teamake-preset--setup (project)
  "Setup `teamake-preset' from PROJECT."
  (unless (teamake-preset--possible project)
    (user-error "Project not correctly configured for using presets"))
  (teamake-setup-transient 'teamake-preset project))

(transient-define-prefix teamake-preset (project)
  "Handle cmake presets for PROJECT."
  [:description
   (lambda ()
     (format "%s %s\n"
             (propertize "CMake Preset" 'face 'teamake-heading)
             (propertize (plist-get (transient-scope) :name) 'face 'teamake-project-name)))
   ("c" teamake-preset--configuration)
   ("b" teamake-preset--build)
   ("t" teamake-preset--test)
   ("p" teamake-preset--package)
   ("w" teamake-preset--workflow)
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
  (teamake-preset--setup project)
  )

(provide 'teamake-preset)
;;; teamake-preset.el ends here
