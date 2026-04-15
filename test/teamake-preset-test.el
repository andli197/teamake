

(require 'ert)
(require 'cmake-preset)

(defconst simple-preset-file
  (file-name-concat (file-name-directory buffer-file-name)
                    "presets"
                    "SimplePreset.json"))

(defconst multi-config-preset-file
  (file-name-concat (file-name-directory buffer-file-name)
                    "presets"
                    "MultiConfigPreset.json"))

(ert-deftest cmake-preset---parse-file--can-parse-a-simple-preset-file ()

  (let ((simple-preset (cmake-preset--parse-file simple-preset-file)))
    (should
     (not
      (eq
       '()
       (seq-find
        (lambda (preset)
          (and (string= (plist-get preset :name) "Config")
               (string= (plist-get preset :generator) "Ninja")
               (string= (plist-get preset :binaryDir) "${sourceParentDir}/build-${sourceDirName}")
               (string= (plist-get preset :installDir) "${sourceParentDir}/install-${sourceDirName}")
               (eq (plist-get preset :category) :configurePresets)
               ))
        simple-preset))))
    (should
     (not
      (eq '()
          (seq-find (lambda (preset)
                      (and (string= (plist-get preset :name) "Build")
                           (eq (plist-get preset :category) :buildPresets)
                           (string= (plist-get preset :configurePreset) "Config")
                           (string= (plist-get preset :configuration) "Debug")
                           ))
                    simple-preset))))
    (should
     (not
      (eq '()
          (seq-find (lambda (preset)
                      (and (string= (plist-get preset :name) "Test")
                           (eq (plist-get preset :category) :testPresets)
                           (string= (plist-get preset :configurePreset) "Config")
                           (eq (plist-get (plist-get preset :output) :outputOnFailure) t)
                           ))
                    simple-preset))))
    ))

(ert-deftest cmake-preset---parse-file--can-parse-a-multi-config-preset-file ()
  (let ((presets (cmake-preset--parse-file multi-config-preset-file)))
    (should
     (not
      (eq '()
          (seq-find (lambda (preset)
                      (and
                       (string= (plist-get preset :name) "Release-GCC")
                       (string= (plist-get preset :displayName) "Build Release")
                       (string= (plist-get preset :inherits) "Base-Release")
                       (string= (plist-get preset :configurePreset) "GCC")
                       (eq (plist-get preset :category) :buildPresets)
                       ))
                    presets))))
    ))


(ert-deftest cmake-preset---fuse--can-fuse-a-preset-from-multi-config-preset-file ()
  (let* ((presets (cmake-preset--parse-file multi-config-preset-file))
         (preset-to-fuse
          (seq-find (lambda (preset)
                      (and
                       (string= (plist-get preset :name) "Release-GCC")
                       (string= (plist-get preset :displayName) "Build Release")
                       (string= (plist-get preset :inherits) "Base-Release")
                       (string= (plist-get preset :configurePreset) "GCC")
                       (eq (plist-get preset :category) :buildPresets)
                       ))
                    presets))
         (fused-preset (cmake-preset--fuse preset-to-fuse presets)))
    (should (string= (plist-get (plist-get fused-preset :fusedConfiguration) :generator)
                     "Ninja"))
    ))
