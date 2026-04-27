
(add-to-list 'load-path (directory-file-name (file-name-concat (file-name-parent-directory (file-name-directory buffer-file-name)) "lisp")))

;;; Sould this be used?
;; (defvar teamake-process--shell-setup
;;   "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvars64.bat"
;;   "Command to process for setting up an execution shell.")

;; (setq teamake-process-cmake-tool-path "c:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/CMake/CMake/bin/cmake.exe")

(require 'teamake)
