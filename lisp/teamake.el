;;; teamake --- Setup for whole of teamake
;;; Commentary:
;;; Code:

(require 'teamake-core)
(require 'teamake-process)
(require 'teamake-project)

(require 'teamake-preset)
(require 'teamake-configure)
(require 'teamake-build)
(require 'teamake-install)
(require 'teamake-test)
(require 'teamake-package)

;;;###autoload
(teamake-load-project-configurations)

;;;###autoload
(add-hook 'kill-emacs-hook 'teamake-save-project-configurations)

(defalias 'teamake 'teamake-project)

(provide 'teamake)
;;; teamake.el ends here
