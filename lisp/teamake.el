;;; teamake --- Setup for whole of teamake
;;; Commentary:
;;; Code:

(require 'teamake-core)
(require 'teamake-project)

;;;###autoload
(teamake-load-project-configurations)

;;;###autoload
(add-hook 'kill-emacs-hook 'teamake-save-project-configurations)

(defalias 'teamake 'teamake-project)

(provide 'teamake)
;;; teamake.el ends here
