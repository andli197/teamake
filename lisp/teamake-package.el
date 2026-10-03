;;; teamake-package --- CPack commands for teamake -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'transient)

(transient-define-suffix teamake-package ()
  [:description
   "CPack"
   ("aaa" "AAAAAAA" "--aaa")
   ]
  (interactive)
  (transient-setup 'teamake-package '() '()))

(provide 'teamake-package)
;;; teamake-package.el ends here
