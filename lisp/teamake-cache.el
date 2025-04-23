
;;; Code:

(defvar teamake-cache-variables '()
  "List of currently handled cache variables.")

   ;; ("-D" teamake-configure--set-cache-entris :transient t
   ;;  :description teamake-configure--describe-cache-entries)

(transient-define-prefix teamake-cache ()
  "Manage Teamake cache entries."
  ["Cache"
   ()]
  )

(provide 'teamake-cache)
;;; teamake-cache.el ends here
