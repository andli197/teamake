
;;; Code:

(defvar cmake-cache-variables '()
  "List of currently handled cache variables.")

   ;; ("-D" cmake-configure--set-cache-entris :transient t
   ;;  :description cmake-configure--describe-cache-entries)

(transient-define-prefix cmake-cache ()
  "Manage CMake cache entries."
  ["Cache"
   ()]
  )

(provide 'cmake-cache)
;;; cmake-cache.el ends here
