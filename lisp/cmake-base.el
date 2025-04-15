
;;; Code:

(defun cmake-return-value-or-default (variable default-value)
  "Return the value of VARIABLE if it is non empty, otherwise DEFAULT-VALUE."
  (if (or (string= variable "") (eq variable '()))
      default-value
    variable))

(provide 'cmake-base)
;;; cmake-base.el ends here
