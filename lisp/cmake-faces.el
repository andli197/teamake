
;;; Code:

(defgroup cmake-faces '()
  "Faces used by cmake project."
  :group 'cmake-project)

(defface cmake-project-heading
  '((t :inherit font-lock-keyword-face))
  "Face for cmake project headings."
  :group 'cmake-faces)

(defface cmake-project-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for cmake project name."
  :group 'cmake-faces)

(defface cmake-project-path
  '((((class color) (background light)) :foreground "Medium aquamarine")
    (((class color) (background  dark)) :foreground "Medium aquamarine"))
  "Face for cmake project path."
  :group 'cmake-faces)

(provide 'cmake-faces)
;;; cmake-faces.el ends here
