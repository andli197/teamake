
;;; Code:

(defgroup teamake-faces '()
  "Faces used by teamake project."
  :group 'teamake-project)

(defface teamake-project-heading
  '((t :inherit font-lock-keyword-face))
  "Face for teamake project headings."
  :group 'teamake-faces)

(defface teamake-project-name
  '((((class color) (background light)) :foreground "Medium spring green")
    (((class color) (background  dark)) :foreground "Medium spring green"))
  "Face for teamake project name."
  :group 'teamake-faces)

(defface teamake-project-path
  '((((class color) (background light)) :foreground "Medium aquamarine")
    (((class color) (background  dark)) :foreground "Medium aquamarine"))
  "Face for teamake project path."
  :group 'teamake-faces)

(provide 'teamake-faces)
;;; teamake-faces.el ends here
