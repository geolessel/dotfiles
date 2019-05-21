; LOOK ---------------------------------------------------------------------

(setq custom-theme-directory "~/.emacs.d/themes/")
(use-package base16-theme
  :demand t)

(load-theme 'geo-dark)
; (load-theme 'geo-light)


; FEEL ---------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p) ; make prompts y/n instead of yes/no

(use-package frame-fns :ensure nil)
(use-package frame-cmds :ensure nil)
(use-package zoom-frm :ensure nil)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "M--") 'zoom-out)
(global-set-key (kbd "M-+") 'zoom-in)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(use-package geo-functions :ensure nil)

(defvar base-leader-map (make-sparse-keymap) "The base leader map")
(use-package bind-map
  :config
  (bind-map base-leader-map
    :overrider-minor-modes t
    :keys ("M-m")
    :evil-keys (",")
    :evil-states (normal motion visual))
  (bind-map-set-keys base-leader-map
    "ee" 'edit-emacs-config
    ","  'hydra-comma/body)
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (use-package flx-ido
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (flx-ido-mode 1)
;;   ;; disable ido faces to see flx highlights
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil)
;;   )

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'undo-tree-mode)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package slim-mode :ensure t)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'geo-look-and-feel)
