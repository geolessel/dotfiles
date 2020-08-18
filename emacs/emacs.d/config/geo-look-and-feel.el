; LOOK ---------------------------------------------------------------------

(setq custom-theme-directory "~/.emacs.d/themes/")
(use-package base16-theme
  :demand t)
(use-package flatui-theme)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; See doom screenshots at https://github.com/hlissner/emacs-doom-themes/tree/screenshots
(load-theme 'doom-nord-light)
; (load-theme 'geo-dark)
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
