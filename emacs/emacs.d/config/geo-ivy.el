;; https://oremacs.com/swiper/

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq projectile-completion-system 'ivy)
  :config
  (use-package swiper)
  (use-package counsel)
  (use-package ivy-hydra :requires hydra)
  (use-package counsel-projectile
    :after (evil projectile)
    :init
    (setq projectile-git-submodule-command nil)
    :config
    (counsel-projectile-mode 1)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    )
  (use-package ivy-posframe
    :init
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
    (setq ivy-posframe-parameters '((right-fringe . 12) (left-fringe . 12)))
    :config
    (ivy-posframe-mode 1)
    )
  (ivy-mode 1)
  (counsel-mode 1)
  :bind (
         ("C-s" . swiper-isearch)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f2> j" . counsel-set-variable)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ;; C-p is defined in geo-evil
        )
  )

(provide 'geo-ivy)
