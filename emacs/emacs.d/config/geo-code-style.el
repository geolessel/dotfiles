(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d")
  )

(use-package rjsx-mode
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  :init
  (add-hook 'rjsx-mode-hook 'smartparens-mode)
  (add-hook 'rjsx-mode-hook '(lambda () (add-hook 'before-save-hook 'prettier-before-save)))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  )

(use-package prettier-js
  :ensure t
  :commands (prettier-js-mode)
  :config
  (setq prettier-args
        '("--semi" "false"
          "--trailing-comma" "es5"))
  (setq prettier-target-mode "rjsx-mode")
  )

(provide 'geo-code-style)
