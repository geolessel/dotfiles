(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (use-package flycheck-status-emoji
    :config
    (flycheck-status-emoji-mode)
    )
  )

(use-package prettier-js
  :commands (prettier-js-mode prettier-before-save)
  :config
  (setq prettier-args
        '("--semi" "false"
          "--trailing-comma" "es5"))
  (setq prettier-target-mode "rjsx-mode")
  )

(use-package rjsx-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  :init
  (add-hook 'rjsx-mode-hook '(lambda () (add-hook 'before-save-hook 'prettier-before-save)))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  )

(provide 'geo-code-style)
