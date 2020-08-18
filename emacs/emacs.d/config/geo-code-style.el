(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;; (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; (setq flycheck-javascript-eslint-executable nil)
  (use-package flycheck-status-emoji
    :config
    (flycheck-status-emoji-mode)
    )
  (use-package flycheck-posframe
    :ensure t
    :after flycheck
    :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    )
  )

 (use-package eslintd-fix
   :init
   (add-hook 'js2-mode-hook 'eslintd-fix-mode)
   (add-hook 'rjsx-mode-hook 'eslintd-fix-mode)
   )

;; (use-package prettier-js
;;   :commands (prettier-js-mode prettier-before-save)
;;   :config
;;   ;; (setq prettier-args
;;   ;;       '("--semi" "false"
;;   ;;         "--trailing-comma" "es5"))
;;   ;; (setq prettier-target-mode "rjsx-mode")
;;   :init
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   (add-hook 'web-mode-hook 'prettier-js-mode)
;;   (use-package add-node-modules-path
;;     :init
;;     (eval-after-load 'rjsx-mode
;;       '(add-hook 'rjsx-mode-hook #'add-node-modules-path))
;;     )
;;   )

(use-package rjsx-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  :init
  (add-hook 'rjsx-mode-hook #'add-node-modules-path)
  ;; (add-hook 'rjsx-mode-hook '(lambda () (add-hook 'before-save-hook 'prettier-before-save)))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  )

(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . gas-mode))


;; (use-package vimish-fold
;;   :init
;;   (vimish-fold-global-mode 1)
;;   )

(use-package origami
  :init
  (global-origami-mode 1)
  )

(provide 'geo-code-style)
