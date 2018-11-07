;;; geo-evil --- Evil mode config
;;; Code:
;;; Commentary:

(use-package evil
  :init
  (setq evil-shift-width 2)
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map ";" 'evil-ex) ; Use ; instead of :
  (define-key evil-normal-state-map ":" 'evil-repeat-find-char) ; Use ; as :
  (define-key evil-normal-state-map " " ctl-x-map) ; Use SPC as "leader" (C-x)
  (define-key evil-normal-state-map "\C-p" nil)
  (define-key evil-motion-state-map "\C-p" nil)
  (define-key evil-insert-state-map "\C-p" nil)
  (define-key evil-emacs-state-map "\C-p" nil)
  (setq-default evil-symbol-word-search t) ; treat _ as part of words in searches
  )

(provide 'geo-evil)
