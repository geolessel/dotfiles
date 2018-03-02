;;; geo-evil --- Evil mode config
;;; Code:
;;; Commentary:

(eval-when-compile
  (require 'use-package))

(setq evil-shift-width 2)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map ";" 'evil-ex) ; Use ; instead of :
(define-key evil-normal-state-map ":" 'evil-repeat-find-char) ; Use ; as :
(define-key evil-normal-state-map "," ctl-x-map) ; Use , as "leader" (C-x)
(define-key evil-normal-state-map " " ctl-x-map) ; Use SPC as "leader" (C-x)
; (define-key evil-normal-state-map (kbd "<tab>") 'evil-indent) ; Use TAB to correct indentation

(provide 'geo-evil)
