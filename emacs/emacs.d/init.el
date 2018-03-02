;;; .emacs --- geo's emacs
;;; Code:
;;; Commentary:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      indent-tabs-mode nil
      ns-use-native-fullscreen nil
      ad-redefinition-action 'accept
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      tooltip-use-echo-area t
      blink-cursor-blinks 0                ; blink cursor forever
      global-hl-line-mode t                ; highlight current row
      show-paren-mode t
      mac-auto-operator-composition-mode t ; get ligatures on those fonts, yo!
      create-lockfiles nil)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package geo-required-packages :ensure nil)
(use-package geo-evil :ensure nil)
(use-package geo-look-and-feel :ensure nil)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "''" 'helm-M-x))

(use-package geo-helm :ensure nil)
(use-package magit
  :bind (
         ("C-x g" . magit-status)))
(use-package evil-magit :ensure nil)


; -----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (twilight-bright)))
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" default)))
 '(mac-auto-operator-composition-mode t)
 '(package-selected-packages
   (quote
    (magit key-chord helm zoom-frm yasnippet yaml-mode writeroom-mode which-key web-mode use-package twilight-bright-theme spaceline-all-the-icons smex smart-mode-line slim-mode sass-mode ruby-refactor ruby-end rspec-mode robe powerline-evil paradox magithub lua-mode ledger-mode keychain-environment json-mode js2-mode helm-rails helm-projectile helm-dash helm-ag go-mode git-timemachine geo-light-2-theme font-lock+ flycheck-elixir flycheck-dialyzer flycheck-credo flycheck-color-mode-line flx-ido exec-path-from-shell evil-rails evil-org evil-matchit evil-magit evil-leader elixir-mix drag-stuff discover-my-major discover diminish coffee-mode base16-theme all-the-icons-dired alchemist ag adoc-mode)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FFFFFF" :foreground "#505050" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "PragmataPro")))))
