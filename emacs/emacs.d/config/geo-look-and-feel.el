(eval-when-compile
  (require 'use-package))

(load-theme 'twilight-bright)

(use-package frame-fns :ensure nil)
(use-package frame-cmds :ensure nil)
(use-package zoom-frm :ensure nil)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "M--") 'zoom-out)
(global-set-key (kbd "M-+") 'zoom-in)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'geo-look-and-feel)
