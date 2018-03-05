(eval-when-compile
  (require 'use-package))

(setq package-list '(
                     base16-theme
		     bind-map
		     evil
		     evil-magit
                     helm
                     helm-projectile
		     hydra
		     key-chord
		     magit
                     paradox
                     prettier-js
                     projectile
                     projectile-rails
		     twilight-bright-theme
		     use-package
		     ))
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
(or (file-exists-p package-user-dir) (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'geo-required-packages)
