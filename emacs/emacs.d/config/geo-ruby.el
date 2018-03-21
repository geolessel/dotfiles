(use-package projectile-rails
  :diminish
  :after (projectile)
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "s-r") 'hydra-projectile-rails/body)  )

(use-package rspec-mode
  :config
  (setq rspec-use-bundler-when-possible t)
  (setq compilation-scroll-output t)
  )

(provide 'geo-ruby)
