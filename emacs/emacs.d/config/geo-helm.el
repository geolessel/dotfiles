(use-package helm
  :diminish
  :config
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)
  :init
  (helm-mode)
  :bind (
	 ("C-x C-f" . helm-find-files)
	 ("M-x"     . helm-M-x)
         ("C-x b" . helm-buffers-list)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x r o"      . helm-occur) ; search
	 ("C-x r m"      . helm-all-mark-rings)
	 ("C-x r k"      . helm-show-kill-ring)
	)
  )

(use-package helm-projectile
  :after (projectile helm)
  :init
  (helm-projectile-on)
  :bind (
         ("C-p" . helm-projectile)
        )
  )

(provide 'geo-helm)
