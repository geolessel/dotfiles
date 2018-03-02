(use-package helm
  :config
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)
  (helm-mode)
  :bind (
	 ("C-x C-f" . helm-find-files)
	 ("M-x"     . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x r o"      . helm-occur) ; search
	 ("C-x r m"      . helm-all-mark-rings)
	 ("C-x r k"      . helm-show-kill-ring)
	)
  )

(defhydra hydra-helm (:hint nil :color pink)
      "
									╭──────┐
  Navigation   Other  Sources     Mark             Do             Help   │ Helm │
╭───────────────────────────────────────────────────────────────────────┴──────╯
      ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
      ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
  _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
      ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
      ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
--------------------------------------------------------------------------------
      "
      ("<tab>" helm-keyboard-quit "back" :exit t)
      ("<escape>" nil "quit")
      ("\\" (insert "\\") "\\" :color blue)
      ("h" helm-beginning-of-buffer)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-end-of-buffer)
      ("g" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("n" helm-next-source)
      ("p" helm-previous-source)
      ("K" helm-scroll-other-window-down)
      ("J" helm-scroll-other-window)
      ("c" helm-recenter-top-bottom-other-window)
      ("m" helm-toggle-visible-mark)
      ("t" helm-toggle-all-marks)
      ("u" helm-unmark-all)
      ("H" helm-help)
      ("s" helm-buffer-help)
      ("v" helm-execute-persistent-action)
      ("d" helm-persistent-delete-marked)
      ("y" helm-yank-selection)
      ("w" helm-toggle-resplit-and-swap-windows)
      ("f" helm-follow-mode))

(provide 'geo-helm)
