(defun edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(provide 'geo-functions)
