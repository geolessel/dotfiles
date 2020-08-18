(use-package elixir-mode
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
    (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (use-package alchemist
    :config
    (add-to-list 'elixir-mode-hook 'alchemist-mode)
    )
  )

;; https://github.com/elixir-editors/emacs-elixir/issues/431
; (defun elixir-format--from-mix-root (elixir-path errbuff format-arguments)
;   "Run mix format where `mix.exs' is located, because mix is
; meant to be run from the project root. Otherwise, run in the
; current directory."
;   (let ((original-default-directory default-directory)
;         (mix-dir (locate-dominating-file buffer-file-name "mix.exs")))
;
;     (when mix-dir
;       (setq default-directory (expand-file-name mix-dir)))
;
;     (message (concat "Run "
;                      (abbreviate-file-name default-directory) ": "
;                      (mapconcat 'identity format-arguments " ")))
;
;     (let* ((formatter-executable (car format-arguments))
;            (formatter-arguments (cdr format-arguments))
;            (result (apply #'call-process
;                          formatter-executable nil errbuff nil formatter-arguments)))
;       (setq default-directory original-default-directory)
;       result)))

(provide 'geo-elixir)
