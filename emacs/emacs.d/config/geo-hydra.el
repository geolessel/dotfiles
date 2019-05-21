(use-package hydra
  :diminish
  )

(defhydra hydra-comma-edit (:hint nil :color pink)
  "
				               ╭──────┐
                                               │ Edit │
╭──────────────────────────────────────────────┴──────╯
      [_e_] emacs config
"
  ("q" (message "Abort") :exit t)
  ("<escape>" (message "Abort") :exit t)
  ("e" edit-emacs-config :exit t)
  )

(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-customize-apropos (:color blue)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))

(defhydra hydra-unicode (:hint nil :color pink)
  "
				            ╭─────────┐
                                            │ UNICODE │
╭───────────────────────────────────────────┴─────────╯
      [_d_] drawing characters
"
  ("d" hydra-unicode-drawing/body :exit t)
  )

(defhydra hydra-unicode-drawing (:hint nil :color pink)
  "
				    ╭─────────────────┐
                                    │ UNICODE DRAWING │
╭───────────────────────────────────┴─────────────────╯
│   _q_ _w_ _e_ ┌─┐   ^^U+2500  ─ ━ │ ┃ ┄ ┅ ┆ ┇ ┈ ┉ ┊ ┋ ┌ ┍ ┎ ┏
│   _a_   ^^_d_ │ │   ^^U+2510  ┐ ┑ ┒ ┓ └ ┕ ┖ ┗ ┘ ┙ ┚ ┛ ├ ┝ ┞ ┟
│   _z_ _x_ _c_ └─┘   ^^U+2520  ┠ ┡ ┢ ┣ ┤ ┥ ┦ ┧ ┨ ┩ ┪ ┫ ┬ ┭ ┮ ┯
│               ^^U+2530  ┰ ┱ ┲ ┳ ┴ ┵ ┶ ┷ ┸ ┹ ┺ ┻ ┼ ┽ ┾ ┿
│               ^^U+2540  ╀ ╁ ╂ ╃ ╄ ╅ ╆ ╇ ╈ ╉ ╊ ╋ ╌ ╍ ╎ ╏
│     ^^_W_    ^^┬    ^^U+2550  ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟
│   _A_ _S_ _D_ ├┼┤   ^^U+2560  ╠ ╡ ╢ ╣ ╤ ╥ ╦ ╧ ╨ ╩ ╪ ╫ ╬ ╭ ╮ ╯
│     ^^_X_    ^^┴    ^^U+2570  ╰ ╱ ╲ ╳ ╴ ╵ ╶ ╷ ╸ ╹ ╺ ╻ ╼ ╽ ╾ ╿
"
  ("q" (insert "┌"))
  ("a" (insert "│"))
  ("A" (insert "├"))
  ("S" (insert "┼"))
  ("z" (insert "└"))
  ("w" (insert "─"))
  ("W" (insert "┬"))
  ("x" (insert "─"))
  ("X" (insert "┴"))
  ("e" (insert "┐"))
  ("d" (insert "│"))
  ("D" (insert "┤"))
  ("c" (insert "┘"))
  ("n" insert-char "insert by code" :hint t)
  ("<escape>" nil "quit")
  )

(defhydra hydra-comma (:hint nil :color pink)
  "
				               ╭──────┐
      Submenus                                 │ Help │
╭──────────────────────────────────────────────┴──────╯
      [_a_] Apropos
      [_e_] edit
      [_u_] unicode
"
  ("q" "quit" :exit t)
  ("<escape>" (message "Abort") :exit t)
  ("a" hydra-apropos/body :exit t)
  ("e" hydra-comma-edit/body :exit t)
  ("u" hydra-unicode-drawing/body :exit t)
  )


(defhydra hydra-helm (:hint nil :color pink)
      "
									                ╭──────┐
      Navigation    Other     Sources     Mark               Do                  Help   │ Helm │
╭───────────────────────────────────────────────────────────────────────────────────────┴──────╯
      ^_k_^         _K_       _p_         [_m_] mark         [_v_] view         [_H_] helm help
      ^^↑^^         ^↑^       ^↑^         [_t_] toggle all   [_d_] delete       [_s_] source help
 _h_ <-   -> _l_    _c_       ^ ^         [_u_] unmark all   [_f_] follow:
      ^^↓^^         ^↓^       ^↓^          ^ ^               [_y_] yank selection
      ^_j_^         _J_       _n_          ^ ^               [_w_] toggle windows
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

;; (defhydra hydra-zoom (:color pink :hint nil)
;;   "
;; ^Mark^           ^Unmark^
;; ^^^^^^^^--------------------------------
;; _g_: mark        _g_: unmark
;; _g_: save        _l_: unmark up
;; _q_: delete      ^ ^
;; "
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out")
;;   ("q" quit-window "quit" :color blue)
;;   )

(provide 'geo-hydra)
