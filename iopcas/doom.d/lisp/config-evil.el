;;; configuration for evil and related packages, including
;;;   - evil-leader

(use-package! evil-leader
  :config
  ;; turn on the leader mode
  (global-evil-leader-mode +1)
  ;; note , as leader conflicts with evil-snipe evil-snipe-repeat-reverse
  ;; bind it to alt+; in the evil-snipe session
  (evil-leader/set-leader mz/evil-leader)
)

(use-package! evil
  :bind
  (:map evil-insert-state-map
        ("s-i" . evil-escape))
  (:map evil-normal-state-map
        ("s-i" . evil-insert))
  (:map evil-visual-state-map
        ("s-i" . evil-insert))
  :config
  (progn
  (evil-set-initial-state 'delve-mode 'emacs)
  )
)

(use-package! evil-snipe
  ;; see L612 of evil-snipe
  :bind
  (:map evil-snipe-parent-transient-map
        ("s-;" . evil-snipe-repeat-reverse))
)

(evil-leader/set-key
; use doom tabs function and evil-leader to switch (centaur) tabs
; learn lambda function to create closure for key binding
; https://stackoverflow.com/a/1030409
  "t1" (lambda () (interactive) (+tabs:next-or-goto 1))
  "t2" (lambda () (interactive) (+tabs:next-or-goto 2))
  "t3" (lambda () (interactive) (+tabs:next-or-goto 3))
  "t4" (lambda () (interactive) (+tabs:next-or-goto 4))
  "t5" (lambda () (interactive) (+tabs:next-or-goto 5))
  "t6" (lambda () (interactive) (+tabs:next-or-goto 6))
  "t7" (lambda () (interactive) (+tabs:next-or-goto 7))
  "t8" (lambda () (interactive) (+tabs:next-or-goto 8))
  "t9" (lambda () (interactive) (+tabs:next-or-goto 9))
  "u"  'outline-up-heading
)

(map!
  :nv (concat mz/evil-leader " w |") #'split-window-below
  :nv (concat mz/evil-leader " w -") #'split-window-right
)

(provide 'config-evil)
;;; config-evil.el ends here
