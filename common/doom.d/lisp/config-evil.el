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
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'pyim-dict-manager-mode 'emacs)
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  )
)

;(use-package! evil-collection
;  :after evil
;  :ensure t
;  :config
;  (evil-collection-init)
;)

(use-package! evil-snipe
  ;; see L612 of evil-snipe
  :bind
  (:map evil-snipe-parent-transient-map
        ("s-;" . evil-snipe-repeat-reverse))
)

(evil-leader/set-key
;;; use doom tabs function and evil-leader to switch (centaur) tabs
;;; learn lambda function to create closure for key binding
;;; https://stackoverflow.com/a/1030409
;   "tb" (lambda () (interactive) (+tabs:previous-or-goto))
;   "tt" (lambda () (interactive) (+tabs:next-or-goto))
;   "t1" (lambda () (interactive) (+tabs:next-or-goto 1))
;   "t2" (lambda () (interactive) (+tabs:next-or-goto 2))
;   "t3" (lambda () (interactive) (+tabs:next-or-goto 3))
;   "t4" (lambda () (interactive) (+tabs:next-or-goto 4))
;   "t5" (lambda () (interactive) (+tabs:next-or-goto 5))
;   "t6" (lambda () (interactive) (+tabs:next-or-goto 6))
;   "t7" (lambda () (interactive) (+tabs:next-or-goto 7))
;   "t8" (lambda () (interactive) (+tabs:next-or-goto 8))
;   "t9" (lambda () (interactive) (+tabs:next-or-goto 9))
  "u"  'outline-up-heading
)

(map!
  :nv (concat mz/evil-leader " w |") #'split-window-below
  :nv (concat mz/evil-leader " w -") #'split-window-right
  ;;; my custom functions
  :nv (concat mz/evil-leader " f f") #'mz/find-other-file
  :nv (concat mz/evil-leader " f p") #'mz/find-pdf
)

(provide 'config-evil)
;;; config-evil.el ends here
