;;; configurations for ui-related behavior, including
;;;   - deft
;;;   - avy
;;;   - rg
;;;   - window-numbering

(use-package! deft
  :after org
  :commands deft
  :init
  (setq ;; disable auto-save
        deft-auto-save-interval -1.0
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex")
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory mz/org-notes))

(use-package! avy
   :bind
   ("C-c a k" . avy-copy-line)
   ("C-c a m" . avy-move-line)
   ("C-c a K" . avy-copy-region)
   ("C-c a M" . avy-move-region)
   ("C-c a l" . avy-goto-end-of-line)
)

(use-package! dired
  :config
  ;;; a work-around for RET overrieden by evil-ret in evil-motion-state-map
  ;;; when in 'motion.
  ; (define-key dired-mode-map (kbd "M-o") 'dired-find-file)
  ; (define-key dired-mode-map (kbd "<normal-state> +") nil)
)

; rg - ripgrep interface
; https://rgel.readthedocs.io/en/latest/
(use-package rg
  :config
  ; (global-set-key (kbd "C-c s") #'rg-menu)
  (setq rg-keymap-prefix "\C-cg")
  (rg-enable-default-bindings)
  (setq rg-ignore-case 'smart)
  ;(global-set-key (kbd "C-c g m") #'rg-menu)
  ;(global-set-key (kbd "C-c g d") #'rg-diwm)
  ;(global-set-key (kbd "C-c g f") #'rg-diwm-current-file)
)

(use-package! window-numbering
  :config
  (window-numbering-mode)
  ;; redefine workspaces shortcuts to resolve the conflict with +doom/workspaces
)

(provide 'config-ui)
;;; config-ui.el ends here
