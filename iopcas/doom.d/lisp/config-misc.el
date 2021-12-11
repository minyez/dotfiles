;;; configurations for miscellaneous pacakges.
;;; should be refactors later

;;; make side by side buffers function the same as the main window
;(setq truncate-partial-width-windows nil)
;; do not wrap lines for long sentence
;; truncate it by myself
;;(setq-default global-visual-line-mode t)
;(setq-default truncate-lines t)

; company-jedi
;     https://github.com/emacsorphanage/company-jedi
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package! move-text
  :config
  (move-text-default-bindings) ; default bindings
)

(use-package! ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point t)
)

(use-package! company
  :config
  (setq 
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode)
        ; 在这些包中停止使用company
        ; company自动补全中文会导致卡顿
  )
)

;; set zsh to default shell of vterm
(use-package! vterm
  :config
  (setq vterm-shell "zsh"))

(use-package! deft
;  :after org
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

;http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
;(defun s-trim-newline (string)
;  "Remove newlines at the end of s."
;  (if (string-match "[\n\r]+\\'" string)
;  (replace-match "" t t string)
;  string)
;)
;(defun s-trim-left (string)
;  "Remove whitespace at the begining of s."
;  (if (string-match "\\`[ \t\n\r]+" string)
;  (replace-match "" t t string)
;  string)
;)
;(defun s-trim-right (string)
;  "Remove whitespace at the begining of s."
;  (if (string-match "[ \t\n\r]+\\'" string)
;  (replace-match "" t t string)
;  string)
;)
;(defun s-trim (string)
;  "Remove whitespace at both ends of s."
;  (s-trim-left (s-trim-right string))
;)

; https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
(defun concat-string (list)
  (format nil "~{~a~}" list)
)
(defun remove-line-breaks (string)
  (replace-regexp-in-string "[\n\r]" " " string)
)

(use-package! avy
   :bind
   ("C-c a k" . avy-copy-line)
   ("C-c a m" . avy-move-line)
   ("C-c a K" . avy-copy-region)
   ("C-c a M" . avy-move-region)
   ("C-c a l" . avy-goto-end-of-line)
)

;
; rg - ripgrep interface
; https://rgel.readthedocs.io/en/latest/
(use-package! rg
  :config
  (global-set-key (kbd "C-c g m") #'rg-menu)
  (global-set-key (kbd "C-c g d") #'rg-diwm)
  (global-set-key (kbd "C-c g f") #'rg-diwm-current-file)
  (with-eval-after-load 'rg
     ;; Your settings goes here.
    (setq rg-ignore-case 'smart)
  )
)

(use-package! window-numbering
  :config
  (window-numbering-mode)
  ;; redefine workspaces shortcuts to resolve the conflict with +doom/workspaces
)

;;; for use of direnv
; envrc from Doom :tool direnv by purcell
(use-package! envrc
 :config
 (envrc-global-mode)
)

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

;; markdown-mode preview
;; https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
;(use-package! simple-httpd
;  :ensure t
;  :config
;  (setq httpd-port 8090)
;  (setq httpd-host (system-name)))
;
;(use-package impatient-mode
;  :ensure t
;  :commands impatient-mode)
;(use-package! markdown-mode
;  :ensure t
;  :mode (("\\.md\\'" . gfm-mode)
;         ("\\.markdown\\'" . markdown-mode))
;  :commands (markdown-mode gfm-mode)
;  :config
;  (setq markdown-command "pandoc -t html5"))
;;; Start Markdown live preview in browser with GitHub CSS
;(defun my-markdown-filter (buffer)
;  (princ
;   (with-temp-buffer
;     (let ((tmp (buffer-name)))
;       (set-buffer buffer)
;       (set-buffer (markdown tmp))
;       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdn.bootcdn.net/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css\"/>
;<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
;   (current-buffer)))
;(defun my-markdown-preview ()
;  "Preview markdown."
;  (interactive)
;  (unless (process-status "httpd")
;    (httpd-start))
;  (impatient-mode)
;  (imp-set-user-filter 'my-markdown-filter)
;  (imp-visit-buffer))
;; End Markdown live preview

(provide 'config-misc)
;;; config-misc.el ends here
