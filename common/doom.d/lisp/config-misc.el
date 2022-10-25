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

;;; running async shell command
;;; https://github.com/xenodium/dwim-shell-command
(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  ;; (defun my/dwim-shell-command-convert-to-gif ()
  ;;   "Convert all marked videos to optimized gif(s)."
  ;;   (interactive)
  ;;   (dwim-shell-command-on-marked-files
  ;;    "Convert to gif"
  ;;    "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
  ;;    :utils "ffmpeg"))
)

(use-package! company
  :config
  ;;; disable company mode in some major modes in my old macos mbp
  (with-system 'darwin
    (setq company-global-modes
        '(not markdown-mode org-mode erc-mode message-mode help-mode gud-mode))
  )
  ;;; disable auto complete
  (setq company-idle-delay nil)
)


(provide 'config-misc)
;;; config-misc.el ends here
