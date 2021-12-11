;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;(server-start)
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Minye Zhang"
      user-mail-address "zmysmile0929@gmail.com"
      )
(setq delete-by-moving-to-trash nil)

(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Sarasa Term SC" :size 20)
      doom-big-font (font-spec :family "Sarasa Term SC" :size 20))
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; map C-u to s-backspace, as C-u is bind to evil-scroll-up in Doom
;; see https://emacs-china.org/t/doom-emacs-c-u/9942/2
(global-set-key (kbd "s-<backspace>") #'universal-argument)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; light themes
;(setq doom-theme 'doom-one-light)
;(setq doom-theme 'doom-nord-light)
;; dark themse
;(setq doom-theme 'doom-dark+)
;(setq doom-theme 'doom-spacegrey)
;(setq doom-theme 'doom-peacock)
;(setq doom-theme 'doom-monokai-pro)
;(setq doom-theme 'doom-dracula)
; make theme adapt to time. Modified from
; https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
(defun synchronize-theme ()
    (setq hour
        (string-to-number
        ; get current time by current-times-string
	; hour value occupies 3 characters (11-13)
            (substring (current-time-string) 11 13)))
    (if (member hour (number-sequence 6 19))
        (setq now 'doom-nord-light)
        (setq now 'doom-one))
    (if (equal now doom-theme)
        nil
        (setq doom-theme now)
        (load-theme now t) ) ) ;; end of (defun ...
; run every 2 hour
(run-with-timer 0 7200 'synchronize-theme)

(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Documents/SelfDevelopment/codes"
                                         "~/Projects"
                                         "~/Documents/SelfDevelopment/Academia"
                                         "~/Documents/SelfDevelopment/Projects"
                                         ))
)

;;; private configs for individual modules and packages
(add-to-list 'load-path (concat doom-private-dir "lisp"))

; Global variables
;   mz/org-notes : all my org(-roam) notes
;   mz/bibfile : bibliography, now a symlink to my exported zotero biblatex library
;   mz/evil-leader : leader key to unify evil-leader and other keys that I want to emulate as evil-leader
;                    the later usage is mainly when I only want to bind key to particular mode
(setq
  mz/org-notes (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam")
  mz/bibfile (concat mz/org-notes "/bibliography_macos.bib")
  ;mz/org-notes (concat (getenv "HOME") "/tmp/org-demo") ; for demonstration
  ;mz/bibfile (concat (getenv "HOME") "/Documents/SelfDevelopment/Database/test.bib")
  mz/org-roam-graph-executable "/usr/local/bin/dot"
  mz/org-roam-graph-viewer "/usr/bin/open"
  mz/evil-leader ","
)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Global key bindings
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
;(map! :ne "SPC n b" #'org-brain-visualize)
(map! :ne "SPC n t" #'counsel-org-capture)
;; copy to system clipboard
(map! :nv "SPC s-x" #'ns-copy-including-secondary)

;; image file extension
(add-to-list 'image-file-name-extensions "pdf")
(add-to-list 'image-file-name-extensions "eps")
(add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "eps")
(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "pdf")

(require 'config-evil)

;; set zsh to default shell of vterm
(use-package! vterm
  :config
  (setq vterm-shell "zsh"))

(use-package! ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point t)
)

(use-package! company
  :config
  (setq 
        company-global-modes
        '(not markdown-mode org-mode erc-mode message-mode help-mode gud-mode)
        ;'(not markdown-mode erc-mode message-mode help-mode gud-mode)
        ; 在这些包中停止使用company
        ; company自动补全中文会导致卡顿
  )
)

;; Start move-text
(move-text-default-bindings) ; default bindings
;; End move-text

(require 'config-org)
(require 'config-org-roam)
;(add-hook 'after-init-hook 'blink-cursor-mode)
;; render latex block, commented due to performance issue
;(add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'config-ox)
(require 'config-latex)

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

;(golden-ratio-mode 1)
;
; centaur-tabs
;(use-package centaur-tabs
;  :demand
;  :config
;  (setq centaur-tabs-set-icons 't
;    centaur-tabs-style "bar"
;    centaur-tabs-set-modified-marker 't
;      centaur-tabs-height 32
;        centaur-tabs-set-bar 'under
;        x-underline-at-descent-line 't
;    ; tab cycling
;        centaur-tabs-cycle-scope 'tabs
;        )
;  (centaur-tabs-headline-match)
;  (centaur-tabs-mode t)
;  (setq uniquify-separator "/")
;  (setq uniquify-buffer-name-style 'forward)
;  :bind
;  ("C-<prior>" . centaur-tabs-backward)
;  ("C-<next>" . centaur-tabs-forward)
;  (:map evil-normal-state-map
;    ("g t" . centaur-tabs-forward)
;    ("g T" . centaur-tabs-backward)
;    )
;  :hook
;  (org-roam-mode . centaur-tabs-mode)
;)

; archive function that includes ancestor tree structure when copying
; https://gist.github.com/edgimar/072d99d8650abe81a9fe7c8687c0c993

;; export to macOS reminder.app
;(use-package ox-icalendar
;  :config
;  (setq org-icalendar-include-todo t
;        org-icalendar-alarm-time 5
;        org-icalendar-use-scheduled '(event-if-todo-not-done)
;  )
;  (defun org-icalendar-open-ics-file (file)
;    ;(start-process "org-icalendar-open-ics-file-process" nil "open" "-a" "/Applications/Calendar.app" file)
;    (start-process "org-icalendar-open-ics-file-process" nil "open" "-a" "/Applications/Reminders.app" file)
;  )
;  :hook
;  (org-icalendar-after-save . org-icalendar-open-ics-file)
;)

(require 'config-pdf)

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

; rg - ripgrep interface
; https://rgel.readthedocs.io/en/latest/
(use-package rg
  :config
  (global-set-key (kbd "C-c s") #'rg-menu)
  (with-eval-after-load 'rg
     ;; Your settings goes here.
    (setq rg-ignore-case 'smart)
  )
)

; company-jedi
;     https://github.com/emacsorphanage/company-jedi
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 ;; make org-latex-and-related-regexp and org-highlight-latex-and-related safe
 ;; when non-nil, the two variables can make scroll large org file slow
 ;; including tables
 ;; see https://stackoverflow.com/questions/59990932/slow-cursor-movement-in-large-org-mode-file-and-the-org-do-latex-and-related-f
 '(safe-local-variable-values
   '((org-latex-and-related-regexp)
     (org-highlight-latex-and-related)
     (org-default-notes-file . "task.org::* Tasks")))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages '(org-roam-server org-journal org-fancy-priorities))
 '(show-paren-mode t)
 '(size-indication-mode t)
 ; suppress the annoying warning when using pyim in org:
 ;   Warning (emacs): org-element--cache: Unregistered buffer modifications detected. Resetting.
 '(warning-suppress-types '((emacs) (:warning)))
 )
 ;'(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(org-document-title ((t (:box 3 :weight bold :height 1.4))))
 '(org-document-title ((t (:weight bold :height 1.0))))
 '(org-meta-line ((t (:slant italic))))
 ;'(org-quote ((t (:slant normal :family "STKaiti"))))
)

;;; make side by side buffers function the same as the main window
;(setq truncate-partial-width-windows nil)
;; do not wrap lines for long sentence
;; truncate it by myself
;;(setq-default global-visual-line-mode t)
;(setq-default truncate-lines t)

;; fix the path to zstd, according to advice by Hlissner
(add-to-list 'jka-compr-compression-info-list
             ["\\.zst\\'"
              "zstd compressing"   "/usr/local/bin/zstd" ("-c" "-q")
              "zstd uncompressing" "/usr/local/bin/zstd" ("-c" "-q" "-d")
              t t "\050\265\057\375"])

;; native-comp
;(setq comp-speed 1)

; Chinese input setting, partly copied from Doom
(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)
  (setq-hook! 'markdown-mode-hook pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp "\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?1:\\cC\\|\\cH\\|\\cK\\)\\)\\(?2:[\(=0-9A-Za-z\\$\\]\\)\\|\\(?1:[=0-9A-Za-z\\$\)]\\)\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?2:\\cC\\|\\cH\\|\\cK\\)\\)")
)

(use-package! liberime
  :config
  ;; use rime user data of ibus-rime on my Linux
  ;(setq liberime-user-data-dir "~/.config/ibus/rime")
  ;(liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
  ;                (file-truename "~/.emacs.d/.local/straight/local/build-28.0.50/pyim/rime"))
  ;(liberime-start "~/Library/Rime"
  ;                "~/Library/Rime")
  (setq liberime-user-data-dir "~/Library/Rime")
  (liberime-select-schema "luna_pinyin_simp")
)

(use-package! pyim
  :config
  (global-set-key (kbd "M-\\") 'pyim-convert-string-at-point)
  (setq pyim-dcache-auto-update nil)
  (setq default-input-method "pyim")
  (setq pyim-page-length 9)
  (setq pyim-default-scheme 'rime-quanpin)
  ;; 中文使用全角标点，英文使用半角标点。
  (setq pyim-punctuation-translate-p '(auto yes no))
  ;; 设置选词框的绘制方式, prefer posframe
  (if (posframe-workable-p)
    (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-tooltip 'popup))
  ;; 探针设置
  ;; 自定义探针, 进入 org-mode source block 之后自动切换到英文输入
  (defun mz/pyim-probe-org-src-block ()
    "自定义探针, 进入 org-mode source block 之后自动切换到英文输入"
    (when (eq major-mode 'org-mode)
      (not (eq (org-in-src-block-p) nil)))
    )
  ;; auto-english 会根据之前的字符来判断是否切换到英文输入, 输入空格时自动切换到英文
  ;; 具体可用 describe-function 查看 docstring 来了解
  ;; 在 latex 块和源码块中全部为英文输入
  (setq-default pyim-english-input-switch-functions
              '(pyim-probe-auto-english
                pyim-probe-org-latex-mode
                mz/pyim-probe-org-src-block
                ;pyim-probe-org-structure-template
                pyim-probe-program-mode))
  ;; 半角标点。主要情形是在行首使用 yasnippet 时有用
  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning pyim-probe-punctuation-after-punctuation))
)

(use-package! pyim-liberime
  :after pyim
)

;(use-package! pyim-basedict
;  :after pyim
;  :config
;  (pyim-basedict-enable)
;)

(use-package! pyim-tsinghua-dict
  :after pyim
  :config
  (pyim-tsinghua-dict-enable)
)

(require 'config-lsp)

;(setq ns-auto-hide-menu-bar t)
(setq frame-resize-pixelwise t)

