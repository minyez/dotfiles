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
(setq doom-font (font-spec :family "Sarasa Term SC" :size 16)
      doom-big-font (font-spec :family "Sarasa Term SC" :size 16))
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

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
        (load-theme now t)
        (doom/reload-theme) ;; this is to sync posframe color to the theme
        ) ) ;; end of (defun ...
; run every hour
(run-with-timer 0 3600 'synchronize-theme)

;;; private configs for individual modules and packages
(add-to-list 'load-path (concat doom-private-dir "lisp"))

; Global variables
;   mz/org-notes : all my org(-roam) notes
;   mz/bibfile : bibliography, now a symlink to my exported zotero biblatex library
;   mz/evil-leader : leader key to unify evil-leader and other keys that I want to emulate as evil-leader
;                    the later usage is mainly when I only want to bind key to a particular mode
(setq
  mz/org-notes (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam")
  ;bibfile (concat mz/org-notes "/bibliography_linux.bib")
  mz/bibfile (concat mz/org-notes "/bibliography_macos.bib")
  mz/org-roam-graph-executable "/usr/bin/dot"
  mz/org-roam-graph-viewer "/usr/bin/xdg-open"
  mz/evil-leader ","
)

;; map C-u to s-backspace, as C-u is bind to evil-scroll-up in Doom
;; see https://emacs-china.org/t/doom-emacs-c-u/9942/2
(global-set-key (kbd "s-<backspace>") #'universal-argument)

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
;(map! :ne "SPC n b" #'org-brain-visualize)
(map! :ne "SPC n t" #'counsel-org-capture)
;; copy to system clipboard
(map! :nv "SPC s-x" #'ns-copy-including-secondary)
(global-set-key (kbd "s-k") #'kill-current-buffer)

;; image file extension
(add-to-list 'image-file-name-extensions "pdf")
(add-to-list 'image-file-name-extensions "eps")
(add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "eps")
(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "pdf")

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
 '(tool-bar-mode nil)
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

;;; =============================================================
;;; global setup ends here, starting below packages-related setup
;;; =============================================================

;; projects management by projectile
(use-package! projectile
  :config
  (setq projectile-project-search-path
        '("~/Documents/SelfDevelopment/codes"
          "~/Documents/SelfDevelopment/Academia"
          "~/Documents/SelfDevelopment/Projects"
          "~/projects"
          ))
)

(require 'config-evil)
(require 'config-org)
(require 'config-org-roam)
(require 'config-ox)
(require 'config-latex)
;; render latex block, commented due to performance issue
;(add-hook 'org-mode-hook 'org-fragtog-mode)
(require 'config-pdf)
(require 'config-cjk)
(require 'config-lsp)
(require 'config-misc)

(require 'config-external)

; native-comp
(setq comp-speed 1)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq package-native-compile t)
    ))

