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
(setq doom-font (font-spec :family "Sarasa Term SC" :size 20)
      doom-big-font (font-spec :family "Sarasa Term SC" :size 20))
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
        (doom/reload-theme) ) ) ;; end of (defun ...
; run every hour
(run-with-timer 0 3600 'synchronize-theme)

;;; private configs for individual modules and packages
(add-to-list 'load-path (concat doom-private-dir "lisp"))

(require 'init-utils)

; Global variables
;   mz/org-notes : all my org(-roam) notes
;   bibfile : bibliography, now a symlink to my exported zotero biblatex library
;   mz/evil-leader : leader key to unify evil-leader and other keys that I want to emulate as evil-leader
;                    the later usage is mainly when I only want to bind key to a particular mode
(setq
 mz/org-notes (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam")
 ;bibfile (concat mz/org-notes "/bibliography_linux.bib")
 mz/bibfile (concat mz/org-notes "/bibliography.bib")
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
        '(not markdown-mode erc-mode message-mode help-mode gud-mode)
        ; 在这些包中停止使用company
        ; company自动补全中文会导致卡顿
  )
)

;; Start move-text
(move-text-default-bindings) ; default bindings
;; End move-text

;; projects management by projectile
(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Documents/SelfDevelopment/codes"
                                         "~/Documents/SelfDevelopment/Academia"
                                         "~/Documents/SelfDevelopment/Projects"
                                         "~/Projects"
                                         ))
  ; for org export
  (add-to-list 'projectile-other-file-alist
               '("org" . ("pdf" "tex" "md")))
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
;(require 'config-lsp)
;(require 'config-misc)

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

; company-jedi
;     https://github.com/emacsorphanage/company-jedi
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages '(org-roam-server org-journal org-fancy-priorities))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
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

;; auto-save by lazycat
(use-package! auto-save
  :load-path "auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
)

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

;;; make side by side buffers function the same as the main window
;(setq truncate-partial-width-windows nil)
;; do not wrap lines for long sentence
;; truncate it by myself
;;(setq-default global-visual-line-mode t)
;(setq-default truncate-lines t)

; native-comp
(setq comp-speed 1)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq package-native-compile t)
    ))

;; smex is replaced by counsel-M-x in Doom. comment
;(use-package! smex
;  :config
;  (smex-initialize)
;  (global-set-key (kbd "M-x") 'smex)
;  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;  ;; This is your old M-x.
;  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;)

(use-package! window-numbering
  :config
  (window-numbering-mode)
  ;; redefine workspaces shortcuts to resolve the conflict with +doom/workspaces
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

;;; for use of direnv
; envrc from Doom :tool direnv by purcell
(use-package! envrc
 :config
 (envrc-global-mode)
)
;;; emacs-direnv from github by wbolster
;;(use-package! direnv
;; :config
;; (direnv-mode)
;;)

(use-package! flycheck
  :config
  ; only use pylint, disable others
  ; setq-default acts as a global operation
  (setq-default flycheck-disabled-checkers '(
                python-flake8 python-pycompile python-pyright python-mypy
                )
  )
  ;;; whitelist dir local variables
  ;;; https://stackoverflow.com/q/19806176
  ;;; for example
  ;;(put 'flycheck-gfortran-include-path 'safe-local-variable #'stringp)

  ;;; custom checkers
  ;;; ifort checker, adopted from flycheck fortran-gfortran checker
  ;;; and https://github.com/edbennett/flycheck-ifort/blob/master/flycheck-ifort.el
  (flycheck-def-args-var flycheck-ifort-args fortran-ifort)
  (flycheck-def-option-var flycheck-ifort-include-path nil fortran-ifort
    "A list of include directories for Intel Fortran.
  
  The value of this variable is a list of strings, where each
  string is a directory to add to the include path of ifort.
  Relative paths are relative to the file being checked."
    :type '(repeat (directory :tag "Include directory"))
    :safe #'flycheck-string-list-p)
  
  (flycheck-def-option-var flycheck-ifort-language-standard "f95"
                           fortran-ifort
    "The language standard to use in Intel fortran.
  
  The value of this variable is either a string denoting a language
  standard, or nil, to use the default standard.  When non-nil,
  pass the language standard via the `-stand' option."
    :type '(choice (const :tag "Default standard" nil)
                   (string :tag "Language standard")))
  
  
  (flycheck-def-option-var flycheck-ifort-layout nil fortran-ifort
    "The source code layout to use in Intel Fortran.
  
  The value of this variable is one of the following symbols:
  
  nil
       Let ifort determine the layout from the extension
  
  `free'
       Use free form layout
  
  
  `fixed'
       Use fixed form layout
  
  In any other case, an error is signaled."
    :type '(choice (const :tag "Guess layout from extension" nil)
                   (const :tag "Free form layout" free)
                   (const :tag "Fixed form layout" fixed))
    :safe (lambda (value) (or (not value) (memq value '(free fixed)))))

  (defun flycheck-option-ifort-layout (value)
    "Option VALUE filter for `flycheck-ifort-layout'."
    (pcase value
      (`nil nil)
      (`free "-free")
      (`fixed "-fixed")
      (_ (error "Invalid value for flycheck-ifort-layout: %S" value))))
  
  (flycheck-def-option-var flycheck-ifort-warnings '("all")
                           fortran-ifort
    "A list of warnings for Intel Fortran.
  
  The value of this variable is a list of strings, where each string
  is the name of a warning category to enable.  By default, all
  recommended warnings are enabled (as by `-Wa')."
    :type '(choice (const :tag "No additional warnings" nil)
                   (repeat :tag "Additional warnings"
                           (string :tag "Warning name")))
    :safe #'flycheck-string-list-p)
  
  (flycheck-define-checker fortran-ifort
    "An Fortran syntax checker using Intel Fortran"
    :command ("ifort"
              "-syntax-only"
              ;;; Fortran has similar include processing as C/C++
              ;;"-iquote" (eval (flycheck-c/c++-quoted-include-directory))
              (option "-stand=" flycheck-ifort-language-standard concat)
              (option "" flycheck-ifort-layout concat
                      flycheck-option-ifort-layout)
              (option-list "-warn " flycheck-ifort-warnings)
              (option-list "-I" flycheck-ifort-include-path concat)
              (eval flycheck-ifort-args)
              source)
    :error-patterns
    (
     (error line-start (file-name) "(" line "): error #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
     (warning line-start (file-name) "(" line "): warning #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
     (info line-start (file-name) "(" line "): remark #" (one-or-more (in "0-9")) ": " (message) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) line-end)
    )
    :modes (fortran-mode f90-mode)
  )
)
;;; language server provider related
;;; used with flycheck
(use-package! eglot
  :config
  (add-hook 'f90-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
)

(after! eglot
  :config
  (set-eglot-client! 'python-mode '("pylsp"))
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  ;;; modified from https://github.com/joaotavora/eglot/issues/590#issuecomment-758233948
  ;;; when using setq, eglot-workspace-configuration is still nil in test project files, opened by projectile
  ;;; use setq-default instead works for me (not enough time to think about why)
  (setq-default eglot-workspace-configuration
        '((pylsp
            (plugins
             (pycodestyle
              (enabled . nil))
             (pyflakes
              (enabled . nil))
             ))
         )
  )
  ;((python-mode . ((eglot-workspace-configuration
  ;    . (
  ;       (:pylsp . (:plugins (:pycodestyle (:enabled nil))))
  ;      )))
  ;  )
  ;)
  ;;; adjust PATH manually, adapted from section 1.2.5 of https://dotdoom.rgoswami.me/config.html
  ;;; put exec-path later to "prepend" effetively
  ;;; I installed pylsp and fortls to my usually working environment, so adding lsp bin to PATH seems not ncessary
  ;(when (string= (system-name) "myz-amd-fedora")
  ;  (setq exec-path (append '(
  ;                          ;(concat (getenv "HOME") "/.pyenv/versions/miniconda3-4.7.12/envs/lsp/bin/") ;; python, fortran
  ;                          "/home/minyez/.pyenv/versions/miniconda3-4.7.12/envs/lsp/bin/"
  ;                          )
  ;                          exec-path 
  ;                          ))
  ;)
)

