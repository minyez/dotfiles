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


; Global variables
;   org_notes : all my org(-roam) notes
;   bibfile : bibliography, now a symlink to my exported zotero biblatex library
;   mz/evil-leader : leader key to unify evil-leader and other keys that I want to emulate as evil-leader
;                    the later usage is mainly when I only want to bind key to particular mode
(setq
 org_notes (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam")
 bibfile (concat org_notes "/bibliography_macos.bib")
 ;org_notes (concat (getenv "HOME") "/tmp/org-demo") ; for demonstration
 ;bibfile (concat (getenv "HOME") "/Documents/SelfDevelopment/Database/test.bib")
 mz/evil-leader ","
)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (format "%s" org_notes))

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

;; set zsh to default shell of vterm
(use-package! vterm
  :config
  (setq vterm-shell "zsh"))

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

;(use-package! evil-collection
;  :after evil
;  :ensure t
;  :config
;  (evil-collection-init)
;)

(use-package! evil-snipe
  ;; see L612 of evil-snipe
  :after evil
  :bind
  (:map evil-snipe-parent-transient-map
        ("s-;" . evil-snipe-repeat-reverse))
)

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

;; Start move-text
(move-text-default-bindings) ; default bindings
;; End move-text
;
; org-mode configuration
(use-package org
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode)
   ;(before-save . zp/org-set-last-modified)
   (before-save . org-update-all-dblocks)        ; update all dynamic table before saving
  )
  :bind
  (:map org-mode-map
        ("C-c l" . org-insert-link)
        ("C-c i" . org-insert-image)
        ("C-c C-i" . org-time-stamp-inactive)
      )
  :config
  (map! :map org-mode-map
        :nv "SPC a a" #'org-agenda-file-to-front
        :nv "SPC a t" #'org-agenda-todo
        :nv "DEL"     #'org-mark-ring-goto
        :nv "M-j"     #'org-metadown
        :nv "M-k"     #'org-metaup
        :nv "SPC d"   #'+org/remove-link
        :nv "M-n"     #'org-next-link
        :nv "M-p"     #'org-previous-link
        )
  (setq org-archive-location (concat org_notes "/archive.org::* From %s"))
  ; for org-capture purpose
  (setq org-default-notes-file (concat org_notes "/todos.org::* Inbox"))
  ; defvar by doom
  (setq +org-capture-todo-file "todos.org")
  (setq +org-capture-notes-file "todos.org")
  ; short title for Beamer export
  ;  (add-to-list 'org-export-options-alist
  ;               '(:short_title "SHORT_TITLE" nil nil parse))
  (setq org-md-headline-style 'atx) ; setext
  (setq org-cycle-include-plain-lists 'integrate) ; allow folded-subtree cycle of plain lists
  (setq org-footnote-auto-adjust t)
  (setq org-tags-column -80)
  ;(setq org-export-in-background t) ; TODO 需要解决下面的init file问题
  ;(setq org-export-async-init-file "/Users/stevezhang/.doom.d/async-init.el")
  (setq org-extend-today-until 4) ; 设置一天结束的时间
  (setq org-table-number-regexp
        "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|N/A\\|[-+u]?inf\\)$")
  ;(fset 'agenda-buffer
  ; (kmacro-lambda-form [?  ?< ?a ?g ?e ?n ?d ?a return] 0 "%d"))
  ; active and inactive timestamp
  (setq org-table-formula-constants ; physical constants in SI units
        '(
          ("pi" . "3.14159265358")
          ("RY" . "13.60569301")
          ("HBAR" . "1.0545718e-34") 
          ("EPS0" . "8.8541878128e−12")
          ("FSCA" . "0.0072973525664")
          ("KB" . "1.38064852e-23")
          ("CLIGHT" . "2.99792458e8")
          ("CE" . "1.6021766208e-19") ; electron charge
;; conversion
          ("BOHR2ANG" . "0.5291772")
          ("ANG2M" . "1e-10")
          ("EV2J" . "1.6021766208e-19")
          ("HA2EV" . "27.21138602")
          ("THZ2HA" . "1.519829846e-4") ; 10^12 h in Ha unit
          ))
  (fset 'now-act
   (kmacro-lambda-form [escape ?  ?u ?  ?u ?\C-c ?.] 0 "%d"))
  (fset 'now-inact
   (kmacro-lambda-form [escape ?  ?u ?  ?u ?\C-c ?\C-i] 0 "%d"))
  ;; set clock to idle state after 10 minutes
  (setq org-clock-persist 'history
  ;      org-clock-idle-time 10
        org-clock-mode-line-total 'current  ; show current clocking time in mode-line
                                            ; 'auto for total; 'today
  )
  ;(add-to-list 'org-clock-clocktable-language-setup
  ;             '("zh" "文件" "L" "时间戳" "标题" "时间" "ALL" "总时间" "归档时间" "用时汇总 于"))
  (setq org-enforce-todo-checkbox-dependencies t)
  ; https://orgmode.org/worg/agenda-optimization.html
  (setq org-agenda-files (concat org_notes "/org-agenda.org")
        org-agenda-skip-scheduled-if-done 't
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup 't
        ;org-blank-before-new-entry '((heading . t) (plain-list-item . nil)) ; add empty line before new heading
        org-log-into-drawer 't
        org-ellipsis " ▼"
        ;org-log-reschedule 'time
        ;org-log-redeadline 'note
        org-log-done 'time
        org-agenda-use-tag-inheritance '(search timeline agenda)
        org-agenda-window-setup 'reorganize-frame
  )
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(i)" "WAITING(w!)" "NEEDREVIEW(r)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
          '(("TODO" :foreground "#ffffff" :background "#ff9933" :weight bold)
            ("WAITING" :foreground "#ffffff" :background "#9f7efe")
            ("WIP" :foreground "#ffffff" :background"#0098dd" :weight bold)
            ("NEEDREVIEW" :foreground "#ffffff" :background "#8c1400" :weight bold)
            ("DONE" :foreground "#ffffff" :background "#50a14f")
            ("CANCELLED" :foreground "#ff6480" :strike-through t)
             )
  )
  ; quick to-do states
  (fset 'waiting  ; WAIT
     (kmacro-lambda-form [?  ?m ?t ?w] 0 "%d"))
  (fset 'todo     ; TO-DO
     (kmacro-lambda-form [?  ?m ?t ?t] 0 "%d"))
  (fset 'inp      ; WIP 
     (kmacro-lambda-form [?  ?m ?t ?i] 0 "%d"))
  ;(setq org-support-shift-select t)

  ;; for paste picture from clipboard to org-mode
  ;; adapted from https://emacs-china.org/t/topic/6601/4
  (defun org-insert-image ()
    "Insert PNG image from the clipboard to the buffer by using =pngpaste=

The image will be created under 'images' directory in =org-directory=
with the name from user input. If image with the same name exists, the paste
will be stopped and you need to try again with another name.
Note that =pngpaste= should be installed outside Emacs"
    (interactive)
    (let* 
      (
       (path (concat org_notes "/images/"))
       (fn (format "%s" (read-string "Enter image name (w/o png):")))
		   (image-file (concat path fn ".png"))
      )
	    (if (not (file-exists-p path)) (mkdir path))
	    (if (file-exists-p image-file)
		(message (format "Warning: found image %s.png in %s" fn path))
                (shell-command (format "pngpaste %s" image-file)))
         (insert (format "#+name: fig:%s\n" fn))
         (insert "#+caption:\n")
         (insert ":IMAGE:\n")
         (insert "#+attr_org: :width 300\n")
         (insert "#+attr_latex: :width 0.6\\linewidth\n")
	 (org-insert-link nil (concat "file:./images/" fn ".png") "")
         ;(insert "\n:PROPERTIES:\n:CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n:END:\n")
         (insert "\n:END:")
         ;; may add further elisp expression to suppress interaction for description
    ) ;; (org-display-inline-images) ;; inline显示图片
	)
  ;; for recent activity search
  ;; https://yqrashawn.com/2018/09/17/record-org-mode-recent-activity/
  (setf ;(elt org-log-note-headings 1) '(      state . "RESTATE: from %S on %t")
        (elt org-log-note-headings 3) '( reschedule . "RESCHEDULE: from %S on %t")
        (elt org-log-note-headings 4) '(delschedule . "UNSCHEDULE: was %S on %t")
        (elt org-log-note-headings 5) '( redeadline . "REDEADLINE: from %S on %t")
        (elt org-log-note-headings 6) '(deldeadline . "UNDEADLINE: was %S on %t")
        )
  ;(defun my/find-state (&optional end)
  ;"Used to search through the logbook of subtrees.

  ; Looking time stamp in logbook."
  ;  (let* ((closed (re-search-forward "^CLOSED: \\[" end t))
  ;         (created (if (not closed) (re-search-forward "^:CREATED: \\[" end t)))
  ;         (logbook (if (not closed) (re-search-forward "^[ \t]*\\(CLOCK\\|\\(RE\\|UN\\)\\(STATE\\|SCHEDULE\\|DEADLINE\\)\\):" end t)))
  ;         (result (or closed logbook created)))
  ;    result))

  ;; source: https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
  ;;------
  ;; Handling ‘CREATED’
  ;;------
  (defvar zp/org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

  ;; TODO: Find the source for this because I’ve improved something which
  ;; already existed
  (defun zp/org-set-created-property (&optional active name)
    "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given, the ‘NAME’
argument will be used instead. If the property already exists, it
will not be modified.
If the function sets CREATED, it returns its value."
    (interactive)
    (let* ((created (or name zp/org-created-property-name))
           (fmt (if active "<%s>" "[%s]"))
           (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
      (unless (org-entry-get (point) created nil)
        (org-set-property created now)
        now)))
  ;;--------------------------
  ;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
  ;;--------------------------
  (defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))
  (defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  ;(defun zp/org-set-last-modified ()
  ;  "Update the LAST_MODIFIED file property in the preamble."
  ;  (when (derived-mode-p 'org-mode)
  ;    (zp/org-set-time-file-property "LAST_MODIFIED")))
  (defun zp/org-capture-set-created-property ()
    "Conditionally set the CREATED property on captured trees."
    (let ((add-created (plist-get org-capture-plist :add-created))
          (type (plist-get org-capture-plist :type)))
      (when (and (eq type 'entry)
                 add-created)
        (unless (buffer-narrowed-p)
          (error "Buffer is not narrowed"))
        (save-excursion
          (goto-char (point-min))
          (zp/org-set-created-property)
        )
      )
     )
  )
  ; ignore headlines with "ignore" when export
  (defun org-export-ignore-headlines (data backend info)
  "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
    (org-element-map data 'headline
      (lambda (object)
        (when (member "ignore" (org-element-property :tags object))
          (let ((level-top (org-element-property :level object))
                level-diff)
            (mapc (lambda (el)
                    ;; recursively promote all nested headlines
                    (org-element-map el 'headline
                      (lambda (el)
                        (when (equal 'headline (org-element-type el))
                          (unless level-diff
                            (setq level-diff (- (org-element-property :level el)
                                                level-top)))
                          (org-element-put-property el
                            :level (- (org-element-property :level el)
                                      level-diff)))))
                    ;; insert back into parse tree
                    (org-element-insert-before el object))
                  (org-element-contents object)))
          (org-element-extract-element object)))
      info nil)
  data)
  (add-hook 'org-export-filter-parse-tree-functions 'org-export-ignore-headlines)
)

(use-package! reftex
  :config
  (setq reftex-cite-format 'biblatex)
)


(use-package! org-roam
  :init
  (setq org-roam-directory (format "%s" org_notes))
  ;(setq org-roam-completion-system 'helm)
  (setq org-roam-graph-executable "/usr/local/bin/dot")
  ;(setq org-roam-graph-executable "/usr/local/bin/neato")
  (setq org-roam-graph-viewer "/usr/bin/open"
        org-roam-index-file "index.org"
        org-roam-graph-extra-config '(("overlap" . "false")) ; man dot for attributes setup
        )
  (map! :leader
        :prefix "r"
        ;;:desc "Org-Roam-Buffer"       "r" #'org-roam
        ;;:desc "Org-Roam-Insert"       "i" #'org-roam-insert
        ;;:desc "Org-Roam-Find"         "." #'org-roam-find-file ;; removed in V2
        :desc "Org-Roam-Buffer"         "r" #'org-roam-buffer-toggle
        :desc "Org-Roam-Insert"         "i" #'org-roam-node-insert
        :desc "Org-Roam-Find"           "." #'org-roam-node-find   ;; used in V2
        :desc "Org-Roam-Store"          "l" #'org-roam-store-link
        :desc "Org-Roam-Unlinked-Refs"  "u" #'org-roam-unlinked-references
        :desc "Orb-Note-Actions"        "n" #'orb-note-actions
        )
  :bind
  (:map org-roam-mode-map
        (;;("C-c r R" . org-roam)
         ;;("C-c r f" . org-roam-find-file)
         ("C-c r R" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r L" . org-roam-store-link)
         ("C-c r u" . org-roam-unlinked-references)
         ("C-c r r" . org-roam-find-ref)
         ("C-c r d" . org-roam-find-directory)
         ("C-c r j" . org-roam-jump-to-index)
         ("C-c r b" . org-roam-switch-to-buffer)
         ("C-c r n" . orb-note-actions)
         ("C-c r g" . org-roam-graph))
   :map org-mode-map
        (
         ;;("C-c r i" . org-roam-insert)
         ("C-c r i" . org-roam-node-insert)
         )
  )
;; source: https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
  :config
  (org-roam-setup)
  ;(org-roam-mode +1) ; set to major mode, dropped in V2
  (defun org-roam-search-dup-ids ()
    (let ((org-id-files (org-roam--list-files org-roam-directory))
      org-agenda-files)
    (org-id-update-id-locations))
  )
  (setq org-roam-rename-file-on-title-change nil)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "%?"
           :if-new (file+head
           "%<%Y-%m-%d>.org"
           "#+title: %<%Y-%m-%d>\n#+options: title:nil toc:nil
#+filetags: :Daily:
#+latex_class: mwe\n#+latex_compiler: pdflatex"
           )
           :unnarrowed t))
  )
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :if-new (file+head
           "%<%Y%m%d%H%M%S>-${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+startup: content\n#+created: %U\n")
           :unnarrowed t)
          ("l" "lecture" plain "%?"
           :if-new (file+head
           "%<%Y%m%d%H%M%S>-lecture-${slug}.org"
           :head "# -*- truncate-lines: t -*-
#+title: ${title}\n#+startup: overview\n#+options: toc:nil\n#+filetags: Lecture\n#+created: %U
#+latex_class: article\n#+latex_compiler: xelatex
#+lecturer:\n#+place:\n")
           :unnarrowed t)
          ("i" "index page" plain "%?"
           :if-new (file+head
           "index-${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+startup: content\n#+created: %U\n")
           :unnarrowed t)
          ("c" "coding related")
          ("ce" "error info" plain "%?"
           :if-new (file+head
           "error-${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+created: %U
#+filetags: :Error:Unresolved:\n
* 问题描述 Question

* 错误归因 Attribution

* 案例 Case

* 参考资料 References")
           :unnarrowed t)
          ("a" "aps manuscript")
          ("ab" "prb" plain "%?"
           :if-new (file+head
           "paper/${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+startup: overview\n#+created: %U#+filetags: Manuscript
#+latex_class: prb
#+latex_compiler: pdflatex
")
           :unnarrowed t)
          ("b" "non-STEM book note" plain "%?"
           :if-new (file+head
           "${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: 《${title}》笔记\n#+startup: overview\n#+filetags: Book
#+created: %U\n#+options: toc:nil email:t f:t
#+latex_compiler: xelatex\n#+latex_class: article\n\n#+latex: \\tableofcontents\n#+latex: \\clearpage\n
* Summary\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n
* Appendix\n#+latex: \\appendix\n** Notes\n")
           :unnarrowed t)
          ("s" "Beamer seminar slides" plain "%?"
           :if-new (file+head "slides/${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+short_title: ${title}\n#+author: Min-Ye Zhang\n#+email: stevezhang@pku.edu.cn
#+startup: overivew beamer
#+filetags: Slides
#+latex_class: beamer
#+export_file_name: ${slug}_slides
# #+latex_class: beamerarticle
# #+export_file_name: ${slug}_handout
# ==============================
#+options: H:3
#+latex_header: \\usepackage[maxnames=3,style=nature,date=year,url=false,isbn=false,doi=false,articletitle=false]{biblatex}
#+latex_header: \\addbibresource{../bibliography.bib}
#+beamer_theme: CambridgeUSzmy
#+beamer_header: \\institute[CCME, PKU]{College of Chemistry and Molecular Engineering\\\\ Peking University}
#+beamer_header: \\date[\\today]{@Founder 312, \\today}
#+beamer_header: \\renewcommand{\\titleheader}{\\itshape TMC Group Seminar}
#+latex_compiler: xelatex
#+created: %U

* Acknowledgement
:PROPERTIES:
:BEAMER_ENV: appendix
:UNNUMBERED: t
:END:
#+beamer: \\begin{frame}{Acknowledgement}{}
#+beamer: \\LARGE Thank you for listening!
#+beamer: \\end{frame}
** LaTex acknowledgement                                               :ignore:
:PROPERTIES:
:BEAMER_ENV: ignoreheading
:END:
I appreciate anyone who reads this handout. Suggestions are totally welcome.

# * Resources # optional section
# :PROPERTIES:
# :BEAMER_ENV: appendix
# :UNNUMBERED: t
# :END:
# #+beamer: \\begin{frame}[allowframebreaks]{Resources}
# - content here as items
# #+beamer: \\end{frame}

* References
:PROPERTIES:
:BEAMER_ENV: appendix
:UNNUMBERED: t
:END:
#+beamer: \\begin{frame}[allowframebreaks]{References}
#+latex: \\printbibliography[heading=none]
#+beamer: \\end{frame}")
           :unnarrowed t)
          ("p" "research project" plain "%?"
           :if-new (file+head "${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n
#+author: Min-Ye Zhang
#+email: stevezhang@pku.edu.cn
#+created: %U
#+startup: overview
#+roam_tags: Research
#+latex_header: \\usepackage[maxnames=3,style=nature,date=year,url=false,isbn=false,doi=false,articletitle=false]{biblatex}
#+latex_header: \\addbibresource{./bibliography.bib}
#+options: email:t
#+latex_compiler: xelatex\n#+latex_class: article\n
#+latex: \\clearpage\n
* Aims
* Literature Review
* Proposal
* Data
* References
:PROPERTIES:
:UNNUMBERED: t
:END:
#+latex: \\printbibliography[heading=none]
* Changelog
:PROPERTIES:
:UNNUMBERED: t
:END:
- %(format-time-string \"[%Y-%m-%d %a %H:%M]\") : initial file")
           :unnarrowed t)
          ("t" "language thesaurus" plain "%?"
           :if-new (file+head 
           "${slug}.org"
           "# -*- truncate-lines: t -*-
#+title: vs. \n#+startup: overview\n#+filetags: Therausus\n#+created: %U\n
* Definition\n* Examples\n* Sources")
           :unnarrowed t)
          ("m" "math phys book" plain "%?"
           :if-new (file+head
           "${slug}"
           "# -*- truncate-lines: t -*-
#+title: ${title}\n#+author: Min-Ye Zhang\n#+email: stevezhang@pku.edu.cn
#+startup: overview\n#+filetags: Book\n#+created: %U
#+latex_class: book\n#+latex_compiler: xelatex\n
* Notes of Ch01

* MISCs :noexport:
** Related books
** Roadmap
** Changelog")
           :unnarrowed t)
    ))
  (setq org-roam-graph-exclude-matcher '("journal"
                                         "slides"
                                         "daily"
                                         "org-agenda.org"
                                         "archive.org"
                                         "read.org"
                                         "work.org"
                                         "exercise.org"
                                         "zatsuyou.org"
                                        ))
  (require 'org-roam-protocol) ; use org-protocol
;  (defun my-org-protocol-focus-advice (orig &rest args)
;    (x-focus-frame nil)
;    (apply orig args)
;  )
;  (advice-add 'org-roam-protocol-open-ref :around
;            #'my-org-protocol-focus-advice)
;  (advice-add 'org-roam-protocol-open-file :around
;            #'my-org-protocol-focus-advice)
  ; use for gnuplot
;  (defun org-init-table-gnuplot()
;    "Create a 2-column table and gnuplot source block for exporting data plot to images directory, used for quick check only"
;    (interactive)
;    (let* 
;      (
;       (title (format "%s" (read-string "Enter data title: ")))
;      )
;         (insert (format "#+name: tab-%s\n" title))
;         (insert (format "#+caption: %s\n" title))
;	 (insert "#+attr_latex: :booktabs t\n")
;	 (insert "| | |\n")
;         (insert (format "#+name: gnuplot-%s\n" title))
;	 (insert (format "#+header: :var data=tab-%s\n" title ))
;	 (insert (format "#+header: :exports results :file images/%s.png\n" title))
;	 (insert "#+begin_src gnuplot\n")
;	 (insert (format "set title \"%s\"\n" title))
;	 (insert "plot data u 1:2 w lp")
;	 (insert "#+end_src\n")
;         (insert (format "#+name: fig-%s\n" title))
;         (insert (format "#+caption: %s\n" title))
;	 (insert "#+attr_org: :width 400\n")
;	 (insert "#+attr_latex: :width 0.6\\linewidth\n")
;	 (insert (format "#+results: gnuplot-%s\n" title))
;    ))
)
;


;(add-hook 'after-init-hook 'org-roam-mode)
;(add-hook 'org-mode-hook 'org-roam-mode) ;; V1
;; no more org-roam-mode in org-roam V2
(setq org-roam-v2-ack t) ;; remove V2 warnings after clean upgrade from V1
;; instead run org-mode-setup before the first build of database

;(add-hook 'after-init-hook 'blink-cursor-mode)
(add-hook 'org-mode-hook 'org-cdlatex-mode)
(add-hook 'org-mode-hook 'reftex-mode)
;; render latex block, commented due to performance issue
;(add-hook 'org-mode-hook 'org-fragtog-mode)

;; visualize roam graph. commented for low usage and performance
;; replaced by org-roam-ui
;(use-package org-roam-server
;;  :ensure t
;  :config
;  (setq org-roam-server-host "127.0.0.1"
;        org-roam-server-port 8080
;        org-roam-server-export-inline-images t
;        org-roam-server-authenticate nil
;	      org-roam-server-serve-files nil
;        org-roam-server-served-file-extensions '("pdf" "mp4")
;        org-roam-server-network-poll t
;        org-roam-server-network-arrows nil
;        org-roam-server-network-label-truncate t
;        org-roam-server-network-label-truncate-length 60
;        org-roam-server-network-label-wrap-length 24)
;)
;;(org-roam-server-mode)

;;; org-roam-ui
(use-package! websocket
    :after org-roam)
(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


; Chinese input setting, partly copied from Doom
(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)
  (setq-hook! 'markdown-mode-hook pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp "\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?1:\\cC\\|\\cH\\|\\cK\\)\\)\\(?2:[\(=0-9A-Za-z\\$\\]\\)\\|\\(?1:[=0-9A-Za-z\\$\)]\\)\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?2:\\cC\\|\\cH\\|\\cK\\)\\)")
)

(use-package ox-beamer
  :config
  ; default 3rd level heading as frame.
  ; 1st and 2nd are sec and subsec
  (setq org-beamer-frame-level 3)
)
; https://superuser.com/questions/896741/how-do-i-configure-org-latex-classes-in-emacs
; https://stackoverflow.com/questions/36197545/org-mode-latex-export-making-todos-red
(use-package ox-latex
  :bind
  ("C-c e o" . my-org-latex-export-latex)
  ;("C-c e o" . org-latex-export-latex)
  ("C-c e b" . my-org-beamer-export-latex)
  ;("C-c e b" . org-beamer-export-latex)
  :config
  ;(fset 'my-org-latex-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?o] 0 "%d"))
  ;(fset 'my-org-beamer-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?O] 0 "%d"))
  (fset 'my-org-latex-export-latex
   (kmacro-lambda-form [?\C-c ?\C-e ?l ?l] 0 "%d"))
  (fset 'my-org-beamer-export-latex
   (kmacro-lambda-form [?\C-c ?\C-e ?l ?b] 0 "%d"))
  (setq org-latex-pdf-process '("latexmk -latexoption=\"-interaction=nonstopmode -shell-escape\" -pdf -pdflatex=%latex -bibtex -f %f"))
  ;; prefer custom label
  (setq org-latex-prefer-user-labels t)
  ;; user-defined headline to-do rendering
  (defun org-latex-format-headline-colored-keywords-function (todo todo-type priority text tags info)
        (concat
           ;(cond 
           ;      ; colorize to-do items
           ;      ; comment whole cond to avoid showing to-do markers
           ;      ((string= todo "TODO")(and todo (format "{\\color{Red}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "WIP")(and todo (format "{\\color{ProcessBlue}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "WAITING")(and todo (format "{\\color{Magenta}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "CANCELLED")(and todo (format "{\\color{Orange}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "DONE")(and todo (format "{\\color{Green}\\bfseries\\sffamily %s} " todo)))
           ;)
           ;(and priority (format "\\framebox{\\#%c} " priority))
           text
           (and tags
           (format "\\hfill{}\\textsc{%s}"
             (mapconcat (lambda (tag) 
                            (if (string= tag "STAR") "\\bigstar" (org-latex-plain-text tag info))) tags ",")))
             ;(mapconcat (lambda (tag) (org-latex-plain-text tag info)) tags ":")))
    )
  )
  (setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function)
  ; customized classes for latex export
  ; general Chinese note
  (setq org-latex-classes
               '(
  ; general English note
                ("article"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; minimal working example
                ("mwe"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\usepackage{mwe}
\\usepackage{amsmath,amssymb}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; General Chinese note
                ("ctexart"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper,scheme=plain]{ctexart}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; For org-journal. section for day, subsec for time
                ("journal"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper,scheme=plain]{ctexart}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ;("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ;("\\subparagraph{%s}" . "\\subparagraph*{%s}")
				 )
  ; Note for math. phys. books
                ("book"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt,titlepage]{book}
[DEFAULT-PACKAGES]
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\usepackage{physics}
\\usepackage{cases}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("report"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt]{report}
[DEFAULT-PACKAGES]
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("ctexbook"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt,titlepage]{ctexbook}
[DEFAULT-PACKAGES]
\\usepackage{physics}
\\usepackage{cases}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]"
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; General Engilish scientific writing
                ("sci"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[a4paper,11pt]{article}
\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; PRB paper
                ("prb"
                 "\\documentclass[aps,prb,twocolumn,a4paper,floatfix,showpacs]{revtex4-1}
\\bibliographystyle{apsrev4-1}
[NO-DEFAULT-PACKAGES]
\\usepackage[version=3]{mhchem}
\\usepackage{times,mathptmx}
\\usepackage{float}
\\usepackage{array}
\\usepackage{epstopdf}
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
\\usepackage{booktabs}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; Beamer for dual export
; https://orgmode.org/worg/exporters/beamer/beamer-dual-format.html
                ("beamer"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=True,linkcolor=,filecolor=,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0}}{hyperref}
\\documentclass[ignorenonframetext,presentation]{beamer}
[DEFAULT-PACKAGES]
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% space between caption and table
\\captionsetup[table]{belowskip=-6pt}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 )
                ("beamerarticle"
                 "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}
\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Blue,pdfborder={0 0 0},breaklinks=true}{hyperref}
\\documentclass[11pt,a4paper]{article} % add twoside for booklet printing
\\usepackage{beamerarticle}
\\usepackage[margin=0.8in,bmargin=1.0in,tmargin=1.0in]{geometry}
[DEFAULT-PACKAGES]
\\usepackage{physics}
% blockquote from eisvogel. 
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\definecolor{bq-border}{RGB}{0, 63, 126}
\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,
          linecolor=bq-border,skipabove=\\parskip]{customblockquote}
\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%
\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}
\\let\\Oldtextbullet\\textbullet
\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
[EXTRA]
[PACKAGES]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ))
  ;(setq org-latex-listings t)
  ;(add-to-list 'org-latex-packages-alist '("newfloat"  "minted"))
  ;; or
  (setq org-latex-listings 'minted
        org-latex-minted-options '(
          ("bgcolor" "bg")
          ("breaklines" "true")
          ("autogobble" "true")
          ("fontsize" "\\small")
          )
  )
)
;
(use-package! org
  :config
  ; packages to load at fisrt i.e. before EXTRA and PACKAGES
  (add-to-list 'org-link-abbrev-alist
               '("arxiv" . "https://arxiv.org/abs/%s"))
  ;; exclude the org-noter tag from inheriting to notes within
  (add-to-list 'org-tags-exclude-from-inheritance "noter")
  (add-to-list 'org-tags-exclude-from-inheritance "Reference")
  (add-to-list 'org-tags-exclude-from-inheritance "Book")
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-latex-default-packages-alist
    '(
      ("" "amsmath" t) ; to avoid iint and iiint error
      ("" "amssymb" t)
      ("" "wasysym" t) ; last to avoid iint and iint error
      ("AUTO" "inputenc"  t ("pdflatex"))
      ("T1"   "fontenc"   t ("pdflatex"))
      (""     "CJKutf8"   t ("pdflatex"))
      (""     "xeCJK"     nil ("xelatex", "xetex"))
      (""     "fontspec"  nil ("xelatex", "xetex", "lualatex", "luatex"))
      (""     "graphicx"  t)
      (""     "xcolor"  t)
      ;("nottoc,numbib"     "tocbibind" nil)
      ; corresponding to "setq org-latex-listings t"
      ;(""           "listings"   nil)
	    ; but minted is better to use
      ("newfloat,cache=true"   "minted"   nil)
      (""     "grffile"   t)
      (""     "longtable" nil)
      (""     "mdframed" nil)   ; for creating blockquote
      (""     "float" nil)
      (""     "wrapfig"   nil)
      (""     "subfig"    nil)
      (""     "rotating"  nil)
      ("normalem" "ulem"  t)    ; strikeout
      (""     "textcomp"  t)
      (""     "capt-of"   nil)
      ("font={small,it},skip=1pt"     "caption"   nil)
      (""     "parskip"   nil)  ; better paragraph spacing
      (""     "booktabs"   nil) ; better table
	 )
  )
  ; packages to load at last
  (setq org-latex-packages-alist
    '(
      ; hyperref and cleverf should be the last packages to load
      (""     "hyperref"  nil)
      (""     "cleveref"   nil)
     )
  )
  (fset 'make-bold
   (kmacro-lambda-form [?\C-c ?\C-x ?\C-f ?*] 0 "%d"))
  :bind
  ("s-b" . make-bold)
)
;; see
;; https://github.com/org-roam/org-roam-bibtex#org-roam-bibtex---bibtex-aware-capture-template-expansion
;; for more
(use-package! org-roam-bibtex ;; shortened as ORB
  :after org-roam
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
    ;'(("citekey" . "=key=")  "title" "author-or-editor" "date" "doi" "file" ("journal" . "journaltitle")  "volume" "pages"))
    '("citekey"  "title" "author-or-editor" "date" "doi" "file" "journaltitle" "volume" "pages"))
;      ;orb-note-actions-frontend 'ivy
  (setq orb-note-actions-interface 'ivy)
  (advice-add 'bibtex-completion-candidates :filter-return 'reverse)
  ; anystyle-related
  (setq orb-autokey-format "%A*[1]%y*"
        orb-pdf-scrapper-export-fields '("author" "journal" "date" "volume" "pages" "title"))
  (add-to-list 'org-roam-capture-templates
          '("r" "reference" plain "%?"
           :if-new (file+head
           "ref/${citekey}.org"
           "# -*- truncate-lines: t -*-
#+title: ${citekey}
#+filetags: :Reference:
#+startup: content
#+created: %U
:PROPERTIES:
:TITLE: ${title}
:AUTHOR: ${author-or-editor}
:JOURNAL: ${journaltitle}
:DATE: ${date}
:VOLUME: ${volume}
:PAGES: ${pages}
:DOI: [[doi:%(replace-regexp-in-string \" \" \"\" \"${doi}\")]]
:END:

- tags ::
- keywords ::

* Summary

* Notes :noter:
:PROPERTIES:
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:END:"
           )
           :unnarrowed t))
)

(after! org-roam (org-roam-bibtex-mode))

;; add executable for running anystyle
;;(add-to-list 'exec-path (concat (getenv "HOME") "/.rvm/rubies/ruby-2.6.5/bin"))

;(after! org-ref
(use-package helm-bibtex
  :config
  ;(when (featurep! :completion ivy)
  ;  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))
  (setq
   bibtex-completion-notes-path (format "%s" org_notes)
   bibtex-completion-bibliography (list bibfile)
   ;bibtex-completion-bibliography bibfile
   bibtex-completion-pdf-field "file"
   bibtex-completion-additional-search-fields '(keywords, journaltitle, publisher)
   bibtex-completion-pdf-open-function 'org-open-file
  )
  (setq bibtex-completion-display-formats
   '((article       . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${journaltitle:20} | ${keywords:*}")
     (inbook        . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| Chapter ${chapter:12} | ${keywords:*}")
     (book          . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${publisher:20} | ${keywords:*}")
     (t             . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${keywords:*}"))
  )
  :bind
  ("C-c n b" . helm-bibtex)
  ("C-c n n" . helm-bibtex-with-notes)
)

(use-package org-ref
    :config
    (setq
     org-ref-completion-library 'org-ref-ivy-cite
     ;; uses the helm-bibtex settings to find the pdf
     org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
     ;; location to file bibliography
     org-ref-default-bibliography (list bibfile)
     org-ref-notes-directory (format "%s" org_notes)
     org-ref-notes-function 'orb-edit-notes
    )
 ;   (add-to-list 'org-ref-helm-user-candidates
 ;                '("Open notes and PDF file" . (lambda ()
 ;                  (save-excursion
 ;                  (org-ref-open-notes-at-point)
 ;                  (org-ref-open-pdf-function)
 ;                  )))
 ;   )
)
;
;;(use-package org-zotxt
;;  :hook
;;  (org-mode . org-zotxt-mode))
;
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
  (deft-directory org_notes))

;(use-package org-fancy-priorities
;  :after org
;  :ensure t
;  :hook
;  (org-mode . org-fancy-priorities-mode)
;  :config
;  (setq org-fancy-priorities-list '((?A . "HIGH")
;                                    (?B . "MID")
;                                    (?C . "LOW")
;                                    (?D . "TRIV")
;                                    (?1 . "⚡")))
;)

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

(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⋆" "◉" "○" "†" "‡" "∵")) ;✸ ✿ ✭ ❡
)

; archive function that includes ancestor tree structure when copying
; https://gist.github.com/edgimar/072d99d8650abe81a9fe7c8687c0c993
;(use-package org-archive
;  :after org
;)

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

(use-package! org-journal
  :after org
  ;:ensure t
  :defer t
  :bind
  ("C-c j n" . org-journal-new-entry)
  ("C-c j j" . org-journal-open-current-journal-file)
  ("C-c j h" . org-journal-previous-entry)
  ("C-c j l" . org-journal-next-entry)
  :config
  (map! :nv "SPC j j" #'org-journal-open-current-journal-file
        :nv "SPC j n" #'org-journal-new-entry)
  (setq org-journal-date-format "%A, %d %B %Y" ; format to match date heading
        org-journal-date-prefix "* "
        org-journal-created-property-timestamp-format "%Y%m%d"
        org-journal-enable-encryption nil
        ;org-journal-file-type 'weekly
        org-journal-file-type 'monthly
        org-journal-dir (concat org_notes "/journal")
        ;org-journal-file-format "%Y-%m-%d.org" ; daily/weekly
        org-journal-file-format "%Y-%b.org"
    )
  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+title: Daily Journal\n#+startup: showeverything\n")
      (`weekly (concat "#+title: Weekly Journal - " (format-time-string "%G W%V") "\n#+startup: content nologdone\n"))
      (`monthly (concat "#+title: Monthly Journal - " (format-time-string "%G %B") "\n#+startup: folded nologdone\n"))
      (`yearly "#+title: Yearly Journal\n#+startup: folded\n"))
    "#+tags: WORK(w) BOOK(b) SPORTS(s) CONTACTS(c)\n"
    "#+options: toc:nil\n#+latex_class: journal\n#+latex_compiler: xelatex\n"
    ))
  (setq org-journal-file-header 'org-journal-file-header-func)
  ; https://github.com/bastibe/org-journal#kill-journal-buffer-after-saving-buffer-by-dhruvparamhans
  (defun org-journal-save-entry-and-exit ()
    "Simple convenience function.
    Saves the buffer of the current day's entry and kills the window
    Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    ;(kill-buffer-and-window)
    (kill-current-buffer)
    )
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)
  (define-key org-journal-mode-map (kbd "s-s") 'org-journal-save-entry-and-exit)
)

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

;(use-package org-noter-pdftools
;  :after org-noter
;  :config
;  (with-eval-after-load 'pdf-annot
;    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! pdf-view
  :config
  (map! :map pdf-view-mode-map
        :nv "z g" #'pdf-view-goto-page
        :nv "z r" #'image-rotate       ;; rotate the page (defined in image.el)
        )
)

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   ;org-noter-set-notes-window-behavior 'scroll
   ;; split fraction. default (0.5 . 0.5). slightly larger on vertical
   org-noter-doc-split-fraction '(0.56 . 0.5)
  )
 (defun my-org-noter-extract-annotation ()
   "Create notes skeleton with the PDF annotations.
 Only available with PDF Tools."
   (interactive)
   (org-noter--with-valid-session
    (cond
    ( (eq (org-noter--session-doc-mode session) 'pdf-view-mode)
      (let* ((ast (org-noter--parse-root))
             (top-level (org-element-property :level ast))
             output-data)
        (with-current-buffer (org-noter--session-doc-buffer session)
            (let ((chosen-annots (list 'highlight
                                       'underline
                                       'squiggly
                                       'text
                                       'strike-out
                                      ))
                  insert-contents pages-with-links)
 
              (setq insert-contents t)
 
              (dolist (item (pdf-info-getannots))
                (let* ((type  (alist-get 'type item))
                       (page  (alist-get 'page item))
                       (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                  (alist-get 'edges item)))
                       (top (nth 1 edges))
                       (item-subject (alist-get 'subject item))
                       (item-contents (alist-get 'contents item))
                       name contents)
                  (when (and (memq type chosen-annots) (> page 0))
                    (if (eq type 'link)
                        (cl-pushnew page pages-with-links)
                      (setq name (cond ((eq type 'highlight)  "Highlight")
                                       ((eq type 'underline)  "Underline")
                                       ((eq type 'squiggly)   "Squiggly")
                                       ((eq type 'text)       "Text note")
                                       ((eq type 'strike-out) "Strikeout")))
                      (when insert-contents
                        (setq contents (cons (pdf-info-gettext page edges)
                                             (and (or (and item-subject (> (length item-subject) 0))
                                                      (and item-contents (> (length item-contents) 0)))
                                                  (concat (or item-subject "")
                                                          (if (and item-subject item-contents) "\n" "")
                                                          (or item-contents ""))))))
 
                      (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                            output-data)
            )
            )))
        )
 
          (when output-data
            (setq output-data
                  (sort output-data
                        (lambda (e1 e2)
                          (or (not (aref e1 1))
                              (and (aref e2 1)
                                   (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
      )
      )
      ; print out
        (with-current-buffer (org-noter--session-notes-buffer session)
          ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
          ;; narrow region to include the new headings
          (widen)
          (save-excursion
            (goto-char (org-element-property :end ast))
 
            (let (last-absolute-level
                  title location relative-level contents
                  level)
              (dolist (data output-data)
                (setq title          (aref data 0)
                      location       (aref data 1)
                      ;relative-level (aref data 2)
                      contents       (aref data 3))
                ;(if (symbolp relative-level)
                ;    (setq level (1+ last-absolute-level))
                ;  (setq last-absolute-level (+ top-level relative-level)
                ;        level last-absolute-level))
                (setq level (1+ top-level))
 
                ;;; add by minyez
                ;(org-noter--insert-heading level title)
                (when (car contents)
                  (org-noter--insert-heading level (remove-line-breaks (car contents))))
                ;; end add by minyez
 
                (when location
                  (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))
 
                (when org-noter-doc-property-in-notes
                  (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                  (org-entry-put nil org-noter--property-auto-save-last-location "nil"))
 
                (when title
                  (org-noter--insert-heading (1+ level) title))
                (when (cdr contents)
                  (org-noter--insert-heading (1+ level) "Comment")
                  ;(insert (cdr contents)))
                  (insert (remove-line-breaks (cdr contents))))
         ))
 
            (setq ast (org-noter--parse-root))
            (org-noter--narrow-to-root ast)
            (goto-char (org-element-property :begin ast))
            (outline-hide-subtree)
            (org-show-children 1)
      )
      )
    )
    )
    (t (user-error "This command is only supported on PDF Tools.")))))
   :bind
   ("C-c n N" . org-noter)
   ("C-c n s" . my-org-noter-extract-annotation)
)

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

;;; comment it for performance and low usage
;;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;;(use-package! org-super-agenda
;;  :after org-agenda
;;  :init
;;  (setq org-super-agenda-groups '((:name "Today"
;;                                         :time-grid t
;;                                         :scheduled today)
;;                                  (:name "Due today"
;;                                         :deadline today)
;;                                  (:name "Overdue"
;;                                         :deadline past)
;;                                  (:name "Due soon"
;;                                         :deadline future)
;;                                ))
;;  :config
;;  (org-super-agenda-mode)
;;)

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

;;; agenda 里面时间块彩色显示
;;; From: https://emacs-china.org/t/org-agenda/8679/3
;;(defun my/org-agenda-time-grid-spacing ()
;;  "Set different line spacing w.r.t. time duration."
;;  (save-excursion
;;    (let* ((background (alist-get 'background-mode (frame-parameters)))
;;           (background-dark-p (string= background "dark"))
;;           (colors (list "#ff9933"  "#9f7efe" "#0098dd" "#8c1400" "#50a14f"))
;;           pos
;;           duration)
;;      (nconc colors colors)
;;      (goto-char (point-min))
;;      (while (setq pos (next-single-property-change (point) 'duration))
;;        (goto-char pos)
;;        (when (and (not (equal pos (point-at-eol)))
;;                   (setq duration (org-get-at-bol 'duration)))
;;          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
;;                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;;            (overlay-put ov 'face `(:background ,(car colors)
;;                                                :foreground
;;                                                ,(if background-dark-p "white" "black")))
;;            (setq colors (cdr colors))
;;            (overlay-put ov 'line-height line-height)
;;            (overlay-put ov 'line-spacing (1- line-height))))))))
;;
;;(add-hook 'org-agenda-finalize-hook #'my/org-agenda-time-grid-spacing)
;
;;(use-package org-download
;  ;:hook
;  ;(org-mode . org-download-enable)
;  ;(dired-mode . org-download-enable)
;  ;:config
;  ;(setq-default org-download-image-dir (format "%s/images" org_notes))
;  ;(setq org-download-timestamp ""
;        ;org-download-heading-lvl nil
;        ;org-download-display-inline-images nil
;        ;org-download-screenshot-method "xclip"
;        ;)
;;)
;
;;(use-package! writegood-mode
;;  :after org
;;  :hook
;;  (org-mode . writegood-mode)
;;)
;
;;(use-package jieba
;;  :load-path "~/.doom.d/jieba"
;;  :commands (jieba-mode jieba-ensure jieba-mode-map)
;;  :hook 
;;  (
;;   (after-init . jieba-mode)
;;  )
;;  :init
;;  (map! :map jieba-mode-map
;;        :nv "M-w" #'jieba-forward-word
;;  ;      :nv "b" #'jieba-backward-word
;;        )
;;  :config
;;  (add-hook 'org-mode-hook 'jieba-ensure)
;;)
;
(use-package asymbol
  :after cdlatex
  :init
  (setq asymbol-help-symbol-linewidth 110
        asymbol-help-tag-linewidth 110)
  ;; add keybindings
  ;(asymbol-global-input-unicode-symbol-on)
  ;(asymbol-latex-input-symbol-on)
  ;(asymbol-org-input-symbol-on)
  :config
  (define-key cdlatex-mode-map (vector asymbol-trigger-key) 'asymbol-insert-text-or-symbol)
  (add-hook 'org-cdlatex-mode-hook
            (lambda () (interactive)
            (define-key org-cdlatex-mode-map (vector asymbol-trigger-key) 'asymbol-insert-text-or-symbol)))
)

; company-jedi
;     https://github.com/emacsorphanage/company-jedi
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package org-archive
  :after org
  :config
  (setq org-archive-mark-done t) ; change subtree state to DONE when archived
)

;(use-package delve
;  :config
;  ;(map! :map delve-mode-map
;  ;      :nv "SPC a a" #'org-agenda-file-to-front
;  ;      :nv "SPC a t" #'org-agenda-todo
;  ;      )
;  (use-package delve-minor-mode
;    :config
;    (add-hook 'org-mode-hook #'delve-minor-mode-maybe-activate)
;  )
;  :bind
;  (("<f12>" . delve-open-or-select))
;  (:map delve-mode-map
;      ("=" . #'delve-add-tag)
;      ("j" . #'next-line)
;      ("k" . #'previous-line)
;      )
;)
(use-package! delve
  :bind
  ;; the main entry point, offering a list of all stored collections
  ;; and of all open Delve buffers:
  (("<f12>" . delve))
  :config
  ;; set meaningful tag names for the dashboard query
  ;(setq delve-dashboard-tags '("Tag1" "Tag2"))
  ;; turn on delve-minor-mode when org roam file is opened:
  (delve-global-minor-mode)
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

;;; auto-save by lazycat
;(use-package! auto-save
;  :load-path "auto-save"
;  :config
;  (auto-save-enable)
;  (setq auto-save-silent t)
;)

(use-package! org-habit)
;; native-comp
;(setq comp-speed 1)

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
(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

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

(use-package! flycheck
  :config
  ; only use pylint, disable others
  ; setq-default acts as a global operation
  (setq-default flycheck-disabled-checkers '(
                python-flake8 python-pycompile python-pyright python-mypy
                )
  )
)


(use-package! eglot
  :config
  (add-hook 'f90-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
)

(after! eglot
  :config
  (set-eglot-client! 'python-mode '("pylsp"))
  (set-eglot-client! 'f90-mode '("/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/envs/py371/bin/fortls"))
  (set-eglot-client! 'cc-mode '("/usr/local/opt/llvm/bin/clangd" "-j=2" "--clang-tidy"))
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
)

; activate
(use-package! pdf-tools
  :config
  ;; activate when opening pdf, otherwise the evil keybindings will not work
  (pdf-loader-install)
)

;(setq ns-auto-hide-menu-bar t)
(setq frame-resize-pixelwise t)


;; QE emacs mode
(use-package! qe-modes
  :load-path "qe-modes"
  ;;  automatically open the the pw.*.in, scf.*.in, nscf.*.in, relax.*.in,
  ;;  vc-relax.*.in, md.*.in, vc-md.*.in files by pw.x mode
  :config
  (add-to-list 'auto-mode-alist
               '("/\\(pw\\|n?scf\\|\\(?:vc-\\)?\\(?:md\\|relax\\)\\)\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . pw-mode))
  ;; automatically open the neb.*.in and smd.*.in files with neb.x mode
  (add-to-list 'auto-mode-alist '("/\\(neb\\|smd\\)\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . neb-mode))
  ;; automatically open the cp.*.in files with cp.x mode
  (add-to-list 'auto-mode-alist '("/cp\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . cp-mode))
  ;; automatically open the ph.*.in files with ph.x mode
  (add-to-list 'auto-mode-alist '("/ph\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . ph-mode))
  ;; automatically open the ld1.*.in files with ld1 mode
  (add-to-list 'auto-mode-alist '("/ld1\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . ld1-mode))
  ;; automatically open the pp.*.in files with pp.x mode
  (add-to-list 'auto-mode-alist '("/pp\\(\\.\\(?:.*\\)?\\)?\\.in\\'" . pp-mode))
)

