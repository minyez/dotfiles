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
(setq doom-theme 'doom-nord-light)
;; dark themse
;(setq doom-theme 'doom-dark+)
;(setq doom-theme 'doom-spacegrey)
;(setq doom-theme 'doom-peacock)
;(setq doom-theme 'doom-monokai-pro)
;(setq doom-theme 'doom-dracula)
;; make theme adapt to time. Modified from
;; https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
;(defun synchronize-theme ()
;    (setq hour
;        (string-to-number
;        ; get current time by current-times-string
;	; hour value occupies 3 characters (11-13)
;            (substring (current-time-string) 11 13)))
;    (if (member hour (number-sequence 6 19))
;        (setq now 'doom-nord-light)
;        (setq now 'doom-dracula))
;    (if (equal now doom-theme)
;        nil
;        (setq doom-theme now)
;        (doom/reload-theme) ) ) ;; end of (defun ...
;; run every hour
;(run-with-timer 0 3600 'synchronize-theme)

(setq projectile-project-search-path '("/Users/stevezhang/Documents/SelfDevelopment/Codes"))

;; Global variables
(setq
 org_notes (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam")
 ;org_notes (concat (getenv "HOME") "/tmp/org-demo") ; for demonstration
 bibfile (concat (getenv "HOME") "/Documents/SelfDevelopment/org-roam/bibliography.bib")
 ;bibfile (concat (getenv "HOME") "/Documents/SelfDevelopment/Database/test.bib")
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

(use-package evil
  :bind
  (:map evil-insert-state-map
        ("s-i" . evil-escape))
  (:map evil-normal-state-map
        ("s-i" . evil-insert))
  (:map evil-visual-state-map
        ("s-i" . evil-insert))
)

(use-package! company
  :config
  (setq 
        company-global-modes
        '(not markdown-mode org-mode erc-mode message-mode help-mode gud-mode)
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
        )
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
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w!)" "NEEDREVIEW(r)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
          '(("TODO" :foreground "#ffffff" :background "#ff9933" :weight bold)
            ("WAITING" :foreground "#ffffff" :background "#9f7efe")
            ("INPROGRESS" :foreground "#ffffff" :background"#0098dd" :weight bold)
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
  (fset 'inp      ; INPROGRESS
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
         (insert (format "#+NAME: fig:%s\n" fn))
         (insert "#+CAPTION:\n")
         (insert ":IMAGE:\n")
         (insert "#+ATTR_ORG: :width 300\n")
         (insert "#+ATTR_LATEX: :width 0.6\\linewidth\n")
	 (org-insert-link nil (concat "file:./images/" fn ".png") "")
         ;(insert "\n:PROPERTIES:\n:CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n:END:\n")
         (insert "\n:END:")
         ;; may add further elisp expression to suppress interaction for description
    ) ;; (org-display-inline-images) ;; inline显示图片
	)
  ;; for recent activity search
  ;; https://yqrashawn.com/2018/09/17/record-org-mode-recent-activity/
  (setf (elt org-log-note-headings 1) '(      state . "RESTATE: from %S on %t")
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

(after! org
  ;(jieba-ensure)
  (map! :map org-mode-map
        :nv "M-j" #'org-metadown
        :nv "M-k" #'org-metaup
        :nv "SPC d" #'+org/remove-link
        :nv "M-n" #'org-next-link
        :nv "M-p" #'org-previous-link
        :nv "DEL" #'org-mark-ring-goto
        )
  (map! :map org-roam-mode-map
        :nv "SPC r n"  #'orb-note-actions
        :nv "SPC r b"  #'helm-bibtex
        :nv "SPC r m"  #'helm-bibtex-with-notes
  )
)

(use-package! reftex
  :config
  (setq reftex-cite-format 'biblatex)
)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory (format "%s" org_notes))
  ;(setq org-roam-completion-system 'helm)
  (setq org-roam-graph-executable "/usr/local/bin/dot")
  ;(setq org-roam-graph-executable "/usr/local/bin/neato")
  (setq org-roam-graph-viewer "/usr/bin/open"
        org-roam-index-file "20200705201000-index.org"
        org-roam-graph-extra-config '(("overlap" . "false")) ; man dot for attributes setup
        )
  (map! :leader
        :prefix "r"
        :desc "Org-Roam-Insert"         "i" #'org-roam-insert
        :desc "Org-Roam-Find"           "." #'org-roam-find-file
        :desc "Org-Roam-Store"          "l" #'org-roam-store-link
        :desc "Org-Roam-Unlinked-Refs"  "u" #'org-roam-unlinked-references
        :desc "Orb-Note-Actions"        "n" #'orb-note-actions
        :desc "Org-Roam-Buffer"         "r" #'org-roam)
  :bind
  (:map org-roam-mode-map
        (("C-c r R" . org-roam)
         ("C-c r f" . org-roam-find-file)
         ("C-c r L" . org-roam-store-link)
         ("C-c r u" . org-roam-unlinked-references)
         ("C-c r r" . org-roam-find-ref)
         ("C-c r d" . org-roam-find-directory)
         ("C-c r j" . org-roam-jump-to-index)
         ("C-c r b" . org-roam-switch-to-buffer)
         ("C-c r n" . orb-note-actions)
         ("C-c r g" . org-roam-graph))
   :map org-mode-map
        (("C-c r i" . org-roam-insert))
  )
; source: https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
  :config
  ;(org-roam-mode +1) ; set to major mode
  (defun org-roam-search-dup-ids ()
    (let ((org-id-files (org-roam--list-files org-roam-directory))
      org-agenda-files)
    (org-id-update-id-locations))
  )
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain (function org-roam-capture--get-point) "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: ${title}\n#+STARTUP: content\n#+ROAM_TAGS: \n#+CREATED: %U
#+LATEX_CLASS: article
#+LATEX_COMPILER: xelatex

#+LATEX: \\clearpage\n"
           :unnarrowed t)
          ("b" "non-STEM book note" plain (function org-roam-capture--get-point) "%?"
           :file-name "${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: 《${title}》笔记\n#+ROAM_ALIAS: ${slug}\n#+STARTUP: overview\n#+ROAM_TAGS:\n#+ROAM_KEY: ${slug}
#+CREATED: %U\n#+OPTIONS: toc:nil email:t f:t
#+LATEX_COMPILER: xelatex\n#+LATEX_CLASS: article\n\n#+LATEX: \\tableofcontents\n#+LATEX: \\clearpage\n
* Summary\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n
* Appendix\n#+LATEX: \\appendix\n** Notes\n"
           :unnarrowed t)
          ("s" "Beamer seminar slides" plain (function org-roam-capture--get-point) "%?"
           :file-name "slides/${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: ${title}\n#+SHORT_TITLE: ${title}\n#+AUTHOR: Min-Ye Zhang\n#+EMAIL: stevezhang@pku.edu.cn
#+STARTUP: overivew beamer\n#+ROAM_TAGS: Slides
#+LATEX_CLASS: beamer
# #+LATEX_CLASS: beamerarticle
# ==============================
#+OPTIONS: H:3
#+LATEX_HEADER: \\usepackage[maxnames=3,style=nature,date=year,url=false,isbn=false,doi=false,articletitle=false]{biblatex}
#+LATEX_HEADER: \\addbibresource{../bibliography.bib}
#+BEAMER_THEME: CambridgeUSzmy
#+BEAMER_HEADER: \\institute[CCME, PKU]{College of Chemistry and Molecular Engineering\\\\ Peking University}
#+BEAMER_HEADER: \\date[\\today]{@Founder 312, \\today}
#+BEAMER_HEADER: \\renewcommand{\\titleheader}{\\itshape TMC Group Seminar}
#+LATEX_COMPILER: xelatex
#+CREATED: %U

* Acknowledgement
:PROPERTIES:
:BEAMER_ENV: appendix
:UNNUMBERED: t
:END:
#+BEAMER: \\begin{frame}{Acknowledgement}{}
#+BEAMER: \\LARGE Thank you for listening!
#+BEAMER: \\end{frame}
** LaTex acknowledgement                                               :ignore:
:PROPERTIES:
:BEAMER_ENV: ignoreheading
:END:
I appreciate anyone who reads this handout.
Suggestions are totally welcome.

# * Resources # optional section
# :PROPERTIES:
# :BEAMER_ENV: appendix
# :UNNUMBERED: t
# :END:
# #+BEAMER: \\begin{frame}[allowframebreaks]{Resources}
# - content here as items
# #+BEAMER: \\end{frame}

* References
:PROPERTIES:
:BEAMER_ENV: appendix
:UNNUMBERED: t
:END:
#+BEAMER: \\begin{frame}[allowframebreaks]{References}
#+LaTeX: \\printbibliography[heading=none]
#+BEAMER: \\end{frame}"
           :unnarrowed t)
          ("p" "research project" plain (function org-roam-capture--get-point) "%?"
           :file-name "${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: ${title}\n#+ROAM_ALIAS: ${slug}
#+AUTHOR: Min-Ye Zhang
#+EMAIL: stevezhang@pku.edu.cn
#+CREATED: %U
#+STARTUP: overview
#+ROAM_TAGS: Research
#+LATEX_HEADER: \\usepackage[maxnames=3,style=nature,date=year,url=false,isbn=false,doi=false,articletitle=false]{biblatex}
#+LATEX_HEADER: \\addbibresource{../bibliography.bib}
#+OPTIONS: email:t
#+LATEX_COMPILER: xelatex\n#+LATEX_CLASS: article\n
#+LATEX: \\clearpage\n
* Aims
* Literature Review
* Proposal
* Data
* References
:PROPERTIES:
:UNNUMBERED: t
:END:
#+LATEX: \\printbibliography[heading=none]
* Changelog
:PROPERTIES:
:UNNUMBERED: t
:END:
- %(format-time-string \"[%Y-%m-%d %a %H:%M]\") : initial file"
           :unnarrowed t)
          ("t" "language thesaurus" plain (function org-roam-capture--get-point) "%?"
           :file-name "${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: ${title}\n#+STARTUP: overview\n#+ROAM_TAGS: Therausus\n#+CREATED: %U\n
* Definition\n* Examples\n* Sources"
           :unnarrowed t)
          ("m" "math phys book" plain (function org-roam-capture--get-point) "%?"
           :file-name "${slug}"
           :head "# -*- truncate-lines: t -*-
#+TITLE: ${title}\n#+AUTHOR: Min-Ye Zhang\n#+EMAIL: stevezhang@pku.edu.cn
#+STARTUP: overview\n#+ROAM_TAGS: Study\n#+CREATED: %U
#+LATEX_CLASS: book\n#+LATEX_COMPILER: xelatex\n"
           :unnarrowed t)
    	 )
        org-roam-capture-ref-templates
        '(("r" "ref" plain
           (function org-roam-capture--get-point)
           ""
           :file-name "{slug}"
           :head "# -*- truncate-lines: t -*-\n#+TITLE: ${title}\n#+STARTUP: content\n#+ROAM_KEY: ${ref}\n#+CREATED: %U\n\n* Notes\n\n"
           :unnarrowed t))
    )
  (setq org-roam-graph-exclude-matcher '("journal"
                                         "slides"
                                         "org-agenda.org"
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
  (defun org-init-table-gnuplot()
    "Create a 2-column table and gnuplot source block for exporting data plot to images directory, used for quick check only"
    (interactive)
    (let* 
      (
       (title (format "%s" (read-string "Enter data title: ")))
      )
         (insert (format "#+NAME: tab-%s\n" title))
         (insert (format "#+CAPTION: %s\n" title))
	 (insert "#+ATTR_LATEX: :booktabs t\n")
	 (insert "| | |\n")
         (insert (format "#+NAME: gnuplot-%s\n" title))
	 (insert (format "#+HEADER: :var data=tab-%s\n" title ))
	 (insert (format "#+HEADER: :exports results :file images/%s.png\n" title))
	 (insert "#+BEGIN_SRC gnuplot\n")
	 (insert (format "set title \"%s\"\n" title))
	 (insert "#+END_SRC\n")
         (insert (format "#+NAME: fig-%s\n" title))
         (insert (format "#+CAPTION: %s\n" title))
	 (insert "#+ATTR_ORG: :width 400\n")
	 (insert "#+ATTR_LATEX: :width 0.6\\linewidth\n")
	 (insert (format "#+RESULTS: gnuplot-%s\n" title))
    ))
)
;

;(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'org-mode-hook 'org-roam-mode)
;(add-hook 'after-init-hook 'blink-cursor-mode)
(add-hook 'org-mode-hook 'org-cdlatex-mode)
(add-hook 'org-mode-hook 'reftex-mode)
;(add-hook 'org-mode-hook 'org-fragtog-mode) ; render latex block

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
	org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 24)
)
;(org-roam-server-mode)

; Chinese input setting
(use-package! pangu-spacing
  :config
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
           ;      ((string= todo "INPROGRESS")(and todo (format "{\\color{ProcessBlue}\\bfseries\\sffamily %s} " todo)))
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
% compact itemize by paralist packages
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
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]
"
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
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]
"
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
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
[EXTRA]
[PACKAGES]
"
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
                ("aps"
                 "\\documentclass[aps,prbtwocolumn,a4paper,floatfix,showpacs]{revtex4-1}
\\bibliographystyle{apsrev4-1}
[DEFAULT-PACKAGES]
\\usepackage[version=3]{mhchem}
\\usepackage{times,mathptmx}
\\usepackage{array}
\\usepackage{epstopdf}
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
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
\\usepackage{physics}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
% alias color in header
\\colorlet{RED}{Red}
\\colorlet{GREEN}{Green}
\\colorlet{PROCESSBLUE}{ProcessBlue}
\\colorlet{MAGENTA}{Magenta}
\\colorlet{ORANGE}{Orange}
% compact itemize by paralist packages
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum
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
% compact itemize by paralist packages
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
      ("font={small,it}"     "caption"   nil)
      (""     "paralist"   nil)
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
)
;; see
;; https://github.com/org-roam/org-roam-bibtex#org-roam-bibtex---bibtex-aware-capture-template-expansion
;; for more
(use-package org-roam-bibtex ;; shortened as ORB
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
    '(("citekey" . "=key=")  "title" "author-or-editor" "date" "doi" "file" ("journal" . "journaltitle")  "volume" "pages")
      orb-note-actions-frontend 'ivy
        orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point) ""
           :file-name "${citekey}"
           :head 
           "# -*- truncate-lines: t -*-
#+TITLE: ${citekey}
#+ROAM_KEY: ${ref}
#+STARTUP: content
#+CREATED: %U
:PROPERTIES:
:TITLE: ${title}
:AUTHOR: ${author-or-editor}
:JOURNAL: ${journal}
:DATE: ${date}
:VOLUME: ${volume}
:PAGES: ${pages}
:DOI: [[${doi}]]
:END:

- tags ::
- keywords ::

* Summary

* Notes :noter:
:PROPERTIES:
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:END:"
)))
  (setq bibtex-completion-display-formats
   '((article       . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${journaltitle:20} | ${keywords:*}")
     (inbook        . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| Chapter ${chapter:12} | ${keywords:*}")
     (book          . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${publisher:20} | ${keywords:*}")
     (t             . "${=key=:16} | ${author:24} | ${title:40} |${year:4}| ${keywords:*}"))
        bibtex-completion-additional-search-fields '(keywords journaltitle publisher)
  )
  (advice-add 'bibtex-completion-candidates :filter-return 'reverse)
  ; anystyle-related
  (setq orb-autokey-format "%A*[1]%y*"
        orb-pdf-scrapper-export-fields '("author" "journal" "date" "volume" "pages" "title"))
)
;; add executable for running anystyle
;;(add-to-list 'exec-path (concat (getenv "HOME") "/.rvm/rubies/ruby-2.6.5/bin"))

(use-package helm-bibtex
  :config
  (when (featurep! :completion ivy)
    (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))
  (setq
   bibtex-completion-notes-path (format "%s" org_notes)
   bibtex-completion-bibliography (list bibfile)
   ;bibtex-completion-bibliography bibfile
   bibtex-completion-pdf-field "file"
   ;bibtex-completion-additional-search-fields '(keywords, journaltitle)
   bibtex-completion-pdf-open-function 'org-open-file
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
     ;org-ref-default-bibliography bibfile
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

;(use-package org-zotxt
;  :hook
;  (org-mode . org-zotxt-mode))

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

(use-package org-journal
  :after org
  :ensure t
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
        org-journal-file-type 'weekly
        org-journal-dir (concat org_notes "/journal")
        org-journal-file-format "%Y-%m-%d.org"
    )
  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything\n")
      (`weekly (concat "#+TITLE: Weekly Journal " (format-time-string "%G W%V") "\n#+STARTUP: content nologdone\n#+OPTIONS: toc:nil\n#+LATEX_CLASS: journal\n#+LATEX_COMPILER: xelatex\n"))
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded\n#+OPTIONS: toc:nil\n#+LATEX_CLASS: journal\n#+LATEX_COMPILER: xelatex\n")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded\n"))
    "#+TAGS: WORK(w) BOOK(b) SPORTS(s) CONTACTS(c)\n"
    "#+TAGS: FAMILY(f) MUSIC(m)\n"
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
;
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

; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                ))
  :config
  (org-super-agenda-mode)
)

(use-package! org-roam-dashboard
  :after org-roam
  :bind
  (("<f12>" . org-roam-dashboard))
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

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
;(defun my/org-agenda-time-grid-spacing ()
;  "Set different line spacing w.r.t. time duration."
;  (save-excursion
;    (let* ((background (alist-get 'background-mode (frame-parameters)))
;           (background-dark-p (string= background "dark"))
;           (colors (list "#ff9933"  "#9f7efe" "#0098dd" "#8c1400" "#50a14f"))
;           pos
;           duration)
;      (nconc colors colors)
;      (goto-char (point-min))
;      (while (setq pos (next-single-property-change (point) 'duration))
;        (goto-char pos)
;        (when (and (not (equal pos (point-at-eol)))
;                   (setq duration (org-get-at-bol 'duration)))
;          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
;                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;            (overlay-put ov 'face `(:background ,(car colors)
;                                                :foreground
;                                                ,(if background-dark-p "white" "black")))
;            (setq colors (cdr colors))
;            (overlay-put ov 'line-height line-height)
;            (overlay-put ov 'line-spacing (1- line-height))))))))
;
;(add-hook 'org-agenda-finalize-hook #'my/org-agenda-time-grid-spacing)

;(use-package org-download
  ;:hook
  ;(org-mode . org-download-enable)
  ;(dired-mode . org-download-enable)
  ;:config
  ;(setq-default org-download-image-dir (format "%s/images" org_notes))
  ;(setq org-download-timestamp ""
        ;org-download-heading-lvl nil
        ;org-download-display-inline-images nil
        ;org-download-screenshot-method "xclip"
        ;)
;)

;(use-package! writegood-mode
;  :after org
;  :hook
;  (org-mode . writegood-mode)
;)

;(use-package jieba
;  :load-path "~/.doom.d/jieba"
;  :commands (jieba-mode jieba-ensure jieba-mode-map)
;  :hook 
;  (
;   (after-init . jieba-mode)
;  )
;  :init
;  (map! :map jieba-mode-map
;        :nv "M-w" #'jieba-forward-word
;  ;      :nv "b" #'jieba-backward-word
;        )
;  :config
;  (add-hook 'org-mode-hook 'jieba-ensure)
;)

;(use-package asymbol
;  :after cdlatex
;  :init
;  (setq asymbol-help-symbol-linewidth 110
;        asymbol-help-tag-linewidth 110)
;  ;; add keybindings
;  (asymbol-global-input-unicode-symbol-on)
;  (asymbol-latex-input-symbol-on)
;  (asymbol-org-input-symbol-on)
;  :config
;  (define-key cdlatex-mode-map (vector asymbol-trigger-key) 'asymbol-insert-text-or-symbol)
;  (add-hook 'org-cdlatex-mode-hook
;            (lambda () (interactive)
;            (define-key org-cdlatex-mode-map (vector asymbol-trigger-key) 'asymbol-insert-text-or-symbol)))
;)

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
 '(tool-bar-mode nil))
 ;'(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:box 3 :weight bold :height 1.4))))
 '(org-meta-line ((t (:slant italic))))
 '(org-quote ((t (:slant normal :family "STKaiti")))))

;;; make side by side buffers function the same as the main window
;(setq truncate-partial-width-windows nil)
;; do not wrap lines for long sentence
;; truncate it by myself
;;(setq-default global-visual-line-mode t)
;(setq-default truncate-lines t)
