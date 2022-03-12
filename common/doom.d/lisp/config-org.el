;;; configurations for org-mode and related modules, including
;;;   - org-superstar
;;;   - org-journal

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (format "%s" mz/org-notes))

(use-package! org
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode)
   ;;; for automatic new line when typing beyond buffer width
   ;;; 70 is appropriate for a two-column view on 15' desktop
   (org-mode . auto-fill-mode)
   (org-mode . (lambda () (setq fill-column 70)))
   ;(org-mode . org-cdlatex-mode) ;; hooked with :lang (latex +cdlatex) in init.el
   ;(before-save . zp/org-set-last-modified)
   (before-save . org-update-all-dblocks)        ; update all dynamic table before saving
  )
  ;:config
  ;(fset 'make-bold
  ; (kmacro-lambda-form [?\C-c ?\C-x ?\C-f ?*] 0 "%d"))
  :bind
  (:map org-mode-map
        ("C-c l" . org-insert-link)
        ("C-c i" . org-insert-image)
        ("C-c C-i" . org-time-stamp-inactive)
        ("C-c e v" . (lambda () "make verbatim"
                       (interactive) (org-emphasize 61))) ; =
        ("C-c e b" . (lambda () "make bold"
                       (interactive) (org-emphasize 42))) ; *
        ("C-c e s" . (lambda () "make strike-through"
                       (interactive) (org-emphasize 43))) ; +
        ("C-c e i" . (lambda () "make italic"
                       (interactive) (org-emphasize 47))) ; /
        ("C-c e u" . (lambda () "make underline"
                       (interactive) (org-emphasize 95))) ; _
        ("C-c e c" . (lambda () "make code"
                       (interactive) (org-emphasize 126))) ; ~
      )
  ;("s-b" . make-bold)
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
      ;; emulate evil-leader key, but only in the org-mode
        :nv (concat mz/evil-leader " ]")     #'org-forward-heading-same-level
        :nv (concat mz/evil-leader " [")     #'org-backward-heading-same-level
        :nv (concat mz/evil-leader " n")     #'org-narrow-to-subtree
        :nv (concat mz/evil-leader " N")     #'widen
        )
  ; show the whole markup
  (setq org-link-descriptive nil)
  (setq org-archive-location (concat mz/org-notes "/archive.org::* From %s"))
  (setq org-default-notes-file (concat mz/org-notes "/todos.org::* Inbox"))
  ; match target and named construct
  (setq org-link-search-must-match-exact-headline nil)
  ; for org-capture purpose
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
  (setq org-agenda-files (concat mz/org-notes "/org-agenda.org")
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
  ; each state with ! is recorded as state change
  ; for org-habit, it seems that DONE logging is enabled automatically
  ; add ! after d will make org-habit log twice
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(i)" "WAITING(w)" "NEEDREVIEW(r!)" "|" "DONE(d)" "CANCELLED(c!@)"))
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
    "Insert PNG image from the clipboard to the buffer by using =pngpaste= (macos) or =xclip= (linux)

The image will be created under 'images' directory in =org-directory=
with the name from user input. If image with the same name exists, the paste
will be stopped, but the link will still be created.
Note that =pngpaste=/=xclip= should be installed outside Emacs"
    (interactive)
    (let* 
      (
       (cpcmd (pcase system-type
          ('darwin "pngpaste %s")
          ('gnu/linux "xclip -selection clipboard -t image/png -o > %s")
          ))
       (path (concat mz/org-notes "/images/"))
       (fn (format "%s" (read-string "Enter image name (w/o png):")))
		   (image-file (concat path fn ".png"))
      )
	    (if (not (file-exists-p path)) (mkdir path))
	    (if (file-exists-p image-file)
		(message (format "Warning: found image %s.png in %s" fn path))
                (if cpcmd (shell-command (format cpcmd image-file))
		              (message "Warning: clipboard -> file not suppored on this OS")
                  ))
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
  ;; state commented out for org-habit
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
  ;; exclude the org-noter tag from inheriting to notes within
  (add-to-list 'org-tags-exclude-from-inheritance "noter")
  (add-to-list 'org-tags-exclude-from-inheritance "Reference")
  (add-to-list 'org-tags-exclude-from-inheritance "Book")
  ; change default to dvisvgm on linux,
  ; according to https://emacs-china.org/t/org-latex-fragments-preview/16400/5
  ; imagemagick/dvipng create a fullwidth picture for both inline and display math
  ; however, this was not the case on macos.
  (with-system 'gnu/linux
    (setq org-preview-latex-default-process 'dvisvgm)
  )
  (add-to-list 'org-link-abbrev-alist
               '("arxiv" . "https://arxiv.org/abs/%s"))
  (add-to-list 'org-link-abbrev-alist
               '("w2kmail" . "http://www.mail-archive.com/wien\%40zeus.theochem.tuwien.ac.at/msg%s.html"))
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
      ("font={small},skip=1pt"     "caption"   nil)
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

(use-package! org-archive
  :after org
  :config
  (setq org-archive-mark-done t) ; change subtree state to DONE when archived
)

(use-package! org-habit)

(use-package! org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⋆" "◉" "○" "†" "‡" "∵")) ;✸ ✿ ✭ ❡
)

(use-package! org-journal
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
        org-journal-file-type 'monthly
        org-journal-dir (concat mz/org-notes "/journal")
        ;org-journal-file-format "%Y-%m-%d.org" ; daily
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
    "#+tags: FAMILY(f) MUSIC(m)\n"
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
;  ;(setq-default org-download-image-dir (format "%s/images" mz/org-notes))
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

; spaced repitition for memorizing things
(use-package! org-drill
  :config
  ;(setq org-drill-scope
  ;    '("index-english_slang_phrases.org"
  ;      "index-english_vocabulary.org"))
  (setq org-drill-scope 'directory)
  (setq org-drill-spaced-repetition-algorithm 'simple8)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (setq org-drill-add-random-noise-to-intervals-p t)
)

(provide 'config-org)
;;; config-org.el ends here
