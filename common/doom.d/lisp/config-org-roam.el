;;; configuration for org-roam and related packages
;;; related packages are:
;;;   - org-roam-bibtex
;;;   - delve
;;;   - org-roam-ui

(use-package! org-roam
  :init
  (setq org-roam-directory (format "%s" mz/org-notes))
  ;(setq org-roam-completion-system 'helm)
  (setq org-roam-graph-executable mz/org-roam-graph-executable)
  (setq org-roam-graph-viewer mz/org-roam-graph-viewer
        org-roam-index-file "index.org"
        org-roam-graph-extra-config '(("overlap" . "false")) ; man dot for attributes setup
        )
  (map! :leader
        :prefix "r"
        :desc "Org-Roam-Buffer"         "R" #'org-roam-buffer-toggle
        :desc "Org-Roam-Buffer"         "r" #'org-roam-find-ref
        :desc "Org-Roam-Insert"         "i" #'org-roam-node-insert
        :desc "Org-Roam-Find"           "." #'org-roam-node-find
        :desc "Org-Roam-Store"          "l" #'org-roam-store-link
        :desc "Org-Roam-Alias-Add"      "a" #'org-roam-alias-add
        :desc "Org-Roam-Unlinked-Refs"  "u" #'org-roam-unlinked-references
        :desc "Orb-Note-Actions"        "n" #'orb-note-actions
        )
  ; (setq org-roam-v2-ack t) ;; remove V2 warnings after clean upgrade from V1
  :bind
  (:map org-mode-map
        (;("C-c r R" . org-roam)
         ;("C-c r f" . org-roam-find-file)
         ("C-c r R" . org-roam-buffer-toggle)
         ("C-c r ." . org-roam-node-find)
         ("C-c r L" . org-roam-store-link)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r u" . org-roam-unlinked-references)
         ("C-c r r" . org-roam-find-ref)
         ("C-c r d" . org-roam-find-directory)
         ("C-c r j" . org-roam-jump-to-index)
         ("C-c r b" . org-roam-switch-to-buffer)
         ("C-c r n" . orb-note-actions)
         ;; ("C-c r g" . org-roam-graph)
         ;;("C-c r i" . org-roam-insert)
         ("C-c r i" . org-roam-node-insert)
         )
  )
;; source: https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
  :config
  ;; from https://org-roam.discourse.group/t/increasing-the-text-size-of-the-side-panel/2053/3
  (add-to-list 'display-buffer-alist
                '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer)))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ; (org-roam-setup)
  ;(org-roam-mode +1) ; set to major mode, dropped in V2
  (defun org-roam-search-dup-ids ()
    (let ((org-id-files (org-roam--list-files org-roam-directory))
      org-agenda-files)
    (org-id-update-id-locations))
  )
  (setq org-roam-rename-file-on-title-change nil)
  ;;; turn off daily template. Use org-journal
  ;(setq org-roam-dailies-capture-templates
  ;      '(("d" "default" entry "%?"
  ;         :if-new (file+head
  ;         "%<%Y-%m-%d>.org"
  ;         "#+title: %<%Y-%m-%d>\n#+options: title:nil toc:nil\n#+filetags: :Daily:"
  ;         )
  ;         :unnarrowed t))
  ;)
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
#+filetags: Unresolved\n
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
           "bookrev/${slug}.org"
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
#+filetags: Research
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
          ("T" "translation" plain "%?"
           :if-new (file+head
           "translate/${slug}"
           "#+title: ${title}
#+filetags: Translation\n#+created: %U

Source:")
           :unnarrowed t)
    ))
  (setq org-roam-graph-exclude-matcher '("journal"
                                         "daily"
                                         "slides"
                                         "org-agenda.org"
                                         "archive.org"
                                         "read.org"
                                         "work.org"
                                        )) ;;; not respected by org-roam-ui
;  (require 'org-roam-protocol) ; use org-protocol
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
;; instead run org-mode-setup before the first build of database

;;; old version delve, comment out
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

;; see
;; https://github.com/org-roam/org-roam-bibtex#org-roam-bibtex---bibtex-aware-capture-template-expansion
;; for more
(use-package! org-roam-bibtex ;; shortened as ORB
  :after org-roam
  :config
  (require 'org-ref)
  (setq org-ref-notes-function 'orb-edit-notes)
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
           "ref/note-${citekey}.org"
           ":PROPERTIES:
:TITLE: ${title}
:AUTHOR: ${author-or-editor}
:JOURNAL: ${journaltitle}
:DATE: ${date}
:VOLUME: ${volume}
:PAGES: ${pages}
:DOI: [[doi:%(replace-regexp-in-string \" \" \"\" \"${doi}\")]]
:END:
#+title: ${citekey}: ${title}
#+startup: content
#+created: %U

* Summary Notes
* Noter Notes :noter:
:PROPERTIES:
:NOTER_DOCUMENT: ${file}
:END:"
           )
           :unnarrowed t))
)

(after! org-roam (org-roam-bibtex-mode))

;; add executable for running anystyle
;;(add-to-list 'exec-path (concat (getenv "HOME") "/.rvm/rubies/ruby-2.6.5/bin"))

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
          org-roam-ui-open-on-start t)
    (setq org-roam-ui-ref-title-template "%^{citekey} %^{title}"))


(provide 'config-org-roam)
;;; config-org-roam.el ends here
