;;; configurations of packages for latex writing, including
;;;   - reftex
;;;   - asymbol
;;;   - bibtex (helm-bibtex)
;;;   - org-ref

(use-package! reftex
  :config
  (setq reftex-cite-format 'biblatex)
)
(after! org
  :config
  (add-hook 'org-mode-hook 'reftex-mode)
)

(use-package! helm-bibtex
  :config
  ;(when (featurep! :completion ivy)
  ;  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))
  (setq
   bibtex-completion-notes-path (format "%s" mz/org-notes)
   bibtex-completion-bibliography (list mz/bibfile)
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

(use-package! asymbol
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

(use-package! org-ref
    :config
    (setq
     ;org-ref-completion-library 'org-ref-ivy-cite
     org-ref-completion-library 'org-ref-helm-cite
     ;; uses the helm-bibtex settings to find the pdf
     org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
     ;; location to file bibliography
     org-ref-default-bibliography (list mz/bibfile)
     org-ref-notes-directory (format "%s" mz/org-notes)
    )
 ;   (add-to-list 'org-ref-helm-user-candidates
 ;                '("Open notes and PDF file" . (lambda ()
 ;                  (save-excursion
 ;                  (org-ref-open-notes-at-point)
 ;                  (org-ref-open-pdf-function)
 ;                  )))
 ;   )
)

(provide 'config-latex)
;;; config-latex.el ends here
