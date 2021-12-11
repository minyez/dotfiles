;;; configurations for pdf related packages, including
;;;   - pdf-view
;;;   - pdf-tools
;;;   - org-noter
;;;   - org-pdftools

;(use-package org-noter-pdftools
;  :after org-noter
;  :config
;  (with-eval-after-load 'pdf-annot
;    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! pdf-tools
  :config
  ;; activate when opening pdf, otherwise the evil keybindings will not work
  (pdf-loader-install)
)

(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package! pdf-view
  :config
  (map! :map pdf-view-mode-map
        :nv "z g" #'pdf-view-goto-page
        :nv "z r" #'image-rotate       ;; rotate the page (defined in image.el)
  ;; evil leader helper key for annotation
        :nv (concat mz/evil-leader " a l")     #'pdf-annot-list-annotations
        :nv (concat mz/evil-leader " a h")     #'pdf-annot-add-highlight-markup-annotation
        :nv (concat mz/evil-leader " a u")     #'pdf-annot-add-underline-markup-annotation
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
   org-noter-notes-search-path (list mz/org-notes)
   ;org-noter-set-notes-window-behavior 'scroll
   ;; split fraction. default (0.5 . 0.5). slightly larger on vertical
   ;; TODO may adapt according to the size of display
   org-noter-doc-split-fraction '(0.58 . 0.5)
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

;; switch off evil mode in pdf viewer
;; not work :(
;(add-hook 'org-mode-hook (unless (eq major-mode 'pdf-view-mode) (evil-mode)))

(provide 'config-pdf)
;;; config-pdf.el ends here
