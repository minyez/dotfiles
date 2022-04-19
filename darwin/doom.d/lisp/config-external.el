;;; configurations for external packages which are not controled by packages.el
;;; or init.el and downloaded at the doomdir (doom-private-dir).
;;;
;;; the followings are available
;;;   - qe-modes
;;;   - jieba
;;;   - auto-save

;; QE emacs mode
(use-package! qe-modes
  ;;; load-path set in the main config file
  ;;  automatically open the the pw.*.in, scf.*.in, nscf.*.in, relax.*.in,
  ;;  vc-relax.*.in, md.*.in, vc-md.*.in files by pw.x mode
  :load-path "../qe-modes"
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

;;(use-package jieba
;;  :load-path "../jieba"
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

;;; auto-save by lazycat
; (use-package! auto-save
;  :load-path "../auto-save"
;  :config
;  (auto-save-enable)
;  (setq auto-save-silent t)
; )

(provide 'config-external)
;;; config-external.el ends here
