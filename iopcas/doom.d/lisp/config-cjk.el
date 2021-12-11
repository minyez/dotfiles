;;; configurations for modules related to input CJK characters, including
;;;   - liberime
;;;   - pyim
;;;   - pyim-liberime
;;;   - +pyim-base-dict+
;;;   - pyim-tsinghua-dict
;;;   - pangu-spacing
;;;

;; pyim with librime
;; https://gist.github.com/merrickluo/553f39c131d0eb717cd59f72c9d4b60d
(use-package! liberime
  :config
  ; use rime user data of ibus-rime on my Linux
  (setq liberime-user-data-dir "~/.config/ibus/rime")
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
  ;; 设置选词框的绘制方式
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

(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)
  (setq-hook! 'markdown-mode-hook pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp "\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?1:\\cC\\|\\cH\\|\\cK\\)\\)\\(?2:[\(=0-9A-Za-z\\$\\]\\)\\|\\(?1:[=0-9A-Za-z\\$\)]\\)\\(?:\\(?3:[、。「」！（），：；？]\\)\\|\\(?2:\\cC\\|\\cH\\|\\cK\\)\\)")
)

(provide 'config-cjk)
;;; config-cjk.el ends here
