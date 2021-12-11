;;; configurations for language-service-provider and on-the-fly syntax checker

(use-package! flycheck
  :config
  ; only use pylint, disable others
  ; setq-default acts as a global operation
  (setq-default flycheck-disabled-checkers '(
                python-flake8 python-pycompile python-pyright python-mypy
                )
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

(provide 'config-lsp)
;;; config-lsp.el ends here
