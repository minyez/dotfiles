;;; configuration for theme
;;; - modus-operandi/modus-vivendi

;;; follow built-in version of
;;; https://protesilaos.com/emacs/modus-themes#h:e979734c-a9e1-4373-9365-0f2cd36107b8
(use-package emacs
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-paren-match '(bold intense)
        modus-themes-links '(neutral-underline background)
        modus-themes-deuteranopia t
        modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9))
        )
  :bind ("<f5>" . modus-themes-toggle)
  :config
  ;; Load the theme of your choice:
  ;; let doom load it
  ;; (load-theme 'modus-operandi) ;; OR (load-theme 'modus-vivendi)
)

(provide 'config-theme)
