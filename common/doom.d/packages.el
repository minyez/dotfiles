;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

(package! rg)
(package! yasnippet-snippets)
(package! jsonrpc) ; to use jieba
(package! move-text)
(package! good-scroll)
;(package! writegood-mode)
;(package! zenburn-theme)

;; org-related
;(package! org-journal)
;; pin org-ref version to a V2
;; when having void-variable doi-link-follow error with new Doom
;; try https://github.com/jkitchin/org-ref/issues/906 and related PR
(package! org-ref
  :recipe (:host github :repo "jkitchin/org-ref" :branch "org-ref-2")
  :pin "37b64e6cc1")
(package! org-super-agenda)
(package! dwim-shell-command
  :recipe (:host github :repo "xenodium/dwim-shell-command"))
(package! org-superstar)
(package! org-fancy-priorities)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(unpin! org)
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;(package! org-roam-server
;  :recipe (:host github :repo "org-roam/org-roam-server"))

(package! command-log-mode
  :recipe (:host github :repo "lewang/command-log-mode"))

(package! org-roam-dashboard
  :recipe (:host github :repo "publicimageltd/org-roam-dashboard"))
(package! org-download)
(package! org-fragtog)
(package! company-org-roam)

(package! org-drill
  :recipe (:host gitlab :repo "phillord/org-drill"))

(package! delve
  :recipe (:host github :repo "publicimageltd/delve" :branch "main"))
; Bibliography
(package! org-roam-bibtex)
(unpin! bibtex-completion helm-bibtex ivy-bibtex)
;; PDF-related tools
(package! pdf-tools)
;(package! org-pdftools)
;(package! interleave
;  :recipe (:host github :repo "rudolfochrist/interleave"))
;(package! golden-ratio
;  :recipe (:host github :repo "roman/golden-ratio.el"))
(package! org-noter)
;(package! org-noter-pdftools)
;; When using org-roam via the `+roam` flag
;(unpin! org-roam company-org-roam)

(package! asymbol
  :recipe (:host github :repo "dwuggh/asymbol"))
(package! company-jedi)

;; window-numbering
;; https://github.com/nschum/window-numbering.el
;; suggested by
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org#%E6%8C%81%E7%BB%AD%E6%94%B9%E8%BF%9B
(package! window-numbering
  :recipe (:host github :repo "nschum/window-numbering.el"
           :files ("window-numbering.el")))

(package! dtrt-indent 
  :recipe (:host github :repo "jscheid/dtrt-indent"))

;; https://github.com/purcell/exec-path-from-shell
;; to handle exec-path in OS X
(package! exec-path-from-shell)

(package! pangu-spacing)
;; pyim+liberime for Chinese input
(package! pyim
  :recipe (:host github :repo "tumashu/pyim"))
(unpin! pyim)
(package! liberime
  :recipe (:host github :repo "merrickluo/liberime"
           :files ("Makefile" "src" "liberime.el")))
;; dict
(package! pyim-basedict
  :recipe (:host github :repo "tumashu/pyim-basedict"
           :files ("pyim-basedict.el" "pyim-basedict.pyim")))
(package! pyim-tsinghua-dict
  :recipe (:host github :repo "redguardtoo/pyim-tsinghua-dict"
           :files ("*.el" "*.pyim")))
;; call dictionary.app on macos
(package! osx-dictionary)

;; xenops for latex live view of equations, tikz, tables and figures
(package! xenops)
;  :recipe (:host github :repo "dandavison/xenops"))
;; org-latex-impatient for live preview of latex snippet in org-mode
(package! org-latex-impatient
  :recipe (:host github :repo "yangsheng6810/org-latex-impatient"))

;; keyfreq
(package! keyfreq
  :recipe (:host github :repo "dacap/keyfreq"))

;; evil-leader to emulate leader in vim
(package! evil-leader
  :recipe (:host github :repo "cofi/evil-leader"))

;; xclip for accessing X11 clipboard
(package! xclip)

;; code-related
;(package! pyenv
;  :recipe (:host github :repo " aiguofer/pyenv.el" :branch "master"))

;(package! company-jedi
;  :recipe (:host github :repo " emacsorphanage/company-jedi"))

;(package! zotxt)
;; equation OCR using Mathpix API
;(package! mathpix
;  :recipe (:host github :repo "jethrokuan/mathpix.el"))
;(package! markdown-mode
;	:recipe (:host github :repo "jrblevin/markdown-mode"))
;(package! impatient-mode)
;(package! simple-httpd)

(package! magic-latex-buffer)
(package! yasnippet
  :recipe (:host github :repo "joaotavora/yasnippet"))

(package! cdlatex)

(package! projectile
  :recipe (:host github :repo "bbatsov/projectile"))
(unpin! projectile)


(package! direnv
  :recipe (:host github :repo "wbolster/emacs-direnv"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
