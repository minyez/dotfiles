;;; configurations about org export (ox-), currently including
;;;   - beamer
;;;   - latex

;;; beamer export
(use-package! ox-beamer
  :bind
  ("C-c x b" . org-beamer-export-to-latex)
  ("C-c x O" . org-beamer-export-to-pdf)
  :config
  ; default 3rd level heading as frame.
  ; 1st and 2nd are sec and subsec
  (setq org-beamer-frame-level 3)
)

(setq mz/org-latex-classes-common-header-passoptions
      "\\PassOptionsToPackage{usenames,dvipsnames}{xcolor}\n\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Rhodamine,pdfborder={0 0 0},breaklinks=true,linktoc=all}{hyperref}"
      mz/org-latex-classes-common-header-after-default-pkgs
      "% blockquote from eisvogel.
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
\\usepackage{paralist}
\\let\\itemize\\compactitem
\\let\\description\\compactdesc
\\let\\enumerate\\compactenum"
      )

;;; latex export
; https://superuser.com/questions/896741/how-do-i-configure-org-latex-classes-in-emacs
; https://stackoverflow.com/questions/36197545/org-mode-latex-export-making-todos-red
(use-package! ox-latex
  :bind
  ("C-c x l" . org-latex-export-to-latex)
  ("C-c x o" . org-latex-export-to-pdf)
  :config
  ;(fset 'my-org-latex-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?o] 0 "%d"))
  ;(fset 'my-org-beamer-export-open-pdf
  ; (kmacro-lambda-form [?\C-c ?\C-e ?l ?O] 0 "%d"))
  (setq org-latex-pdf-process '("latexmk -latexoption=\"-interaction=nonstopmode -shell-escape\" -pdf -pdflatex=%latex -bibtex -f %f"))
  ;; prefer custom label
  (setq org-latex-prefer-user-labels t)
  ;;; remove default hyperset with author names included
  ;;; for local variable setup, use for each file
  ;;; # -*- org-latex-hyperref-template: nil; -*-
  ; (setq org-latex-hyperref-template nil)
  ;; user-defined headline to-do rendering
  (defun org-latex-format-headline-colored-keywords-function (todo todo-type priority text tags info)
        (concat
           ;(cond 
           ;      ; colorize to-do items
           ;      ; comment whole cond to avoid showing to-do markers
           ;      ((string= todo "TODO")(and todo (format "{\\color{Red}\\bfseries\\sffamily %s} " todo)))
           ;      ((string= todo "WIP")(and todo (format "{\\color{ProcessBlue}\\bfseries\\sffamily %s} " todo)))
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
               `(
  ; general English note
                ("article"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper]{article}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n" mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; minimal working example
                ("mwe"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper]{article}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "\\usepackage{mwe}\n\\usepackage{amsmath,amssymb}\n"
                           "[NO-DEFAULT-PACKAGES]\n[EXTRA]\n[NO-PACKAGES]"
                           )
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; General Chinese note
                ("ctexart"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper,scheme=plain]{ctexart}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n" mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; For org-journal. section for day, subsec for time
                ("journal"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper,scheme=plain]{ctexart}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n" mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ;("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ;("\\subparagraph{%s}" . "\\subparagraph*{%s}")
				 )
  ; Note for math. phys. books
                ("book"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper,titlepage]{book}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n\\usepackage{physics}\n\\usepackage{cases}\n"
                           mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("report"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt]{report}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n" mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("ctexbook"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper,titlepage]{ctexbook}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n\\usepackage{physics}\n\\usepackage{cases}\n"
                           mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\chapter{%s}" . "\\chaper*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; General Engilish scientific writing
                ("sci"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[a4paper,11pt]{article}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "[DEFAULT-PACKAGES]\n"
                           "\\usepackage{threeparttable}\n"
                           "\\usepackage{physics}\n\\usepackage{multirow}\n"
                           "\\usepackage{siunitx}  % align by decimal\n"
                           mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
; PRB paper
                ("prb"
                 "\\documentclass[aps,prb,twocolumn,a4paper,floatfix,showpacs]{revtex4-1}
\\bibliographystyle{apsrev4-1}
[NO-DEFAULT-PACKAGES]
\\usepackage[version=3]{mhchem}
\\usepackage{times,mathptmx}
\\usepackage{float}
\\usepackage{array}
\\usepackage{epstopdf}
\\usepackage{threeparttable}
\\usepackage{physics}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{multirow}
\\usepackage{siunitx}  % align by decimal
\\usepackage{booktabs}
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
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[ignorenonframetext,presentation]{beamer}\n"
                           "\\usepackage{beamerseminar}" "\n"
                           "[DEFAULT-PACKAGES]\n"
                           "% space between caption and table" "\n"
                           "\\captionsetup[table]{belowskip=-6pt}" "\n"
                           mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 )
                ("beamerarticle"
                  ,(concat mz/org-latex-classes-common-header-passoptions "\n"
                           "\\documentclass[11pt,a4paper]{article}\n"
                           "\\usepackage[margin=0.9in,bmargin=1.0in,tmargin=1.0in]{geometry}\n"
                           "\\usepackage{beamerarticle}" "\n"
                           "\\usepackage{beamerseminar}" "\n"
                           "[DEFAULT-PACKAGES]\n"
                           "\\usepackage{physics}" "\n"
                           mz/org-latex-classes-common-header-after-default-pkgs "\n"
                           "[EXTRA]\n[PACKAGES]")
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

(provide 'config-ox)
;;; config-ox.el ends here
