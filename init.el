;;;
;;; Do not manually edit this file. It is automatically generated from config.org, change that file instead.
;;;

;;; Reset Emacs to minimal -----------------------------------------------------


;; first things first: get rid of superfluous UI parts
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setf inhibit-startup-message t)
(setf inhibit-startup-echo-area-message t)

;; prevent customize from polluting our carefully-crafted init file :) instead, store that crap in its own file
(customize-save-variable 'custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; store backup files in their own directory
(setf backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; start maximized
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


;;; custom variables -----------------------------------------------------------


(defconst nils/slime-dir "~/.slime/"
  "Directory into which SLIME has been 'git clone'-d")

(defconst nils/common-lisp-dir "~/cl"
  "Base directory for Common Lisp stuff, e.g. HyperSpec, Quicklisp packages, ...")

(defconst nils/hyperspec-location (let* ((expanded-cl-dir (expand-file-name nils/common-lisp-dir))
                                         (hyperspec-base-dir (file-name-concat expanded-cl-dir "hyperspec/")))
                                    (concat "file://" hyperspec-base-dir))
  "Base URL to Common Lisp HyperSpec")

(defconst nils/projects-dir "~/code"
  "Directory for projectile to search for (code) projects")


;;; package system setup -------------------------------------------------------


;; "activate" package system
(require 'package)

;; list of package archives from which to pull packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/")
             t)

;; package repo priorities
(setf package-archive-priorities
      '(("melpa" . 99)
        ("org" . 10)
        ("melpa-stable" . 1)
        ("gnu" . 0))) ; ELPA is our last resort...

;; initialize package system
(package-initialize)

;; fetch available packages
;; option 1: refresh only if there is no package archive (faster)
(unless package-archive-contents
  (package-refresh-contents))
;; option 2: always refresh (safer; use in case emacs shows errors on startup regarding packages not found)
;(package-refresh-contents)

;; set up use-package for package management: install it, load it, make sure it automatically installs missing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setf use-package-always-ensure t)


;;; install packages -----------------------------------------------------------


;;--- appearance and visuals ---


;; icons :)
;; on freshly-set-up machines, needs "M-x all-the-icons-install-fonts" on first run, to install necessary OS fonts
(use-package all-the-icons)
(use-package all-the-icons-dired
    :hook
    (dired-mode . all-the-icons-dired-mode))

;; slightly adapted solarized theme that works well with doom-modeline
;; other available themes in the package: see https://github.com/doomemacs/themes
(use-package doom-themes
    :init
    (load-theme 'doom-solarized-light t)) ; t suppresses "loading themes is dangerous..."-prompt

;; clean, modern modeline
(use-package doom-modeline
    :init
    (doom-modeline-mode 1))


;;--- suggestions and auto-complete ---


;; suggestions and selection in minibuffer
(use-package ivy
    :diminish ivy-mode
    :config
    (ivy-mode 1)
    :bind
    (("C-s" . swiper) ; enhanced search (swiper instead of Emacs standard)
     :map ivy-minibuffer-map
     ("TAB" . ivy-alt-done) ; select and apply option (instead of selecting, and applying via <enter>)
     :map ivy-switch-buffer-map
     ("TAB" . ivy-done))) ; <tab> will directly switch to the selected buffer

;; additional information for ivy selection options in minibuffer
(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

 ;; ivy-enhanced emacs commands
(use-package counsel
    :bind
    (("M-x" . counsel-M-x)
     ("C-x b" . counsel-ibuffer)
     ("C-x C-f" . counsel-find-file)
     :map minibuffer-local-map
     ("C-r" . 'counsel-minibuffer-history)))

;; key bindings grouped by common prefix
(use-package hydra)

;; key combination hints for the current, incomplete key chord
(use-package which-key
    :diminish which-key-mode
    :init
    (which-key-mode)
    :config
    (setf which-key-idle-delay 0.5))

;; emacs help system on steroids
(use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    (([remap describe-function] . counsel-describe-function)
     ([remap describe-command] . helpful-command)
     ([remap describe-variable] . counsel-describe-variable)
     ([remap describe-key] . helpful-key)))


;;--- general coding/dev stuff ---


;; project management
(use-package projectile
    :diminish projectile-mode
    :init
    (when (file-directory-p nils/projects-dir)
      (setf projectile-project-search-path (list nils/projects-dir)))
    (setf projectile-switch-project-action #'projectile-dired)
    :config
    (projectile-mode)
    :custom
    ((projectily-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map))

;; ivy/counsel extension for projectile; nicer projects list etc.
(use-package counsel-projectile
    :config
    (counsel-projectile-mode))

;; git
(use-package magit)

;; snippet functionality
(use-package yasnippet
    :config
    (setf yas-snippet-dirs `(,(file-name-concat user-emacs-directory "snippets")))
    (yas-reload-all) ; load snippet tables; necessary since yas-global-mode is not enabled
    :hook
    ((prog-mode
      org-mode
      text-mode
      snippet-mode) . yas-minor-mode-on))

;; some pre-defined snippets for yasnippet
(use-package yasnippet-snippets)

;; rainbow parens
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))

;; Emacs Speaks Statistics
(use-package ess)


;;--- org mode ---


(defun nils/org-mode-setup ()
  (org-indent-mode) ; indent content according to outline
  (visual-line-mode 1) ; auto-wrap long lines
  (variable-pitch-mode 1)) ; enable variable-pitch (i.e. proportional) fonts

(defun nils/org-agenda-mode-setup ()
  (setf org-agenda-start-day "-2d")) ; agenda view: two past days + future; setting doesn't work in :config or :custom, so set it via :hook

(defconst nils/org-structure-templates '(("el" . "src emacs-lisp")
                                         ("cl" . "src lisp")
                                         ("clj" . "src clojure")
                                         ("r" . "src R")
                                         ("py" . "src python")
                                         ("sh" . "src shell")
                                         ("sql" . "src sql"))
  "Custom snippet templates for org-mode to use with org-tempo")

(defun nils/add-custom-org-structure-templates (templates)
  "Add the given TEMPLATES for use with org-tempo.

TEMPLATES must be an alist containing (KEY . VALUE) pairs
that are added to the variable `org-structure-template-alist'."
  (mapcar (lambda (template) (add-to-list 'org-structure-template-alist template)) nils/org-structure-templates))

(defconst nils/org-enabled-languages '((emacs-lisp . t) ; should always be enabled, and is enabled by default
                                       (clojure . t)
                                       (latex . t)
                                       (lisp . t)
                                       (R . t))
  "Language integrations that should be available in org-mode.

Must be an alist containing (KEY . VALUE) pairs, where
KEY is the language to enable (or disable), and
VALUE is either t or nil.
The value of this variable is applied via `org-babel-do-load-languages'.")

(defun nils/find-available-font (fonts)
  "From the given list of FONTS, find the first one available on the system."
  (seq-find (lambda (font) (x-family-fonts font)) fonts))

(defconst nils/font-serif
  `(:family ,(nils/find-available-font '("Noto Serif" "Source Serif 4" "Bookman Old Style" "Times New Roman")))
  "Preferred proportional serif font family, depending on which fonts are available on the system.")
(defconst nils/font-sans
  `(:family ,(nils/find-available-font '("Noto Sans" "Source Sans 3" "Century Gothic" "Arial")))
  "Preferred proportional sans-serif font family, depending on which fonts are available on the system.")
(defconst nils/font-mono
  `(:family ,(nils/find-available-font '("Noto Sans Mono" "Source Code Pro" "Cascadia Mono" "Courier New")))
  "Preferred fixed (monospace) font family, depending on which fonts are available on the system")

;; set default fonts for variable-pitch-mode, which will be enabled for org-mode
(custom-theme-set-faces
 'user
 `(variable-pitch ((t (,@nils/font-serif))))
 `(fixed-pitch ((t (,@nils/font-mono)))))


;; declarative org-mode capture templates
(use-package doct
    :commands (doct)) ; defer loading the package until doct function is invoked

(defconst nils/capture-templates
  '(("Tasks" :keys "t"
     :file (lambda () (file-name-concat (car org-agenda-files) "gtd.org")) ; must be a lambda: org-agenda-files is not defined yet
     :prepend t
     :empty-lines 1
     :kill-buffer t
     :template ("* TODO %^{Description}"
                ":PROPERTIES:"
                ":Created: %U"
                ":Location: %a"
                ":END:"
                "%?")
     :children (("Todo" :keys "t"
                 :headline "Inbox")))))



(defconst nils/agenda-commands
  '(("o" "Overview"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))))


(use-package org
    :config
    (require 'org-tempo) ; org-mode specific integrated snippets
    (require 'ox-latex)  ; org-mode exporter for LaTeX
    (add-to-list 'org-latex-packages-alist '("" "listings")) ; for lstlisting environments
    (add-to-list 'org-latex-packages-alist '("" "xcolor")) ; colors and color names
    (add-to-list 'org-latex-packages-alist '("lighttt" "lmodern")) ; latin modern fonts (light tt-font); default ttfamily font has no bold face
    (setf org-latex-listings t) ; export src blocks as lstlisting-environments (instead of verbatim); needs "listings" package (see above)
    (setf org-latex-listings-options '(("basicstyle" "\\ttfamily") ; basic style: monospaced font for code
                                       ("backgroundcolor" "\\color{lightgray!10}") ; lightgray is too dark -> 10% lightgray, 90% white
                                       ("xleftmargin" "1cm") ; alternatively: \parindent (if not set to 0)
                                       ("breaklines" "true") ; auto-wrap long lines
                                       ("keywordstyle" "\\bfseries") ; bold keywords (needs non-default font if used together with ttfamily)
                                       ("commentstyle" "\\itshape\\color{darkgray}") ; slightly tone down the comments
                                       ("numbers" "left") ; line numbers
                                       ("numberstyle" "\\footnotesize\\color{darkgray}") ; make line numbers less prominent
                                       ("numbersep" ".5cm") ; sizing guideline: framesep + size of frame + offset from frame
                                       ("frame" "L") ; double line on the left
                                       ("framesep" ".25cm") ; some breathing room between frame and code
                                       ("tabsize" "2") ; one tab = 2 spaces
                                       ("captionpos" "t"))) ; does not seem to override captionpos? might need manual fixing in exported file
    (nils/add-custom-org-structure-templates nils/org-structure-templates)
    :custom
    (org-hide-emphasis-markers t)
    (org-ellipsis " ⋮")
    (org-agenda-files `(,(file-name-concat org-directory "agenda/")))
    (org-agenda-span 10)
    (org-agenda-start-on-weekday nil) ; start today instead of on a fixed weekday
    (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c@)")))
    (org-capture-templates (doct nils/capture-templates))
    (org-agenda-custom-commands nils/agenda-commands)
    (org-babel-load-languages nils/org-enabled-languages)
    :custom-face
    ;; title styling (larger font size and sans-serif font)
    (org-document-title ((t (:height 2.0 ,@nils/font-sans))))
    (org-level-1 ((t (:inherit default :weight bold :height 1.5 ,@nils/font-sans))))
    (org-level-2 ((t (:height 1.25 ,@nils/font-sans))))
    (org-level-3 ((t (:height 1.1 ,@nils/font-sans))))
    (org-level-4 ((t (:height 1.05 ,@nils/font-sans))))
    (org-level-5 ((t (,@nils/font-sans))))
    (org-level-6 ((t (,@nils/font-sans))))
    (org-level-7 ((t (,@nils/font-sans))))
    (org-level-8 ((t (,@nils/font-sans))))
    ;; some faces should be fixed-pitch, e.g. code, indentation, tables, etc.
    (org-block ((t (:inherit fixed-pitch))))
    (org-code ((t (:inherit fixed-pitch))))
    (org-indent ((t (:inherit (org-hide fixed-pitch)))))
    (org-table ((t (:inherit fixed-pitch))))
    :hook
    (org-mode . nils/org-mode-setup)
    (org-agenda-mode . nils/org-agenda-mode-setup)
    :bind
    (("C-c c" . org-capture)
     ("C-c t" . (lambda () (interactive) (org-capture nil "tt"))))) ; directly capture a GTD inbox task

(use-package org-bullets
    :after org
    :custom
    (org-bullets-bullet-list '("◉"))
    :hook
    (org-mode . org-bullets-mode))

(defun org-mode-update-section-item-stats ()
  "Update all statistics cookies in the current org-file"
  (when (equal major-mode 'org-mode)
    (ignore-errors
      (org-update-statistics-cookies t))))

(defadvice org-kill-line (after fix-cookies activate)
  "fix statistics cookies after org-mode-killing a line"
  (org-mode-update-section-item-stats))

(defadvice kill-whole-line (after fix-cookies activate)
  "fix statistics cookies after killing a line"
  (org-mode-update-section-item-stats))

(defun nils/vis-fill-setup ()
  (setf visual-fill-column-width 150 ; text area width is 150 chars
        visual-fill-column-center-text t) ; text area is centered
  (visual-fill-column-mode 1))

;; make org-mode feel more wordprocessor-y by giving the text some whitespace at the sides
(use-package visual-fill-column
    :hook
    (org-mode . nils/vis-fill-setup))

;; org-roam for personal knowledge management
(use-package org-roam
    :init
    (setf org-roam-v2-ack t)
    :config
    (org-roam-setup)
    (org-roam-db-autosync-mode)
    (require 'org-roam-dailies)
    :custom
    (org-roam-directory (file-name-concat org-directory "roam/"))
    (org-roam-dailies-directory "daily/") ; relative to org-roam-directory
    :bind
    (("C-c n f" . org-roam-node-find)
     ("C-c n i" . org-roam-node-insert)
     ("C-c n l" . org-roam-buffer-toggle))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map))


;;--- Lisp and related things ---


;; SLIME-y Clojure(Script) :)
(use-package cider)

(defun nils/paredit-backward-wrap-round ()
  "wrap preceding sexp"
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

;; paredit-mode in the minibuffer has stopped working at some point, RET would insert a newline instead of evaluating the expression
;; solution found here: https://shawnhoover.dev/notes/emacs-paredit-eval-minibuffer.html (CC-BY 4.0)
(defun nils/eval-minibuffer-enable-paredit ()
  (enable-paredit-mode)
  (unbind-key (kbd "RET") paredit-mode-map))

;; paren-matching
(use-package paredit
    :hook
    (eval-expression-minibuffer-setup . nils/eval-minibuffer-enable-paredit)
    ((lisp-mode
      inferior-lisp-mode
      lisp-interaction-mode
      emacs-lisp-mode
      ielm-mode ; inferior emacs lisp mode
      clojure-mode
      cider-mode
      cider-repl-mode
      scheme-mode) . paredit-mode)
    :bind
    (:map paredit-mode-map
     ("C-ö" . paredit-backward-barf-sexp) ; rebind command for easier access on DE keyboard layout
     ("C-ä" . paredit-forward-barf-sexp) ; rebind command for easier access on DE keyboard layout
     ("M-)" . nils/paredit-backward-wrap-around)))

;; if there is a Slime installation (the version from git), configure it
;; since this isn't installed from an Emacs package, make sure to only configure it if it is actually present
(when (file-directory-p nils/slime-dir)
  (progn
   ;; add Slime; using the version from Git (instead of MELPA), so we need to tell Emacs where to find it
   (add-to-list 'load-path nils/slime-dir)
   (require 'slime-autoloads)
   (setf inferior-lisp-program "sbcl")
   ;; to use custom SBCL core including pre-loaded packages (e.g. sb-bsd-sockets, sb-posix, asdf, ...),
   ;; see https://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html#Loading-Swank-faster
   (setf slime-lisp-implementations
         '((sbcl ("sbcl"))
           (abcl ("abcl"))))
   ;; SLIME/Common Lisp hooks
   (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))
   (add-hook 'lisp-mode-hook (lambda () (slime-mode 1)))
   (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode 1)))))

;; SLIME/Common Lisp config stuff
(setf lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root nils/hyperspec-location)



;;; misc stuff -----------------------------------------------------------------


;; turn on line numbers on the left, and current column in the mode line
(global-display-line-numbers-mode)
(column-number-mode)

;; turn line numbers off for some modes (e.g. shell)
(dolist (mode '(shell-mode-hook
                eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; disable VC for Git (-> Magit for Git, VC for everything else)
(setf vc-handled-backends (delq 'Git vc-handled-backends))
;; to also back up files under VCS control, uncomment this:
;; (setf vc-make-backup-files t)


;;; key bindings ---------------------------------------------------------------


;;--- helper functions (for keybindings etc.) ---

(defun nils/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun nils/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun nils/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun nils/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))


;;--- global key bindings ---

;; insert empty line below/above current line
(global-set-key (kbd "<C-return>") 'nils/open-line-below)
(global-set-key (kbd "<C-S-return>") 'nils/open-line-above)

;; move current line down/up (i.e. swap current line with the one below/above)
(global-set-key (kbd "<C-S-down>") 'nils/move-line-down)
(global-set-key (kbd "<C-S-up>") 'nils/move-line-up)

;; C-n/C-p x10
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 10))))
(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 10))))

;; join current and next line by pulling next line up into current line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; hydra for window management
(defhydra hydra-window (:hint nil)
  "
Split:         _x_ horizontal^    ^_v_ertical
Move:          _h_ left  _j_ down  _k_ up  _l_ right
Move splitter: _H_ left  _J_ down  _K_ up  _L_ right
Delete:        _o_ther windows^   ^_b_ selected buffer
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ;; H/J/K/L: hydra-move-splitter-... does not seem to work :/
  ("H" shrink-window-horizontally)
  ("J" enlarge-window)
  ("K" shrink-window)
  ("L" enlarge-window-horizontally)
  ("v" split-window-right)
  ("x" split-window-below)
  ("o" delete-other-windows)
  ("b" kill-current-buffer)
  ("q" nil "quit" :color blue))
(define-key global-map (kbd "C-S-f") 'hydra-window/body)


;;; enable some emacs features that are disabled by default --------------------


;; enable up-/down-case selected region functions (C-x C-u / C-x C-l)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; kill dired buffer when selecting new directory; prevents lots of dired buffers when jumping between lots of directories
(setf dired-kill-when-opening-new-dired-buffer t)
;; better dired format: directories before files + properly-formatted date and time
(setf dired-listing-switches (combine-and-quote-strings '("-alh" "--group-directories-first" "--time-style=+%d-%m-%Y %H:%M:%S")))

;; always use y/n for confirmation (instead of yes/no)
(fset 'yes-or-no-p 'y-or-n-p)
