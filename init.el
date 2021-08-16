;; first things first: get rid of superfluous UI parts
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; store backup files in their own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; "imports" ------------------------------------------------------

;;; import package system
(require 'package)

;;; list of package archives from which to pull packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/")
	     t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/")
	     t)

;;; package repo priorities
(setq package-archive-priorities
      '(("melpa" . 99)
	("org" . 10)
	("melpa-stable" . 1)
	("gnu" . 0))) ; ELPA is our last resort...

;;; activate packages
(package-initialize)

;;; fetch available packages
;; option 1: refresh only if there is no package archive (faster)
(unless package-archive-contents
  (package-refresh-contents))
;; option 2: always refresh (safer; use in case emacs shows errors on startup regarding packages not found)
;(package-refresh-contents)

;;; list of packages to install; insert new packages here
(setq package-list
      '(magit
	clj-refactor
	clojure-snippets
	flx-ido
	yasnippet
	yasnippet-snippets
	org-projectile
	projectile
	rainbow-delimiters
	paredit
	markdown-mode
	solarized-theme
	cider))

;;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; if there is a Slime installation (the version from git), configure it
;;; since this isn't installed from an Emacs package, make sure to only configure it if it is actually present
(when (file-directory-p "~/.slime")
  (progn
   ;;; add Slime; using the version from Git (instead of MELPA), so we need to tell Emacs where to find it
   (add-to-list 'load-path "~/.slime") ; assumes Slime has been "git clone"-d into ~/.slime
   (require 'slime-autoloads)
   (setq inferior-lisp-program "abcl")
   ;;; use custom SBCL core including pre-loaded packages (sb-bsd-sockets, sb-posix, sb-introspect, sb-cltl2, asdf)
   ;;; see https://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html#Loading-Swank-faster
   (setq slime-lisp-implementations
	 '((abcl ("abcl"))
	   (sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))))

;; -----------------------------------------------------------------

;; activate solarized theme (only use one!)
(load-theme 'solarized-dark t)
;(load-theme 'solarized-light t)


;; disable VC for Git (-> Magit for Git, VC for everything else)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
;; to also back up files under VCS control, uncomment this:
;; (setq vc-make-backup-files t)

;; Magit status buffer hotkey
(global-set-key (kbd "C-x g") 'magit-status)

;; 150 cols x 50 lines (-> decent window size)
(setq initial-frame-alist '((top . 1) (left . 1) (width . 150) (height . 50)))

;; IDO auto-complete with flx fuzzy matching
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil) ; use flx faces instead of ido defaults

;; auto-configured stuff (via "wizard"-like dialogs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (actionscript-mode markdown-mode rainbow-delimiters org-projectile yasnippet-snippets flx-ido clojure-snippets magit clj-refactor))))

;; even though it's empty, it's still necessary; if removed emacs will re-insert it at the end of the file
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; key bindings
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; hooks

;;; paredit mode hook so barfing (paren-shrinking) works via ö/ä instead of {/}
(defun paredit-barf-key-rebind-hook ()
  "rebinds backward/forward barfing to C-ö/C-ä"
  (define-key paredit-mode-map (kbd "C-ö") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-ä") 'paredit-forward-barf-sexp))

(defun paredit-backward-wrap-round ()
  "wrap preceding sexp"
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(add-hook 'paredit-mode-hook 'paredit-barf-key-rebind-hook)
(add-hook 'paredit-mode-hook
	  (lambda ()
	    (define-key paredit-mode-map (kbd "M-)") 'paredit-backward-wrap-round)))


;;; common stuff for lisp modes (paredit, rainbow parens, ...)
(defun lisp-mode-common-config-hooks ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1))

;;; clojure-specific stuff (clj-refactor, yas, projectile)
(require 'clj-refactor)
(defun clojure-mode-config-hooks ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (projectile-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-r"))

;;; apply default lisp hooks for lisp/emacs lisp/clojure modes
(add-hook 'lisp-mode-hook 'lisp-mode-common-config-hooks)
(add-hook 'inferior-lisp-mode-hook 'lisp-mode-common-config-hooks)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-common-config-hooks)
(add-hook 'clojure-mode-hook 'lisp-mode-common-config-hooks)
(add-hook 'cider-mode-hook 'lisp-mode-common-config-hooks)
(add-hook 'cider-repl-mode-hook 'lisp-mode-common-config-hooks)

;;; clojure-specific hooks
(add-hook 'clojure-mode-hook 'clojure-mode-config-hooks)
(add-hook 'cider-mode-hook 'clojure-mode-config-hooks)

;;; SLIME/Common Lisp hooks
(add-hook 'lisp-mode-hook (lambda () (slime-mode 1)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode 1)))

;;; SLIME/Common Lisp config stuff
(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///home/nils/cl/HyperSpec")

;;; additional (non-hook) functions
(global-set-key (kbd "C-S-n")
		(lambda ()
		  (interactive)
		  (ignore-errors (next-line 10))))
(global-set-key (kbd "C-S-p")
		(lambda ()
		  (interactive)
		  (ignore-errors (previous-line 10))))
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))

;;; stolen from some other guy's config somewhere on the 'net: org-mode section stats fix
(defun org-mode-update-section-item-stats ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
	(org-back-to-heading)
	(org-update-parent-todo-statistics)))))
(defadvice org-kill-line (after fix-cookies activate) (org-mode-update-section-item-stats))
(defadvice kill-whole-line (after fix-cookies activate) (org-mode-update-section-item-stats))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate) (setq mode-name ,new-name))))
(rename-modeline "clojure-mode" clojure-mode "clj")

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(put 'upcase-region 'disabled nil)
