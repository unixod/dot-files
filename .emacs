;;; Helpers

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed"
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; General settings

;(set-default 'cursor-type 'bar)
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(when window-system
  (set-background-color "#F0F0F0"))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(setq inhibit-startup-message t)
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")
(load-theme 'deeper-blue)

;;; Packages

(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
			  ("marmalade" . "http://marmalade-repo.org/packages/")
			  ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(ensure-package-installed
 'auto-complete
 'ac-slime
 'org
 'org-ac
 'multi-term)


;;; SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;; SLIME autocompletion in editing sources
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;; Semantic
;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;(semantic-mode 1)

;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)
;(when (cedet-etag-version-check)
;  (semantic-load-enabled-primary-exuberent-ctags-support 'c-mode))


;;; Modes
(show-paren-mode 1)
(set-default 'org-startup-indented 1)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'auto-complete-mode)

;(add-hook 'c-mode-hook (lambda ()
;			 (smart-tabs-insinuate 'c)
;			 (smart-tabs-mode-enable)
;			 (smart-tabs-advice c-indent-line c-basic-offset)
;			 (smart-tabs-advice c-indent-region c-basic-offset)))


;;; Variables configured via the interactive 'customize' interface

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
