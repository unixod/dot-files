;;; Helpers
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed"
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; General settings
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; (unless (display-graphic-p)
;;   (xterm-mouse-mode))
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
 'multi-term
 'yasnippet
 'cmake-mode)


;;; SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;; SLIME autocompletion in editing sources
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


;;; Modes
(setq cmake-tab-width 4) ; for cmake-mode
(show-paren-mode 1)
(global-auto-complete-mode t)
(yas-global-mode 1)
(add-to-list 'yas-dont-activate
	     #'(lambda ()
		 (eq major-mode 'term-mode)))
(set-default 'org-startup-indented 1)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'auto-complete-mode)
(when (eq system-type 'windows-nt)
  (setq multi-term-program "cmd.exe"))
(add-hook 'c-mode-hook
	  #'(lambda ()
	      (c-set-style "stroustrup")))
(add-hook 'c++-mode-hook
	  #'(lambda ()
	      (c-set-style "stroustrup")))


;(add-hook 'c-mode-hook (lambda ()
;			 (smart-tabs-insinuate 'c)
;			 (smart-tabs-mode-enable)
;			 (smart-tabs-advice c-indent-line c-basic-offset)
;			 (smart-tabs-advice c-indent-region c-basic-offset)))


;;; Variables configured via the interactive 'customize' interface

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
