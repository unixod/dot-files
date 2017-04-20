;;; Helpers
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed"
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

                                        ; http://stackoverflow.com/a/23299809
(defun process-exit-code-and-ouput (program &rest args)
  "Run PROGRAM with ARGS and return the exit code output goes to the current buffer"
  (with-temp-buffer
    (values (apply 'call-process program nil (current-buffer) nil args)
            (buffer-string))))

(defun have-go-environment-p ()
  (and (executable-find "go")
       (getenv "GOPATH")))

;;; General settings
(setq-default cursor-type 'bar)
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; Changing the windows size using keyboard
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Scrolling down/up (using keyboard) without moving the cursor
(global-set-key (kbd "S-C-p") (lambda ()
                                (interactive)
                                (scroll-down 1)))
(global-set-key (kbd "S-C-n") (lambda ()
                                (interactive)
                                (scroll-up 1)))


(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; (unless (display-graphic-p)
;;   (xterm-mouse-mode))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(setq inhibit-startup-message t)


(load-theme (if (display-graphic-p)
                'deeper-blue
              'wombat))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; prevent emacs for mixing tabs and spaces


;;; Packages
(setq package-archives  '(("gnu"       . "https://elpa.gnu.org/packages/")
                          ("marmalade" . "https://marmalade-repo.org/packages/")
                          ("melpa"     . "https://melpa.org/packages/")
                          ("org"       . "http://orgmode.org/elpa/")))

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
 'cmake-mode
 'go-mode
 'demangle-mode
 'perl6-mode)

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
(add-hook 'perl6-mode-hook 'auto-complete-mode)
(when (eq system-type 'windows-nt)
  (setq multi-term-program "cmd.exe"))
(add-hook 'c-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")))
(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")))

;;; Go
                                        ; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
                                        ; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
(add-hook 'before-save-hook 'gofmt-before-save)

(when (have-go-environment-p)
  ;; gocode
  (let ((go-autocomplete-el (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs/go-autocomplete.el")))
    (unless (require 'go-autocomplete go-autocomplete-el t)
      (when (yes-or-no-p "Do you want to install gocode?")
        (message "Installing gocode...")
        (multiple-value-bind (status-code stdout) (process-exit-code-and-ouput
                                                   "go" "get" "-u" "-v" "github.com/nsf/gocode")
          (message stdout)
          (if (zerop status-code)
              (require 'go-autocomplete go-autocomplete-el)
            (message "Could't retrive the gocode from github.com/nsf/gocode"))))))
  ;; godef
  (unless (executable-find "godef")
    (when (yes-or-no-p "Do you want to install godef?")
      (message "Installing godef...")
      (multiple-value-bind (status-code stdout) (process-exit-code-and-ouput
                                                 "go" "get" "-u" "-v" "github.com/rogpeppe/godef")
        (message stdout)
        (unless (zerop status-code)
          (message "Could't retrive the gocode from  github.com/rogpeppe/godef"))))))

;;; Input methods
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\from"    #X2190)
 ("\\to"      #X2192)
 ("\\over"    #X0305)
 ("\\intersect"   #X2229)
 ("\\union"   #X222A))

;;; Variables configured via the interactive 'customize' interface

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
