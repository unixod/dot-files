;;; General settings ------------------------------------------
(setq inhibit-startup-message t)
(setq-default cursor-type 'bar)
(column-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1) ; Remove selection when typing
(show-paren-mode 1)

;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (ido-mode t)

(setq vc-follow-symlinks t) ; Don't ask if to follow a symlink

(dolist (mode '(org-mode-hook
                treemacs-mode-hook
                eat-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
              
    

                  
;; (load-theme (if (display-graphic-p)
;;                 'modus-operandi
;;                 ;'deeper-blue
;;                 ;'modus-vivendi
;;               'wombat))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; Don't mix tabs and spaces


;; Windows management ------------------------------------------

; Move cursor between windows
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

; Change window size
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Scrolling --------------------------------------------------
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(setq mouse-wheel-progressive-speed nil)

;; Scrolling (using keyboard) without moving the cursor
(global-set-key (kbd "S-C-p") (lambda ()
                                (interactive)
                                (scroll-down 1)))
(global-set-key (kbd "S-C-n") (lambda ()
                                (interactive)
                                (scroll-up 1)))

;; Scaling ----------------------------------------------------
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;; Packages ---------------------------------------------------
(use-package package
  :ensure nil
  :config
  (package-initialize)
  (add-to-list 'package-archives  '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives  '("org" . "http://orgmode.org/elpa/"))
  :custom
  (package-native-compile t))

(use-package diminish :ensure t)

;(if (display-graphic-p)
    (use-package modus-themes
      :ensure t
      :config
      (load-theme 'modus-operandi-tinted :no-confirm))
;  (load-theme 'wombat))

(use-package eat
  :ensure t)

(use-package company
  :ensure t)

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . 'company-box-mode))

(use-package ivy
  :diminish
  :ensure t
  :init
  (use-package counsel :diminish :ensure t :config (counsel-mode 1))
  (ivy-mode 1)
  ;; :config
  ;; (ivy-mode 1)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
              ("RET" . ivy-alt-done)
              ("C-j" . ivy-immediate-done)))

(use-package lsp-mode
  ;;  :init
  :config
  (lsp-enable-which-key-integration t)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-l")
  (lsp-file-watch-threshold 2000))

;; (use-package lsp-ido
;;   ;:ensure t
;;   :commands lsp-ido-workspace-symbol)
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-delay 1 "Number of seconds before showind doc")
  ;; (lsp-ui-doc-use-webkit t) ; need for emacs built --with-xwidgets
  :commands lsp-ui-mode)

;; (use-package treemacs
;;   :bind (:map treemacs-mode-map
;;               ([mouse-1] . treemacs-single-click-expand-action)
;;          ))
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-symbols)

;; (use-package origami
;;   :ensure t)

;; (use-package lsp-origami
;;   :ensure t
;;   :config
;;   (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

;; To use treesitter
(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode) ;; To use treesitter
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; (use-package hideshowvis
;;   :ensure t)

; TODO: try to remove the block for Emacs version >= 30
(use-package which-key
  :diminish
  :if (version< emacs-version "30")
  :ensure t
  :config
  (which-key-mode))

;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (ido-ubiquitous-mode 1))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
;; (use-package nerd-icons :ensure t)

;;; Modes -----------------------------------------------------
(setq cmake-tab-width 4) ; for cmake-mode
(set-default 'org-startup-indented 1)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'c-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")))
(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")))

(add-hook 'prolog-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-j") 'ediprolog-dwim)))

;;; Input methods
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\from"    #X2190)
 ("\\to"      #X2192)
 ("\\over"    #X0305)
 ("\\intersect"   #X2229)
 ("\\union"   #X222A))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-safe-themes
   '("712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" default))
 '(package-selected-packages '(rustic lsp-mode company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
