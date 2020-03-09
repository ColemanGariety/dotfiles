;;;; package --- Summary
;;; Commentary:

;;;;;;;;;;;;;;;;;
;; Performance ;;
;;;;;;;;;;;;;;;;;

(defvar gc-cons-threshold-original)
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(defvar file-name-handler-alist-original)
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;; byte compile func
(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;;;;;;;;;;;
;; Custom ;;
;;;;;;;;;;;;

;;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(c-basic-offset 4)
 '(company-backends
   (quote
    (company-tide company-semantic company-capf company-files
                  (company-dabbrev-code company-gtags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "09cadcc2784baa744c6a7c5ebf2a30df59c275414768b0719b800cabd8d1b842" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(flycheck-temp-prefix ".flycheck")
 '(focus-dimness nil)
 '(frame-background-mode (quote dark))
 '(global-linum-mode t)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-interactive-mode-eval-mode nil)
 '(helm-completion-window-scroll-margin 5)
 '(helm-default-prompt-display-function (quote evil-collection-helm--set-prompt-display))
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-inside-p t)
 '(helm-swoop-pre-input-function (lambda nil))
 '(json-reformat:indent-width 2)
 '(linum-format " %d ")
 '(package-selected-packages
   (quote
    (evil-mc hl-todo rjsx-mode import-js psc-ide reason-mode evil-easymotion easymotion helm-adaptive diredfl company-prescient evil-vimish-fold move-text rainbow-delimiters helm-swoop doom-themes solarized-theme one-dark-theme doom-modeline auto-compile spaceline-config spaceline general tide json-mode evil-collection avy typescript-mode handlebars-mode mustache-mode mustache yaml-mode jsx-mode babel-repl toml-mode slack bundler projectile-rails neotree tabbar ack auto-dim-other-buffers svg-mode-line-themes helm-org-rifle helm-dictionary ac-helm company apt-utils readline-complete bash-completion cargo ac-racer racer smart-mode-line helm-hoogle wiki-summary ac-haskell-process buffer-move eshell-did-you-mean eshell-z multi-term helm-ag go-autocomplete go-mode smex pophint evil-avy grizzl slime evil-surround god-mode evil-tutor helm-cider cider ghc haskell-mode showkey magit evil web-mode wc-mode wc-goal-mode w3m sass-mode pandoc-mode pandoc helm-projectile golden-ratio flycheck flx-isearch fill-column-indicator ergoemacs-mode eh-gnus dired-hacks-utils color-theme-solarized)))
 '(projectile-enable-caching t)
 '(show-paren-delay 0.0)
 '(showkey-log-mode nil)
 '(solarized-bold t)
 '(solarized-termcolors 16)
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(window-divider-default-places (quote right-only))
 '(window-divider-mode t))

;;;;;;;;;;;;
;; Nudity ;;
;;;;;;;;;;;;

(transient-mark-mode t)     ;; show region, drop mark
(global-font-lock-mode t)   ;; for all buffers
(global-visual-line-mode t) ;; word-wrap
(setq shift-select-mode nil) ;; Shift select
(show-paren-mode t)         ;; show matching parentheses
;; (setq initial-scratch-message ";; ^    first non-whitespace
;; mx   set mark
;; 'x   go to mark
;; '.   last changed line
;; zz   center cursor line
;; C-o  previous location
;; C-i  next location
;; #    previous token under cursor
;; *    next token under cursor
;; cc   replace line
;; C    change to end of line
;; D    delete to end of line
;; dab  change around block (or anything)
;; dab  change in block (or anything)
;; I    insert at first non-whitespace")
(setq initial-scratch-message nil)
(setq load-prefer-newer t)
(setq initial-buffer-choice t)
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode 0)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(save-place-mode)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(set-face-inverse-video 'vertical-border nil)
(setq scroll-error-top-bottom t) ;; Pgdn & Pgup work properly
(setq large-file-warning-threshold 100000) ;; Large file warning
(setq mode-require-final-newline t) ;; Newlines
(setq scroll-margin 4)
(setq scroll-conservatively 1)
(setq column-number-mode t) ;; Column numbers in modeline
(delete-selection-mode) ;; Replace selection
(fset 'yes-or-no-p 'y-or-n-p) ;; Changes all yes/no questions to y/n type
(setq create-lockfiles nil) ;; Disable lockfiles in server mode
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil) ;; soft Tabs
(electric-pair-mode) ;; electric pair
(push '(?\' . ?\') electric-pair-pairs) ;; single quote pairs
(global-linum-mode)
(setq linum-format " %d ")

;; Ask before closing

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

;;;;;;;;;;;;;;;;
;; Auto-modes ;;
;;;;;;;;;;;;;;;;

(load-file "~/Git/web-mode/web-mode.el")

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ;;;;;;;;;;;;;;;;;
;; ;; Auto-compie ;;
;; ;;;;;;;;;;;;;;;;;

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t))

;;;;;;;;;;;;;;;
;; Mode line ;;
;;;;;;;;;;;;;;;

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode))

;;;;;;;;;;;;;;;
;; Evil Mode ;;
;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq-default evil-cross-lines t)
  :config
  (global-evil-surround-mode)
  (evil-collection-init)
  (evil-mode)
  (define-key evil-normal-state-map "\C-k" 'move-text-up)
  (define-key evil-normal-state-map "\C-j" 'move-text-down)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-visual-state-map "\C-n" 'evil-next-line)
  (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-visual-state-map "\C-p" 'evil-previous-line))

(use-package evil-collection
  :ensure t)

(use-package evil-surround
  :ensure t)

(use-package move-text
  :ensure t)

;;;;;;;;;;;;;;
;; Hideshow ;;
;;;;;;;;;;;;;;

;; (add-hook 'c-mode-common-hook   'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'java-mode-hook       'hs-minor-mode)
;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
;; (add-hook 'perl-mode-hook       'hs-minor-mode)
;; (add-hook 'sh-mode-hook         'hs-minor-mode)
;; (add-hook 'web-mode-hook        'hs-minor-mode)

;;;;;;:;;;;;;;;;;;;
;; Easymotion(s) ;;
;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t)

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  (setq avy-all-windows nil)
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'evil-avy-goto-char-2))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(use-package diredfl
  :ensure t
  :init
  (diredfl-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion (Company, Auto-Complete) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

;;;;;;;;;;;;;;;
;; Helm Mode ;;
;;;;;;;;;;;;;;;

(use-package helm-projectile
  :ensure t
  :init
  ;; Hide advice
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))

  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; makes TAB work in terminal
  ;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (setq helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)

  ;; Helm + Projectile
  (helm-projectile-on)
  (projectile-mode)
  (global-set-key (kbd "C-x C-d") 'helm-projectile-find-file)
  (global-set-key (kbd "C-x C-g") 'helm-projectile-ag)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-k") 'helm-swoop)
  (global-set-key (kbd "C-x C-l") 'helm-locate)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-mini)

  ;; A lil' performance
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

(use-package helm-adaptive
  :config
  (helm-adaptive-mode))

(use-package helm-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; keys
(define-key global-map (kbd "M-k") 'windmove-up)
(define-key global-map (kbd "M-j") 'windmove-down)
(define-key global-map (kbd "M-h") 'windmove-left)
(define-key global-map (kbd "M-l") 'windmove-right)
(define-key global-map (kbd "M-K") 'evil-window-split)
(define-key global-map (kbd "M-J") 'evil-window-split)
(define-key global-map (kbd "M-H") 'evil-window-vsplit)
(define-key global-map (kbd "M-L") 'evil-window-vsplit)
(define-key global-map (kbd "C-q") 'delete-window)

;;;;;;;;;;;;;;;;
;; Copy/paste ;;
;;;;;;;;;;;;;;;;

;; If emacs is run in a terminal, the clipboard- functions have NO
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
;;
;; Idea from
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
(when (eq system-type 'gnu/linux)
  (unless window-system
    (when (getenv "DISPLAY")
      ;; Callback for when user cuts
      (defun xsel-cut-function (text &optional push)
	;; Insert text to temp-buffer, and "send" content to xsel stdin
	(with-temp-buffer
	  (insert text)
	  ;; I prefer using the "clipboard" selection (the one the
	  ;; typically is used by c-c/c-v) before the primary selection
	  ;; (that uses mouse-select/middle-button-click)
	  (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
      ;; Call back for when user pastes
      (defun xsel-paste-function()
	;; Find out what is current selection by xsel. If it is different
	;; from the top of the kill-ring (car kill-ring), then return
	;; it. Else, nil is returned, so whatever is in the top of the
	;; kill-ring will be used.
	(let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	  (unless (string= (car kill-ring) xsel-output)
	    xsel-output )))
      ;; Attach callbacks to hooks
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function))))

;;;;;;;;;;;;;;;
;; Solarized ;;
;;;;;;;;;;;;;;;

;; (use-package color-theme-solarized
;;   :ensure t
;;   :init
;;   (load-theme 'solarized t))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq flycheck-check-syntax-automatically '(save))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

;;;;;;;;;;
;; Tide ;;
;;;;;;;;;;

;; (use-package web-mode
;;   :ensure nil
;;   )

(use-package tide
  :ensure t
  :init
  (add-hook 'web-mode-hook #'setup-tide-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode +1))

(add-hook 'web-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions
                         'delete-trailing-whitespace)))

;;;;;;;;;;;;;
;; Rainbow ;;
;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :init
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Highlight Todos ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-mc
  :ensure t
  :init
  (global-evil-mc-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Custom faces... ;;
;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#51afef" :foreground "brightblack" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#525252" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "color-239"))))
 '(linum ((t (:inherit default :foreground "#587094" :strike-through nil :underline nil :slant normal :weight normal)))))

(provide '.emacs)
;;; .emacs ends here
