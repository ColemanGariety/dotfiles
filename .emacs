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
 '(company-idle-delay 0.05)
 '(company-minimum-prefix-length 2)
 '(compilation-message-face (quote default))
 '(counsel-rg-base-command
   "rg -M 120 --glob !yarn.lock --with-filename --no-heading --line-number --color never %s")
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "09cadcc2784baa744c6a7c5ebf2a30df59c275414768b0719b800cabd8d1b842" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(evil-goggles-duration 0.3)
 '(flycheck-temp-prefix ".flycheck")
 '(focus-dimness nil)
 '(frame-background-mode (quote dark))
 '(haskell-indentation-cycle-warn nil)
 '(haskell-interactive-mode-eval-mode nil)
 '(helm-completion-window-scroll-margin 5)
 '(helm-default-prompt-display-function (quote evil-collection-helm--set-prompt-display))
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-rg-thing-at-point (quote none))
 '(helm-scroll-amount 8)
 '(helm-split-window-inside-p t)
 '(helm-swoop-pre-input-function (lambda nil))
 '(helm-swoop-split-direction (quote split-window-vertically))
 '(helm-swoop-split-with-multiple-windows nil)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(linum-format " %d " t)
 '(package-selected-packages
   (quote
    (number-lock evil-swap-keys ivy-hydra evil-easymotion evil-textobj-column origami evil-textobj-syntax evil-textobj-line evil-snipe ivy-smex doom-todo-ivy ivy-posframe counsel-projectile counsel ivy-mode evil-args evil-matchit iedit git-gutter smartparens helm-rg hl-todo psc-ide easymotion helm-adaptive diredfl company-prescient move-text rainbow-delimiters helm-swoop doom-themes one-dark-theme doom-modeline auto-compile spaceline-config general tide json-mode evil-collection avy handlebars-mode mustache-mode mustache yaml-mode jsx-mode babel-repl toml-mode slack bundler projectile-rails neotree tabbar ack auto-dim-other-buffers svg-mode-line-themes helm-org-rifle helm-dictionary ac-helm company apt-utils readline-complete bash-completion cargo ac-racer racer smart-mode-line helm-hoogle wiki-summary ac-haskell-process buffer-move eshell-did-you-mean eshell-z multi-term go-autocomplete go-mode smex pophint evil-avy slime evil-surround god-mode evil-tutor helm-cider cider ghc haskell-mode showkey magit evil web-mode wc-mode wc-goal-mode w3m sass-mode pandoc-mode pandoc helm-projectile golden-ratio flycheck flx-isearch fill-column-indicator ergoemacs-mode eh-gnus dired-hacks-utils no-littering use-package)))
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
;; (global-hl-line-mode)
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
;; cib  change in block (or symbol)
;; cil change in line
;; cia change in argument
;; I    insert at first non-whitespace"
;; g d  jump to definition
;; C-x C-e  iedit
;; L    next argument
;; H    previous argument
;; C-x RET evil marks)

(setq initial-scratch-message nil)
(setq text-quoting-style 'grave)
(setq load-prefer-newer t)
(setq line-number-display-limit-width 2000000)
(setq initial-buffer-choice t)
(setq inhibit-startup-screen t)
;; (scroll-bar-mode -1)
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
(setq scroll-margin 5)
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
(display-time)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil) ;; soft Tabs
(put 'narrow-to-region 'disabled nil)
;; (electric-pair-mode) ;; electric pair
;; (push '(?\' . ?\') electric-pair-pairs) ;; single quote pairs

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(unless (and (file-exists-p (expand-file-name "elpa/archives/gnu" user-emacs-directory))
             (file-exists-p (expand-file-name "elpa/archives/melpa" user-emacs-directory)))
  (package-refresh-contents))

(defvar my-package-list '(no-littering use-package))

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (message "Pre-installing %s" (symbol-name package))
    (package-install package)))

(dolist (package my-package-list)
  (message "Pre-loading %s" (symbol-name package))
  (require package))

;;;;;;;;;;;;;;;;;
;; Use Package ;;
;;;;;;;;;;;;;;;;;

(defvar use-package-verbose)
(setq use-package-verbose t)
(defvar use-package-always-ensure)
(setq use-package-always-ensure t)

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
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t))

;;;;;;;;;;;;;;;;;;
;; Code Folding ;;
;;;;;;;;;;;;;;;;;;

;; (add-hook 'web-mode-hook 'hs-minor-mode)
(use-package origami
  :config
  (global-origami-mode)
  (global-set-key (kbd "M-o") 'origami-toggle-node))

;;;;;;;;;;;;;;;
;; Mode line ;;
;;;;;;;;;;;;;;;

(use-package doom-modeline
  :config
  (doom-modeline-mode))

;;;;;;;;;;;;;;;
;; Evil Mode ;;
;;;;;;;;;;;;;;;

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq-default evil-cross-lines t)
  (define-key evil-normal-state-map (kbd "C-k") 'move-text-up)
  (define-key evil-normal-state-map (kbd "C-j") 'move-text-down)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-visual-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (evil-mode))

;; (use-package evil-visual-mark-mode
;;   :config
;;   (define-globalized-minor-mode global-evil-visual-mark-mode evil-visual-mark-mode
;;     (lambda () (evil-visual-mark-mode)))
;;   (global-evil-visual-mark-mode))

;; (use-package evil-goggles
;;   :config
;;   (evil-goggles-mode))

;; (use-package evil-string-inflection)

(use-package evil-textobj-column)
(use-package evil-textobj-line)
(use-package evil-textobj-syntax)

;; (use-package evil-nerd-commenter
;;   :config
;;   (evilnc-default-hotkeys))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package move-text)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg))

;; ;; This is ALMOST useful. But not quite.
;; (use-package evil-easymotion
;;   :config
;;   (define-key evil-normal-state-map "f" 'evilem-motion-find-char)
;;   (define-key evil-normal-state-map "F" 'evilem-motion-find-char-backward)
;;   (define-key evil-normal-state-map "t" 'evilem-motion-find-char-to)
;;   (define-key evil-normal-state-map "T" 'evilem-motion-find-char-to-backward))

;;;;;;:;;
;; Avy ;;
;;;;;;;;;

(use-package avy)

;; ;; It's so buggy...
;; (use-package evil-snipe
;;   :config
;;   (define-key evil-normal-state-map "f" 'evil-snipe-f)
;;   (define-key evil-motion-state-map "f" 'evil-snipe-f)
;;   (define-key evil-normal-state-map "F" 'evil-snipe-F)
;;   (define-key evil-motion-state-map "F" 'evil-snipe-F))

(use-package general
  :config
  (defvar general-override-states)
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
  :config
  (diredfl-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion (Company, Auto-Complete) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;;;;;;;;;
;; Ivy ;;
;;;;;;;;;

(use-package counsel
  :config
  (counsel-mode)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "M-p") 'counsel-evil-registers)
  (global-set-key (kbd "C-x C-r") 'counsel-buffer-or-recentf))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x C-j") 'counsel-projectile)
  (define-key projectile-mode-map (kbd "C-x C-g") 'counsel-projectile-rg))

(use-package ivy
  :config
  (ivy-mode)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (defun ivy-resize--minibuffer-setup-hook ()
    (add-hook 'post-command-hook #'ivy-resize--post-command-hook nil t))
  (defun ivy-resize--post-command-hook ()
    (when ivy-mode
      (shrink-window (1+ ivy-height))))
  (add-hook 'minibuffer-setup-hook 'ivy-resize--minibuffer-setup-hook)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-call)
  (define-key counsel-ag-map (kbd "C-n") 'ivy-next-line-and-call)
  (define-key counsel-ag-map (kbd "C-p") 'ivy-previous-line-and-call)
  ;; (define-key counsel-find-file-map (kbd "C-n") 'ivy-next-line-and-call) ;; waste of memory
  ;; (define-key counsel-find-file-map (kbd "C-p") 'ivy-previous-line-and-call) ;; waste of memory
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory))

(use-package ivy-hydra)

(use-package swiper
  :config
  (setq counsel-grep-base-command "rg --no-heading --line-number --color never '%s' %s")
  (global-set-key (kbd "C-s") 'counsel-grep-or-swiper))

;; ;; Package isn't in MELPA
;; (use-package doom-todo-ivy
;;   :hook (after-init . doom-todo-ivy))

;; ;; This doesn't work :(
;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((swiper          . ivy-posframe-display-at-point)
;;           (complete-symbol . ivy-posframe-display-at-point)
;;           (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;           (t               . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; keys
(define-key global-map (kbd "M-k") 'evil-window-up)
(define-key global-map (kbd "M-j") 'evil-window-down)
(define-key global-map (kbd "M-h") 'evil-window-left)
(define-key global-map (kbd "M-l") 'evil-window-right)
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

;;;;;;;;;;;
;; Color ;;
;;;;;;;;;;;

;; (use-package color-theme-solarized
;;   :config
;;   (load-theme 'solarized t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :config
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq flycheck-check-syntax-automatically '(save))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

;;;;;;;;;;
;; Tide ;;
;;;;;;;;;;

(use-package web-mode)

(use-package tide
  :config
  (add-hook 'web-mode-hook #'setup-tide-mode))

(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode +1))

(add-hook 'web-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions
                         'delete-trailing-whitespace)))

;; (use-package prettier-js
;;   :config
;;   (setq prettier-js-args '("--bracket-spacing" "true"
;;                            "---single-quote" "true")))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; (use-package aggressive-indent)

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

;; (use-package magit)

;;;;;;;;;;;
;; iedit ;;
;;;;;;;;;;;

(use-package iedit
  :config
  (global-set-key (kbd "C-x C-e") 'iedit-mode))

;;;;;;;;;;;;;;;;;;
;; Smart Parens ;;
;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :config
  (smartparens-global-mode))

;;;;;;;;;;;;;
;; Rainbow ;;
;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :config
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Highlight Todos ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; (use-package smart-jump
;;   :config
;;   (smart-jump-setup-default-registers))

;;;;;;;;;;;;;;;;
;; Git Gutter ;;
;;;;;;;;;;;;;;;;

(use-package git-gutter
  :config
  (global-git-gutter-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Custom faces... ;;
;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "brightblack" :foreground "red" :weight bold))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face :background "brightblack" :foreground "red"))))
 '(counsel-variable-documentation ((t (:inherit nil))))
 '(evil-goggles-default-face ((t (:inherit region :background "black"))))
 '(evil-snipe-first-match-face ((t (:background "brightblack" :foreground "red" :weight bold))))
 '(evil-snipe-matches-face ((t (:background "brightblack" :foreground "red" :underline t :weight bold))))
 '(flycheck-error ((t (:background "red" :foreground "white" :underline (:color "#ff6655" :style wave)))))
 '(flycheck-warning ((t (:background "red" :foreground "white" :underline (:color "#ECBE7B" :style wave)))))
 '(font-lock-comment-face ((t (:foreground "#525252" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "color-239"))))
 '(helm-buffer-modified ((t (:inherit nil))))
 '(helm-ff-dotted-directory ((t (:foreground "brightblue"))))
 '(helm-ff-symlink ((t (:inherit nil))))
 '(helm-source-header ((t (:background "brightblack" :foreground "#51afef" :weight bold :height 1.0))))
 '(helm-swoop-target-line-face ((t (:foreground "#ECBE7B" :inverse-video t))))
 '(helm-swoop-target-word-face ((t (:inherit bold :background "brightblack" :foreground "red"))))
 '(hl-line ((t (:background "black"))))
 '(ivy-separator ((t (:inherit nil))))
 '(ivy-virtual ((t (:inherit nil :foreground "#ddd"))))
 '(linum ((t (:inherit default :foreground "#a9a1e1" :strike-through nil :underline nil :slant normal :weight normal)))))

(provide '.emacs)
;;; .emacs ends here
