;;;; package --- Summary
;;; Commentary:

;;;;;;;;;;;;;;;;;
;; Performance ;;
;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

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
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "PaleBlue" "magenta" "cyan" "white"])
 '(c-basic-offset 4)
 '(company-idle-delay 0.25)
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-rectangle-modifier-key (quote meta))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(focus-dimness 1)
 '(frame-background-mode (quote dark))
 '(haskell-indentation-cycle-warn nil)
 '(haskell-interactive-mode-eval-mode nil)
 '(helm-completion-window-scroll-margin 5)
 '(helm-default-prompt-display-function (quote evil-collection-helm--set-prompt-display))
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-mode nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(helm-split-window-inside-p t)
 '(helm-swoop-pre-input-function (lambda nil ""))
 '(package-selected-packages
   (quote
    (purescript-mode general tide json-mode evil-collection avy typescript-mode handlebars-mode mustache-mode mustache yaml-mode jsx-mode babel-repl toml-mode slack bundler projectile-rails neotree tabbar ack auto-dim-other-buffers powerline svg-mode-line-themes helm-org-rifle helm-dictionary ac-helm company apt-utils readline-complete bash-completion cargo ac-racer racer smart-mode-line helm-hoogle wiki-summary ac-haskell-process buffer-move eshell-did-you-mean eshell-z multi-term helm-ag go-autocomplete go-mode smex pophint evil-avy grizzl slime evil-surround god-mode evil-tutor helm-cider cider ghc haskell-mode showkey magit evil web-mode wc-mode wc-goal-mode w3m sass-mode pandoc-mode pandoc helm-projectile golden-ratio flycheck flx-isearch fill-column-indicator ergoemacs-mode eh-gnus dired-hacks-utils color-theme-solarized auctex ace-flyspell)))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:inherit popup-face :background "black" :foreground "brightyellow"))))
 '(ac-candidate-mouse-face ((t (:background "black" :foreground "cyan"))))
 '(ac-completion-face ((t (:foreground "brightgreen"))))
 '(ac-selection-face ((t (:inherit popup-menu-selection-face :background "black" :foreground "green"))))
 '(avy-lead-face ((t (:background "#e52b50" :foreground "black"))))
 '(avy-lead-face-0 ((t (:background "#4f57f9" :foreground "black"))))
 '(border ((t nil)))
 '(cursor ((t (:background "#93a1a1" :height 1.0))))
 '(flycheck-error ((t (:inherit error :background "white" :foreground "red" :underline t))))
 '(flycheck-info ((t (:inherit success :background "red" :foreground "white" :underline nil))))
 '(flycheck-warning ((t (:inherit warning :background "yellow" :foreground "white" :underline t))))
 '(font-lock-variable-name-face ((t (:foreground "magenta"))))
 '(helm-bookmark-directory ((t (:inherit nil))))
 '(helm-buffer-directory ((t (:foreground "DarkRed"))))
 '(helm-ff-directory ((t (:background "brightblack" :foreground "green"))))
 '(helm-ff-dotted-directory ((t nil)))
 '(helm-ff-executable ((t (:foreground "red" :weight bold))))
 '(helm-header ((t (:background "brightyellow" :foreground "black"))))
 '(helm-header-line-left-margin ((t nil)))
 '(helm-prefarg ((t (:foreground "green"))))
 '(helm-selection ((t (:inherit region :background "white" :foreground "black" :weight normal))))
 '(helm-source-header ((t (:inherit helm-header :background "brightblack" :foreground "cyan" :weight bold))))
 '(helm-visible-mark ((t (:background "brightblack" :foreground "blue" :weight bold))))
 '(mode-line ((t (:background "black" :foreground "brightyellow" :inverse-video t :box nil))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "brightgreen" :foreground "black" :inverse-video t :box nil))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t nil)))
 '(term-color-white ((t (:background "white" :foreground "white"))))
 '(vertical-border ((t (:background "black" :foreground "brightgreen" :inverse-video nil))))
 '(web-mode-function-call-face ((t (:inherit font-lock-function-name-face))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face :foreground "yellow"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "white"))))
 '(web-mode-html-tag-face ((t (:foreground "white"))))
 '(web-mode-javascript-comment-face ((t (:inherit web-mode-comment-face :foreground "brightgreen"))))
 '(web-mode-javascript-string-face ((t (:inherit web-mode-string-face))))
 '(web-mode-variable-name-face ((t (:inherit default :foreground "magenta")))))

;;;;;;;;;;;;
;; Nudity ;;
;;;;;;;;;;;;

;; (transient-mark-mode t)     ;; show region, drop mark
;; (global-font-lock-mode t)   ;; for all buffers
;; (global-visual-line-mode t) ;; word-wrap
;; (setq shift-select-mode nil) ;; Shift select
;; (show-paren-mode nil)         ;; show matching parentheses
;; (setq initial-scratch-message ";; ^    first non-whitespace
;; ;; mx   set mark
;; ;; 'x   go to mark
;; ;; '.   last changed line
;; ;; zz   center cursor line
;; ;; C-o  previous location
;; ;; C-i  next location
;; ;; #    previous token under cursor
;; ;; *    next token under cursor

;; ;; cc   replace line
;; ;; C    change to end of line
;; ;; D    delete to end of line
;; ;; ci(  change in parens (or anything)
;; ;; cib  change current block
;; ;; I    insert at first non-whitespace")
(setq initial-scratch-message nil)
;; (setq inhibit-startup-screen t)
;; (scroll-bar-mode -1)
(menu-bar-mode -1)
;; (tool-bar-mode 0)
;; (setq visible-bell nil)
;; (setq ring-bell-function 'ignore)
(save-place-mode)
;; (blink-cursor-mode 0)
;; (setq visible-cursor nil)
;; (set-face-inverse-video-p 'vertical-border nil)
;; (setq-default mode-line-format (list (make-string 10 ? )
;;                                      "%b"
;;                                      "%*"
;;                                      (make-string 10 ? )
;;                                      "%l,%c"
;;                                      (make-string 10 ? )))
;; (set-display-table-slot standard-display-table
;;                         'vertical-border
;;                         (make-glyph-code ?|))

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

;;;;;;;;;;;;;;;
;; Evil Mode ;;
;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq-default evil-cross-lines nil)
  (setq-default evil-kill-on-visual-paste nil)
  :config
  (global-evil-surround-mode)
  (evil-collection-init)
  (evil-mode)
  (evil-set-initial-state 'erc-mode 'insert)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-a" 'beginning-of-line)
  (define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-visual-state-map "\C-k" 'kill-line)
  (define-key evil-motion-state-map "\C-k" 'kill-line)
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

;;;;;;:;;
;; Avy ;;
;;;;;;;;;

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
   "SPC" 'avy-goto-char-2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion (Company, Auto-Complete) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

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
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)

  ;; Helm + Projectile
  (helm-projectile-on)
  (projectile-global-mode)
  (global-set-key (kbd "C-x C-d") 'helm-projectile-find-file)
  (global-set-key (kbd "C-x C-g") 'helm-projectile-ag)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-k") 'helm-swoop)
  (global-set-key (kbd "C-x C-l") 'helm-locate)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-mini)

  ;; A lil' performance
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

(use-package helm-ag
  :ensure t)

;;;;;;;;;;;;;;
;; Windmove ;;
;;;;;;;;;;;;;;

(global-set-key (kbd "C-q") 'delete-window)

;; Buffer swaping
(defun buffer-up-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
    (progn (windmove-up)
	   (setq swaped-window (selected-window))
	   (setq swaped-buffer (buffer-name))
	   (if (and (not (string= swaped-buffer current-buffer)))
	       (progn (set-window-buffer swaped-window current-buffer)
		      (set-window-buffer current-window swaped-buffer))))))

(defun buffer-down-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
    (progn (windmove-down)
	   (setq swaped-window (selected-window))
	   (setq swaped-buffer (buffer-name))
	   (if (and (not (string= swaped-buffer current-buffer)))
	       (progn (set-window-buffer swaped-window current-buffer)
		      (set-window-buffer current-window swaped-buffer))))))

(defun buffer-right-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
    (progn (windmove-right)
	   (setq swaped-window (selected-window))
	   (setq swaped-buffer (buffer-name))
	   (if (and (not (string= swaped-buffer current-buffer)))
	       (progn (set-window-buffer swaped-window current-buffer)
		      (set-window-buffer current-window swaped-buffer))))))

(defun buffer-left-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
    (progn (windmove-left)
	   (setq swaped-window (selected-window))
	   (setq swaped-buffer (buffer-name))
	   (if (and (not (string= swaped-buffer current-buffer)))
	       (progn (set-window-buffer swaped-window current-buffer)
		      (set-window-buffer current-window swaped-buffer))))))


;; keys
(define-key global-map (kbd "M-k") 'windmove-up)
(define-key global-map (kbd "M-j") 'windmove-down)
(define-key global-map (kbd "M-h") 'windmove-left)
(define-key global-map (kbd "M-l") 'windmove-right)
(define-key global-map (kbd "M-K") 'evil-window-split)
(define-key global-map (kbd "M-J") 'evil-window-split)
(define-key global-map (kbd "M-H") 'evil-window-vsplit)
(define-key global-map (kbd "M-L") 'evil-window-vsplit)
(define-key global-map (kbd "C-M-j") 'buffer-down-swap)
(define-key global-map (kbd "C-M-k") 'buffer-up-swap)
(define-key global-map (kbd "C-M-h") 'buffer-left-swap)
(define-key global-map (kbd "C-M-l") 'buffer-right-swap)

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

;; (require 'cl)

;;;;;;;;;;;;;;;
;; Solarized ;;
;;;;;;;;;;;;;;;

(use-package color-theme-solarized
  :ensure t
  :init
  (load-theme 'solarized t))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq flycheck-check-syntax-automatically '(save))
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))


;;;;;;;;;;
;; Tide ;;
;;;;;;;;;;

(use-package tide
  :ensure t
  :init
  (add-hook 'web-mode-hook #'setup-tide-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode +1)
  (flycheck-add-next-checker 'typescript-tslint 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (flycheck-disable-checker 'tsx-tide)
  (flycheck-disable-checker 'typescript-eslint))

;;;;;;;;;;;;
;; Etc... ;;
;;;;;;;;;;;;

(setq scroll-error-top-bottom t) ;; Pgdn & Pgup work properly
(setq large-file-warning-threshold 100000) ;; Large file warning
(setq mode-require-final-newline t) ;; Newlines
;; Scrolling
(setq redisplay-dont-pause nil
      scroll-margin 4
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq column-number-mode t) ;; Column numbers in modeline

;; Remove whitespace on save (web-mode + ruby-mode)
(add-hook 'web-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions
			 'delete-trailing-whitespace)))

(delete-selection-mode) ;; Replace selection
(fset 'yes-or-no-p 'y-or-n-p) ;; Changes all yes/no questions to y/n type
(setq create-lockfiles nil) ;; Disable lockfiles in server mode

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ;;; UTF-8 4 lyfe
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; (setq-default indent-tabs-mode nil) ;; soft Tabs
;; (-default tab-width 2)
(electric-pair-mode) ;; electric pair
;; single quotes too
(push '(?\' . ?\') electric-pair-pairs)
