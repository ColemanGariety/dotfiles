;;;;;;;;;;;;
;; Custom ;;
;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.0)
 '(ac-auto-start 1)
 '(ac-delay 0.0)
 '(ac-quick-help-delay 0.0)
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "PaleBlue" "magenta" "cyan" "white"])
 '(c-basic-offset 4)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-rectangle-modifier-key (quote meta))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(dired-hide-details-hide-information-lines nil)
 '(dired-hide-details-hide-symlink-targets t)
 '(diredp-hide-details-initially-flag nil)
 '(focus-dimness 1)
 '(frame-background-mode (quote dark))
 '(haskell-indentation-cycle-warn nil)
 '(haskell-interactive-mode-eval-mode nil)
 '(helm-completion-window-scroll-margin 5)
 '(helm-default-prompt-display-function (quote evil-collection-helm--set-prompt-display))
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-locate-command "locate %s %s")
 '(helm-mode nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(helm-split-window-inside-p t)
 '(helm-swoop-pre-input-function (lambda nil ""))
 '(js-indent-level 2)
 '(js2-mode-show-strict-warnings nil)
 '(jsx-indent-level 4)
 '(jsx-use-auto-complete t)
 '(magit-diff-use-overlays nil)
 '(neo-window-width 35)
 '(org-agenda-files (quote ("~/Org/todo.org")))
 '(org-support-shift-select nil)
 '(package-selected-packages
   (quote
    (json-mode rjsx-mode evil-collection avy typescript-mode handlebars-mode mustache-mode mustache yaml-mode jsx-mode js2-mode babel-repl toml-mode slack bundler ranger projectile-rails helm-swoop neotree tabbar ace-window ack auto-dim-other-buffers powerline svg-mode-line-themes helm-org-rifle helm-dictionary ac-helm company apt-utils readline-complete bash-completion cargo ac-racer racer rust-mode smart-mode-line helm-hoogle wiki-summary ac-haskell-process buffer-move eshell-prompt-extras eshell-did-you-mean eshell-z multi-term helm-ag go-autocomplete go-mode smex pophint evil-avy grizzl slime evil-surround god-mode evil-tutor helm-cider cider ghc haskell-mode showkey magit evil web-mode wc-mode wc-goal-mode w3m sass-mode pandoc-mode pandoc helm-projectile golden-ratio flycheck flx-isearch fill-column-indicator ergoemacs-mode eh-gnus dired-hacks-utils color-theme-solarized auctex ace-flyspell)))
 '(ranger-deer-show-details nil)
 '(ranger-override-dired t)
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
 '(js2-object-property ((t (:inherit default))))
 '(linum ((t (:background "brightblack" :foreground "brightgreen" :underline nil))))
 '(mode-line ((t (:background "black" :foreground "brightyellow" :inverse-video t :box nil))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "brightgreen" :foreground "black" :inverse-video t :box nil))))
 '(org-todo ((t (:background "red" :distant-foreground "red" :foreground "brightblack" :weight bold))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:background "cyan" :foreground "brightblack" :weight bold))))
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

(transient-mark-mode t)     ;; show region, drop mark
(global-font-lock-mode t)   ;; for all buffers
(global-visual-line-mode t) ;; word-wrap
(setq shift-select-mode nil) ;; Shift select
(show-paren-mode t)         ;; show matching parentheses
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode 0)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(save-place-mode 1)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(set-face-inverse-video-p 'vertical-border nil)
(setq-default mode-line-format (list (make-string 10 ?-)
                                     "%b"
                                     "%*"
                                     (make-string 10 ?-)
                                     "%l,%c"
                                     (make-string 10 ?-)
                                     (make-string 1024 ?-)))
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?|))

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

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
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

(require 'evil)
(evil-mode 1)
(evil-collection-init)
(global-evil-surround-mode)
(setq avy-all-windows nil)
(define-key evil-normal-state-map "f" 'avy-goto-char-2)
(define-key evil-motion-state-map "f" 'avy-goto-char-2)
(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
(setq evil-want-C-u-scroll t)

;; ERC
(evil-set-initial-state 'erc-mode 'insert)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Make horizontal movement cross lines
(setq-default evil-cross-lines nil)

;; Controvertsial bindings for eVIl
;; Arguably more like default Unix commands
;; bol, eol, next, prev, etc.
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
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion (Company, Auto-Complete) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;
;; Helm Mode ;;
;;;;;;;;;;;;;;;

(require 'helm-config)

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
(remove-hook 'find-file-hooks 'vc-find-file-hook)

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

(require 'cl)

;;;;;;;;;;;;;;;
;; Solarized ;;
;;;;;;;;;;;;;;;

(load-theme 'solarized t)

;;;;;;;;;;;;
;; Etc... ;;
;;;;;;;;;;;;

;; Pgdn & Pgup work properly
(setq scroll-error-top-bottom t)

;; Large file warning
(setq large-file-warning-threshold 100000)

;; Newlines
(setq mode-require-final-newline t)

;; Scrolling
(setq redisplay-dont-pause t
      scroll-margin 0
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Column numbers in modeline
(setq column-number-mode t)

;; Remove whitespace on save (web-mode + ruby-mode)
(add-hook 'web-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions
			 'delete-trailing-whitespace)))
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions
			 'delete-trailing-whitespace)))

;; Replace selection
(delete-selection-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable lockfiles in server mode
(setq create-lockfiles nil)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; UTF-8 4 lyfe
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Soft Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

					; Auto close brackets
(electric-pair-mode 1)
