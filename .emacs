;;;; package --- Summary
;;; Commentary: no comment :)

;;;;;;;;;;;;;;;;;;;;;;
;; Boot Performance ;;
;;;;;;;;;;;;;;;;;;;;;;

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
;; Nudity ;;
;;;;;;;;;;;;

(transient-mark-mode t)     ;; show region, drop mark
(global-font-lock-mode t)   ;; for all buffers
(global-visual-line-mode t) ;; word-wrap
(setq shift-select-mode nil)
(show-paren-mode t)         ;; show matching parentheses
(setq echo-keystrokes 0.1)
;; (setq initial-scratch-message ";; ^    first non-whitespace
;; 'x   go to mark
;; mx   set mark
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
(setq scroll-conservatively most-positive-fixnum)
;; (global-display-line-numbers-mode)
;; (global-hl-line-mode)
(menu-bar-mode -1)
(tool-bar-mode 0)
(setq visible-bell nil)
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(setq ring-bell-function 'ignore)
(save-place-mode)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(set-face-inverse-video 'vertical-border nil)
(setq scroll-error-top-bottom t) ;; Pgdn & Pgup work properly
(setq large-file-warning-threshold 100000) ;; Large file warning
(setq mode-require-final-newline t) ;; Newlines
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
(setq-default indent-tabs-mode nil) ;; soft Tabs
(prefer-coding-system 'utf-8)
;; inhibit "when done with this frame..." message
(add-hook 'after-make-frame-functions
          (lambda (&optional frame)
            (with-selected-frame (or frame (selected-frame))
              (setq inhibit-message t)
              (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))))

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

(defvar use-package-verbose)
(setq use-package-verbose t)
(defvar use-package-always-ensure)
(setq use-package-always-ensure t)

(use-package paradox
  :config
  (paradox-enable))

;;;;;;;;;;
;; Evil ;;
;;;;;;;;;;

(defun move-line-or-region (arg)
  "ARG: See https://github.com/syl20bnr/spacemacs/issues/5365."
  (interactive "P")
  (if (or (not arg) (>= arg 0))
      (let ((reg-or-lin (if (region-active-p) "'>" "."))
            (reactivate-region (if (region-active-p) "gv=gv" ""))
            (num (if arg arg 1)))
        (execute-kbd-macro
         (concat ":m" reg-or-lin "+" (number-to-string num) (kbd "RET") reactivate-region)))
    (backward-move-line-or-region (- arg))))

(defun backward-move-line-or-region (arg)
  "ARG: See https://github.com/syl20bnr/spacemacs/issues/5365."
  (interactive "P")
  (if (or (not arg) (>= arg 0))
      (let ((reg-or-lin (if (region-active-p) "'<" "."))
            (reactivate-region (if (region-active-p) "gv=gv" ""))
            (num (if arg (+ arg 1) 2)))
        (execute-kbd-macro
         (concat ":m" reg-or-lin "-" (number-to-string num) (kbd "RET") reactivate-region)))
    (move-line-or-region (- arg))))

(use-package evil
  :preface
  (setq evil-want-keybinding    nil)
  (setq evil-cross-lines        t)
  (setq evil-symbol-word-search t)
  (setq evil-ex-interactive-search-highlight 'selected-window)
  (setq-default evil-kill-on-visual-paste nil) ;; doesn't work :(
  :config
  (define-key evil-normal-state-map (kbd "M-K") 'backward-move-line-or-region)
  (define-key evil-normal-state-map (kbd "M-J") 'move-line-or-region)
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

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
         (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (when switch-buffer-alist
      (setcar switch-buffer-alist '+ivy-rich-buffer-name)))
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)
  (ivy-rich-mode +1))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :after evil
    :ensure t
    :init
    (setq evil-normal-state-cursor 'box)
    (setq evil-motion-state-cursor 'box)
    (setq evil-visual-state-cursor 'hollow) ;; does not work in kitty
    (setq evil-insert-state-cursor 'bar)
    (setq evil-emacs-state-cursor  'hbar)
    :config
    (etcc-on)))

;; (use-package evil-visual-mark-mode
;;   :config
;;   (define-globalized-minor-mode global-evil-visual-mark-mode evil-visual-mark-mode
;;     (lambda () (evil-visual-mark-mode)))
;;   (global-evil-visual-mark-mode))

;; (use-package evil-goggles
;;   :config
;;   (evil-goggles-mode))

;; (use-package evil-string-inflection)

;; (use-package rotate-text
;;   :load-path "~/Git/rotate-text.el"
;;   :init
;;   (autoload 'rotate-text "rotate-text" nil t)
;;   (autoload 'rotate-text-backward "rotate-text" nil t)
;;   (push '("true" "false") rotate-text-words))

(use-package evil-textobj-column)
(use-package evil-textobj-line)
(use-package evil-textobj-syntax)
(use-package evil-indent-plus)
(use-package evil-embrace)
(use-package exato)
;; (use-package targets
;;   :load-path "~/Git/targets.el/targets.el"
;;   :init
;;   (defvar targets-user-text-objects)
;;   (setq targets-user-text-objects '((pipe "|" nil separator)
;;                                     (paren "(" ")" pair :more-keys "b")
;;                                     (bracket "[" "]" pair :more-keys "r")
;;                                     (curly "{" "}" pair :more-keys "c")))
;;   :config
;;   (targets-setup t
;;                  :inside-key nil
;;                  :around-key nil
;;                  :remote-key nil))

;; Awesome!
(use-package evil-exchange
  :config
  (evil-exchange-install))

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

;; (use-package evil-args
;;   :config
;;   (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
;;   (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;;   (define-key evil-normal-state-map "L" 'evil-forward-arg)
;;   (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;   (define-key evil-motion-state-map "L" 'evil-forward-arg)
;;   (define-key evil-motion-state-map "H" 'evil-backward-arg)
;;   (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;   (define-key evil-motion-state-map "L" 'evil-forward-arg))

;;;;;;;;;;;;;;;;
;; Which key? ;;
;;;;;;;;;;;;;;;;

;; This is so awesome.
(use-package which-key
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;
;; Auto-modes ;;
;;;;;;;;;;;;;;;;

(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
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
  (unless after-init-time
    ;; prevent flash of unstyles modeline at startup
    (setq-default mode-line-format nil))
  (size-indication-mode +1)
  (column-number-mode +1) ;; Column numbers in modeline
  (doom-modeline-mode))

(use-package evil-anzu
  :after evil)

;;;;;;;;;;;;;;;;;;;
;; Expand region ;;
;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :config
  (global-set-key (kbd "C-u") 'er/expand-region))

;; ;; broken
;; (use-package highlight-indent-guides
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-auto-enabled nil)
;;   (highlight-indent-guides-auto-set-faces)
;;   (highlight-indent-guides-mode))

;; ;; can't use glyphs :/
;; (use-package highlight-indentation
;;   :config
;;   (set-face-attribute 'highlight-indentation-face nil
;;                       :stipple (list 7 4 (string 16 0 0 0))
;;                       :inherit nil)

;;   (set-face-attribute 'highlight-indentation-current-column-face nil
;;                       :stipple (list 7 4 (string 16 0 0 0))
;;                       :inherit nil
;;                       :foreground "yellow")
;;   (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;; ;; moves with cursor ???
;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode))

;;;;;;:;;
;; Avy ;;
;;;;;;;;;

;; (use-package avy
;;   :config
;;   (define-key evil-normal-state-map (kbd "SPC") 'evil-avy-goto-char-2)
;;   (define-key evil-visual-state-map kbd "SPC") 'evil-avy-goto-char-2)
;;   (define-key evil-motion-state-map (kbd "SPC") 'evil-avy-goto-char-2))

(use-package general
  :ensure t
  :init
  (defvar general-override-states)
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-space/body
   ;; there can be no better config
   "f" 'evilem-motion-find-char
   "F" 'evilem-motion-find-char-backward
   "t" 'evilem-motion-find-char-to
   "T" 'evilem-motion-find-char-to-backward))

;; (use-package evil-easymotion
;;   :config
;;   (define-key evil-normal-state-map (kbd "f") 'evilem-motion-find-char)
;;   (define-key evil-normal-state-map (kbd "F") 'evilem-motion-find-char-backward)
;;   (define-key evil-normal-state-map (kbd "t") 'evilem-motion-find-char-to)
;;   (define-key evil-normal-state-map (kbd "T") 'evilem-motion-find-char-to-backward)
;;   (define-key evil-visual-state-map (kbd "f") 'evilem-motion-find-char)
;;   (define-key evil-visual-state-map (kbd "F") 'evilem-motion-find-char-backward)
;;   (define-key evil-visual-state-map (kbd "t") 'evilem-motion-find-char-to)
;;   (define-key evil-visual-state-map (kbd "T") 'evilem-motion-find-char-to-backward))

;; ;; can't get it the way I want ...
;; (use-package evil-snipe
;;   :config
;;   (define-key evil-normal-state-map "f" 'evil-snipe-f)
;;   (define-key evil-motion-state-map "f" 'evil-snipe-f)
;;   (define-key evil-normal-state-map "F" 'evil-snipe-F)
;;   (define-key evil-motion-state-map "F" 'evil-snipe-F))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(use-package diredfl
  :config
  (diredfl-global-mode))

;; (use-package dired-subtree)

;;;;;;;;;;;;;;;;;
;; Completion  ;;
;;;;;;;;;;;;;;;;;

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 14)
  (defvar company-dabbrev-downcase)
  (setq company-dabbrev-downcase nil)
  (defvar company-dabbrev-ignore-case)
  (setq company-dabbrev-ignore-case nil)
  (defvar company-dabbrev-code-other-buffers)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  (setq company-idle-delay 0.05)
  (defvar company-statistics-size)
  (setq company-statistics-size 600)
  (setq company-backends '(company-capf))
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  :config
  ;; don't persist company when switching back to normal mode
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package prescient)
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode +1))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package company-dict
  :defer t
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (add-to-list 'company-backends 'company-dict)
  (define-key evil-insert-state-map (kbd "C-x C-k") 'company-dict))

;;;;;;;;;
;; Ivy ;;
;;;;;;;;;

(use-package counsel
  :config
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x RET") 'counsel-evil-marks)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "M-p") 'counsel-evil-registers)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-r") 'counsel-buffer-or-recentf)
  (counsel-mode))

;; (use-package ibuffer-vc)

(use-package counsel-projectile
  :init
  (setq counsel-projectile-sort-files t)
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x C-j") 'counsel-projectile)
  (define-key projectile-mode-map (kbd "C-x C-g") 'counsel-projectile-rg))

(use-package ivy
  :init
  (setq ivy-wrap t)
  (setq projectile-completion-system 'ivy)
  (setq ivy-magic-slash-non-match-action nil)
  (setq ivy-on-del-error-function #'ignore)
  (setq ivy-use-selectable-prompt t)
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line)
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

(use-package flx
  :init
  (setq ivy-flx-limit 10000)
  :config
  ;; fuzzy matching in ivy
  (setq ivy-re-builders-alist
        '((counsel-find-file  . ivy--regex-fuzzy)
          (counsel-projectile . ivy--regex-fuzzy)
          (t                  . ivy--regex-plus))))

(use-package ivy-historian
  :after ivy
  :config
  (historian-mode +1)
  (ivy-historian-mode +1))

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (setq swiper-action-recenter t))
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


;;;;;;;;;;;;;;;;
;; PureScript ;;
;;;;;;;;;;;;;;;;

(use-package psc-ide)
(use-package purescript-mode)

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-display-errors-delay 0.25)
  :config
  (global-flycheck-mode +1)
  ;; disable jshint since we prefer eslint checking
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

(use-package git-timemachine)

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

;;;;;;;;;
;; RSS ;;
;;;;;;;;;

(use-package elfeed
  :config
  (setq elfeed-feeds
        '("https://oremacs.com/atom.xml"
          "https://medium.com/feed/@gcanti")))

;;;;;;;;;
;; IRC ;;
;;;;;;;;;

;; ;; I'm using hexchat
;; (use-package circe
;;   :init
;;   (setq circe-network-options
;;         '(("Freenode"
;;            :tls t
;;            :nick "clmg"
;;            :sasl-username "clmg"))))

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
 '(compilation-message-face (quote default))
 '(counsel-rg-base-command
   "rg -M 120 --glob !yarn.lock --with-filename --no-heading --line-number --color never %s")
 '(css-indent-offset 2)
 '(doom-modeline-buffer-encoding nil)
 '(doom-modeline-github nil)
 '(doom-modeline-icon nil)
 '(flycheck-temp-prefix ".flycheck")
 '(focus-dimness nil)
 '(frame-background-mode (quote dark))
 '(indent-guide-char "│")
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(package-selected-packages
   (quote
    (circe ivy-rich company-dict company-tng company-flx simpleclip elfeed highlight-indentation indent-guide vim-empty-lines-mode purescript-mode ibuffer-vc git-timemachine ibuffer-projectile general exato highlight-indent-guides rotate-text evil-embrace evil-indent-plus exacto ranger so-long evil-terminal-cursor-changer expand-region evil-exchange which-key paradox ivy-historian company-statistics selectrum ivy-prescient number-lock evil-swap-keys evil-easymotion evil-textobj-column origami evil-textobj-syntax evil-textobj-line evil-snipe ivy-smex doom-todo-ivy ivy-posframe counsel-projectile counsel ivy-mode evil-args evil-matchit iedit smartparens hl-todo psc-ide easymotion diredfl company-prescient move-text rainbow-delimiters doom-themes one-dark-theme doom-modeline auto-compile spaceline-config tide json-mode evil-collection avy handlebars-mode mustache-mode mustache yaml-mode jsx-mode babel-repl toml-mode slack bundler projectile-rails neotree tabbar ack auto-dim-other-buffers svg-mode-line-themes company apt-utils readline-complete bash-completion cargo ac-racer racer smart-mode-line wiki-summary ac-haskell-process buffer-move eshell-did-you-mean eshell-z multi-term go-autocomplete go-mode smex pophint evil-avy slime evil-surround god-mode evil-tutor cider ghc haskell-mode showkey magit evil web-mode wc-mode wc-goal-mode w3m sass-mode pandoc-mode pandoc golden-ratio flycheck flx-isearch fill-column-indicator ergoemacs-mode eh-gnus dired-hacks-utils no-littering use-package)))
 '(paradox-github-token t)
 '(projectile-enable-caching t)
 '(show-paren-delay 0.0)
 '(showkey-log-mode nil)
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
 '(avy-lead-face ((t (:background "inherit" :foreground "red" :weight bold))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face :background "inherit" :foreground "#FF6666"))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face :background "inherit" :foreground "#FF9999"))))
 '(counsel-variable-documentation ((t (:inherit nil))))
 '(doom-modeline-bar-inactive ((t nil)))
 '(doom-modeline-battery-normal ((t (:inherit mode-line :background "black" :weight normal))))
 '(evil-goggles-default-face ((t (:inherit region :background "black"))))
 '(flycheck-error ((t (:background "red" :foreground "white" :underline (:color "#ff6655" :style wave)))))
 '(flycheck-warning ((t (:background "red" :foreground "white" :underline (:color "#ECBE7B" :style wave)))))
 '(font-lock-comment-face ((t (:foreground "#525252" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "color-239"))))
 '(highlight-indent-guides-character-face ((t (:foreground "black"))))
 '(highlight-indent-guides-even-face ((t (:foreground "black"))))
 '(highlight-indent-guides-odd-face ((t (:foreground "brightgreen"))))
 '(hl-line ((t (:background "black"))))
 '(indent-guide-face ((t (:inherit hl-line :background "brightblack" :foreground "black"))))
 '(ivy-separator ((t (:inherit nil))))
 '(ivy-virtual ((t (:inherit nil :foreground "#ddd"))))
 '(line-number ((t (:inherit default :foreground "#073642" :strike-through nil :underline nil :slant normal :weight normal))))
 '(line-number-current-line ((t (:inherit (hl-line defaultblack) :foreground "#657b83" :strike-through nil :underline nil :slant normal :weight normal))))
 '(linum ((t (:inherit default :foreground "#a9a1e1" :strike-through nil :underline nil :slant normal :weight normal))))
 '(mode-line-inactive ((t (:background "black" :foreground "#525252" :box nil))))
 '(vim-empty-lines-face ((t (:inherit font-lock-comment-face)))))

(provide '.emacs)
;;; .emacs ends here
