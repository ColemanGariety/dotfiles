;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(avy-background t)
;;  '(ccm-recenter-at-end-of-file nil)
;;  '(comment-auto-fill-only-comments t)
;;  '(comment-column 32)
;;  '(comment-multi-line t)
;;  '(company-begin-commands
;;    '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash))
;;  '(company-idle-delay 0)
;;  '(company-require-match 'never)
;;  '(company-tng-mode t)
;;  '(evil-undo-system 'undo-tree)
;;  '(js-jsx-syntax t)
;;  '(package-selected-packages
;;    '(doom-modeline eshell-syntax-highlighting yaml-mode flycheck rjsx-mode rjxs-mode typescript-mode tsi-css-mode tsi-css tree-sitter-langs tree-sitter origami exec-path-from-shell purescript-mode reason-mode horizon-theme evil-anzu doom-themes undo-tree dashboard xterm-title html-mode prettier-js gcmh git-timemachine highlight-numbers hl-todo lsp-ui lsp-mode smex counsel-projectile projectile counsel company general evil-easymotion avy json-mode which-key evil-surround evil-collection evil-terminal-cursor-changer evil use-package no-littering))
;;  '(paradox-github-token t)
;;  '(web-mode-comment-formats
;;    '(("java" . "/*")
;;      ("javascript" . "//")
;;      ("typescript" . "//")
;;      ("php" . "/*")
;;      ("css" . "/*")) t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "unspecified-bg" :foreground "#c7c9cb" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
;;  '(ac-candidate-face ((t (:inherit popup-face :background "black" :foreground "#073642"))))
;;  '(ac-completion-face ((t (:foreground "color-239" :underline nil))))
;;  '(ac-selection-face ((t (:inherit popup-menu-selection-face :background "black" :foreground "#2aa198"))))
;;  '(avy-background-face ((t (:foreground "#525252"))))
;;  '(avy-lead-face ((t (:background "inherit" :foreground "red" :weight bold))))
;;  '(avy-lead-face-0 ((t (:inherit avy-lead-face :background "inherit" :foreground "#FF6666"))))
;;  '(avy-lead-face-1 ((t (:inherit avy-lead-face :foreground "brightblack"))))
;;  '(avy-lead-face-2 ((t (:inherit avy-lead-face :background "inherit" :foreground "#FF9999"))))
;;  '(company-echo-common ((t (:foreground "blue"))))
;;  '(company-preview-common ((t (:inherit company-preview :foreground "blue"))))
;;  '(company-tooltip ((t (:inherit tooltip :background "black" :foreground "white"))))
;;  '(company-tooltip-annotation ((t (:foreground "yellow"))))
;;  '(company-tooltip-common ((t (:foreground "blue"))))
;;  '(company-tooltip-scrollbar-thumb ((t (:background "cyan"))))
;;  '(company-tooltip-scrollbar-track ((t (:inherit tooltip :background "black"))))
;;  '(company-tooltip-selection ((t (:background "brightblack" :foreground "brightcyan" :weight bold))))
;;  '(counsel-variable-documentation ((t (:inherit nil))))
;;  '(custom-button-unraised ((t (:background "unspecified" :foreground "#b877db" :box (:line-width (1 . 1) :style none)))))
;;  '(doom-modeline-bar-inactive ((t nil)))
;;  '(doom-modeline-battery-normal ((t (:inherit mode-line :background "black" :weight normal))))
;;  '(doom-modeline-debug-visual ((t (:background "#073642"))))
;;  '(error ((t (:foreground "red" :weight bold))))
;;  '(fill-column-indicator ((t (:inherit shadow :stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
;;  '(flycheck-error ((t (:underline t))))
;;  '(flycheck-info ((t (:underline t))))
;;  '(flycheck-warning ((t (:background "red" :foreground "white" :underline (:color "#ECBE7B" :style wave)))))
;;  '(font-lock-comment-face ((t (:foreground "#525252" :slant italic))))
;;  '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "color-239"))))
;;  '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
;;  '(font-lock-string-face ((t (:foreground "magenta"))))
;;  '(font-lock-type-face ((t (:foreground "magenta"))))
;;  '(font-lock-variable-name-face ((t (:foreground "blue"))))
;;  '(highlight ((t (:background "color-89" :foreground "black"))))
;;  '(highlight-blocks-depth-1-face ((t (:background "black"))))
;;  '(highlight-indent-guides-character-face ((t (:foreground "black"))))
;;  '(highlight-indent-guides-even-face ((t (:foreground "black"))))
;;  '(highlight-indent-guides-odd-face ((t (:foreground "brightgreen"))))
;;  '(highlight-numbers-number ((t (:inherit font-lock-constant-face :foreground "yellow"))))
;;  '(hl-line ((t (:extend t :background "color-236"))))
;;  '(indent-guide-face ((t (:inherit hl-line :background "brightblack" :foreground "black"))))
;;  '(ivy-current-match ((t (:extend t :background "color-24" :foreground "#bfbfbf"))))
;;  '(ivy-minibuffer-match-face-1 ((t (:background "magenta" :foreground "black"))))
;;  '(ivy-minibuffer-match-face-2 ((t (:background "color-125" :foreground "black" :weight bold))))
;;  '(ivy-minibuffer-match-face-3 ((t (:background "#7777ff" :foreground "black" :weight bold))))
;;  '(ivy-separator ((t (:inherit nil))))
;;  '(ivy-virtual ((t (:inherit nil :foreground "#ddd"))))
;;  '(line-number ((t (:inherit default :background "unspecified" :foreground "color-237" :strike-through nil :underline nil :slant normal :weight normal))))
;;  '(line-number-current-line ((t (:inherit (hl-line default) :background "color-236" :foreground "brightcyan" :strike-through nil :underline nil :slant normal :weight normal))))
;;  '(linum ((t (:inherit default :foreground "#a9a1e1" :strike-through nil :underline nil :slant normal :weight normal))))
;;  '(lsp-face-highlight-read ((t (:background "color-25" :foreground "#dfdfdf" :weight bold))))
;;  '(lsp-face-highlight-textual ((t (:inherit highlight :background "color-25" :foreground "#dfdfdf" :weight bold))))
;;  '(lsp-ui-doc-background ((t (:inherit tooltip))))
;;  '(lsp-ui-peek-filename ((t (:inherit mode-line-buffer-id :background "black"))))
;;  '(lsp-ui-peek-footer ((t (:inherit lsp-ui-peek-header :background "black"))))
;;  '(lsp-ui-peek-header ((t (:background "black" :foreground "#bfbfbf" :weight bold))))
;;  '(lsp-ui-peek-list ((t (:background "black"))))
;;  '(lsp-ui-peek-peek ((t (:background "black"))))
;;  '(lsp-ui-peek-selection ((t (:background "black" :foreground "cyan" :weight bold))))
;;  '(markdown-blockquote-face ((t (:inherit italic :foreground "color-240"))))
;;  '(markdown-footnote-marker-face ((t (:inherit markdown-markup-face :foreground "cyan"))))
;;  '(markdown-markup-face ((t (:foreground "white"))))
;;  '(mode-line ((t (:background "#222" :foreground "#bfbfbf" :box nil))))
;;  '(mode-line-inactive ((t (:background "#222" :foreground "#525252" :box nil))))
;;  '(popup-scroll-bar-background-face ((t (:background "black"))))
;;  '(popup-scroll-bar-foreground-face ((t (:background "brightyellow"))))
;;  '(region ((t (:extend t :background "color-248" :foreground "black"))))
;;  '(rust-unsafe-face ((t (:inherit font-lock-error-face :foreground "brightred"))))
;;  '(show-paren-match ((t (:background "black" :foreground "#ff6655"))))
;;  '(tooltip ((t (:inherit variable-pitch :background "black" :foreground "black"))))
;;  '(vertical-border ((t (:background "color-234" :foreground "color-234" :inverse-video nil))))
;;  '(warning ((t (:foreground "yellow" :weight bold))))
;;  '(web-mode-html-tag-face ((t (:foreground "cyan"))))
;;  '(window-divider ((t (:inherit nil :background "black" :foreground "black")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default))
 '(package-selected-packages
   '(lsp-ui lsp-mode yaml-mode doom-modeline company-ctags which-key use-package undo-tree typescript-mode tree-sitter-langs smex rjsx-mode reason-mode purescript-mode prettier-js origami no-littering json-mode horizon-theme hl-todo highlight-numbers git-timemachine general gcmh flycheck exec-path-from-shell evil-terminal-cursor-changer evil-surround evil-easymotion evil-collection evil-anzu doom-themes dashboard counsel-projectile company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#00212B" :box nil))))
 '(vertical-border ((t (:background "#00212B" :foreground "#00212B")))))
