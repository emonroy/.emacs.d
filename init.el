;; .emacs.d by E. Monroy
;;
;; Dependencies ----------------------------------------------------------------
;; ag, tern
;;
;; Custom bindings -------------------------------------------------------------
;; undo                      -- C-z       -- undo
;; windmove                  -- M-<up>    -- move to frame above
;;                           -- M-<right> -- move to frame on the right
;;                           -- M-<down>  -- move to frame below
;;                           -- M-<left>  -- move to frame on the left
;; expand-region             -- C-+       -- expand region
;;                           -- C--       -- decrease region (after expanding)
;; ace-jump-mode             -- C-c SPC   -- enables ace jump mode
;; default-text-scale        -- C-c +     -- increases text size
;;                           -- C-c -     -- decreases text size
;; smex                      -- M-x       -- smex M-x command
;; helm-M-x                  -- C-c x     -- helm M-x command
;; helm-mini                 -- C-c b     -- helm buffer selection
;; helm-do-ag                -- C-c a     -- search in directory
;; helm-do-ag-project-root   -- C-c r     -- search in project root
;; helm-swoop                -- C-c s     -- search in buffer
;; helm-multi-swoop-all      -- C-c S     -- search in all buffers
;; helm-projectile-find-file -- C-c f     -- search file in project

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package emacs-config
  :init (provide 'emacs-config)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (winner-mode)
  (line-number-mode)
  (column-number-mode)
  (electric-pair-mode)
  (show-paren-mode)
  (global-hl-line-mode)
  (delete-selection-mode)
  (ido-mode)
  (global-auto-revert-mode t)
  (setq inhibit-startup-screen t
        make-backup-files nil
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package paradox
  :ensure t
  :config
  (setq paradox-github-token -1))

(use-package bind-key
  :ensure t
  :bind (("C-z" . undo)
         ("M-<up>" . windmove-up)
         ("M-<right>" . windmove-right)
         ("M-<down>" . windmove-down)
         ("M-<left>" . windmove-left)))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq whitespace-style '(empty trailing tab-mark)
        whitespace-action '(auto-cleanup))
  (global-whitespace-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 't)
  (smooth-scrolling-mode))

(use-package expand-region
  :ensure t
  :bind (("C-+" . er/expand-region)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package default-text-scale
  :ensure t
  :bind (("C-c +" . default-text-scale-increase)
         ("C-c -" . default-text-scale-decrease)))

(use-package golden-ratio
  :ensure t
  :disabled t
  :diminish golden-ratio-mode
  :config
  (setq golden-ratio-auto-scale t)
  (defun pl/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
  (add-hook 'prog-mode-hook 'golden-ratio-mode))

(use-package yascroll
  :ensure t
  :config
  (setq-default yascroll:delay-to-hide nil)
  (global-yascroll-bar-mode))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

(use-package flx-ido
  :ensure t
  :config
  (setq ido-enable-dot-prefix t
        ido-enable-flex-matching t)
  (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :ensure t
  :config
  (setq ido-everywhere t)
  (ido-ubiquitous-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
  (setq yas-also-auto-indent-first-line t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  (setq ac-auto-show-menu nil))

(use-package column-enforce-mode
  :disabled t
  :ensure t
  :diminish column-enforce-mode
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (defun c++11-mode-hook ()
    (setq flycheck-gcc-language-standard "c++11"))
  (add-hook 'c++-mode-hook 'c++11-mode-hook)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package helm
  :ensure t
  :bind (("C-c x" . helm-M-x)
         ("C-c b" . helm-mini))
  :config
  (setq-default helm-split-window-in-side-p t
                helm-M-x-fuzzy-match t))

(use-package ag
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("C-c a" . helm-do-ag)
         ("C-c r" . helm-do-ag-project-root))
  :config
  (setq helm-ag-command-option "--ignore-case"
        helm-ag-use-agignore t
        helm-ag-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . helm-swoop-multiline-4)
         ("C-c S" . helm-multi-swoop-all))
  :config
  (defun helm-swoop-multiline-4 ()
    (interactive)
    (setq current-prefix-arg 4)
    (helm-swoop))
  (setq helm-swoop-speed-or-color t
        helm-swoop-move-to-line-cycle nil
        helm-swoop-use-line-number-face t
        helm-swoop-split-with-multiple-windows t
        helm-swoop-pre-input-function (lambda () "")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package helm-projectile
  :ensure t
  :bind (("C-c f" . helm-projectile-find-file))
  :config
  (setq projectile-completion-system 'helm
        projectile-indexing-method 'alien)
  (add-hook 'projectile-mode-hook 'helm-projectile-on))

(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq-default web-mode-engines-alist '(("handlebars" . "\\.html?\\'"))
                web-mode-css-indent-offset 4
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 4
                web-mode-style-padding 2
                web-mode-script-padding 2
                web-mode-block-padding 0
                web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                            ("html" . (ac-source-words-in-buffer
                                                       ac-source-abbrev)))))

(use-package ac-html
  :ensure t
  :config  (require 'web-mode)
  (add-hook 'web-mode-hook 'ac-html-enable)
  (add-to-list 'web-mode-ac-sources-alist '("html" . (ac-source-html-attribute-value
                                                      ac-source-html-tag
                                                      ac-source-html-attribute
                                                      ac-source-words-in-buffer
                                                      ac-source-abbrev))))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less\\'"
         "\\.css\\'")
  :config
  (add-hook 'less-css-mode-hook 'ac-css-mode-setup))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq-default lua-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("$"
                                     "jQuery"
                                     "_"
                                     "Backbone"
                                     "hello"
                                     "Whizz"
                                     "Namespace"
                                     "Constants"
                                     "Messages"
                                     "Api"
                                     "Config"
                                     "Utils"
                                     "GameEvents"))
  (setq js2-include-node-externs t
        js2-strict-trailing-comma-warning nil))

(use-package tern
  :ensure t
  :diminish tern-mode
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package tern-auto-complete
  :ensure t
  :config
  (tern-ac-setup))

(use-package jquery-doc
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'jquery-doc-setup))

(use-package dummy-h-mode
  :ensure t
  :mode "\\.h\\'"
  :config
  (setq dummy-h-mode-default-major-mode 'c++-mode))

(use-package c-mode-config
  :init (provide 'c-mode-config)
  :config
  (setq c-default-style "k&r"
        c-basic-offset 4))

(use-package c++-mode-config
  :init (provide 'c++-mode-config)
  :config
  (c-set-offset 'inline-open 0))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package theme-changer
  :ensure t
  :config
  ;; (setq calendar-location-name "Spain"
  ;;       calendar-latitude 38.68
  ;;       calendar-longitude -4.1)
  (setq calendar-location-name "San Francisco, CA"
        calendar-latitude 37.47
        calendar-longitude -112.25)
  (change-theme 'sanityinc-tomorrow-blue 'sanityinc-tomorrow-bright))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-enforce-face ((t (:underline (:color "#e78c45" :style wave)))))
 '(helm-swoop-line-number-face ((t (:foreground "DarkOrange1"))))
 '(helm-swoop-target-line-block-face ((t (:inherit highlight))))
 '(helm-swoop-target-line-face ((t (:inherit higlight))))
 '(helm-swoop-target-word-face ((t (:foreground "gold" :underline t))))
 '(js2-external-variable ((t (:inherit error :foreground nil))))
 '(yascroll:thumb-fringe ((t (:background "gold" :foreground "gold"))))
 '(yascroll:thumb-text-area ((t (:background "gold" :foreground "black")))))

(provide '.init)
