;; .emacs.d by E. Monroy
;;
;; Dependencies ----------------------------------------------------------------
;; ag
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
;; auto-complete             -- <tab>     -- auto complete
;; yas-expand                -- C-<tab>   -- expand snippet

;; Package management ----------------------------------------------------------

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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package paradox
  :ensure t
  :config
  (setq-default paradox-github-token -1))

;; Themes ----------------------------------------------------------------------

(defun current-hour ()
  (string-to-number (substring (current-time-string) 11 13)))

(defun is-daylight-hour (hour)
  (when (member hour (number-sequence 6 18))
    t))

(use-package solarized-theme
  :ensure t
  :config
  (setq-default solarized-distinct-fringe-background t
                solarized-high-contrast-mode-line t)

  (if (is-daylight-hour (current-hour))
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

;; Basic configuration ---------------------------------------------------------

(defun display-width ()
  (if window-system
      (x-display-pixel-width)
    0))

(defun is-hd-display (width)
  (when (>= width 1920)
    t))

(use-package emacs-config
  :init
  (provide 'emacs-config)
  :config
  (setq-default inhibit-startup-screen t
                custom-file "~/.emacs.d/custom.el"
                make-backup-files nil
                dired-listing-switches "-alh"
                initial-scratch-message nil
                initial-major-mode 'text-mode
                scroll-conservatively 101
                mouse-wheel-scroll-amount '(1)
                mouse-wheel-progressive-speed nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load custom-file t)

  (unless (is-hd-display (display-width))
    (set-face-attribute 'default nil
                        :height 100))

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
  (global-auto-revert-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq-default indent-tabs-mode nil
                whitespace-style '(empty trailing tab-mark)
                whitespace-action '(auto-cleanup))

  (global-whitespace-mode))

(use-package yascroll
  :ensure t
  :config
  (setq-default yascroll:delay-to-hide nil)

  (global-yascroll-bar-mode))

(use-package bind-key
  :ensure t
  :bind (("C-z" . undo)
         ("M-SPC" . cycle-spacing)
         ("M-<up>" . windmove-up)
         ("M-<right>" . windmove-right)
         ("M-<down>" . windmove-down)
         ("M-<left>" . windmove-left)))

(use-package anzu
  :ensure t
  :demand t
  :diminish anzu-mode
  :bind (("C-c %" . anzu-query-replace))
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :inherit 'mode-line
                      :foreground nil)

  (global-anzu-mode))

(use-package expand-region
  :ensure t
  :bind (("C-+" . er/expand-region)))

(use-package default-text-scale
  :ensure t
  :bind (("C-c +" . default-text-scale-increase)
         ("C-c -" . default-text-scale-decrease)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

;; Ido -------------------------------------------------------------------------

(use-package flx-ido
  :ensure t
  :config
  (setq-default ido-enable-dot-prefix t
                ido-enable-flex-matching t)

  (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq-default ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

  (ido-vertical-mode))

(use-package ido-ubiquitous
  :ensure t
  :config
  (setq-default ido-everywhere t)

  (ido-ubiquitous-mode))

;; Helm   ----------------------------------------------------------------------

(use-package helm
  :ensure t
  :bind (("C-c x" . helm-M-x)
         ("C-c b" . helm-mini))
  :config
  (setq-default helm-split-window-in-side-p t
                helm-M-x-fuzzy-match t
                helm-candidate-number-limit 10)

  (set-face-attribute 'helm-selection nil
                      :underline nil)

  (helm-autoresize-mode))

(use-package helm-ag
  :ensure t
  :bind (("C-c a" . helm-do-ag)
         ("C-c r" . helm-do-ag-project-root))
  :config
  (setq-default helm-ag-command-option "--ignore-case"
                helm-ag-use-agignore t
                helm-ag-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . helm-swoop-multiline-4)
         ("C-c S" . helm-multi-swoop-all))
  :config
  (defun helm-swoop-pre-input-function-empty ()
    "")
  (setq-default helm-swoop-move-to-line-cycle nil
                helm-swoop-split-with-multiple-windows t
                helm-swoop-pre-input-function 'helm-swoop-pre-input-function-empty)

  (defun helm-swoop-multiline-4 ()
    (interactive)
    (setq current-prefix-arg 4)
    (helm-swoop)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c f" . helm-projectile-find-file)))

;; Minor modes -----------------------------------------------------------------

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq-default projectile-enable-caching t
                projectile-completion-system 'helm
                projectile-indexing-method 'alien)

  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (require 'auto-complete-config)
  (setq-default ac-use-menu-map t
                ac-sources '(ac-source-words-in-buffer
                             ac-source-words-in-same-mode-buffers))

  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")

  (ac-linum-workaround)
  (add-hook 'prog-mode-hook 'auto-complete-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq-default yas-also-auto-indent-first-line t)

  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package dumb-jump
  :ensure t)

;; Major modes -----------------------------------------------------------------

(use-package emacs-lisp-mode-config
  :init
  (provide 'emacs-lisp-mode-config)
  :config
  (defun init-emacs-lisp-mode ()
    (setq ac-sources (append ac-sources '(ac-source-functions
                                          ac-source-variables
                                          ac-source-yasnippet
                                          ac-source-symbols
                                          ac-source-features)))
    (rainbow-mode))

  (add-hook 'emacs-lisp-mode-hook 'init-emacs-lisp-mode))

(use-package dummy-h-mode
  :ensure t
  :mode "\\.h\\'"
  :config
  (setq-default dummy-h-mode-default-major-mode 'c++-mode))

(use-package c-mode-config
  :init
  (provide 'c-mode-config)
  :config
  (setq-default c-default-style "k&r"
                c-basic-offset 4)
  (defun init-c-mode ()
    (linum-mode))

  (add-hook 'c-mode-hook 'init-c-mode))

(use-package c++-mode-config
  :init
  (provide 'c++-mode-config)
  :config
  (c-set-offset 'inline-open 0)

  (defun init-c++-mode ()
    (setq flycheck-gcc-language-standard "c++11")
    (linum-mode))

  (add-hook 'c++-mode-hook 'init-c++-mode))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less\\'"
         "\\.css\\'")
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)

  (defun init-less-css-mode ()
    (setq ac-sources (append ac-sources '(ac-source-css-property
                                          ac-source-words-in-all-buffer)))
    (rainbow-mode))

  (add-hook 'less-css-mode-hook 'init-less-css-mode))

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
                web-mode-enable-auto-quoting nil
                web-mode-enable-auto-pairing nil
                web-mode-enable-current-element-highlight t)

  (defun init-web-mode()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer))))

  (add-hook 'web-mode-hook 'init-web-mode))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("$"
                                     "_")
                js2-include-node-externs t
                js2-strict-trailing-comma-warning nil)

  (defun init-js2-mode ()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer)))
    (linum-mode)
    (dumb-jump-mode))

  (add-hook 'js2-mode-hook 'init-js2-mode))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq-default lua-indent-level 2))


(provide 'init)
