;;; init.el --- Edu's Emacs configuration with use-package -*- lexical-binding: t; -*-

;;; Commentary:
;; Author: E. Monroy
;; URL: https://github.com/emonroy/.emacs.d

;;; Code:
;;; Package management ---------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package paradox
  :ensure t
  :config
  (setq-default paradox-github-token -1))

;;; Emacs configuration --------------------------------------------------------

(use-package emonroy--emacs-config
  :bind (("C-z" . undo)
         ("M-SPC" . cycle-spacing))
  :init
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil
                initial-major-mode 'text-mode
                custom-file "~/.emacs.d/custom.el"
                backup-directory-alist `((".*" . ,temporary-file-directory))
                auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
                create-lockfiles nil
                dired-listing-switches "-alh"
                scroll-conservatively 101
                mouse-wheel-scroll-amount '(1)
                mouse-wheel-progressive-speed nil)
  (load custom-file t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (windmove-default-keybindings 'meta)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (winner-mode)
  (line-number-mode)
  (column-number-mode)
  (show-paren-mode)
  (delete-selection-mode)
  (ido-mode)
  (global-auto-revert-mode)
  (provide 'emonroy--emacs-config))

(use-package expand-region
  :ensure t
  :bind (("C-c e" . er/expand-region)))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))

(use-package default-text-scale
  :ensure t
  :bind (("<f5>" . default-text-scale-decrease)
         ("<f6>" . default-text-scale-increase)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

(use-package neotree
  :ensure t
  :demand t
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq-default neo-auto-indent-point t
                neo-create-file-auto-open t
                neo-show-hidden-files t
                neo-force-change-root t)
  (defun emonroy--neotree-hook ()
    (interactive)
    (neotree-show)
    (call-interactively 'other-window))
  (add-hook 'after-init-hook 'emonroy--neotree-hook))

;;; Theme ----------------------------------------------------------------------

(use-package monokai-theme
  :ensure t
  :config
  (setq-default monokai-use-variable-pitch t)
  (load-theme 'monokai t))

(defun emonroy--available-font-p (font)
  "Check font availability.
Returns t when FONT is available."
  (when (find-font (font-spec :name font))
    t))

(defun emonroy--display-width ()
  "Get display width.
Returns the display width in pixels."
  (if (display-graphic-p)
      (display-pixel-width)
    0))

(use-package emonroy--default-face-config
  :init
  (let ((frame-font "fira mono")
        (display-width (emonroy--display-width))
        (default-face-height 110))
    (when (emonroy--available-font-p frame-font)
      (set-frame-font frame-font))
    (cond ((>= display-width 2560)
           (setq default-face-height 140))
          ((>= display-width 1440)
           (setq default-face-height 140)))
    (set-face-attribute 'default nil
                        :height default-face-height))
  (toggle-frame-maximized)
  (provide 'emonroy--default-face-config))

;;; Ido ------------------------------------------------------------------------

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

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode))

;;; Helm   ---------------------------------------------------------------------

(use-package helm
  :ensure t
  :bind (("C-c x" . helm-M-x)
         ("C-c b" . helm-mini))
  :config
  (setq-default helm-full-frame t
                helm-input-idle-delay 0.2
                helm-M-x-fuzzy-match t
                helm-candidate-number-limit 50
                helm-allow-mouse t)
  (set-face-attribute 'helm-selection nil
                      :underline nil))

(use-package helm-ag
  :ensure t
  :bind (("C-c a" . helm-do-ag)
         ("C-c r" . helm-do-ag-project-root))
  :config
  (setq-default helm-ag-command-option "--ignore-case"
                helm-ag-insert-at-point 'symbol
                helm-ag-use-agignore t))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . helm-swoop)
         ("C-c S" . emonroy--helm-swoop-multiline-4))
  :config
  (setq-default helm-swoop-move-to-line-cycle nil
                helm-swoop-speed-or-color t
                helm-swoop-use-line-number-face t
                helm-swoop-split-with-multiple-windows t
                helm-swoop-split-direction 'split-window-vertically)
  (defun emonroy--helm-swoop-multiline-4 ()
    (interactive)
    (setq current-prefix-arg 4)
    (helm-swoop)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c f" . helm-projectile-find-file)))

;;; Minor modes ----------------------------------------------------------------

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq-default indent-tabs-mode nil
                whitespace-style '(empty trailing tab-mark)
                whitespace-action '(auto-cleanup))
  (global-whitespace-mode))

(use-package linum
  :config
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  :config
  (set-face-background 'auto-dim-other-buffers-face "#0d0d0d")
  (auto-dim-other-buffers-mode))

(use-package anzu
  :ensure t
  :demand t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace))
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :inherit 'mode-line
                      :foreground nil)
  (global-anzu-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package dumb-jump
  :ensure t
  :bind (("C-c g" . dumb-jump-go)
         ("C-c p" . dumb-jump-back)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq-default projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (setq-default projectile-enable-caching t
                projectile-completion-system 'helm
                projectile-indexing-method 'alien)
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package diff-hl
  :ensure t
  :config
  (setq-default diff-hl-draw-borders nil)
  (set-face-background 'diff-hl-insert monokai-green)
  (set-face-background 'diff-hl-change monokai-yellow)
  (set-face-background 'diff-hl-delete monokai-red)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

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
  :demand t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-<tab>" . yas-expand))
  :config
  (setq-default yas-also-auto-indent-first-line t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;;; Major modes ----------------------------------------------------------------

(use-package org
  :bind (:map org-mode-map
              ("M-<up>" . nil)
              ("M-<right>" . nil)
              ("M-<down>" . nil)
              ("M-<left>" . nil))
  :config
  (setq-default org-startup-folded nil))

(use-package emonroy--emacs-lisp-mode
  :init
  (defun emonroy--emacs-lisp-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-functions
                                          ac-source-variables
                                          ac-source-yasnippet
                                          ac-source-symbols
                                          ac-source-features)))
    (rainbow-mode))
  (add-hook 'emacs-lisp-mode-hook 'emonroy--emacs-lisp-mode-hook)
  (provide 'emonroy--emacs-lisp-mode))

(use-package emonroy--c-mode
  :init
  (setq-default c-default-style "k&r"
                c-basic-offset 4)
  (provide 'emonroy--c-mode))

(use-package emonroy--c++-mode
  :init
  (c-set-offset 'inline-open 0)
  (defun emonroy--c++-mode-hook ()
    (setq flycheck-gcc-language-standard "c++11"))
  (add-hook 'c++-mode-hook 'emonroy--c++-mode-hook)
  (provide 'emonroy--c++-mode))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less\\'"
         "\\.css\\'")
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  (defun emonroy--less-css-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-css-property
                                          ac-source-words-in-all-buffer)))
    (rainbow-mode))
  (add-hook 'less-css-mode-hook 'emonroy--less-css-mode-hook))

(use-package web-mode
  :ensure t
  :mode ("\\.html?"
         "\\.hbs"
         "\\.tl")
  :config
  (setq-default web-mode-engines-alist '(("django" . "\\.html?")
                                         ("ctemplate" . "\\.hbs")
                                         ("dust" . "\\.tl"))
                web-mode-css-indent-offset 4
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 4
                web-mode-style-padding 2
                web-mode-script-padding 2
                web-mode-block-padding 0
                web-mode-enable-auto-quoting nil
                web-mode-enable-auto-pairing nil
                web-mode-enable-current-element-highlight t)
  (defun emonroy--web-mode-hook()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer))))
  (add-hook 'web-mode-hook 'emonroy--web-mode-hook))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("$"
                                     "_")
                js2-include-node-externs t
                js2-strict-trailing-comma-warning nil)
  (defun emonroy--js2-mode-hook ()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer))))
  (add-hook 'js2-mode-hook 'emonroy--js2-mode-hook))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq-default lua-indent-level 2))

(provide 'init)
;;; init.el ends here
