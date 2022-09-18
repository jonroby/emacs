(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq ring-bell-function 'ignore)

(setq auto-save-default nil)

(setq make-backup-files nil)

(setq create-lockfiles nil)

(set-fringe-mode 0)

(setq evil-want-keybinding nil)

(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)

(straight-use-package 'evil-collection)
(evil-collection-init)

(straight-use-package 'avy)
(avy-setup-default)

(with-eval-after-load 'avy
  (set-face-attribute 'avy-lead-face nil :background "#BF616A")
  (set-face-attribute 'avy-lead-face-0 nil :background "#5E81AC")
  )

(straight-use-package 'drag-stuff)
(require 'drag-stuff)

;; (drag-stuff-global-mode 1)

(straight-use-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

(straight-use-package 'general)
(require 'general)

(general-evil-setup t)

(electric-pair-mode 1)

(straight-use-package 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)

(straight-use-package 'helm)
(require 'helm)
(helm-mode 1)

(straight-use-package 'helm-swoop)

(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)

(fset 'helm-display-mode-line #'ignore)
;; (add-hook 'helm-after-initialize-hook
;;           (defun hide-mode-line-in-helm-buffer ()
;;             "Hide mode line in `helm-buffer'."
;;             (with-helm-buffer
;;               (setq-local mode-line-format nil))))

(straight-use-package 'projectile)
(require 'projectile)

(projectile-global-mode)

(setq projectile-enable-caching t)

(setq projectile-track-known-projects-automatically nil)

(straight-use-package 'perspective)

;; (setq persp-state-default-file "~/.emacs.d/perspective/default")

(straight-use-package 'which-key)
(which-key-mode)

;; (straight-use-package 'treemacs)

(straight-use-package 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))

(straight-use-package 'editorconfig)
(editorconfig-mode 1)

(straight-use-package 'prettier)

(straight-use-package 'web-mode)

(straight-use-package 'org)
(require 'org)

;; (setq org-agenda-files (directory-files-recursively "~/.emacs.d/org" "\.org$"))

(setq org-src-preserve-indentation t)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq org-src-tab-acts-natively nil)

(setq org-startup-indented t)

(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :background "#27c12cf13750"))

(setq org-ellipsis " ▾")

(straight-use-package 'org-bullets)
(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(straight-use-package 'vterm)

(straight-use-package 'multi-vterm)

(straight-use-package 'lsp-mode)

(straight-use-package 'lsp-ui)
(setq lsp-ui-sideline-diagnostic-max-lines 20)
(setq lsp-ui-sideline-enable t)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(straight-use-package 'lsp-treemacs)
(with-eval-after-load 'lsp-treemacs
  (set-face-attribute 'lsp-treemacs-file-error nil :foreground "#BF616A")) ;; left

(straight-use-package 'company)

(setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        create-lockfiles nil) ;; lock files will kill `npm start'

(with-eval-after-load 'lsp-mode
    (setq lsp-headerline-breadcrumb-enable nil))
;;    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;    (yas-global-mode))

(straight-use-package 'flycheck)

(with-eval-after-load 'flycheck
  (set-face-attribute 'flycheck-info nil :underline '(:color "#EBCB8B" :style wave))
  (set-face-attribute 'flycheck-error nil :underline '(:color "#BF616A" :style wave))
  
  )

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)
(require 'nano-theme-dark)

(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq-default line-spacing 2)

(straight-use-package 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

(straight-use-package 'helm-ag)

(straight-use-package 'helm-lsp)

(general-create-definer lsp-leader-def
  :prefix "SPC l")

(lsp-leader-def
 :states 'normal
 "l" 'lsp
 "t" 'lsp-goto-type-definition
 "r" 'lsp-ui-peek-find-references
 "j" 'lsp-ui-peek-find-definitions
 "k" 'lsp-ui-peek-jump-backward
 "p" 'prettier-prettify
 )

(general-create-definer window-leader-def
  :prefix "SPC w")

(window-leader-def
 :states 'normal
 "j" 'split-window-below
 "l" 'split-window-right
 "d" 'delete-window
 )

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(general-create-definer buffer-leader-def
  :prefix "SPC b")

(buffer-leader-def
 :states 'normal
 "a" 'save-buffer
 "j" 'previous-buffer
 "k" 'next-buffer
 "l" 'switch-to-last-buffer
 "b" 'switch-to-buffer
 "d" 'kill-buffer
 )

(general-create-definer projectile-leader-def
  :prefix "SPC p")

(projectile-leader-def
 :states 'normal
 "a" 'projectile-add-known-project
 "p" 'helm-projectile-switch-project
 "f" 'helm-projectile-find-file
 "t" 'helm-projectile-ag
 "b" 'projectile-display-buffer
 )

(general-create-definer emacs-leader-def
  :prefix "SPC e")

(emacs-leader-def
 :states 'normal
 "q" 'save-buffers-kill-terminal
 "e" 'helm-M-x
 "y" 'helm-show-kill-ring
 "r" 'eval-last-sexp
 "i" '(lambda () (interactive) (find-file "~/.emacs.d/emacs.org"))
 )

(general-create-definer emacs-leader-def
  :prefix "SPC v")

(emacs-leader-def
 :states 'normal
 "l" 'persp-list-buffers
 "s" 'persp-switch
 "k" 'persp-kill
 "b" 'persp-switch-to-buffer
 "n" 'persp-next
 "x" 'persp-remove-buffer
 "a" 'persp-set-buffer
 "A" 'persp-add-buffer
 "r" 'persp-rename
 "S" 'persp-state-save
 "L" 'persp-state-load
 )

(general-create-definer jonroby/leader-keys
  :keymaps 'normal
  :prefix "SPC")

(jonroby/leader-keys
  "." 'helm-buffers-list
  "/" 'helm-find-files
  "s" 'helm-swoop
  "m" 'magit
  "a" 'save-buffer
  )

(evil-define-key '(normal visual) 'global (kbd ",") 'evil-scroll-down)
(evil-define-key '(normal visual) 'global (kbd ".") 'evil-scroll-up)

(evil-define-key '(normal) 'global (kbd "<DEL>") 'delete-backward-char)
(evil-define-key '(normal) 'global (kbd "M-n") 'drag-stuff-down)
(evil-define-key '(normal) 'global (kbd "M-p") 'drag-stuff-up)
(evil-define-key '(normal) 'global (kbd "C-o") 'open-line)
(evil-define-key '(normal) 'global (kbd "M-o") 'delete-blank-lines)

(setq evil-move-beyond-eol t)

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character
  on the current line. If COUNT is given, move COUNT - 1
  lines downward first."
  :type inclusive
  (evil-end-of-line count)
  (re-search-backward "^\\|[^[:space:]]")
  (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))

(define-key evil-motion-state-map "g-" 'evil-end-of-line)
(define-key evil-motion-state-map "-" 'evil-last-non-blank)
(define-key evil-motion-state-map "1" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "9" 'other-window)
(define-key evil-motion-state-map "f" 'avy-goto-word-1)

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(global-set-key (kbd "C-c y") 'copy-full-path-to-kill-ring)

(setq company-idle-delay 0)
(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip nil :foreground "#5E81AC" :background "#27c12cf13750")
  (set-face-attribute 'company-tooltip-annotation nil :foreground "#4C566A" :background "#27c12cf13750")
  (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "#A3BE8C")
  (set-face-attribute 'company-tooltip-selection nil :foreground "white" :background "#434C5E")
  ;; (set-face-attribute 'company-tooltip-common nil :foreground "#A3BE8C" :background "#27c12cf13750")
  ;; (set-face-attribute 'company-tooltip-annotation-selection nil :foreground "#4C566A" :background "#A3BE8C")
  )

(with-eval-after-load 'helm
  (set-face-attribute 'helm-buffer-process nil :foreground "#BF616A")
  (set-face-attribute 'helm-ff-directory nil :background "#2E3440" :foreground "RosyBrown")
  (set-face-attribute 'helm-ff-dotted-directory nil :background "#2E3440" :foreground "RosyBrown")
  (set-face-attribute 'helm-ff-file-extension nil :foreground "#81A1C1")
  (set-face-attribute 'helm-ff-executable nil :foreground "#EBCB8B")
  (set-face-attribute 'helm-ff-prefix nil :background "#2E3440" :foreground "#4C566A")
  (set-face-attribute 'helm-header-line-left-margin nil :background "#2E3440" :foreground "#4C566A")
  (set-face-attribute 'helm-separator nil :foreground "#2E3440"))

;;  '(orderless-match-face-0 ((t (:foreground "#A3BE8C" :weight normal)))))

(with-eval-after-load 'helm-swoop
  (set-face-attribute 'helm-swoop-target-word-face nil :background "#434C5E" :foreground "#A3BE8C"))

(setq lsp-ui-sideline-diagnostic-max-lines 3)
(with-eval-after-load 'lsp-ui
  (set-face-attribute 'lsp-ui-peek-header nil :foreground "white" :background "#3B4252")
  (set-face-attribute 'lsp-ui-peek-footer nil :foreground "white" :background "#3B4252")
  (set-face-attribute 'lsp-ui-peek-highlight nil :foreground "#A3BE8C" :background "#27c12cf13750" :box nil)
  (set-face-attribute 'lsp-ui-peek-peek nil :background "#27c12cf13750") ;; left 
  (set-face-attribute 'lsp-ui-peek-list nil :background "#27c12cf13750") ;; right
  (set-face-attribute 'lsp-ui-peek-filename nil :foreground "RosyBrown")
  (set-face-attribute 'lsp-ui-peek-selection nil :foreground "white" :background "#A3BE8C"))

(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :background "#27c12cf13750"))

(set-face-attribute 'error nil :foreground "#BF616A" :background "#2E3440")

(defun hosts ()
  "Open /etc/hosts as root."
  (interactive)
  (find-file "/sudo::/etc/hosts"))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(straight-use-package 'highlight-indent-guides)

(setq highlight-indent-guides-method 'character)
(add-hook 'typescript-mode-hook 'highlight-indent-guides-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-M-x-key ((t (:extend t :foreground "#434C5E")))))
