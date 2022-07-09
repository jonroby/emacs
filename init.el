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

;; TODO Temporary
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/emacs.org")))

(setq ring-bell-function 'ignore)

(setq auto-save-default nil)

(setq make-backup-files nil)

(setq create-lockfiles nil)

(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)

;; (straight-use-package 'avy)
;; (avy-setup-default)

;; (drag-stuff-global-mode 1)

(straight-use-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

(straight-use-package 'general)
(require 'general)

(general-evil-setup t)

(straight-use-package 'helm)
(require 'helm)
(helm-mode 1)

(straight-use-package 'projectile)
(require 'projectile)

(projectile-global-mode)

(setq projectile-enable-caching t)

(setq projectile-track-known-projects-automatically nil)

(straight-use-package 'org)
(require 'org)

(setq org-agenda-files (directory-files-recursively "~/.emacs.d/org" "\.org$"))

(setq org-src-preserve-indentation t)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq org-src-tab-acts-natively nil)

(org-indent-mode)
(visual-line-mode 1)

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(setq org-ellipsis " ▾")

(straight-use-package 'org-bullets)
(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(general-create-definer lsp-leader-def
  :prefix "SPC l")

(lsp-leader-def
 :states 'normal
 ;; :keymaps 'org-mode-map
 "l" 'lsp
 "t" 'lsp-go-to-type-definition
 "r" 'lsp-ui-peek-find-references
 "j" 'lsp-ui-peek-find-definitions
 "k" 'lsp-ui-peek-jump-backward
 )

(general-create-definer window-leader-def
  :prefix "SPC w")

(window-leader-def
 :states 'normal
 "j" 'split-window-below
 "l" 'split-window-right
 "d" 'delete-window
 )

(general-create-definer buffer-leader-def
  :prefix "SPC b")

(buffer-leader-def
 :states 'normal
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
 "b" 'projectile-display-buffer
 )

(general-create-definer emacs-leader-def
  :prefix "SPC e")

(emacs-leader-def
 :states 'normal
 "q" 'save-buffers-kill-terminal
 "e" 'execute-extended-command
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
  )

(evil-define-key '(normal visual) 'global (kbd ",") 'evil-scroll-down)
(evil-define-key '(normal visual) 'global (kbd ".") 'evil-scroll-up)

(evil-define-key '(normal) 'global (kbd "TAB") 'avy-goto-word-1)
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
