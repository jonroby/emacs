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
