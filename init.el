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

(setq org-src-preserve-indentation t)

(setq ring-bell-function 'ignore)

(setq auto-save-default nil)

(setq make-backup-files nil)

(setq create-lockfiles nil)

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
                    :width 'normal))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq-default line-spacing 2)
