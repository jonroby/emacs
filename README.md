# Nano Changes

## Allow meta to work
Inside nano-defaults:

;; (when (eq system-type 'darwin)
;;   (setq ns-use-native-fullscreen t
;;         mac-option-key-is-meta nil
;;         mac-command-key-is-meta t
;;         mac-command-modifier 'meta
;;         mac-option-modifier nil
;;         mac-use-title-bar nil))

## Nano-dark
Inside nano.el:

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (nano-theme-set-dark))
 (t (nano-theme-set-dark)))
