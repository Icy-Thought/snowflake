(in-package #:nyxt-user)

(define-configuration :document-mode
    "Add a single keybinding for the extension-provided `kaomoji-fill' command."
  ((keymap-scheme
    (alter-keyscheme %slot-value% nyxt/keyscheme:emacs
                     "C-c K" 'nx-kaomoji:kaomoji-fill))))

(define-configuration buffer
    ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
