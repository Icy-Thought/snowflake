(define-configuration buffer
    ((default-modes
         (append '(nyxt/mode/blocker:blocker-mode
                   nyxt/mode/reduce-tracking:reduce-tracking-mode
                   nx-dark-reader:dark-reader-mode)
                 %slot-value%))))

;; :NOTE| light -> dark theme
(define-configuration browser
    ((external-editor-program '("handlr" "open"))
     (restore-session-on-startup-p nil)))

;; :NOTE| import custom Nyxt modules
(define-nyxt-user-system-and-load nyxt-user/basic-config
    :components ("aesthetics" "bindings" "search-engines"))
