(in-package #:nyxt-user)

;; :NOTE| import custom Nyxt modules
(define-nyxt-user-system-and-load nyxt-user/basic-config
    :components ("aesthetics" "bindings" "search-engines"))

(defmacro load-extensions (&rest extensions)
  "Helper macro to load extensions along with config files with the same name."
  `(progn ,@(loop for extension
                  in extensions
                  collect `(nyxt:define-nyxt-user-system-and-load
                               ,(alexandria:symbolicate 'nyxt-user/ extension '-proxy)
                               :description ,(format t "This proxy system saves us if ~a fails to load.
                            Otherwise it will break all the config loading." extension)
                               :depends-on (,extension)))))

(load-extensions ${builtins.concatStringsSep "\n" (map (p: ":" + p) plugins)})

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
