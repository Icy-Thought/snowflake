(in-package #:nyxt-user)

(define-configuration browser
    ((theme (make-instance
             'theme:theme
             :background-color "#${colors.types.bg}"
             :primary-color "#${colors.types.fg}"
             :secondary-color "#${palette.surface0.hex}"

             :action-color "#${colors.main.yellow}"
             :success-color "#${colors.main.green}"
             :warning-color "#${colors.main.red}"

             :highlight-color "#${colors.types.highlight}"
             :codeblock-color "#${colors.types.panelbg}"
             :contrast-text-color "#${colors.main.magenta}"

             :text-color "#${colors.types.fg}"
             :font-family "${font.family}"))))

(define-configuration nx-dark-reader:dark-reader-mode
    ((nxdr:background-color "#${colors.types.bg}")
     (nxdr:text-color "#${colors.types.fg}")
     (nxdr:selection-color "#${colors.types.highlight}")))

;; :NOTE| Time to introduce glyphs to the equation

(define-configuration :status-buffer
    "Display modes as short glyphs."
  ((glyph-mode-presentation-p t)))

(defmacro define-glyphs (&rest glyphs)
  "Helper macro to set `glyph' slot for multiple modes at once."
  `(progn
     ,@(loop for (mode glyph prefix)
             in glyphs
             collect `(define-configuration
                          ,(read-from-string
                            (format nil "~a~a:~a-MODE"
                                    (if prefix prefix "NYXT/MODE/") mode mode))
                          ((glyph ,glyph))))))

(define-glyphs ((blocker         "󰂭")
                (reduce-tracking "")
                (repl            "")
                (demeter         "" "")
                (dark-reader     "󰔎" "nx-")))

;; :NOTE| Our status-bar requires some adjustments too

(define-configuration status-buffer
    "Hide most of the status elements but URL and modes."
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
                             `("#controls,#tabs"
                               :display none !important))))))

(defmethod format-status-load-status ((status status-buffer))
  "A fancier load status."
  (spinneret:with-html-string
      (:span (if (and (current-buffer)
                      (web-buffer-p (current-buffer)))
                 (case (slot-value (current-buffer) 'nyxt::status)
                   (:unloaded "∅")
                   (:loading "∞")
                   (:finished ""))
                 ""))))
