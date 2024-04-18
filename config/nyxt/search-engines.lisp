(in-package #:nyxt-user)

(defvar *duckduckgo-keywords*
  '(:help-improve-duckduckgo nil
    :homepage-privacy-tips nil
    :privacy-newsletter nil
    :newsletter-reminders nil
    :install-reminders nil
    :install-duckduckgo nil
    :units-of-measure :metric
    :keyboard-shortcuts t
    :advertisements nil
    :open-in-new-tab nil
    :infinite-scroll t
    :safe-search :off
    :header-behavior :on-fixed
    :center-alignment t
    :font "${font.sans.family}"
    :font-size :medium
    :theme :dark
    :background-color "${palette.base.hex}"
    :header-color "${palette.mantle.hex}"
    :result-title-font "${font}"
    :result-title-color "${palette.${config.catppuccin.accent}.hex}"
    :result-visited-title-color "${palette.lavender.hex}"
    :result-description-color "${palette.text.hex}"
    :result-url-color "${palette.rosewater.hex}"
    :result-module-color "${palette.mantle.hex}"
    :result-full-urls t
    :result-urls-above-snippet t
    :result-visible-checkmark t))

(define-configuration :context-buffer
    ((search-engines (list
                      (engines:google :shortcut "gmaps"
                                      :object :maps)
                      (make-instance 'search-engine
                                     :shortcut "osm"
                                     :search-url "https://www.openstreetmap.org/search?query=~a"
                                     :fallback-url (quri:uri "https://www.openstreetmap.org/"))
                      (engines:wikipedia :shortcut "w")
                      (engines:google :shortcut "g"
                                      :safe-search nil)
                      (engines:google-scholar :shortcut "gs")
                      (engines:google-scholar :shortcut "scholar-new"
                                              :starting-time 2015)
                      (apply #'engines:duckduckgo-images
                             :shortcut "i" *duckduckgo-keywords*)
                      (engines:github :shortcut "git")
                      (engines:libgen :shortcut "l")
                      (apply #'engines:duckduckgo
                             :shortcut "d" *duckduckgo-keywords*)
                      (engines:searx
                       :shortcut "a"
                       :base-search-url "https://search.atlas.engineer/searxng/search?q=~a"
                       :fallback-url (quri:uri "https://search.atlas.engineer")
                       :completion-function (engines:make-duckduckgo-completion)
                       :auto-complete :yandex
                       :style :dark
                       :safe-search :none
                       :request-method :post)))))
