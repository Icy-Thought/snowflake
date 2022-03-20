;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Personal Info][Personal Info:1]]
(setq user-full-name "Icy-Thought"
      user-mail-address "gilganyx@pm.me")
;; Personal Info:1 ends here

;; [[file:config.org::*Simple settings][Simple settings:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; Simple settings:1 ends here

;; [[file:config.org::*Auto-customizations][Auto-customizations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Auto-customizations:1 ends here

;; [[file:config.org::*Window Management][Window Management:1]]
;; Resize windows for optimal window space
(setq window-combination-resize t)
;; Window Management:1 ends here

;; [[file:config.org::*Window Management][Window Management:2]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Window Management:2 ends here

;; [[file:config.org::*Window Management][Window Management:3]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
;; Window Management:3 ends here

;; [[file:config.org::*Window Management][Window Management:4]]
(map! :map evil-window-map
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)
;; Window Management:4 ends here

;; [[file:config.org::*Mouse movement][Mouse movement:1]]
(map! :n [mouse-8] #'scroll-up-command
      :n [mouse-9] #'scroll-down-command)
;; Mouse movement:1 ends here

;; [[file:config.org::*Buffer defaults][Buffer defaults:1]]
;; (setq-default major-mode 'org-mode)
;; Buffer defaults:1 ends here

;; [[file:config.org::*Font-face][Font-face:1]]
(setq doom-font
      (font-spec
       :family "VictorMono Nerd Font"
       :size 12.0
       :weight 'semi-bold)
      doom-big-font
      (font-spec
       :family "VictorMono Nerd Font"
       :size 15.0
       :weight 'semi-bold)
      doom-variable-pitch-font
      (font-spec
       :family "VictorMono Nerd Font"
       :size 12.0
       :weight 'semi-bold))
;; Font-face:1 ends here

;; [[file:config.org::*Font-face][Font-face:2]]
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil
                    :weight 'bold :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
;; Font-face:2 ends here

;; [[file:config.org::*Theme & Modeline][Theme & Modeline:1]]
(setq doom-theme 'doom-city-lights)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)
;; Theme & Modeline:1 ends here

;; [[file:config.org::*Theme & Modeline][Theme & Modeline:2]]
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
;; Theme & Modeline:2 ends here

;; [[file:config.org::*Theme & Modeline][Theme & Modeline:3]]
(defun fix-emacsclient-theme ()
  (interactive)
  (load-theme 'doom-city-lights t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (fix-emacsclient-theme))))
  (fix-emacsclient-theme))
;; Theme & Modeline:3 ends here

;; [[file:config.org::*Theme & Modeline][Theme & Modeline:4]]
(with-eval-after-load 'solaire-mode
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-"))
;; Theme & Modeline:4 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:1]]
(setq confirm-kill-emacs nil
      display-line-numbers-type 'relative
      all-the-icons-dired-monochrome nil)
;; Miscellaneous:1 ends here

;; [[file:config.org::*Asynchronous config tangling][Asynchronous config tangling:1]]
(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))
(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)
;; Asynchronous config tangling:1 ends here

;; [[file:config.org::*PDF-tools][PDF-tools:2]]
(use-package! pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page))
;; PDF-tools:2 ends here

;; [[file:config.org::*Which-key][Which-key:1]]
(setq which-key-idle-delay 0.5) ;; I need the help, I really do
;; Which-key:1 ends here

;; [[file:config.org::*Which-key][Which-key:2]]
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; Which-key:2 ends here

;; [[file:config.org::*EVIL][EVIL:1]]
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring
;; EVIL:1 ends here

;; [[file:config.org::*Consult][Consult:1]]
(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))
;; Consult:1 ends here

;; [[file:config.org::*Company][Company:1]]
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
;; Company:1 ends here

;; [[file:config.org::*Company][Company:2]]
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
;; Company:2 ends here

;; [[file:config.org::*Plain-text][Plain-text:1]]
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))
;; Plain-text:1 ends here

;; [[file:config.org::*ESS][ESS:1]]
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))
;; ESS:1 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; Projectile:1 ends here

;; [[file:config.org::*Screenshot][Screenshot:2]]
(use-package! screenshot
  :defer t
  :config (setq screenshot-upload-fn "upload %s 2>/dev/null"))
;; Screenshot:2 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(setq yas-triggers-in-field t)
;; YASnippet:1 ends here

;; [[file:config.org::*Smart parentheses][Smart parentheses:1]]
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))
;; Smart parentheses:1 ends here

;; [[file:config.org::*Centaur-tabs][Centaur-tabs:1]]
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "VictorMono Nerd Font" 125)

  (setq centaur-tabs-height 32
        centaur-tabs-style "wave"
        centaur-tabs-set-bar nil
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-modified-marker "◉"
        centaur-tabs-close-button "✕"
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-down-tab-text "✦"
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵")

  (custom-set-faces!
    `(tab-line :background ,(doom-color 'base1) :foreground ,(doom-color 'base1))
    `(centaur-tabs-default :background ,(doom-color 'base1) :foreground ,(doom-color 'base1))
    `(centaur-tabs-active-bar-face :background ,(doom-color 'base1) :foreground ,(doom-color 'base1))
    `(centaur-tabs-unselected-modified :background ,(doom-color 'base1) :foreground ,(doom-color 'violet))
    `(centaur-tabs-unselected :background ,(doom-color 'base1) :foreground ,(doom-color 'base4))
    `(centaur-tabs-selected-modified :background ,(doom-color 'bg) :foreground ,(doom-color 'violet))
    `(centaur-tabs-selected :background ,(doom-color 'bg) :foreground ,(doom-color 'blue))))
;; Centaur-tabs:1 ends here

;; [[file:config.org::*Doom modeline][Doom modeline:1]]
(after! doom-modeline
 (setq evil-normal-state-tag "λ"
       evil-insert-state-tag ""
       evil-visual-state-tag "麗"
       evil-motion-state-tag ""
       evil-emacs-state-tag "<EMACS>")

  (setq doom-modeline-height 35
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
        doom-modeline-buffer-encoding nil
        inhibit-compacting-font-caches t
        find-file-visit-truename t)

  (custom-set-faces!
    '(doom-modeline-evil-insert-state :inherit doom-modeline-urgent)
    '(doom-modeline-evil-visual-state :inherit doom-modeline-warning)
    '(doom-modeline-evil-normal-state :inherit doom-modeline-buffer-path))

  ;; (display-time-mode 1)
  (display-battery-mode 1)

  (setq doom-modeline-enable-word-count t)
    (doom-modeline-def-segment buffer-name
    "Display the current buffer's name, without any other information."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))


  ;; PDF-modeline = buffer name + icon.
  (doom-modeline-def-segment pdf-icon
    "PDF icon from all-the-icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline-icon 'octicon "file-pdf" nil nil
                         :face (if (doom-modeline--active)
                                   'all-the-icons-red
                                 'mode-line-inactive)
                         :v-adjust 0.02)))

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages pdf-icon buffer-name)
    '(misc-info matches major-mode process vcs)))
;; Doom modeline:1 ends here

;; [[file:config.org::*Keycast][Keycast:2]]
(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))
;; Keycast:2 ends here

;; [[file:config.org::*Marginalia][Marginalia:1]]
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))
;; Marginalia:1 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:2]]
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))
;; Prettier page breaks:2 ends here

;; [[file:config.org::*Screencast][Screencast:2]]
(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))
;; Screencast:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; Treemacs:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
;; Treemacs:2 ends here

;; [[file:config.org::*File Templates][File Templates:1]]
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)
;; File Templates:1 ends here

;; [[file:config.org::*Plain-text][Plain-text:1]]
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))
;; Plain-text:1 ends here

(after! org
  (setq org-directory "~/.org"                      ; let's put files here
        org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time a item is done sounds convenient
        org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{})       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link")))
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (map! :map evil-org-mode-map
        :after evil-org
        :n "g <up>" #'org-backward-heading-same-level
        :n "g <down>" #'org-forward-heading-same-level
        :n "g <left>" #'org-up-element
        :n "g <right>" #'org-down-element)
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("1)" . "a)") ("1." . "a.")))
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (defadvice! org-edit-latex-emv-after-insert ()
    :after #'org-cdlatex-environment-indent
    (org-edit-latex-environment))
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (setq centaur-lsp 'lsp-mode)
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (make-temp-file "babel-lsp-")))
             (setq buffer-file-name file-name)
             (lsp-deferred)))
         (put ',intern-pre 'function-documentation
              (format "Enable lsp-mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))
  (defvar org-babel-lang-list
    '("go" "python" "ipython" "bash" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang)))
  (setq org-roam-directory "~/org/Roam/")
  (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    :around #'doom-modeline-buffer-file-name ; takes no args
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔(\\1-\\2-\\3) "
         (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))
  (defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
    :around #'org-fancy-priorities-mode
    :around #'org-superstar-mode
    (ignore-errors (apply orig-fn args)))
  (add-hook 'org-mode-hook #'+org-pretty-mode)
  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.25)
    '(outline-2 :weight bold :height 1.15)
    '(outline-3 :weight bold :height 1.12)
    '(outline-4 :weight semi-bold :height 1.09)
    '(outline-5 :weight semi-bold :height 1.06)
    '(outline-6 :weight semi-bold :height 1.03)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold))
  (custom-set-faces!
    '(org-document-title :height 1.2))
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))
  (setq org-fontify-quote-and-verse-blocks t)
  (defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 50000)
      (setq-local jit-lock-defer-time 0.05
                  jit-lock-stealth-time 1)))
  
  (add-hook 'org-mode-hook #'locally-defer-font-lock)
  (defvar org-prettify-inline-results t
    "Whether to use (ab)use prettify-symbols-mode on {{{results(...)}}}.
  Either t or a cons cell of strings which are used as substitutions
  for the start and end of inline results, respectively.")
  
  (defvar org-fontify-inline-src-blocks-max-length 200
    "Maximum content length of an inline src block that will be fontified.")
  
  (defun org-fontify-inline-src-blocks (limit)
    "Try to apply `org-fontify-inline-src-blocks-1'."
    (condition-case nil
        (org-fontify-inline-src-blocks-1 limit)
      (error (message "Org mode fontification error in %S at %d"
                      (current-buffer)
                      (line-number-at-pos)))))
  
  (defun org-fontify-inline-src-blocks-1 (limit)
    "Fontify inline src_LANG blocks, from `point' up to LIMIT."
    (let ((case-fold-search t)
          (initial-point (point)))
      (while (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t) ; stolen from `org-element-inline-src-block-parser'
        (let ((beg (match-beginning 0))
              pt
              (lang-beg (match-beginning 1))
              (lang-end (match-end 1)))
          (remove-text-properties beg lang-end '(face nil))
          (font-lock-append-text-property lang-beg lang-end 'face 'org-meta-line)
          (font-lock-append-text-property beg lang-beg 'face 'shadow)
          (font-lock-append-text-property beg lang-end 'face 'org-block)
          (setq pt (goto-char lang-end))
          ;; `org-element--parse-paired-brackets' doesn't take a limit, so to
          ;; prevent it searching the entire rest of the buffer we temporarily
          ;; narrow the active region.
          (save-restriction
            (narrow-to-region beg (min (point-max) limit (+ lang-end org-fontify-inline-src-blocks-max-length)))
            (when (ignore-errors (org-element--parse-paired-brackets ?\[))
              (remove-text-properties pt (point) '(face nil))
              (font-lock-append-text-property pt (point) 'face 'org-block)
              (setq pt (point)))
            (when (ignore-errors (org-element--parse-paired-brackets ?\{))
              (remove-text-properties pt (point) '(face nil))
              (font-lock-append-text-property pt (1+ pt) 'face '(org-block shadow))
              (unless (= (1+ pt) (1- (point)))
                (if org-src-fontify-natively
                    (org-src-font-lock-fontify-block (buffer-substring-no-properties lang-beg lang-end) (1+ pt) (1- (point)))
                  (font-lock-append-text-property (1+ pt) (1- (point)) 'face 'org-block)))
              (font-lock-append-text-property (1- (point)) (point) 'face '(org-block shadow))
              (setq pt (point))))
          (when (and org-prettify-inline-results (re-search-forward "\\= {{{results(" limit t))
            (font-lock-append-text-property pt (1+ pt) 'face 'org-block)
            (goto-char pt))))
      (when org-prettify-inline-results
        (goto-char initial-point)
        (org-fontify-inline-src-results limit))))
  
  (defun org-fontify-inline-src-results (limit)
    (while (re-search-forward "{{{results(\\(.+?\\))}}}" limit t)
      (remove-list-of-text-properties (match-beginning 0) (point)
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))
      (font-lock-append-text-property (match-beginning 0) (match-end 0) 'face 'org-block)
      (let ((start (match-beginning 0)) (end (match-beginning 1)))
        (with-silent-modifications
          (compose-region start end (if (eq org-prettify-inline-results t) "⟨" (car org-prettify-inline-results)))
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))
      (let ((start (match-end 1)) (end (point)))
        (with-silent-modifications
          (compose-region start end (if (eq org-prettify-inline-results t) "⟩" (cdr org-prettify-inline-results)))
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))))
  
  (defun org-fontify-inline-src-blocks-enable ()
    "Add inline src fontification to font-lock in Org.
  Must be run as part of `org-font-lock-set-keywords-hook'."
    (setq org-font-lock-extra-keywords
          (append org-font-lock-extra-keywords '((org-fontify-inline-src-blocks)))))
  
  (add-hook 'org-font-lock-set-keywords-hook #'org-fontify-inline-src-blocks-enable)
  (setq doom-themes-org-fontify-special-tags nil)
  (after! org-superstar
    (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-superstar-prettify-item-bullets t ))
  
  (setq org-ellipsis " ▾ "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue)))
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :startup       "⏻"
              :macro         "𝓜"
              :html_head     "🅷"
              :html          "🅗"
              :latex_class   "🄻"
              :latex_header  "🅻"
              :beamer_header "🅑"
              :latex         "🅛"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :attr_org      "⒪"
              :begin_quote   "❝"
              :end_quote     "❞"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⏩"
              :end_export    "⏪"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]")
  (plist-put +ligatures-extra-symbols :name "⁍")
  ;; (package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")
  ;; (use-package org-pretty-tags
  ;; :config
  ;;  (setq org-pretty-tags-surrogate-strings
  ;;        `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
  ;;          ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
  ;;          ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
  ;;          ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
  ;;          ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
  ;;          ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
  ;;          ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
  ;;          ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
  ;;          ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
  ;;          ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
  ;;          ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
  ;;          ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
  ;;          ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
  ;;          ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
  ;;  (org-pretty-tags-global-mode))
  (setq org-highlight-latex-and-related '(native script entities))
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-format-latex-header "\\documentclass{article}
  \\usepackage[usenames]{xcolor}
  
  \\usepackage[T1]{fontenc}
  
  \\usepackage{booktabs}
  
  \\pagestyle{empty}             % do not remove
  % The settings below are copied from fullpage.sty
  \\setlength{\\textwidth}{\\paperwidth}
  \\addtolength{\\textwidth}{-3cm}
  \\setlength{\\oddsidemargin}{1.5cm}
  \\addtolength{\\oddsidemargin}{-2.54cm}
  \\setlength{\\evensidemargin}{\\oddsidemargin}
  \\setlength{\\textheight}{\\paperheight}
  \\addtolength{\\textheight}{-\\headheight}
  \\addtolength{\\textheight}{-\\headsep}
  \\addtolength{\\textheight}{-\\footskip}
  \\addtolength{\\textheight}{-3cm}
  \\setlength{\\topmargin}{1.5cm}
  \\addtolength{\\topmargin}{-2.54cm}
  % my custom stuff
  \\usepackage[nofont,plaindd]{bmc-maths}
  \\usepackage{arev}
  ")
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (after! org-plot
    (defun org-plot/generate-theme (_type)
      "Use the current Doom theme colours to generate a GnuPlot preamble."
      (format "
  fgt = \"textcolor rgb '%s'\" # foreground text
  fgat = \"textcolor rgb '%s'\" # foreground alt text
  fgl = \"linecolor rgb '%s'\" # foreground line
  fgal = \"linecolor rgb '%s'\" # foreground alt line
  
  # foreground colors
  set border lc rgb '%s'
  # change text colors of  tics
  set xtics @fgt
  set ytics @fgt
  # change text colors of labels
  set title @fgt
  set xlabel @fgt
  set ylabel @fgt
  # change a text color of key
  set key @fgt
  
  # line styles
  set linetype 1 lw 2 lc rgb '%s' # red
  set linetype 2 lw 2 lc rgb '%s' # blue
  set linetype 3 lw 2 lc rgb '%s' # green
  set linetype 4 lw 2 lc rgb '%s' # magenta
  set linetype 5 lw 2 lc rgb '%s' # orange
  set linetype 6 lw 2 lc rgb '%s' # yellow
  set linetype 7 lw 2 lc rgb '%s' # teal
  set linetype 8 lw 2 lc rgb '%s' # violet
  
  # border styles
  set tics out nomirror
  set border 3
  
  # palette
  set palette maxcolors 8
  set palette defined ( 0 '%s',\
  1 '%s',\
  2 '%s',\
  3 '%s',\
  4 '%s',\
  5 '%s',\
  6 '%s',\
  7 '%s' )
  "
              (doom-color 'fg)
              (doom-color 'fg-alt)
              (doom-color 'fg)
              (doom-color 'fg-alt)
              (doom-color 'fg)
              ;; colours
              (doom-color 'red)
              (doom-color 'blue)
              (doom-color 'green)
              (doom-color 'magenta)
              (doom-color 'orange)
              (doom-color 'yellow)
              (doom-color 'teal)
              (doom-color 'violet)
              ;; duplicated
              (doom-color 'red)
              (doom-color 'blue)
              (doom-color 'green)
              (doom-color 'magenta)
              (doom-color 'orange)
              (doom-color 'yellow)
              (doom-color 'teal)
              (doom-color 'violet)
              ))
    (defun org-plot/gnuplot-term-properties (_type)
      (format "background rgb '%s' size 1050,650"
              (doom-color 'bg)))
    (setq org-plot/gnuplot-script-preamble #'org-plot/generate-theme)
    (setq org-plot/gnuplot-term-extra #'org-plot/gnuplot-term-properties))
  (setq org-export-headline-levels 5) ; I like nesting
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-transient-hook! #'org-babel-execute-src-block
    (require 'ob-async))
  
  (defvar org-babel-auto-async-languages '()
    "Babel languages which should be executed asyncronously by default.")
  
  (defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
    "Eagarly add an :async parameter to the src information, unless it seems problematic.
  This only acts o languages in `org-babel-auto-async-languages'.
  Not added when either:
  + session is not \"none\"
  + :sync is set"
    :around #'org-babel-get-src-block-info
    (let ((result (funcall orig-fn light datum)))
      (when (and (string= "none" (cdr (assoc :session (caddr result))))
                 (member (car result) org-babel-auto-async-languages)
                 (not (assoc :async (caddr result))) ; don't duplicate
                 (not (assoc :sync (caddr result))))
        (push '(:async) (caddr result)))
      result))
  (after! cdlatex
    (setq cdlatex-env-alist
          '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
            ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
    (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
     cdlatex-math-symbol-alist
     '( ;; adding missing functions to 3rd level symbols
       (?_    ("\\downarrow"  ""           "\\inf"))
       (?2    ("^2"           "\\sqrt{?}"     ""     ))
       (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
       (?^    ("\\uparrow"    ""           "\\sup"))
       (?k    ("\\kappa"      ""           "\\ker"))
       (?m    ("\\mu"         ""           "\\lim"))
       (?c    (""             "\\circ"     "\\cos"))
       (?d    ("\\delta"      "\\partial"  "\\dim"))
       (?D    ("\\Delta"      "\\nabla"    "\\deg"))
       ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
       (?F    ("\\Phi"))
       ;; now just convenience
       (?.    ("\\cdot" "\\dots"))
       (?:    ("\\vdots" "\\ddots"))
       (?*    ("\\times" "\\star" "\\ast")))
     cdlatex-math-modify-alist
     '( ;; my own stuff
       (?B    "\\mathbb"        nil          t    nil  nil)
       (?a    "\\abs"           nil          t    nil  nil))))
  (package! laas :recipe (:local-repo "lisp/LaTeX-auto-activating-snippets"))
  (use-package! laas
    :hook (LaTeX-mode . laas-mode)
    :config
    (defun laas-tex-fold-maybe ()
      (unless (equal "/" aas-transient-snippet-key)
        (+latex-fold-last-macro-a)))
    (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))
)

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

(use-package! org-transclusion
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "<f12>" #'org-transclusion-mode))

(use-package! org-pandoc-import
  :after org)

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; [[file:config.org::*Markdown][Markdown:1]]
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
;; Markdown:1 ends here

;; [[file:config.org::*Markdown][Markdown:2]]
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))
;; Markdown:2 ends here

;; [[file:config.org::*Haskell][Haskell:1]]
(after! haskell-mode
  (set-formatter! 'stylish-haskell "stylish-haskell"
    :modes '(haskell-mode)))
;; Haskell:1 ends here

;; [[file:config.org::*Rust][Rust:1]]
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))
;; Rust:1 ends here
