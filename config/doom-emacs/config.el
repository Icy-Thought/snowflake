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
(custom-set-faces!
  '(font-lock-builtin-face :slant italic)
  '(font-lock-comment-face :slant italic)
  '(font-lock-function-name-face :weight bold :slane italic)
  '(font-lock-keyword-face :slant italic))
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
  (setq evil-ex-substitute-global t))     ; I like my s/../.. to by global by default
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

;; [[file:config.org::*Doom-Modeline][Doom-Modeline:1]]
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
;; Doom-Modeline:1 ends here

;; [[file:config.org::*Prettier Page Breaks][Prettier Page Breaks:2]]
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))
;; Prettier Page Breaks:2 ends here

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
  \\usepackage{gfsartemisia-euler}
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
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  ;; org-latex-compilers = ("pdflatex" "xelatex" "lualatex"), which are the possible values for %latex
  (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (after! ox-latex
    (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
           (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                  article-sections))
           (hanging-secnum-preamble "
  \\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
  \\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
  \\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}
  ")
           (big-chap-preamble "
  \\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
  \\setkomafont{chapter}{\\normalfont\\Huge}
  \\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
  \\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
  \\renewcommand*{\\chapterformat}{%
    \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
  \\renewcommand*\\chapterlinesformat[3]{%
    \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
      \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
      \\hfill\\makebox[0pt][l]{#2}}
  "))
      (setcdr (assoc "article" org-latex-classes)
              `(,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
                ,@article-sections))
      (add-to-list 'org-latex-classes
                   `("report" ,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
                     ,@article-sections))
      (add-to-list 'org-latex-classes
                   `("book" ,(concat "\\documentclass[twoside=false]{scrbook}"
                                     big-chap-preamble hanging-secnum-preamble)
                     ,@book-sections))
      (add-to-list 'org-latex-classes
                   `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                     ,@article-sections))
      (add-to-list 'org-latex-classes
                   `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                     ,@article-sections))
      (add-to-list 'org-latex-classes
                   `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                     ,@book-sections))))
  
  (setq org-latex-tables-booktabs t
        org-latex-hyperref-template "
  \\providecolor{url}{HTML}{0077bb}
  \\providecolor{link}{HTML}{882255}
  \\providecolor{cite}{HTML}{999933}
  \\hypersetup{
    pdfauthor={%a},
    pdftitle={%t},
    pdfkeywords={%k},
    pdfsubject={%d},
    pdfcreator={%c},
    pdflang={%L},
    breaklinks=true,
    colorlinks=true,
    linkcolor=link,
    urlcolor=url,
    citecolor=cite\n}
  \\urlstyle{same}
  "
        org-latex-reference-command "\\cref{%s}")
  (defvar org-latex-embed-files-preamble "
  \\usepackage[main,include]{embedall}
  \\IfFileExists{./\\jobname.org}{\\embedfile[desc=The original file]{\\jobname.org}}{}
  "
    "Preamble that embeds files within the pdf.")
  
  (defvar org-latex-caption-preamble "
  \\usepackage{subcaption}
  \\usepackage[hypcap=true]{caption}
  \\setkomafont{caption}{\\sffamily\\small}
  \\setkomafont{captionlabel}{\\upshape\\bfseries}
  \\captionsetup{justification=raggedright,singlelinecheck=true}
  \\usepackage{capt-of} % required by Org
  "
    "Preamble that improves captions.")
  
  (defvar org-latex-checkbox-preamble "
  \\newcommand{\\checkboxUnchecked}{$\\square$}
  \\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
  \\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}
  "
    "Preamble that improves checkboxes.")
  
  (defvar org-latex-box-preamble "
  \\ExplSyntaxOn
  \\NewCoffin\\Content
  \\NewCoffin\\SideRule
  \\NewDocumentCommand{\\defsimplebox}{O{\\ding{117}} O{0.36em} m m m}{%
    % #1 ding, #2 ding offset, #3 name, #4 colour, #5 default label
    \\definecolor{#3}{HTML}{#4}
    \\NewDocumentEnvironment{#3}{ O{#5} }
    {
      \\vcoffin_set:Nnw \\Content { \\linewidth }
      \\noindent \\ignorespaces
      \\par\\vspace{-0.7\\baselineskip}%
      \\textcolor{#3}{#1}~\\textcolor{#3}{\\textbf{##1}}%
      \\vspace{-0.8\\baselineskip}
      \\begin{addmargin}[1em]{1em}
      }
      {
      \\end{addmargin}
      \\vspace{-0.5\\baselineskip}
      \\vcoffin_set_end:
      \\SetHorizontalCoffin\\SideRule{\\color{#3}\\rule{1pt}{\\CoffinTotalHeight\\Content}}
      \\JoinCoffins*\\Content[l,t]\\SideRule[l,t](#2,-0.7em)
      \\noindent\\TypesetCoffin\\Content
      \\vspace*{\\CoffinTotalHeight\\Content}\\bigskip
      \\vspace{-2\\baselineskip}
    }
  }
  \\ExplSyntaxOff
  "
    "Preamble that provides a macro for custom boxes.")
  (defun org-latex-embed-extra-files ()
    "Return a string that uses embedfile to embed all tangled files."
    (mapconcat
     (lambda (file-desc)
       (format "\\IfFileExists{%1$s}{\\embedfile[desc=%2$s]{%1$s}}{}"
               (thread-last (car file-desc)
                 (replace-regexp-in-string "\\\\" "\\\\\\\\")
                 (replace-regexp-in-string "~" "\\\\string~"))
               (cdr file-desc)))
     (append
      (mapcar (lambda (f-block)
                (let ((file-lang (cons (or (car f-block) (caddr (cadr f-block))) (caadr f-block))))
                  (cons (car file-lang) (format "Tangled %s file" (cdr file-lang)))))
              (org-babel-tangle-collect-blocks)) ; all files being tangled to
      (let (extra-files)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+embed:" nil t)
            (let* ((file-desc (split-string (org-element-property :value (org-element-at-point)) " :desc\\(?:ription\\)? ")))
              (push (cons (car file-desc) (or (cdr file-desc) "Extra file")) extra-files))))
        (nreverse extra-files)))
     "\n"))
  (defvar org-latex-embed-files t
    "Embed the source .org, .tex, and any tangled files.")
  (defvar org-latex-use-microtype t
    "Use the microtype pakage.")
  (defvar org-latex-italic-quotes t
    "Make \"quote\" environments italic.")
  (defvar org-latex-par-sep t
    "Vertically seperate paragraphs, and remove indentation.")
  
  (defvar org-latex-conditional-features
    '(("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]\\|\\\\\\]\\)+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\|jbig2\\)\\]\\]\\|\\\\includegraphics[\\[{]" . image)
      ("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]+?\\|\\\\\\]\\)\\.svg\\]\\]\\|\\\\includesvg[\\[{]" . svg)
      ("\\\\(\\|\\\\\\[\\|\\\\begin{\\(?:math\\|displaymath\\|equation\\|align\\|flalign\\|multiline\\|gather\\)[a-z]*\\*?}" . maths)
      ("^[ \t]*|" . table)
      ("cref:\\|\\cref{\\|\\[\\[[^\\]+\n?[^\\]\\]\\]" . cleveref)
      ("[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]" . acronym)
      ("\\+[^ ].*[^ ]\\+\\|_[^ ].*[^ ]_\\|\\\\uu?line\\|\\\\uwave\\|\\\\sout\\|\\\\xout\\|\\\\dashuline\\|\\dotuline\\|\\markoverwith" . underline)
      (":float wrap" . float-wrap)
      (":float sideways" . rotate)
      ("^[ \t]*#\\+caption:\\|\\\\caption" . caption)
      ("\\[\\[xkcd:" . (image caption))
      (org-latex-use-microtype . microtype)
      ((and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}") . italic-quotes)
      (org-latex-par-sep . par-sep)
      ((org-latex-embed-extra-files) . embed-files)
      ((and org-latex-embed-files "^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC") . embed-tangled)
      ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]" . checkbox)
      ("^[ \t]*#\\+begin_warning\\|\\\\begin{warning}" . box-warning)
      ("^[ \t]*#\\+begin_info\\|\\\\begin{info}"       . box-info)
      ("^[ \t]*#\\+begin_notes\\|\\\\begin{notes}"     . box-notes)
      ("^[ \t]*#\\+begin_success\\|\\\\begin{success}" . box-success)
      ("^[ \t]*#\\+begin_error\\|\\\\begin{error}"     . box-error))
    "Org feature tests and associated LaTeX feature flags.
  
  Alist where the car is a test for the presense of the feature,
  and the cdr is either a single feature symbol or list of feature symbols.
  
  When a string, it is used as a regex search in the buffer.
  The feature is registered as present when there is a match.
  
  The car can also be a
  - symbol, the value of which is fetched
  - function, which is called with info as an argument
  - list, which is `eval'uated
  
  If the symbol, function, or list produces a string: that is used as a regex
  search in the buffer. Otherwise any non-nil return value will indicate the
  existance of the feature.")
  (defvar org-latex-feature-implementations
    '((image         :snippet "\\usepackage{graphicx}" :order 2)
      (svg           :snippet "\\usepackage[inkscapelatex=false]{svg}" :order 2)
      (maths         :snippet "\\usepackage[nofont]{bmc-maths}" :order 0.2)
      (table         :snippet "\\usepackage{longtable}\n\\usepackage{booktabs}" :order 2)
      (cleveref      :snippet "\\usepackage[capitalize]{cleveref}" :order 1) ; after bmc-maths
      (underline     :snippet "\\usepackage[normalem]{ulem}" :order 0.5)
      (float-wrap    :snippet "\\usepackage{wrapfig}" :order 2)
      (rotate        :snippet "\\usepackage{rotating}" :order 2)
      (caption       :snippet org-latex-caption-preamble :order 2.1)
      (microtype     :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}\n" :order 0.1)
      (embed-files   :snippet org-latex-embed-files-preamble :order -2)
      (embed-tangled :requires embed-files :snippet (concat (org-latex-embed-extra-files) "\n") :order -1)
      (acronym       :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}" :order 0.4)
      (italic-quotes :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n" :order 0.5)
      (par-sep       :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n" :order 0.5)
      (.pifont       :snippet "\\usepackage{pifont}")
      (.xcoffins     :snippet "\\usepackage{xcoffins}")
      (checkbox      :requires .pifont :order 3
                     :snippet (concat (unless (memq 'maths features)
                                        "\\usepackage{amssymb} % provides \\square")
                                      org-latex-checkbox-preamble))
      (.fancy-box    :requires (.pifont .xcoffins) :snippet org-latex-box-preamble :order 3.9)
      (box-warning   :requires .fancy-box :snippet "\\defsimplebox{warning}{e66100}{Warning}" :order 4)
      (box-info      :requires .fancy-box :snippet "\\defsimplebox{info}{3584e4}{Information}" :order 4)
      (box-notes     :requires .fancy-box :snippet "\\defsimplebox{notes}{26a269}{Notes}" :order 4)
      (box-success   :requires .fancy-box :snippet "\\defsimplebox{success}{26a269}{\\vspace{-\\baselineskip}}" :order 4)
      (box-error     :requires .fancy-box :snippet "\\defsimplebox{error}{c01c28}{Important}" :order 4))
    "LaTeX features and details required to implement them.
  
  List where the car is the feature symbol, and the rest forms a plist with the
  following keys:
  - :snippet, which may be either
    - a string which should be included in the preamble
    - a symbol, the value of which is included in the preamble
    - a function, which is evaluated with the list of feature flags as its
      single argument. The result of which is included in the preamble
    - a list, which is passed to `eval', with a list of feature flags available
      as \"features\"
  
  - :requires, a feature or list of features that must be available
  - :when, a feature or list of features that when all available should cause this
      to be automatically enabled.
  - :prevents, a feature or list of features that should be masked
  - :order, for when ordering is important. Lower values appear first.
      The default is 0.
  - :eager, when non-nil the feature will be eagerly loaded, i.e. without being detected.")
  (defun org-latex-detect-features (&optional buffer info)
    "List features from `org-latex-conditional-features' detected in BUFFER."
    (let ((case-fold-search nil))
      (with-current-buffer (or buffer (current-buffer))
        (delete-dups
         (mapcan (lambda (construct-feature)
                   (when (let ((out (pcase (car construct-feature)
                                      ((pred stringp) (car construct-feature))
                                      ((pred functionp) (funcall (car construct-feature) info))
                                      ((pred listp) (eval (car construct-feature)))
                                      ((pred symbolp) (symbol-value (car construct-feature)))
                                      (_ (user-error "org-latex-conditional-features key %s unable to be used" (car construct-feature))))))
                           (if (stringp out)
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward out nil t))
                             out))
                     (if (listp (cdr construct-feature)) (cdr construct-feature) (list (cdr construct-feature)))))
                 org-latex-conditional-features)))))
  (defun org-latex-expand-features (features)
    "For each feature in FEATURES process :requires, :when, and :prevents keywords and sort according to :order."
    (dolist (feature features)
      (unless (assoc feature org-latex-feature-implementations)
        (message "Feature %s not provided in org-latex-feature-implementations, ignoring." feature)
        (setq features (remove feature features))))
    (setq current features)
    (while current
      (when-let ((requirements (plist-get (cdr (assq (car current) org-latex-feature-implementations)) :requires)))
        (setcdr current (if (listp requirements)
                            (append requirements (cdr current))
                          (cons requirements (cdr current)))))
      (setq current (cdr current)))
    (dolist (potential-feature
             (append features (delq nil (mapcar (lambda (feat)
                                                  (when (plist-get (cdr feat) :eager)
                                                    (car feat)))
                                                org-latex-feature-implementations))))
      (when-let ((prerequisites (plist-get (cdr (assoc potential-feature org-latex-feature-implementations)) :when)))
        (setf features (if (if (listp prerequisites)
                               (cl-every (lambda (preq) (memq preq features)) prerequisites)
                             (memq prerequisites features))
                           (append (list potential-feature) features)
                         (delq potential-feature features)))))
    (dolist (feature features)
      (when-let ((prevents (plist-get (cdr (assoc feature org-latex-feature-implementations)) :prevents)))
        (setf features (cl-set-difference features (if (listp prevents) prevents (list prevents))))))
    (sort (delete-dups features)
          (lambda (feat1 feat2)
            (if (< (or (plist-get (cdr (assoc feat1 org-latex-feature-implementations)) :order) 1)
                   (or (plist-get (cdr (assoc feat2 org-latex-feature-implementations)) :order) 1))
                t nil))))
  (defun org-latex-generate-features-preamble (features)
    "Generate the LaTeX preamble content required to provide FEATURES.
  This is done according to `org-latex-feature-implementations'"
    (let ((expanded-features (org-latex-expand-features features)))
      (concat
       (format "\n%% features: %s\n" expanded-features)
       (mapconcat (lambda (feature)
                    (when-let ((snippet (plist-get (cdr (assoc feature org-latex-feature-implementations)) :snippet)))
                      (concat
                       (pcase snippet
                         ((pred stringp) snippet)
                         ((pred functionp) (funcall snippet features))
                         ((pred listp) (eval `(let ((features ',features)) (,@snippet))))
                         ((pred symbolp) (symbol-value snippet))
                         (_ (user-error "org-latex-feature-implementations :snippet value %s unable to be used" snippet)))
                       "\n")))
                  expanded-features
                  "")
       "% end features\n")))
  (defvar info--tmp nil)
  
  (defadvice! org-latex-save-info (info &optional t_ s_)
    :before #'org-latex-make-preamble
    (setq info--tmp info))
  
  (defadvice! org-splice-latex-header-and-generated-preamble-a (orig-fn tpl def-pkg pkg snippets-p &optional extra)
    "Dynamically insert preamble content based on `org-latex-conditional-preambles'."
    :around #'org-splice-latex-header
    (let ((header (funcall orig-fn tpl def-pkg pkg snippets-p extra)))
      (if snippets-p header
        (concat header
                (org-latex-generate-features-preamble (org-latex-detect-features nil info--tmp))
                "\n"))))
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "xcolor" nil) ; Generally useful
          ("" "hyperref" nil)))
  (defvar org-latex-default-fontset 'artemisia
    "Fontset from `org-latex-fontsets' to use by default.
  As cm (computer modern) is TeX's default, that causes nothing
  to be added to the document.
  
  If \"nil\" no custom fonts will ever be used.")
  
  (eval '(cl-pushnew '(:latex-font-set nil "fontset" org-latex-default-fontset)
                     (org-export-backend-options (org-export-get-backend 'latex))))
  (defun org-latex-fontset-entry ()
    "Get the fontset spec of the current file.
  Has format \"name\" or \"name-style\" where 'name' is one of
  the cars in `org-latex-fontsets'."
    (let ((fontset-spec
           (symbol-name
            (or (car (delq nil
                           (mapcar
                            (lambda (opt-line)
                              (plist-get (org-export--parse-option-keyword opt-line 'latex)
                                         :latex-font-set))
                            (cdar (org-collect-keywords '("OPTIONS"))))))
                org-latex-default-fontset))))
      (cons (intern (car (split-string fontset-spec "-")))
            (when (cadr (split-string fontset-spec "-"))
              (intern (concat ":" (cadr (split-string fontset-spec "-"))))))))
  
  (defun org-latex-fontset (&rest desired-styles)
    "Generate a LaTeX preamble snippet which applies the current fontset for DESIRED-STYLES."
    (let* ((fontset-spec (org-latex-fontset-entry))
           (fontset (alist-get (car fontset-spec) org-latex-fontsets)))
      (if fontset
          (concat
           (mapconcat
            (lambda (style)
              (when (plist-get fontset style)
                (concat (plist-get fontset style) "\n")))
            desired-styles
            "")
           (when (memq (cdr fontset-spec) desired-styles)
             (pcase (cdr fontset-spec)
               (:serif "\\renewcommand{\\familydefault}{\\rmdefault}\n")
               (:sans "\\renewcommand{\\familydefault}{\\sfdefault}\n")
               (:mono "\\renewcommand{\\familydefault}{\\ttdefault}\n"))))
        (error "Font-set %s is not provided in org-latex-fontsets" (car fontset-spec)))))
  (add-to-list 'org-latex-conditional-features '(org-latex-default-fontset . custom-font) t)
  (add-to-list 'org-latex-feature-implementations '(custom-font :snippet (org-latex-fontset :serif :sans :mono) :order 0) t)
  (add-to-list 'org-latex-feature-implementations '(.custom-maths-font :eager t :when (custom-font maths) :snippet (org-latex-fontset :maths) :order 0.3) t)
  (defvar org-latex-fontsets
    '((cm nil) ; Computer Modern
      (## nil) ; no-font set
      (artemisia
       :sans "\\usepackage{gfsartemisia}"
       :maths "\\usepackage{gfsartemisia-euler}")
      (source
       :serif "\\usepackage[osf,semibold]{sourceserifpro}"
       :sans "\\usepackage[osf,semibold]{sourcesanspro}"
       :mono "\\usepackage[scale=0.92]{sourcecodepro}"
       :maths "\\usepackage{gfsartemisia-euler}")
      (times ; Times Newer Roman
       :serif "\\setmainfont{Times Newer Roman}"
       :maths "\\usepackage{newtxmath}")))
  (defvar org-latex-cover-page 'auto
    "When t, use a cover page by default.
  When auto, use a cover page when the document's wordcount exceeds
  `org-latex-cover-page-wordcount-threshold'.
  
  Set with #+option: coverpage:{yes,auto,no} in org buffers.")
  (defvar org-latex-cover-page-wordcount-threshold 5000
    "Document word count at which a cover page will be used automatically.
  This condition is applied when cover page option is set to auto.")
  (defvar org-latex-subtitle-coverpage-format "\\\\\\bigskip\n\\LARGE\\mdseries\\itshape\\color{black!80} %s\\par"
    "Variant of `org-latex-subtitle-format' to use with the cover page.")
  (defvar org-latex-cover-page-maketitle "
  \\usepackage{tikz}
  \\usetikzlibrary{shapes.geometric}
  \\usetikzlibrary{calc}
  
  \\newsavebox\\orgicon
  \\begin{lrbox}{\\orgicon}
    \\begin{tikzpicture}[y=0.80pt, x=0.80pt, inner sep=0pt, outer sep=0pt]
      \\path[fill=black!6] (16.15,24.00) .. controls (15.58,24.00) and (13.99,20.69) .. (12.77,18.06)arc(215.55:180.20:2.19) .. controls (12.33,19.91) and (11.27,19.09) .. (11.43,18.05) .. controls (11.36,18.09) and (10.17,17.83) .. (10.17,17.82) .. controls (9.94,18.75) and (9.37,19.44) .. (9.02,18.39) .. controls (8.32,16.72) and (8.14,15.40) .. (9.13,13.80) .. controls (8.22,9.74) and (2.18,7.75) .. (2.81,4.47) .. controls (2.99,4.47) and (4.45,0.99) .. (9.15,2.41) .. controls (14.71,3.99) and (17.77,0.30) .. (18.13,0.04) .. controls (18.65,-0.49) and (16.78,4.61) .. (12.83,6.90) .. controls (10.49,8.18) and (11.96,10.38) .. (12.12,11.15) .. controls (12.12,11.15) and (14.00,9.84) .. (15.36,11.85) .. controls (16.58,11.53) and (17.40,12.07) .. (18.46,11.69) .. controls (19.10,11.41) and (21.79,11.58) .. (20.79,13.08) .. controls (20.79,13.08) and (21.71,13.90) .. (21.80,13.99) .. controls (21.97,14.75) and (21.59,14.91) .. (21.47,15.12) .. controls (21.44,15.60) and (21.04,15.79) .. (20.55,15.44) .. controls (19.45,15.64) and (18.36,15.55) .. (17.83,15.59) .. controls (16.65,15.76) and (15.67,16.38) .. (15.67,16.38) .. controls (15.40,17.19) and (14.82,17.01) .. (14.09,17.32) .. controls (14.70,18.69) and (14.76,19.32) .. (15.50,21.32) .. controls (15.76,22.37) and (16.54,24.00) .. (16.15,24.00) -- cycle(7.83,16.74) .. controls (6.83,15.71) and (5.72,15.70) .. (4.05,15.42) .. controls (2.75,15.19) and (0.39,12.97) .. (0.02,10.68) .. controls (-0.02,10.07) and (-0.06,8.50) .. (0.45,7.18) .. controls (0.94,6.05) and (1.27,5.45) .. (2.29,4.85) .. controls (1.41,8.02) and (7.59,10.18) .. (8.55,13.80) -- (8.55,13.80) .. controls (7.73,15.00) and (7.80,15.64) .. (7.83,16.74) -- cycle;
    \\end{tikzpicture}
  \\end{lrbox}
  
  \\makeatletter
  \\g@addto@macro\\tableofcontents{\\clearpage}
  \\renewcommand\\maketitle{
    \\thispagestyle{empty}
    \\hyphenpenalty=10000 % hyphens look bad in titles
    \\renewcommand{\\baselinestretch}{1.1}
    \\let\\oldtoday\\today
    \\renewcommand{\\today}{\\LARGE\\number\\year\\\\\\large%
      \\ifcase \\month \\or Jan\\or Feb\\or Mar\\or Apr\\or May \\or Jun\\or Jul\\or Aug\\or Sep\\or Oct\\or Nov\\or Dec\\fi
      ~\\number\\day}
    \\begin{tikzpicture}[remember picture,overlay]
      %% Background Polygons %%
      \\foreach \\i in {2.5,...,22} % bottom left
      {\\node[rounded corners,black!3.5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.west)+(2.5,-4.2)$) {} ;}
      \\foreach \\i in {0.5,...,22} % top left
      {\\node[rounded corners,black!5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {} ;}
      \\node[rounded corners,fill=black!4,regular polygon,regular polygon sides=6, minimum size=5.5 cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {};
      \\foreach \\i in {0.5,...,24} % top right
      {\\node[rounded corners,black!2,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {} ;}
      \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2.5 cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {};
      \\foreach \\i in {21,...,3} % bottom right
      {\\node[black!3,rounded corners,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {} ;}
      \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2 cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {};
      \\node[align=center, scale=1.4] at ($(current page.south east)+(-1.5,0.75)$) {\\usebox\\orgicon};
      %% Text %%
      \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=3cm, rounded corners,font=\\Huge\\bfseries] at ($(current page.north east)+(-2,-8.5)$)
      {\\@title};
      \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=2cm, rounded corners, font=\\Large] at ($(current page.north east)+(-2,-11.8)$)
      {\\scshape \\@author};
      \\renewcommand{\\baselinestretch}{0.75}
      \\node[align=center,rounded corners,fill=black!3,text=black,regular polygon,regular polygon sides=6, minimum size=2.5 cm,inner sep=0, font=\\Large\\bfseries ] at ($(current page.west)+(2.5,-4.2)$)
      {\\@date};
    \\end{tikzpicture}
    \\let\\today\\oldtoday
    \\clearpage}
  \\makeatother
  "
    "LaTeX snippet for the preamble that sets \\maketitle to produce a cover page.")
  
  (eval '(cl-pushnew '(:latex-cover-page nil "coverpage" org-latex-cover-page)
                     (org-export-backend-options (org-export-get-backend 'latex))))
  
  (defun org-latex-cover-page-p ()
    "Whether a cover page should be used when exporting this Org file."
    (pcase (or (car
                (delq nil
                      (mapcar
                       (lambda (opt-line)
                         (plist-get (org-export--parse-option-keyword opt-line 'latex) :latex-cover-page))
                       (cdar (org-collect-keywords '("OPTIONS"))))))
               org-latex-cover-page)
      ((or 't 'yes) t)
      ('auto (when (> (count-words (point-min) (point-max)) org-latex-cover-page-wordcount-threshold) t))
      (_ nil)))
  
  (defadvice! org-latex-set-coverpage-subtitle-format-a (contents info)
    "Set the subtitle format when a cover page is being used."
    :before #'org-latex-template
    (when (org-latex-cover-page-p)
      (setf info (plist-put info :latex-subtitle-format org-latex-subtitle-coverpage-format))))
  
  (add-to-list 'org-latex-feature-implementations '(cover-page :snippet org-latex-cover-page-maketitle :order 9) t)
  (add-to-list 'org-latex-conditional-features '((org-latex-cover-page-p) . cover-page) t)
  (defvar org-latex-condense-lists t
    "Reduce the space between list items.")
  (defvar org-latex-condensed-lists "
  \\newcommand{\\setuplistspacing}{\\setlength{\\itemsep}{-0.5ex}\\setlength{\\parskip}{1.5ex}\\setlength{\\parsep}{0pt}}
  \\let\\olditem\\itemize\\renewcommand{\\itemize}{\\olditem\\setuplistspacing}
  \\let\\oldenum\\enumerate\\renewcommand{\\enumerate}{\\oldenum\\setuplistspacing}
  \\let\\olddesc\\description\\renewcommand{\\description}{\\olddesc\\setuplistspacing}
  ")
  
  (add-to-list 'org-latex-conditional-features '((and org-latex-condense-lists "^[ \t]*[-+]\\|^[ \t]*[1Aa][.)] ") . condensed-lists) t)
  (add-to-list 'org-latex-feature-implementations '(condensed-lists :snippet org-latex-condensed-lists :order 0.7) t)
  (setq org-latex-listings 'engraved) ; NOTE non-standard value
  (defadvice! org-latex-src-block-engraved (orig-fn src-block contents info)
    "Like `org-latex-src-block', but supporting an engraved backend"
    :around #'org-latex-src-block
    (if (eq 'engraved (plist-get info :latex-listings))
        (org-latex-scr-block--engraved src-block contents info)
      (funcall orig-fn src-block contents info)))
  
  (defadvice! org-latex-inline-src-block-engraved (orig-fn inline-src-block contents info)
    "Like `org-latex-inline-src-block', but supporting an engraved backend"
    :around #'org-latex-inline-src-block
    (if (eq 'engraved (plist-get info :latex-listings))
        (org-latex-inline-scr-block--engraved inline-src-block contents info)
      (funcall orig-fn src-block contents info)))
  
  (defvar-local org-export-has-code-p nil)
  
  (defadvice! org-export-expect-no-code (&rest _)
    :before #'org-export-as
    (setq org-export-has-code-p nil))
  
  (defadvice! org-export-register-code (&rest _)
    :after #'org-latex-src-block-engraved
    :after #'org-latex-inline-src-block-engraved
    (setq org-export-has-code-p t))
  
  (setq org-latex-engraved-code-preamble "
  \\usepackage{fvextra}
  \\fvset{
    commandchars=\\\\\\{\\},
    highlightcolor=white!95!black!80!blue,
    breaklines=true,
    breaksymbol=\\color{white!60!black}\\tiny\\ensuremath{\\hookrightarrow}}
  \\renewcommand\\theFancyVerbLine{\\footnotesize\\color{black!40!white}\\arabic{FancyVerbLine}}
  
  \\definecolor{codebackground}{HTML}{f7f7f7}
  \\definecolor{codeborder}{HTML}{f0f0f0}
  \\providecolor{EFD}{HTML}{28292e}
  
  % TODO have code boxes keep line vertical alignment
  \\usepackage[breakable,xparse]{tcolorbox}
  \\DeclareTColorBox[]{Code}{o}%
  {colback=codebackground, colframe=codeborder,
    fontupper=\\footnotesize,
    colupper=EFD,
    IfNoValueTF={#1}%
    {boxsep=2pt, arc=2.5pt, outer arc=2.5pt,
      boxrule=0.5pt, left=2pt}%
    {boxsep=2.5pt, arc=0pt, outer arc=0pt,
      boxrule=0pt, leftrule=1.5pt, left=0.5pt},
    right=2pt, top=1pt, bottom=0.5pt,
    breakable}
  ")
  
  (add-to-list 'org-latex-conditional-features '((and org-export-has-code-p "^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC\\|src_[A-Za-z]") . engraved-code) t)
  (add-to-list 'org-latex-conditional-features '("^[ \t]*#\\+begin_example\\|^[ \t]*#\\+BEGIN_EXAMPLE" . engraved-code-setup) t)
  (add-to-list 'org-latex-feature-implementations '(engraved-code :requires engraved-code-setup :snippet (engrave-faces-latex-gen-preamble) :order 99) t)
  (add-to-list 'org-latex-feature-implementations '(engraved-code-setup :snippet org-latex-engraved-code-preamble :order 98) t)
  
  (defun org-latex-scr-block--engraved (src-block contents info)
    (let* ((lang (org-element-property :language src-block))
           (attributes (org-export-read-attribute :attr_latex src-block))
           (float (plist-get attributes :float))
           (num-start (org-export-get-loc src-block info))
           (retain-labels (org-element-property :retain-labels src-block))
           (caption (org-element-property :caption src-block))
           (caption-above-p (org-latex--caption-above-p src-block info))
           (caption-str (org-latex--caption/label-string src-block info))
           (placement (or (org-unbracket-string "[" "]" (plist-get attributes :placement))
                          (plist-get info :latex-default-figure-position)))
           (float-env
            (cond
             ((string= "multicolumn" float)
              (format "\\begin{listing*}[%s]\n%s%%s\n%s\\end{listing*}"
                      placement
                      (if caption-above-p caption-str "")
                      (if caption-above-p "" caption-str)))
             (caption
              (format "\\begin{listing}[%s]\n%s%%s\n%s\\end{listing}"
                      placement
                      (if caption-above-p caption-str "")
                      (if caption-above-p "" caption-str)))
             ((string= "t" float)
              (concat (format "\\begin{listing}[%s]\n"
                              placement)
                      "%s\n\\end{listing}"))
             (t "%s")))
           (options (plist-get info :latex-minted-options))
           (content-buffer
            (with-temp-buffer
              (insert
               (let* ((code-info (org-export-unravel-code src-block))
                      (max-width
                       (apply 'max
                              (mapcar 'length
                                      (org-split-string (car code-info)
                                                        "\n")))))
                 (org-export-format-code
                  (car code-info)
                  (lambda (loc _num ref)
                    (concat
                     loc
                     (when ref
                       ;; Ensure references are flushed to the right,
                       ;; separated with 6 spaces from the widest line
                       ;; of code.
                       (concat (make-string (+ (- max-width (length loc)) 6)
                                            ?\s)
                               (format "(%s)" ref)))))
                  nil (and retain-labels (cdr code-info)))))
              (funcall (org-src-get-lang-mode lang))
              (engrave-faces-latex-buffer)))
           (content
            (with-current-buffer content-buffer
              (buffer-string)))
           (body
            (format
             "\\begin{Code}\n\\begin{Verbatim}[%s]\n%s\\end{Verbatim}\n\\end{Code}"
             ;; Options.
             (concat
              (org-latex--make-option-string
               (if (or (not num-start) (assoc "linenos" options))
                   options
                 (append
                  `(("linenos")
                    ("firstnumber" ,(number-to-string (1+ num-start))))
                  options)))
              (let ((local-options (plist-get attributes :options)))
                (and local-options (concat "," local-options))))
             content)))
      (kill-buffer content-buffer)
      ;; Return value.
      (format float-env body)))
  
  (defun org-latex-inline-scr-block--engraved (inline-src-block _contents info)
    (let ((options (org-latex--make-option-string
                    (plist-get info :latex-minted-options)))
          code-buffer code)
      (setq code-buffer
            (with-temp-buffer
              (insert (org-element-property :value inline-src-block))
              (funcall (org-src-get-lang-mode
                        (org-element-property :language inline-src-block)))
              (engrave-faces-latex-buffer)))
      (setq code (with-current-buffer code-buffer
                   (buffer-string)))
      (kill-buffer code-buffer)
      (format "\\Verb%s{%s}"
              (if (string= options "") ""
                (format "[%s]" options))
              code)))
  (add-to-list 'org-latex-feature-implementations
               '(.no-protrusion-in-code :snippet "\\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}"
                                        :when (microtype engraved-code-setup)
                                        :eager t
                                        :order 98.5) t)
  (defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
    "Like `org-latex-example-block', but supporting an engraved backend"
    :around #'org-latex-example-block
    (let ((output-block (funcall orig-fn example-block contents info)))
      (if (eq 'engraved (plist-get info :latex-listings))
          (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
        output-block)))
  (defvar +org-latex-abbreviations
    '(;; Latin
      "cf." "e.g." "etc." "et al." "i.e." "v." "vs." "viz." "n.b."
      ;; Corperate
      "inc." "govt." "ltd." "pty." "dept."
      ;; Temporal
      "est." "c."
      ;; Honorifics
      "Prof." "Dr." "Mr." "Mrs." "Ms." "Miss." "Sr." "Jr."
      ;; Components of a work
      "ed." "vol." "sec." "chap." "pt." "pp." "op." "no."
      ;; Common usage
      "approx." "misc." "min." "max.")
    "A list of abbreviations that should be spaced correctly when exporting to LaTeX.")
  
  (defun +org-latex-correct-latin-abbreviation-spaces (text backend _info)
    "Normalise spaces after Latin abbreviations."
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string (rx (group (or line-start space)
                                           (regexp (regexp-opt-group +org-latex-abbreviations)))
                                    (or line-end space))
                                "\\1\\\\ "
                                text)))
  
  (add-to-list 'org-export-filter-paragraph-functions #'+org-latex-correct-latin-abbreviation-spaces t)
  (setq org-latex-text-markup-alist
        '((bold . "\\textbf{%s}")
          (code . protectedtexttt)
          (italic . "\\emph{%s}")
          (strike-through . "\\sout{%s}")
          (underline . "\\uline{%s}")
          (verbatim . verb)))
)

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

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! ox-chameleon
  :after ox)

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
