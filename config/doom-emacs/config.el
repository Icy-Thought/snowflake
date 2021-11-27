;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Icy-Thought"
      user-mail-address "gilganyx@pm.me")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'Medium)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20 :weight 'Medium)
      doom-theme 'doom-horizon
      doom-themes-treemacs-theme "doom-horizon")

;; (doom/set-frame-opacity 95)

(setq confirm-kill-emacs nil                      ; Disable Emacs confirm-exit messages.
      display-line-numbers-type 'relative
      treemacs-width 25
      all-the-icons-dired-monochrome nil
      x-stretch-cursor t)                         ; Cursor size = glyph width.

(with-eval-after-load 'solaire-mode ; Invert -> Darker Colorscheme.
  (add-to-list 'solaire-mode-themes-to-face-swap 'doom-horizon))

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
  (setq doom-modeline-enable-word-count t))

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "JetBrainsMono Nerd Font" 125)

  (setq centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-height 30
        centaur-tabs-close-button ""
        centaur-tabs-down-tab-text "✦"
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵")

  (custom-set-faces!
    `(tab-line :background ,(doom-color 'base1) :foreground ,(doom-color 'base1))
    `(centaur-tabs-default :background ,(doom-color 'base1) :foreground ,(doom-color 'base1))
    `(centaur-tabs-unselected :background ,(doom-color 'base1) :foreground ,(doom-color 'base4))
    `(centaur-tabs-selected :background ,(doom-color 'bg) :foreground ,(doom-color 'blue))))

(setq scroll-margin 2)

(setq-default delete-by-moving-to-trash t)              ; Move file tot trash when deleted.

(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))

(setq window-combination-resize t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)                        ; Enter new window.

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))                                 ; Call forward ivy.

(setq +ivy-buffer-preview t)                            ; Preview new buffer.

(map! :map evil-window-map
      "SPC" #'rotate-layout

      ;; Navigation
      "<left>"          #'evil-window-left
      "<down>"          #'evil-window-down
      "<up>"            #'evil-window-up
      "<right>"         #'evil-window-right

      ;; Swapping windows
      "C-<left>"        #'+evil/window-move-left
      "C-<down>"        #'+evil/window-move-down
      "C-<up>"          #'+evil/window-move-up
      "C-<right>"       #'+evil/window-move-right)

(map! [C-next]  #'next-buffer
      [C-prior] #'previous-buffer)
;; or
(map! "<C-next>"  #'next-buffer
      "<C-prior>" #'previous-buffer)

(map! :n [mouse-8] #'scroll-up-command
      :n [mouse-9] #'scroll-down-command)

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 1
        company-show-numbers t))

(after! haskell-mode
  (set-formatter! 'stylish-haskell "stylish-haskell"
    :modes '(haskell-mode)))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(setq org-directory "~/org/"                            ; Default Org-Directories
      org-ascii-charset 'utf-8
      org-list-allow-alphabetical t                     ; have a. A. a) A) list bullets
      org-export-in-background t                        ; run export processes in external emacs process
      org-catch-invisible-edits 'smart)                 ; try not to accidently do weird stuff in invisible regions

(setq org-ascii-charset 'utf-8
      org-ellipsis " ▾"
      org-src-fontify-natively t
      org-hide-block-startup nil
      org-startup-folded 'content
      org-cycle-separator-lines 2)

(setq org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+") ("1)" . "a)") ("1." . "a.")))

(custom-set-faces!
  '(org-document-title :height 1.20)
  '(outline-1 :weight ExtraBold :height 1.25)
  '(outline-2 :weight Bold :height 1.15)
  '(outline-3 :weight Bold :height 1.12)
  '(outline-4 :weight Medium :height 1.09)
  '(outline-5 :weight Medium :height 1.06)
  '(outline-6 :weight Medium :height 1.03)
  '(outline-8 :weight Medium)
  '(outline-9 :weight Medium))

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

(remove-hook 'text-mode-hook #'spell-fu-mode)

(setq org-download-screenshot-method "shotgun -g $(hacksaw) %s")

(setq org-roam-directory "~/org/roam/"
      deft-directory "~/org/roam/")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t)
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(after! org-roam
   (setq +org-roam-open-buffer-on-find-file nil))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  :config

  (setq-default pdf-view-display-size 'fit-page))

(setq which-key-idle-delay 0.5)
