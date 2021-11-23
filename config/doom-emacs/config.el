;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Icy-Thought"
      user-mail-address "gilganyx@pm.me")

(setq
 doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'Medium)
 doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20 :weight 'Medium)
 doom-theme 'doom-horizon)

;; (doom/set-frame-opacity 95)

(setq
 confirm-kill-emacs nil                         ; Disable Emacs confirm-exit messages.
 display-line-numbers-type t
 all-the-icons-dired-monochrome nil
 x-stretch-cursor t)                            ; Cursor size = glyph width.

(with-eval-after-load 'solaire-mode             ; Inverted solaire color-schemes.
  (add-to-list 'solaire-mode-themes-to-face-swap 'doom-horizon))

(setq
 doom-modeline-height 35
 doom-modeline-major-mode-icon t)                   ; Enable Major-Mode (file-type) icons.

(unless (equal "Battery status not available"      ; Display Battery-status in Doom-Modeline
               (battery))
  (display-battery-mode 1))

(setq-default delete-by-moving-to-trash t)              ; Move file tot trash when deleted.

(setq
 window-combination-resize t)

(setq
 evil-vsplit-window-right t
 evil-split-window-below t)                             ; Enter new window.

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

(setq org-directory "~/org/")                           ; Default Org-Directories

(setq org-ascii-charset 'utf-8
      org-ellipsis " ▾"
      org-src-fontify-natively t
      org-hide-block-startup nil
      org-startup-folded 'content
      org-cycle-separator-lines 2)

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

(remove-hook 'text-mode-hook #'spell-fu-mode)

(setq org-download-screenshot-method "shotgun -g $(hacksaw) %s")

(setq org-roam-directory "~/org"
      deft-directory "~/org")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  :config

  (setq-default pdf-view-display-size 'fit-page))

(setq which-key-idle-delay 0.5)

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/git/Icy-Thought/Snowflake/config/doom-emacs/README.org"))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config
                                              'run-at-end 'only-in-org-mode)))
