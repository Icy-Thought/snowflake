;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       chinese
       ;;japanese
       )

(doom! :completion
       (company +auto                           ; the ultimate code completion backend
                +childframe)                    ; Better UI!
       (ivy +icons                              ; a search engine for love and life
            +fuzzy                              ; fuzzy matching.
            +prescient)                         ; for sorting/filtering.
       )

(doom! :ui
       deft                                     ; notational velocity for Emacs
       doom                                     ; what makes DOOM look the way it does
       doom-dashboard                           ; a nifty splash screen for Emacs
       doom-quit                                ; DOOM quit-message prompts when you quit Emacs
       hl-todo                                  ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline                                 ; snazzy, Atom-inspired modeline, plus API
       ophints                                  ; highlight the region an operation acts on
       (popup +all                              ; tame sudden yet inevitable temporary windows
              +defaults)
       tabs                                     ; a tab bar for Emacs
       (emoji +unicode
              +github)
       vc-gutter                                ; vcs diff in the fringe
       vi-tilde-fringe                          ; fringe tildes to mark beyond EOB
       workspaces                               ; tab emulation, persistence & separate workspaces
       treemacs                                 ; a project drawer, like neotree but cooler
       ;;zen                                    ; distraction-free coding or writing
       )

(doom! :editor
       (evil +everywhere)                       ; come to the dark side, we have cookies
       file-templates                           ; auto-snippets for empty files
       fold                                     ; (nigh) universal code folding
       (format +onsave)                         ;  automated prettiness
       ;;multiple-cursors                       ; editing in many places at once
       snippets                                 ; my elves. They type so I don't have to
       word-wrap                                ; soft wrapping with language-aware indent
       )

(doom! :emacs
       (dired +ranger                           ; making dired pretty [functional]
              +icons)
       electric                                 ; smarter, keyword-based electric-indent
       (ibuffer +icon)                          ; interactive buffer management
       (undo +tree)                             ; persistent, smarter undo for your inevitable mistakes
       vc                                       ; version-control and Emacs, sitting in a tree
       )

(doom! :term
       ;;eshell                                 ; the elisp shell that works everywhere
       vterm                                    ; the best terminal emulation in Emacs
       )

(doom! :checkers
       (syntax +childframe)                     ; Tasing you for every semicolon you forget.
       (spell +aspell)                          ; Tasing you for mispelling on the fly.
       grammar                                  ; tasing grammar mistake every you make
       )

(doom! :tools
       (debugger +lsp)                          ; FIXME stepping through code, to help you add bugs
       (eval +overlay)                          ; run code, run (also, repls)
       (lookup +dictionary)                     ; navigate your code and its documentation
       lsp
       (magit +forge)                           ; a git porcelain for Emacs
       make                                     ; run make tasks from Emacs
       pdf                                      ; pdf enhancements
       )

(doom! :os
       tty                                      ; improve the terminal Emacs experience
       )

(doom! :lang
       ;;cc                                     ; C/C++/Obj-C madness.
       ;;common-lisp                            ; If you've seen one lisp, you've seen them all.
       ;;coq                                    ; Proofs-as-programs.
       ;;data                                   ; Config/data formats.
       ;;(dart +flutter)                        ; Paint ui and not much else.
       emacs-lisp                               ; Drown in parentheses.
       ;;erlang                                 ; An elegant language for a more civilized age.
       ;;ess                                    ; Emacs speaks statistics.
       ;;(go +lsp)                              ; The hipster dialect.
       (haskell +lsp)                           ; a language that's lazier than I am
       json                                     ; At least it ain't XML.
       ;;(java +meghanada)                      ; The poster child for carpal tunnel syndrome.
       (javascript +lsp)                        ; All(hope(abandon(ye(who(enter(here)))))).
       ;;(julia +lsp)                           ; A better, faster MATLAB.
       ;;kotlin                                 ; A better, slicker Java(Script).
       (latex +latexmk                          ; LaTeX compiler of choice. (alt: lualatex)
              +cdlatex                          ; LaTeX math completions.
              +fold)                            ; Folding ability inside LaTeX.
       ;;ledger                                 ; An accounting system in Emacs.
       lua                                      ; One-based indices? one-based indices.
       nix                                      ; I hereby declare "nix geht mehr!"
       (org +pretty                             ; Prettify org-mode.
            +dragndrop                          ; Enables drag & drop in org-mode.
            +gnuplot                            ; Enables gnu-plotting.
            +present                            ; Org-mode presentations.
            +jupyter                            ; ipython/jupyter support for babel.
            ;;+hugo                             ; Enable ox-hugo support.
            +roam)                              ; Note-taking done correct in org-mode.
       ;;perl                                   ; write code no one else can comprehend
       (python +lsp                             ; Python + LSP support.
               +pyright)                        ; Beautiful is better than ugly
       ;;qt                                     ; The 'cutest' gui framework ever
       ;;(ruby +rails)                          ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)                              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala                                  ; Java, but good
       ;;scheme                                 ; A fully conniving family of lisps
       ;;solidity                               ; Do you need a blockchain? No.
       ;;swift                                  ; Who asked for emoji variables?
       ;;terra                                  ; Earth and Moon in alignment for performance.
       yaml                                     ; JSON, but readable.
       ;;(zig +lsp)                             ; C, but simpler.
       )

(doom! :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)
       )

(doom! :app
       ;;calendar
       ;;emms                                   ; Emacs Multimedia System.
       ;;everywhere                             ; *Leave* Emacs!? You must be joking.
       ;;irc                                    ; How neckbeards socialize
       ;;(rss +org)                             ; Emacs as an RSS reader
       ;;twitter                                ; Twitter client https://twitter.com/vnought
       )

(doom! :config
       ;;literate
       (default +bindings +smartparens)
       )
