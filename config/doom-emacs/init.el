;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       (company                                ; the ultimate code completion backend
        +childframe)                           ; ... when your children are better than you
       (vertico +icons)                        ; the search engine of the future

       :ui
       doom                                    ; what makes DOOM look the way it does
       doom-dashboard                          ; a nifty splash screen for Emacs
       doom-quit                               ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)                        ; 🙂
       hl-todo                                 ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures +extra)                      ; ligatures and symbols to make your code pretty again
       modeline                                ; snazzy, Atom-inspired modeline, plus API
       nav-flash                               ; blink the current line after jumping
       ophints                                 ; highlight the region an operation acts on
       (popup                                  ; tame sudden yet inevitable temporary windows
        +all                                   ; catch all popups that start with an asterix
        +defaults)                             ; default popup rules
       (tabs                                   ; an tab bar for Emacs
        +centaur-tabs)                         ; ... with prettier tabs
       treemacs                                ; a project drawer, like neotree but cooler
       vc-gutter                               ; vcs diff in the fringe
       vi-tilde-fringe                         ; fringe tildes to mark beyond EOB
       (window-select +numbers)                ; visually switch windows
       workspaces                              ; tab emulation, persistence & separate workspaces
       zen                                     ; distraction-free coding or writing

       :editor
       (evil +everywhere)                      ; come to the dark side, we have cookies
       file-templates                          ; auto-snippets for empty files
       fold                                    ; (nigh) universal code folding
       (format +onsave)                        ;  automated prettiness
       multiple-cursors                        ; editing in many places at once
       ;;rotate-text                           ; cycle region at point between text candidates
       snippets                                ; my elves. They type so I don't have to
       ;;word-wrap                             ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)                          ; making dired pretty [functional]
       electric                                ; smarter, keyword-based electric-indent
       (ibuffer +icon)                         ; interactive buffer management
       undo                                    ; persistent, smarter undo for your inevitable mistakes
       vc                                      ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell                                ; the elisp shell that works everywhere
       vterm                                   ; the best terminal emulation in Emacs

       :checkers
       syntax                                  ; Tasing you for every semicolon you forget.
       (:if (executable-find "aspell") spell)  ; tasing you for misspelling mispelling
       grammar                                 ; tasing grammar mistake every you make

       :tools
       ansible                                 ; a crucible for infrastructure as code
       ;;(debugger +lsp)                       ; FIXME stepping through code, to help you add bugs
       ;;editorconfig                          ; let someone else argue about tabs vs spaces
       ;;ein                                   ; tame Jupyter notebooks with emacs
       (eval +overlay)                         ; run code, run (also, repls)
       ;;gist                                  ; interacting with github gists
       (lookup                                 ; helps you navigate your code and documentation
        +dictionary                            ; dictionary/thesaurus is nice
        +docsets)                              ; ...or in Dash docsets locally
       lsp                                     ; Language Server Protocol
       (magit +forge)                          ; a git porcelain for Emacs
       make                                    ; run make tasks from Emacs
       pdf                                     ; pdf enhancements
       rgb                                     ; creating color strings
       upload                                  ; map local to remote projects via ssh/ftp

       :os
       tty                                     ; improve the terminal Emacs experience

       :lang
       ;;agda                                  ; types of types of types of types...
       ;;cc                                    ; C/C++/Obj-C madness.
       ;;common-lisp                           ; If you've seen one lisp, you've seen them all.
       ;;clojure                               ; java with a lisp
       ;;coq                                   ; Proofs-as-programs.
       ;;data                                  ; Config/data formats.
       ;;(dart +flutter)                       ; Paint ui and not much else.
       ;;dhall                                 ; JSON with FP sprinkles
       ;;elixir                                ; erlang done right
       ;;elm                                   ; care for a cup of TEA?
       emacs-lisp                              ; Drown in parentheses.
       ;;erlang                                ; An elegant language for a more civilized age.
       ess                                     ; Emacs speaks statistics.
       ;;(go +lsp)                             ; The hipster dialect.
       (haskell +lsp)                          ; a language that's lazier than I am
       ;;idris                                 ; a language you can depend on
       ;;json                                  ; At least it ain't XML.
       ;;(java +meghanada)                     ; The poster child for carpal tunnel syndrome.
       (javascript +lsp)                       ; All(hope(abandon(ye(who(enter(here)))))).
       ;;(julia +lsp)                          ; A better, faster MATLAB.
       ;;kotlin                                ; A better, slicker Java(Script).
       (latex                                  ; writing papers in Emacs has never been so fun
        +latexmk                               ; what else would you use?
        +cdlatex                               ; quick maths symbols
        +fold)                                 ; fold the clutter away nicities
       ;;ledger                                ; An accounting system in Emacs.
       ;;lean                                  ; proof that mathematicians need help
       lua                                     ; One-based indices? one-based indices.
       markdown                                ; Writing docs for people to ignore.
       nix                                     ; I hereby declare "nix geht mehr!"
       ;;ocaml                                 ; an objective camel
       (org                                    ; organize your plain life in plain text
        +dragndrop                             ; drag & drop files/images into org buffers
        +gnuplot                               ; who doesn't like pretty pictures
        ;;+hugo                                ; use Emacs for hugo blogging
        +jupyter                               ; ipython/jupyter support for babel
        +noter                                 ; enhanced PDF notetaking
        +pandoc                                ; export-with-pandoc support
        ;;+pomodoro                            ; be fruitful with the tomato technique
        +present                               ; using org-mode for presentations
        +pretty                                ; yessss my pretties! (nice unicode symbols)
        +roam2)                                ; wander around notes
       ;;perl                                  ; write code no one else can comprehend
       (python +lsp +pyright)                  ; beautiful is better than ugly
       ;;qt                                    ; The 'cutest' gui framework ever
       ;;racket                                ; a DSL for DSLs
       ;;(ruby +rails)                         ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)                             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala                                 ; Java, but good
       ;;scheme                                ; A fully conniving family of lisps
       sh                                      ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity                              ; Do you need a blockchain? No.
       ;;swift                                 ; Who asked for emoji variables?
       ;;terra                                 ; Earth and Moon in alignment for performance.
       ;;web                                   ; the tubes
       yaml                                    ; JSON, but readable.
       ;;(zig +lsp)                            ; C, but simpler.

       :email
       (:if (executable-find "mu") (mu4e +org +gmail))
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms                                  ; Emacs Multimedia System.
       ;;everywhere                            ; *Leave* Emacs!? You must be joking.
       ;;irc                                   ; How neckbeards socialize
       ;;(rss +org)                            ; Emacs as an RSS reader
       ;;twitter                               ; Twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens)
       )
