;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*PDF-tools][PDF-tools:1]]
(package! pdf-tools
  :built-in 'prefer)
;; PDF-tools:1 ends here

;; [[file:config.org::*Screenshot][Screenshot:1]]
(package! screenshot
  :recipe (:host github :repo "tecosaur/screenshot"
           :files ("*.el"))
  :pin "7621e0cd176f65e22bc7e7d03a8003e59426f7f7")
;; Screenshot:1 ends here

;; [[file:config.org::*Prettier Page Breaks][Prettier Page Breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
;; Prettier Page Breaks:1 ends here

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "ccc0aaa72732ea633bf52bcc8a0345cd3ac178fd")

(package! org-pandoc-import
  :recipe (:host github :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;; [[file:config.org::*Roam][Roam:2]]
(package! org-roam :disable t)
;; Roam:2 ends here

;; [[file:config.org::*Graph view][Graph view:1]]
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"
           :files ("*.el" "out"))
  :pin "309fe3c58c7081de4e2c9c64f7b40ea291926048")
(package! websocket
  :pin "fda4455333309545c0787a79d73c19ddbeb57980")
;; Graph view:1 ends here

;; [[file:config.org::*Render LaTeX on the fly!][Render LaTeX on the fly!:1]]
(package! org-fragtog
  :pin "680606189d5d28039e6f9301b55ec80517a24005")
;; Render LaTeX on the fly!:1 ends here
