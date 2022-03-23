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

;; [[file:config.org::*Keycast][Keycast:1]]
(package! keycast
  :pin "72d9add8ba16e0cae8cfcff7fc050fa75e493b4e")
;; Keycast:1 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
;; Prettier page breaks:1 ends here

;; [[file:config.org::*Screencast][Screencast:1]]
(package! gif-screencast
  :pin "5517a557a17d8016c9e26b0acb74197550f829b9")
;; Screencast:1 ends here

(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table")
  :pin "7bd68b420d3402826fea16ee5099d04aa9879b78")

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "ccc0aaa72732ea633bf52bcc8a0345cd3ac178fd")

(package! org-graph-view
  :recipe (:host github :repo "alphapapa/org-graph-view")
  :pin "233c6708c1f37fc60604de49ca192497aef39757")

(package! org-pandoc-import
  :recipe (:host github :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! citar
  :pin "79512aefdf11071b66908320aa346255dd349234")
(package! citeproc
  :pin "ba49516265fa24b138346c4918d39d19b4de8a62")
(package! org-cite-csl-activate
  :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate")
  :pin "a9b3f3643b587076538a767666372b31b873c294")

(package! org-roam :disable t)

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"
           :files ("*.el" "out"))
  :pin "309fe3c58c7081de4e2c9c64f7b40ea291926048")
(package! websocket
  :pin "fda4455333309545c0787a79d73c19ddbeb57980")

;; (package! org-pretty-tags
;;   :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")

(package! org-fragtog
  :pin "680606189d5d28039e6f9301b55ec80517a24005")

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"
           :files ("*.el"))
  :pin "9e41d5f9a8f8e91d2e1424bc1cc4948cf5165ac8")

(package! ox-chameleon
  :recipe (:host github :repo "tecosaur/ox-chameleon"
           :files ("*.el"))
  :pin "8c7f6362c49a80b4bc96acc63f527a212ebd9428")

(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"
           :files ("*.el"))
  :pin "b372f9a44bea03cce09b20cd2409e3ae3fa2d651")
