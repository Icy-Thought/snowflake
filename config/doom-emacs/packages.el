;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! pdf-tools :built-in 'prefer)

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "cd1aefd56f648d32a25aae672ac1ab90893c0133")

(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'
