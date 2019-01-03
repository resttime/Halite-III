(asdf:defsystem hbot
  :description "Halite 3 Bot"
  :homepage "https://halite.io"
  :bug-tracker "https://github.com/HaliteChallenge/Halite-III/issues"
  :source-control (:git "https://github.com/HaliteChallenge/Halite-III.git")
  :build-operation "asdf:program-op"
  :build-pathname "hbot"
  :entry-point "hbot:main"
  :serial t
  :components ((:file "hbot"))
  :depends-on (:halite))
