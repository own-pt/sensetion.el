(require 'buttercup)
(require 'sensetion)


(describe "sensetion.el indexes files and persists this index and
  a state"
  
  (xit "Indexes a directory of files asyncly"
    (expect (sensetion-make-state "example" (lambda (x) (print "indexed alright yo"))))))


(describe "sensetion.el annotates sentences which have a token
with the given lemma and PoS tag"

  (it "Fails when there's no token with that lemma"
    (expect (sensetion-annotate "xjfhdjhfdjfhd" "n") :to-throw))

  (it "Creates a buffer with the results when there are results."
    (expect (sensetion-annotate "William_Shakespeare" "n" :not :to-throw))))
