(ns org.eightnotrump.clearley.test.simplepocoparsertest
  "A simple test for a simple CFG parser that emits ASTs of POCOs
  (Plain Old Clojure Objects)." 
  (:use org.eightnotrump.clearley))

(def ruleset1 [(rule :head :sum)
               (rule :sum :sum \+ :times)
               (rule :sum :times)
               (rule :times :times \* :num)
               (rule :times :num)
               (rule :num \1)
               (rule :num \2)
               (rule :num \3)
               (rule :num \4)])

(def rulemap1 (to-rulemap ruleset1))

(def myparser (earley-parser rulemap1 :head))

(parse myparser "2+3*4")
