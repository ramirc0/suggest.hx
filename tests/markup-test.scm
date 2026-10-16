(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))
(require "../src/markup.scm")

(provide __module__)
(define __module__ "markup-test")

(define (kinds text)
  (map Span-kind (parse text)))

(test-module
 "parse"
 (check-equal? "insertion" (parse "a{++b++}c") (list (Span 'ins 1 8 #f 8)))
 (check-equal? "substitution mid" (parse "{~~a~>b~~}") (list (Span 'sub 0 10 4 10)))
 (check-equal? "attached comment" (parse "{++x++}{>>n<<}") (list (Span 'ins 0 7 #f 14)))
 (check-equal? "comment takes no comment" (kinds "{>>a<<}{>>b<<}") '(com com))
 (check-equal? "all kinds" (kinds "{++a++} {--b--} {~~c~>d~~} {>>e<<} {==f==}") '(ins del sub com hl))
 (check-equal? "no closer before paragraph break" (parse "{++a\n\nb++}") '())
 (check-equal? "substitution needs ~>" (parse "{~~ab~~}") '()))

(test-module
 "track-insert"
 (check-equal? "wraps plain text" (track-insert "ab" 1 "x" 1 "") (cons "a{++x++}b" 5))
 (check-equal? "joins insertion body" (track-insert "a{++x++}b" 5 "y" 1 "") (cons "a{++xy++}b" 6))
 (check-equal? "extends from after" (track-insert "a{++x++}b" 8 "y" 1 "") (cons "a{++xy++}b" 6))
 (check-equal? "extends from before" (track-insert "a{++x++}b" 1 "y" 1 "") (cons "a{++yx++}b" 5))
 (check-equal? "autopair keeps cursor between" (track-insert "ab" 1 "()" 1 "") (cons "a{++()++}b" 5))
 (check-equal? "adds note" (track-insert "ab" 1 "x" 1 "{>>@me<<}") (cons "a{++x++}{>>@me<<}b" 5))
 (check-equal? "extends past note"
               (track-insert "a{++x++}{>>@me<<}b" 17 "y" 1 "{>>@me<<}")
               (cons "a{++xy++}{>>@me<<}b" 6))
 (check-equal? "typing in deletion lands after it" (track-insert "{--ab--}c" 4 "x" 1 "") (cons "{--ab--}{++x++}c" 12))
 (check-equal? "typing in old side goes to new side" (track-insert "{~~ab~>cd~~}" 4 "x" 1 "") (cons "{~~ab~>cdx~~}" 10))
 (check-equal? "typing in comment is plain" (track-insert "{>>ab<<}" 4 "x" 1 "") (cons "{>>axb<<}" 5))
 (check-equal? "blank line splits insertion" (track-insert "{++a++}" 4 "\n\n" 2 "") (cons "{++a++}\n\n{++++}" 12))
 (check-equal? "paste splits paragraphs" (track-insert "" 0 "a\n\nb" 4 "") (cons "{++a++}\n\n{++b++}" 13)))

(test-module
 "track-delete"
 (check-equal? "wraps plain char" (track-delete "abc" 1 2 'backward "") (cons "a{--b--}c" 1))
 (check-equal? "backspace merges left" (track-delete "a{--b--}c" 0 1 'backward "") (cons "{--ab--}c" 0))
 (check-equal? "delete merges right" (track-delete "{--a--}bc" 7 8 'forward "") (cons "{--ab--}c" 8))
 (check-equal? "deletes in insertion for real" (track-delete "{++ab++}" 4 5 'backward "") (cons "{++a++}" 4))
 (check-equal? "backspace on insertion closer" (track-delete "{++ab++}" 7 8 'backward "") (cons "{++a++}" 4))
 (check-equal? "backspace on deletion closer" (track-delete "x{--a--}" 7 8 'backward "") (cons "{--xa--}" 0))
 (check-equal? "backspace inside deletion" (track-delete "x{--ab--}" 5 6 'backward "") (cons "{--xab--}" 0))
 (check-equal? "delete on insertion opener" (track-delete "{++ab++}" 0 1 'forward "") (cons "{++b++}" 3))
 (check-equal? "nothing before start" (track-delete "{--a--}" 0 1 'backward "") (cons "{--a--}" 1))
 (check-equal? "range drops own insertion" (track-delete "a{++b++}c" 0 9 'forward "") (cons "{--ac--}" 8))
 (check-equal? "range keeps partial markup" (track-delete "ab{++cd++}" 1 6 'forward "") (cons "a{--b--}{++d++}" 11))
 (check-equal? "adds note" (track-delete "ab" 0 1 'backward "{>>@me<<}") (cons "{--a--}{>>@me<<}b" 0))
 (check-equal? "merge keeps one note"
               (track-delete "x{--a--}{>>@me<<}" 0 1 'backward "{>>@me<<}")
               (cons "{--xa--}{>>@me<<}" 0))
 (check-equal? "splits at paragraph break" (track-delete "a\n\nb" 0 4 'forward "") (cons "{--a--}\n\n{--b--}" 16)))

(test-module
 "track-change"
 (check-equal? "empty substitution" (track-change "abc" 1 2 "" "") (cons "a{~~b~>~~}c" 7))
 (check-equal? "substitution" (track-change "abc" 1 2 "XY" "") (cons "a{~~b~>XY~~}c" 9))
 (check-equal? "inside insertion is real" (track-change "{++abc++}" 4 5 "X" "") (cons "{++aXc++}" 5))
 (check-equal? "markup falls back to deletion" (track-change "a{++b++}c" 0 9 "" "") (cons "{--ac--}" 8))
 (check-equal? "fallback then insertion" (track-change "a{++b++}c" 0 9 "x" "") (cons "{--ac--}{++x++}" 12)))

(define all "a{++b++}{--c--}{~~d~>e~~}{>>n<<}{==f==}{>>g<<}")
(define spans (parse "a{++b++}c{--d--}"))

(test-module
 "resolve"
 (check-equal? "accept all" (resolve-all all #t) "abef")
 (check-equal? "reject all" (resolve-all all #f) "acdf")
 (check-equal? "drops attached comment" (resolve-all "{++x++}{>>n<<}y" #t) "xy"))

(test-module
 "cleanup"
 (check-equal? "empty insertion" (cleanup "a{++++}b" 4) (cons "ab" 1))
 (check-equal? "empty new side" (cleanup "{~~ab~>~~}" 10) (cons "{--ab--}" 8))
 (check-equal? "highlight without comment" (cleanup "{==a==}{>><<}" 0) (cons "a" 0))
 (check-equal? "empty note" (cleanup "{++a++}{>><<}" 0) (cons "{++a++}" 0))
 (check-equal? "keeps content" (cleanup "{++a++}" 3) (cons "{++a++}" 3)))

(test-module
 "navigation"
 (check-equal? "next" (Span-start (next-span spans 2)) 9)
 (check-equal? "next wraps" (Span-start (next-span spans 10)) 1)
 (check-equal? "prev" (Span-start (prev-span spans 9)) 1)
 (check-equal? "prev wraps" (Span-start (prev-span spans 1)) 9)
 (check-equal? "at" (Span-kind (span-at spans 3)) 'ins)
 (check-equal? "outside" (span-at spans 0) #f))
