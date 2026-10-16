(provide Span
         Span?
         Span-kind
         Span-start
         Span-end
         Span-mid
         Span-stop
         parse
         span-at
         next-span
         prev-span
         track-insert
         track-delete
         track-change
         resolve
         resolve-all
         cleanup
         diff)

;;@doc
;; A CriticMarkup construct found by `parse`.
;;
;; * kind : one of 'ins 'del 'sub 'com 'hl
;; * start : index of the opening `{`
;; * end : index just past the closing `}`
;; * mid : index of `~>` for 'sub, otherwise #f
;; * stop : index just past an attached `{>>…<<}` comment, or `end` if there is none
(struct Span (kind start end mid stop) #:transparent)

(define markers
  (list (list #\+ 'ins "++}") (list #\- 'del "--}") (list #\~ 'sub "~~}")
        (list #\> 'com "<<}") (list #\= 'hl "==}")))

(define (opener kind)
  (case kind
    [(ins) "{++"]
    [(del) "{--"]))

(define (closer kind)
  (case kind
    [(ins) "++}"]
    [(del) "--}"]))

;; Steel's JIT panics on comparisons with more than two arguments.
(define (within? lo x hi)
  (and (<= lo x) (<= x hi)))

(define (match-at? v pat i)
  (define m (string-length pat))
  (and (<= (+ i m) (vector-length v))
       (let loop ([j 0])
         (or (= j m) (and (char=? (vector-ref v (+ i j)) (string-ref pat j)) (loop (+ j 1)))))))

(define (find-in v pat i limit)
  (let loop ([i i])
    (cond
      [(> (+ i (string-length pat)) limit) #f]
      [(match-at? v pat i) i]
      [else (loop (+ i 1))])))

(define (blank-run v i)
  (let loop ([j i])
    (cond
      [(>= j (vector-length v)) #f]
      [(char=? (vector-ref v j) #\newline) j]
      [(memv (vector-ref v j) '(#\space #\tab)) (loop (+ j 1))]
      [else #f])))

;; A paragraph break is a newline, optional blanks, then another newline.
;; Returns (start . end) of the break covering every consecutive blank line.
(define (next-break v i limit)
  (let loop ([i i])
    (cond
      [(>= i limit) #f]
      [(and (char=? (vector-ref v i) #\newline) (blank-run v (+ i 1)))
       (let extend ([end (+ (blank-run v (+ i 1)) 1)])
         (define more (blank-run v end))
         (if (and more (< more limit)) (extend (+ more 1)) (cons i (min end limit))))]
      [else (loop (+ i 1))])))

(define (span-at-index v i n)
  (define entry
    (and (char=? (vector-ref v i) #\{)
         (< (+ i 2) n)
         (char=? (vector-ref v (+ i 1)) (vector-ref v (+ i 2)))
         (assv (vector-ref v (+ i 1)) markers)))
  (define brk (and entry (next-break v i n)))
  (define limit (if brk (car brk) n))
  (define close (and entry (find-in v (list-ref entry 2) (+ i 3) limit)))
  (define kind (and close (list-ref entry 1)))
  (define mid (and (eq? kind 'sub) (find-in v "~>" (+ i 3) close)))
  (define end (and close (+ close 3)))
  (and kind
       (or (not (eq? kind 'sub)) mid)
       (Span kind i end mid (or (and (not (eq? kind 'com)) (note-end v end limit)) end))))

(define (note-end v i limit)
  (and (match-at? v "{>>" i)
       (let ([close (find-in v "<<}" (+ i 3) limit)]) (and close (+ close 3)))))

;;@doc
;; Parse every CriticMarkup construct in `text`, left to right.
;; Constructs do not nest and cannot cross a paragraph break.
;; An opener without a closer in the same paragraph is plain text.
;;
;; ```scheme
;; (parse text) -> (listof Span?)
;; ```
(define (parse text)
  (define v (string->vector text))
  (define n (vector-length v))
  (let loop ([i 0]
             [acc '()])
    (if (> (+ i 3) n)
        (reverse acc)
        (let ([s (span-at-index v i n)])
          (if s
              (loop (Span-stop s) (cons s acc))
              (loop (+ i 1) acc))))))

(define (body-from s)
  (+ (Span-start s) 3))
(define (body-to s)
  (- (Span-end s) 3))
(define (has-note? s)
  (> (Span-stop s) (Span-end s)))

;; Ranges [lo, hi] where typing or deleting is a real edit.
(define (zones s)
  (append (case (Span-kind s)
            [(ins com) (list (cons (body-from s) (body-to s)))]
            [(sub) (list (cons (+ (Span-mid s) 2) (body-to s)))]
            [else '()])
          (if (has-note? s)
              (list (cons (+ (Span-end s) 3) (- (Span-stop s) 3)))
              '())))

;; Returns (span lo . hi) for the editable zone holding p, or #f.
(define (zone-at spans p)
  (let loop ([spans spans])
    (if (null? spans)
        #f
        (let ([z (findf (lambda (z) (within? (car z) p (cdr z))) (zones (car spans)))])
          (if z
              (cons (car spans) z)
              (loop (cdr spans)))))))

(define (enclosing spans i)
  (findf (lambda (s) (and (<= (Span-start s) i) (< i (Span-stop s)))) spans))

;; How a deletion treats the char at i: 'plain becomes a deletion, 'zone is
;; deleted for real, 'orig and 'keep stay.
(define (char-class spans i)
  (define s (enclosing spans i))
  (cond
    [(not s) 'plain]
    [(findf (lambda (z) (and (<= (car z) i) (< i (cdr z)))) (zones s)) 'zone]
    [(and (>= i (body-from s)) (< i (body-to s)) (not (memq (Span-kind s) '(ins com)))) 'orig]
    [else 'keep]))

;;@doc
;; Return the span whose markup, including an attached comment, covers `pos`.
;;
;; ```scheme
;; (span-at spans pos) -> (or Span? #f)
;; ```
(define (span-at spans pos)
  (enclosing spans pos))

;;@doc
;; Return the first span starting after `pos`, wrapping to the first span.
;;
;; ```scheme
;; (next-span spans pos) -> (or Span? #f)
;; ```
(define (next-span spans pos)
  (or (findf (lambda (s) (> (Span-start s) pos)) spans) (and (pair? spans) (car spans))))

;;@doc
;; Return the last span starting before `pos`, wrapping to the last span.
;;
;; ```scheme
;; (prev-span spans pos) -> (or Span? #f)
;; ```
(define (prev-span spans pos)
  (define before (filter (lambda (s) (< (Span-start s) pos)) spans))
  (cond
    [(pair? before) (last before)]
    [(pair? spans) (last spans)]
    [else #f]))

;; Wrap `body` as `kind` markup, one construct per paragraph.
;; `k` is a cursor offset into `body`, or #f.
;; Returns (markup . cursor) where cursor is the offset of `k` in the markup.
(define (wrap kind body k note)
  (define v (string->vector body))
  (define n (vector-length v))
  (let loop ([ps 0]
             [out ""]
             [cur #f])
    (define brk (next-break v ps n))
    (define pe (if brk (car brk) n))
    (define piece (substring body ps pe))
    (define here (and k (not cur) (within? ps k pe)))
    (define emitted
      (if (or here (> (string-length piece) 0))
          (string-append out (opener kind) piece (closer kind) note)
          out))
    (define cur* (if here (+ (string-length out) 3 (- k ps)) cur))
    (if brk
        (let ([sep (substring body (car brk) (cdr brk))])
          (loop (cdr brk)
                (string-append emitted sep)
                (if (and k (not cur*) (< (car brk) k) (< k (cdr brk)))
                    (+ (string-length emitted) (- k (car brk)))
                    cur*)))
        (cons emitted (or cur* (string-length emitted))))))

(define (splice text from to s)
  (string-append (substring text 0 from) s (substring text to (string-length text))))

;;@doc
;; Record an insertion of `s` at `p` in `text` as a suggestion.
;; Text typed inside an editable zone (an insertion body, the new side of a
;; substitution, a comment) joins it. Text typed next to an insertion extends it.
;; Anywhere else it is wrapped in `{++ ++}` followed by `note`.
;; `k` is the cursor offset into `s` after the edit.
;;
;; ```scheme
;; (track-insert text p s k note) -> (cons string? int?)
;; ```
(define (track-insert text p s k note)
  (define spans (parse text))
  (define hit (zone-at spans p))
  (define inside (enclosing spans p))
  (define ins-start (findf (lambda (x) (and (eq? (Span-kind x) 'ins) (= (Span-start x) p))) spans))
  (define ins-stop (findf (lambda (x) (and (eq? (Span-kind x) 'ins) (= (Span-stop x) p))) spans))
  (cond
    [(and hit (eq? (Span-kind (car hit)) 'ins) (= (cadr hit) (body-from (car hit))))
     (define x (car hit))
     (define body (substring text (body-from x) (body-to x)))
     (define out (wrap 'ins (splice body (- p (body-from x)) (- p (body-from x)) s) (+ (- p (body-from x)) k) ""))
     (cons (splice text (Span-start x) (Span-end x) (car out)) (+ (Span-start x) (cdr out)))]
    [hit (cons (splice text p p s) (+ p k))]
    [ins-start (track-insert text (body-from ins-start) s k note)]
    [ins-stop (track-insert text (body-to ins-stop) s k note)]
    [(and inside (< (Span-start inside) p))
     (track-insert text (if (eq? (Span-kind inside) 'sub) (body-to inside) (Span-stop inside)) s k note)]
    [else
     (define out (wrap 'ins s k note))
     (cons (splice text p p (car out)) (+ p (cdr out)))]))

;; Move a one-char deletion that lands on markup or already-deleted text to
;; the char it stands for: into a neighbouring editable zone, or past the span.
(define (retarget spans i dir n depth)
  (define s (enclosing spans i))
  (define class (char-class spans i))
  (define zone
    (cond
      [(not s) #f]
      [(>= i (Span-end s)) (and (has-note? s) (last (zones s)))]
      [(memq (Span-kind s) '(ins com sub)) (car (zones s))]
      [else #f]))
  (define open? (and zone (< (car zone) (cdr zone))))
  (define target
    (cond
      [(memq class '(plain zone)) i]
      [(eq? class 'orig) (if (eq? dir 'backward) (- (Span-start s) 1) (Span-stop s))]
      [(eq? dir 'backward) (if (and open? (>= i (cdr zone))) (- (cdr zone) 1) (- (Span-start s) 1))]
      [(and open? (< i (car zone))) (car zone)]
      [else (Span-stop s)]))
  (cond
    [(or (< target 0) (>= target n)) #f]
    [(or (= target i) (= depth 0)) target]
    [else (retarget spans target dir n (- depth 1))]))

;;@doc
;; Record a deletion of [a, b) in `text` as a suggestion.
;; Plain text becomes `{-- --}` followed by `note`. Text inside an editable zone
;; is deleted for real. Existing deletions next to the range absorb it.
;; A one-char deletion on markup acts on the char the markup stands for.
;; `dir` is 'backward or 'forward. The cursor lands before the result for
;; 'backward and after it for 'forward.
;;
;; ```scheme
;; (track-delete text a b dir note) -> (cons string? int?)
;; ```
(define (track-delete text a b dir note)
  (define spans (parse text))
  (define n (string-length text))
  (define target (if (= b (+ a 1)) (retarget spans a dir n 4) a))
  (if (not target)
      (cons text (if (eq? dir 'backward) b a))
      (let* ([a* (if (= b (+ a 1)) target a)]
             [b* (if (= b (+ a 1)) (+ target 1) b)]
             [left (findf (lambda (x) (and (eq? (Span-kind x) 'del) (= (Span-stop x) a*))) spans)]
             [right (findf (lambda (x) (and (eq? (Span-kind x) 'del) (= (Span-start x) b*))) spans)]
             [from (if left (Span-start left) a*)]
             [to (if right (Span-stop right) b*)]
             [out (delete-walk text spans from to note)]
             [cursor (if (eq? dir 'backward) from (+ from (string-length out)))])
        (cons (splice text from to out) cursor))))

(define (span-note text s)
  (if (has-note? s) (substring text (Span-end s) (Span-stop s)) #f))

;; Rewrite [from, to) for a deletion. Returns the replacement string.
(define (delete-walk text spans from to note)
  (let loop ([i from]
             [out ""]
             [pending ""]
             [kept #f])
    (define (flush)
      (if (equal? pending "") out (string-append out (car (wrap 'del pending #f (or kept note))))))
    (define s (findf (lambda (x) (and (= (Span-start x) i) (<= (Span-stop x) to))) spans))
    (cond
      [(>= i to) (flush)]
      [(and s (eq? (Span-kind s) 'ins)) (loop (Span-stop s) out pending kept)]
      [(and s (eq? (Span-kind s) 'com)) (loop (Span-stop s) (string-append (flush) (substring text i (Span-stop s))) "" #f)]
      [s
       (define old (substring text (body-from s) (if (Span-mid s) (Span-mid s) (body-to s))))
       (loop (Span-stop s) out (string-append pending old) (or kept (span-note text s)))]
      [else
       (case (char-class spans i)
         [(plain) (loop (+ i 1) out (string-append pending (substring text i (+ i 1))) kept)]
         [(zone) (loop (+ i 1) out pending kept)]
         [else (loop (+ i 1) (string-append (flush) (substring text i (+ i 1))) "" #f)])])))

;;@doc
;; Record replacing [a, b) in `text` with `s` as a suggestion.
;; Plain text in one paragraph becomes `{~~old~>s~~}` followed by `note`, with
;; the cursor after `s`. A range inside one editable zone is replaced for real.
;; Anything else becomes a deletion followed by an insertion.
;;
;; ```scheme
;; (track-change text a b s note) -> (cons string? int?)
;; ```
(define (track-change text a b s note)
  (define spans (parse text))
  (define old (substring text a b))
  (define hit (zone-at spans a))
  (cond
    [(and (let loop ([i a]) (or (= i b) (and (eq? (char-class spans i) 'plain) (loop (+ i 1)))))
          (not (next-break (string->vector old) 0 (string-length old))))
     (cons (splice text a b (string-append "{~~" old "~>" s "~~}" note))
           (+ a 5 (string-length old) (string-length s)))]
    [(and hit (<= b (cddr hit)))
     (cons (splice text a b s) (+ a (string-length s)))]
    [else
     (define deleted (track-delete text a b 'forward note))
     (if (equal? s "")
         deleted
         (track-insert (car deleted) (cdr deleted) s (string-length s) note))]))

;;@doc
;; The text that replaces span `s` of `text` when it is accepted or rejected.
;; An attached comment is always dropped.
;;
;; ```scheme
;; (resolve text s accept?) -> string?
;; ```
(define (resolve text s accept?)
  (define body (substring text (body-from s) (body-to s)))
  (case (Span-kind s)
    [(ins) (if accept? body "")]
    [(del) (if accept? "" body)]
    [(sub)
     (if accept?
         (substring text (+ (Span-mid s) 2) (body-to s))
         (substring text (body-from s) (Span-mid s)))]
    [(com) ""]
    [(hl) body]))

;;@doc
;; Accept or reject every span in `text`.
;;
;; ```scheme
;; (resolve-all text accept?) -> string?
;; ```
(define (resolve-all text accept?)
  (foldl (lambda (s acc) (splice acc (Span-start s) (Span-stop s) (resolve text s accept?)))
         text
         (reverse (parse text))))

;; The replacement for a span left empty by editing, or #f to keep it.
(define (tidy text s)
  (define note-empty (and (has-note? s) (= (+ (Span-end s) 6) (Span-stop s))))
  (define note (or (and (not note-empty) (span-note text s)) ""))
  (define markup (substring text (Span-start s) (Span-end s)))
  (define body (substring text (body-from s) (body-to s)))
  (case (Span-kind s)
    [(ins del com) (if (equal? body "") "" (and note-empty markup))]
    [(sub)
     (define old (substring text (body-from s) (Span-mid s)))
     (define new (substring text (+ (Span-mid s) 2) (body-to s)))
     (cond
       [(equal? new "") (string-append "{--" old "--}" note)]
       [(equal? old "") (string-append "{++" new "++}" note)]
       [else (and note-empty markup)])]
    [(hl) (and note-empty body)]))

;;@doc
;; Remove markup left empty by editing and map `cursor` through the change.
;; Empty insertions, deletions and comments vanish. A substitution with an
;; empty side becomes a deletion or insertion. An empty attached comment is
;; dropped, and a highlight that loses its comment is unwrapped.
;;
;; ```scheme
;; (cleanup text cursor) -> (cons string? int?)
;; ```
(define (cleanup text cursor)
  (foldl (lambda (s acc)
           (define r (tidy text s))
           (define c (cdr acc))
           (if r
               (cons (splice (car acc) (Span-start s) (Span-stop s) r)
                     (cond
                       [(>= c (Span-stop s)) (+ c (- (string-length r) (- (Span-stop s) (Span-start s))))]
                       [(> c (Span-start s)) (+ (Span-start s) (min (- c (Span-start s)) (string-length r)))]
                       [else c]))
               acc))
         (cons text cursor)
         (reverse (parse text))))

;;@doc
;; The single edit that turns `old` into `new`: old[a, b) became `s`.
;; Returns (list a b s), or #f when the strings are equal.
;; A pure insertion that could sit at several offsets, such as a newline
;; next to a newline, slides left until `cursor` (an offset in `new`) is at
;; its end.
;;
;; ```scheme
;; (diff old new [cursor]) -> (or (list int? int? string?) #f)
;; ```
(define (diff old new [cursor #f])
  (define change (minimal-diff old new))
  (if (and change cursor (= (car change) (cadr change)))
      (slide new change cursor)
      change))

(define (slide new change cursor)
  (define a (car change))
  (define n (string-length (list-ref change 2)))
  (if (and (> a 0) (< cursor (+ a n)) (char=? (string-ref new (- a 1)) (string-ref new (+ a n -1))))
      (slide new (list (- a 1) (- a 1) (substring new (- a 1) (+ a n -1))) cursor)
      change))

(define (minimal-diff old new)
  (define ov (string->vector old))
  (define nv (string->vector new))
  (define on (vector-length ov))
  (define nn (vector-length nv))
  (define p
    (let loop ([i 0])
      (if (and (< i on) (< i nn) (char=? (vector-ref ov i) (vector-ref nv i))) (loop (+ i 1)) i)))
  (define q
    (let loop ([j 0])
      (if (and (< j (- on p)) (< j (- nn p)) (char=? (vector-ref ov (- on j 1)) (vector-ref nv (- nn j 1))))
          (loop (+ j 1))
          j)))
  (and (not (and (= p on) (= p nn))) (list p (- on q) (substring new p (- nn q)))))
