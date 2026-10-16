(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix.static. "helix/static.scm"))
(require-builtin helix/core/text as text.)

(provide current-doc
         doc-length
         doc-string
         paragraph-bounds
         read-range
         replace-range!
         set-cursor!
         select-range!
         primary-range
         single-cursor?
         line-of)

;;@doc
;; The id of the focused document.
(define (current-doc)
  (editor->doc-id (editor-focus)))

(define (rope)
  (editor->text (current-doc)))

;;@doc
;; The length of the focused document in chars.
(define (doc-length)
  (text.rope-len-chars (rope)))

;;@doc
;; The whole focused document as a string.
(define (doc-string)
  (text.rope->string (rope)))

(define (blank-line? r line)
  (equal? (trim (text.rope->string (text.rope->line r line))) ""))

;;@doc
;; The char range (start . end) of the paragraphs that hold [from, to].
;; Paragraphs are separated by blank lines.
;;
;; ```scheme
;; (paragraph-bounds from to) -> (cons int? int?)
;; ```
(define (paragraph-bounds from to)
  (define r (rope))
  (define last-line (- (text.rope-len-lines r) 1))
  (define top
    (let loop ([l (text.rope-char->line r from)])
      (if (and (> l 0) (not (blank-line? r (- l 1)))) (loop (- l 1)) l)))
  (define bottom
    (let loop ([l (text.rope-char->line r to)])
      (if (and (< l last-line) (not (blank-line? r (+ l 1)))) (loop (+ l 1)) l)))
  (cons (text.rope-line->char r top)
        (if (= bottom last-line) (text.rope-len-chars r) (text.rope-line->char r (+ bottom 1)))))

;;@doc
;; The text of [from, to) in the focused document.
(define (read-range from to)
  (text.rope->string (text.rope->slice (rope) from to)))

;;@doc
;; Replace [from, to) in the focused document with `s`.
(define (replace-range! from to s)
  (helix.static.set-current-selection-object! (helix.static.range->selection (helix.static.range from to)))
  (if (= from to)
      (helix.static.insert_string s)
      (helix.static.replace-selection-with s)))

(define (insert-mode?)
  (equal? (editor-mode) (string->editor-mode "insert")))

;;@doc
;; Put the primary cursor at char `pos`.
;; In insert mode the range keeps its direction. `i` makes it backward. `a`
;; makes it forward and also covers the char before the cursor, because
;; leaving append mode shrinks a forward range by one char.
(define (set-cursor! pos)
  (define len (doc-length))
  (define r (helix.static.selection->primary-range (helix.static.current-selection-object)))
  (define backward? (< (helix.static.range-head r) (helix.static.range-anchor r)))
  (cond
    [(not (insert-mode?))
     (let ([at (max 0 (min pos (- len 1)))]) (select-range! at (min (+ at 1) len)))]
    [backward? (select-range! (min (+ pos 1) len) pos)]
    [else (select-range! (max 0 (- pos 1)) (min (+ pos 1) len))]))

;;@doc
;; Select [from, to) as the only selection.
(define (select-range! from to)
  (helix.static.set-current-selection-object! (helix.static.range->selection (helix.static.range from to))))

;;@doc
;; The primary selection as (from . to).
(define (primary-range)
  (define r (helix.static.selection->primary-range (helix.static.current-selection-object)))
  (cons (helix.static.range->from r) (helix.static.range->to r)))

;;@doc
;; The zero-based line of char `pos` in the focused document.
(define (line-of pos)
  (text.rope-char->line (rope) pos))

;;@doc
;; Whether the focused view has exactly one selection range.
(define (single-cursor?)
  (= (length (helix.static.selection->ranges (helix.static.current-selection-object))) 1))
