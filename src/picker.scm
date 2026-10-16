(require "helix/components.scm")
(require "helix/misc.scm")

(provide open-picker)

(struct Picker (items query view index on-select) #:mutable)

(define max-rows 12)

(define (labels items)
  (map car items))

(define (refilter! p)
  (define query (Picker-query p))
  (define matches (if (equal? query "") (labels (Picker-items p)) (fuzzy-match query (labels (Picker-items p)))))
  (set-Picker-view! p (map (lambda (label) (assoc label (Picker-items p))) matches))
  (set-Picker-index! p 0))

(define (move! p delta)
  (define n (length (Picker-view p)))
  (when (> n 0)
    (set-Picker-index! p (modulo (+ (Picker-index p) delta) n))))

(define (handle-event p event)
  (define char (key-event-char event))
  (cond
    [(key-event-escape? event) event-result/close]
    [(key-event-enter? event)
     (define view (Picker-view p))
     (when (pair? view)
       ((Picker-on-select p) (cdr (list-ref view (Picker-index p)))))
     event-result/close]
    [(key-event-up? event) (move! p -1) event-result/consume]
    [(key-event-down? event) (move! p 1) event-result/consume]
    [(key-event-tab? event)
     (move! p (if (equal? (key-event-modifier event) key-modifier-shift) -1 1))
     event-result/consume]
    [(key-event-backspace? event)
     (define query (Picker-query p))
     (unless (equal? query "")
       (set-Picker-query! p (substring query 0 (- (string-length query) 1)))
       (refilter! p))
     event-result/consume]
    [(char? char)
     (set-Picker-query! p (string-append (Picker-query p) (string char)))
     (refilter! p)
     event-result/consume]
    [else event-result/consume]))

(define (clip s width)
  (if (> (string-length s) width) (substring s 0 width) s))

(define (render p rect frame)
  (define view (Picker-view p))
  (define index (Picker-index p))
  (define width (min 90 (- (area-width rect) 4)))
  (define rows (min max-rows (max 1 (length view))))
  (define height (+ rows 3))
  (define x (+ (area-x rect) (quotient (- (area-width rect) width) 2)))
  (define y (+ (area-y rect) (quotient (- (area-height rect) height) 3)))
  (define box (area x y width height))
  (define top (max 0 (- index (- rows 1))))
  (buffer/clear frame box)
  (block/render frame
                box
                (make-block (theme-scope-ref "ui.popup") (theme-scope-ref "ui.window") "all" "rounded"))
  (frame-set-string! frame (+ x 2) (+ y 1) (clip (string-append "> " (Picker-query p)) (- width 4)) (theme-scope-ref "ui.text"))
  (let loop ([i top]
             [row 0])
    (when (and (< row rows) (< i (length view)))
      (frame-set-string! frame
                         (+ x 2)
                         (+ y 2 row)
                         (clip (car (list-ref view i)) (- width 4))
                         (theme-scope-ref (if (= i index) "ui.menu.selected" "ui.menu")))
      (loop (+ i 1) (+ row 1)))))

;;@doc
;; Open a fuzzy picker over `items`, a list of (label . value) pairs.
;; Enter calls `on-select` with the value of the highlighted item.
;;
;; ```scheme
;; (open-picker items on-select)
;; ```
(define (open-picker items on-select)
  (push-component! (new-component! "suggest-picker"
                                   (Picker items "" items 0 on-select)
                                   render
                                   (hash "handle_event" handle-event))))
