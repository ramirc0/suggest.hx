(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/keymaps.scm")
(require "helix/configuration.scm")
(require (prefix-in helix.static. "helix/static.scm"))
(require-builtin helix/core/keymaps as helix.keymaps.)
(require "src/markup.scm")
(require "src/edit.scm")
(require "src/picker.scm")

(provide suggest-mode
         suggest-author
         suggest-key
         suggest-next
         suggest-prev
         suggest-accept
         suggest-reject
         suggest-accept-all
         suggest-reject-all
         suggest-comment
         suggest-highlight
         suggest-list
         suggest-paste-clipboard-after
         suggest-paste-clipboard-before
         suggest-replace-clipboard
         suggest-newline-below
         suggest-newline-above)

(define *on* (hash))
(define *lengths* (hash))
(define *author* (with-handler (lambda (_) #f) (env-var "USER")))

(define (doc-key)
  (doc-id->usize (current-doc)))

(define (on?)
  (hash-contains? *on* (doc-key)))

(define (note)
  (if *author* (string-append "{>>@" *author* "<<}") ""))

(define (remember-length!)
  (set! *lengths* (hash-insert *lengths* (doc-key) (doc-length))))

;; Each entry: the Helix command and how its change is recorded.
;; 'block entries are only bound in suggestion-mode buffers.
(define commands
  (hash "backspace" (list helix.static.delete_char_backward 'backward)
        "delete" (list helix.static.delete_char_forward 'forward)
        "word-backward" (list helix.static.delete_word_backward 'backward)
        "word-forward" (list helix.static.delete_word_forward 'forward)
        "kill-start" (list helix.static.kill_to_line_start 'backward)
        "kill-end" (list helix.static.kill_to_line_end 'forward)
        "newline" (list helix.static.insert_newline 'insert)
        "tab" (list helix.static.smart_tab 'insert)
        "insert-tab" (list helix.static.insert_tab 'insert)
        "delete-selection" (list helix.static.delete_selection 'forward)
        "delete-selection-noyank" (list helix.static.delete_selection_noyank 'forward)
        "change" (list helix.static.change_selection 'change)
        "change-noyank" (list helix.static.change_selection_noyank 'change)
        "paste-after" (list helix.static.paste_after 'insert)
        "paste-before" (list helix.static.paste_before 'insert)
        "paste-clipboard-after" (list helix.static.paste_clipboard_after 'insert)
        "paste-clipboard-before" (list helix.static.paste_clipboard_before 'insert)
        "replace-yanked" (list helix.static.replace_with_yanked 'change)
        "replace-clipboard" (list helix.static.replace_selections_with_clipboard 'change)
        "open-below" (list helix.static.open_below 'insert)
        "open-above" (list helix.static.open_above 'insert)
        "newline-below" (list helix.static.add_newline_below 'insert)
        "newline-above" (list helix.static.add_newline_above 'insert)
        "block" (list #f 'block)))

(define (key-map pairs)
  (foldl (lambda (pair acc) (hash-insert acc (car pair) (string-append ":suggest-key " (cdr pair))))
         (hash)
         pairs))

(define normal-keys
  (key-map '(("d" . "delete-selection") ("A-d" . "delete-selection-noyank")
             ("c" . "change") ("A-c" . "change-noyank")
             ("p" . "paste-after") ("P" . "paste-before")
             ("R" . "replace-yanked")
             ("o" . "open-below") ("O" . "open-above")
             ("r" . "block") ("~" . "block") ("`" . "block") ("A-`" . "block")
             (">" . "block") ("<" . "block") ("=" . "block")
             ("J" . "block") ("A-J" . "block") ("C-a" . "block") ("C-x" . "block")
             ("A-(" . "block") ("A-)" . "block")
             ("|" . "block") ("!" . "block") ("A-!" . "block"))))

(define insert-keys
  (key-map '(("backspace" . "backspace") ("S-backspace" . "backspace") ("C-h" . "backspace")
             ("del" . "delete") ("C-d" . "delete")
             ("C-w" . "word-backward") ("A-backspace" . "word-backward")
             ("A-d" . "word-forward") ("A-del" . "word-forward")
             ("C-u" . "kill-start") ("C-k" . "kill-end")
             ("ret" . "newline") ("C-j" . "newline")
             ("tab" . "tab") ("S-tab" . "insert-tab")
             ("C-r" . "block"))))

;; Buffer keymaps only hold single keys: a prefix here would swallow the
;; global bindings that share it.
(helix.keymaps.#%add-extension-or-labeled-keymap
 "suggest.hx"
 (helix.keymaps.helix-string->keymap
  (value->jsexpr-string (hash "normal" normal-keys "select" normal-keys "insert" insert-keys))))

;;@doc
;; Toggle suggestion mode
;;
;; While it is on in the focused buffer, edits are recorded as CriticMarkup
;; suggestions instead of changing the text.
(define (suggest-mode)
  (define key (doc-key))
  (if (on?)
      (begin
        (set! *on* (hash-remove *on* key))
        (helix.keymaps.#%add-reverse-mapping key "suggest.hx/off")
        (set-status! "Suggestion mode off"))
      (begin
        (set! *on* (hash-insert *on* key #t))
        (helix.keymaps.#%add-reverse-mapping key "suggest.hx")
        (remember-length!)
        (set-status! "Suggestion mode on"))))

;;@doc
;; Set the author attached to new suggestions
;;
;; Each new suggestion gets `{>>@name<<}`. The default is `$USER`.
;; An empty name turns attribution off.
(define (suggest-author name)
  (set! *author* (if (equal? name "") #f name)))

(define (record op old change cursor)
  (define a (list-ref change 0))
  (define b (list-ref change 1))
  (define s (list-ref change 2))
  (define rest (string-append (substring old 0 a) (substring old b (string-length old))))
  (cond
    [(and (eq? op 'insert) (equal? s "")) (cons rest (min cursor (string-length rest)))]
    [(eq? op 'insert) (track-insert rest a s (max 0 (min (- cursor a) (string-length s))) (note))]
    [(eq? op 'change) (track-change old a b s (note))]
    [(equal? s "") (track-delete old a b op (note))]
    [else (track-change old a b s (note))]))

;; Run `cmd`, then rewrite the change it made to the paragraphs around the
;; selection as a suggestion.
(define (tracked cmd op)
  (let* ([sel (primary-range)]
         [len (doc-length)]
         [bounds (paragraph-bounds (max 0 (- (car sel) 1)) (min len (+ (cdr sel) 1)))]
         [ws (car bounds)]
         [we (cdr bounds)]
         [old (read-range ws we)])
    (cmd)
    (let* ([end (+ we (- (doc-length) len))]
           [cursor (- (cursor-position) ws)]
           [new (if (>= end ws) (read-range ws end) #f)]
           [change (and new (diff old new cursor))])
      (cond
        [(not new) (set-warning! "suggest.hx: edit reached past the paragraph and was not tracked")]
        [change
         (let ([result (record op old change cursor)])
           (unless (equal? (car result) new)
             (replace-range! ws end (car result)))
           (set-cursor! (+ ws (cdr result))))]))
    (remember-length!)))

;;@doc
;; Run a tracked editing command
;;
;; Runs the command `name` from the suggestion key table. In a buffer in
;; suggestion mode its change becomes a suggestion. Elsewhere it runs as usual.
(define (suggest-key name)
  (define entry (hash-ref commands name))
  (cond
    [(not (on?)) ((car entry))]
    [(eq? (cadr entry) 'block) (set-warning! "suggest.hx: this edit can't be tracked in suggestion mode")]
    [(not (single-cursor?)) (set-warning! "suggest.hx: suggestion mode tracks a single cursor")]
    [else (tracked (car entry) (cadr entry))]))

;; Typed chars land before the hook runs. The length change tells a plain
;; char (1) from an autopair (2) or a skipped closer (0).
(define (on-insert-char _)
  (when (on?)
    (let* ([len (doc-length)]
           [key (doc-key)]
           [grew (- len (if (hash-contains? *lengths* key) (hash-ref *lengths* key) (- len 1)))])
      (cond
        [(not (single-cursor?)) (set-warning! "suggest.hx: suggestion mode tracks a single cursor")]
        [(or (= grew 1) (= grew 2))
         (let* ([from (- (cursor-position) 1)]
                [bounds (paragraph-bounds from (+ from grew))]
                [ws (car bounds)]
                [new (read-range ws (cdr bounds))]
                [a (- from ws)]
                [old (string-append (substring new 0 a) (substring new (+ a grew) (string-length new)))]
                [result (track-insert old a (substring new a (+ a grew)) 1 (note))])
           (unless (equal? (car result) new)
             (replace-range! ws (cdr bounds) (car result))
             (set-cursor! (+ ws (cdr result)))))])
      (remember-length!))))

(define (tidy!)
  (let* ([p (cursor-position)]
         [bounds (paragraph-bounds p p)]
         [ws (car bounds)]
         [text (read-range ws (cdr bounds))]
         [result (cleanup text (- p ws))])
    (unless (equal? (car result) text)
      (replace-range! ws (cdr bounds) (car result))
      (set-cursor! (+ ws (cdr result))))))

(define (on-mode-switch event)
  (when (on?)
    (when (equal? (mode-switch-old event) (string->editor-mode "insert"))
      (tidy!))
    (remember-length!)))

(register-hook 'post-insert-char on-insert-char)
(register-hook 'on-mode-switch on-mode-switch)

(push-status-element! 'left
                      (status-element (lambda (view-id _)
                                        (if (hash-contains? *on* (doc-id->usize (editor->doc-id view-id)))
                                            (list (span " SUGGEST " (theme-scope-ref "warning")))
                                            '()))))

(define (doc-spans)
  (parse (doc-string)))

(define (jump! s)
  (select-range! (Span-start s) (Span-stop s))
  (helix.static.align_view_center))

;;@doc
;; Select next suggestion
;;
;; Wraps to the first suggestion after the last.
(define (suggest-next)
  (define s (next-span (doc-spans) (cursor-position)))
  (if s (jump! s) (set-status! "No suggestions")))

;;@doc
;; Select previous suggestion
;;
;; Wraps to the last suggestion before the first.
(define (suggest-prev)
  (define s (prev-span (doc-spans) (car (primary-range))))
  (if s (jump! s) (set-status! "No suggestions")))

(define (resolve-at! accept?)
  (let* ([text (doc-string)]
         [spans (parse text)]
         [s (or (span-at spans (cursor-position)) (span-at spans (car (primary-range))))])
    (if s
        (begin
          (replace-range! (Span-start s) (Span-stop s) (resolve text s accept?))
          (set-cursor! (Span-start s)))
        (set-status! "No suggestion at cursor"))))

;;@doc
;; Accept suggestion under cursor
(define (suggest-accept)
  (resolve-at! #t))

;;@doc
;; Reject suggestion under cursor
(define (suggest-reject)
  (resolve-at! #f))

(define (resolve-all! accept?)
  (let* ([text (doc-string)]
         [new (resolve-all text accept?)]
         [p (cursor-position)])
    (if (equal? new text)
        (set-status! "No suggestions")
        (begin
          (replace-range! 0 (string-length text) new)
          (set-cursor! (min p (max 0 (- (string-length new) 1))))))))

;;@doc
;; Accept all suggestions in buffer
(define (suggest-accept-all)
  (resolve-all! #t))

;;@doc
;; Reject all suggestions in buffer
(define (suggest-reject-all)
  (resolve-all! #f))

;;@doc
;; Comment after selection
;;
;; Inserts `{>><<}` after the selection and starts insert mode inside it.
(define (suggest-comment)
  (define to (cdr (primary-range)))
  (replace-range! to to "{>><<}")
  (helix.static.insert_mode)
  (set-cursor! (+ to 3)))

;;@doc
;; Highlight selection with comment
;;
;; Wraps the selection as `{==sel==}{>><<}` and starts insert mode in the comment.
(define (suggest-highlight)
  (let* ([sel (primary-range)]
         [from (car sel)]
         [to (cdr sel)]
         [text (read-range from to)]
         [spans (doc-spans)])
    (if (or (span-at spans from) (span-at spans (- to 1)) (pair? (parse text)) (string-contains? text "\n\n"))
        (set-warning! "suggest.hx: a highlight can't overlap a suggestion or span paragraphs")
        (begin
          (replace-range! from to (string-append "{==" text "==}{>><<}"))
          (helix.static.insert_mode)
          (set-cursor! (+ from (string-length text) 9))))))

(define kind-names (hash 'ins "insert" 'del "delete" 'sub "replace" 'com "comment" 'hl "highlight"))

(define (preview text s)
  (define body
    (if (Span-mid s)
        (string-append (substring text (+ (Span-start s) 3) (Span-mid s))
                       " → "
                       (substring text (+ (Span-mid s) 2) (- (Span-end s) 3)))
        (substring text (+ (Span-start s) 3) (- (Span-end s) 3))))
  (string-replace body "\n" "⏎"))

(define (label text s)
  (string-append (number->string (+ (line-of (Span-start s)) 1))
                 "  "
                 (hash-ref kind-names (Span-kind s))
                 "  "
                 (preview text s)))

;;@doc
;; Pick a suggestion to jump to
;;
;; Lists every suggestion in the buffer in a fuzzy picker.
(define (suggest-list)
  (let* ([text (doc-string)]
         [spans (parse text)])
    (if (null? spans)
        (set-status! "No suggestions")
        (open-picker (map (lambda (s) (cons (label text s) s)) spans) jump!))))

;;@doc
;; Paste clipboard after selections
(define (suggest-paste-clipboard-after)
  (suggest-key "paste-clipboard-after"))

;;@doc
;; Paste clipboard before selections
(define (suggest-paste-clipboard-before)
  (suggest-key "paste-clipboard-before"))

;;@doc
;; Replace selections by clipboard content
(define (suggest-replace-clipboard)
  (suggest-key "replace-clipboard"))

;;@doc
;; Add newline below
(define (suggest-newline-below)
  (suggest-key "newline-below"))

;;@doc
;; Add newline above
(define (suggest-newline-above)
  (suggest-key "newline-above"))

;; Each entry: key sequence, command name, command.
(define review-bindings
  (list (list '("space" "m" "t") "suggest-mode" suggest-mode)
        (list '("space" "m" "n") "suggest-next" suggest-next)
        (list '("space" "m" "p") "suggest-prev" suggest-prev)
        (list '("space" "m" "a") "suggest-accept" suggest-accept)
        (list '("space" "m" "r") "suggest-reject" suggest-reject)
        (list '("space" "m" "A") "suggest-accept-all" suggest-accept-all)
        (list '("space" "m" "R") "suggest-reject-all" suggest-reject-all)
        (list '("space" "m" "c") "suggest-comment" suggest-comment)
        (list '("space" "m" "h") "suggest-highlight" suggest-highlight)
        (list '("space" "m" "s") "suggest-list" suggest-list)
        (list '("space" "p") "suggest-paste-clipboard-after" suggest-paste-clipboard-after)
        (list '("space" "P") "suggest-paste-clipboard-before" suggest-paste-clipboard-before)
        (list '("space" "R") "suggest-replace-clipboard" suggest-replace-clipboard)
        (list '("]" "s") "suggest-next" suggest-next)
        (list '("[" "s") "suggest-prev" suggest-prev)
        (list '("]" "space") "suggest-newline-below" suggest-newline-below)
        (list '("[" "space") "suggest-newline-above" suggest-newline-above)))

(define (insert-path h path value)
  (if (null? (cdr path))
      (hash-insert h (car path) value)
      (hash-insert h
                   (car path)
                   (insert-path (if (hash-contains? h (car path)) (hash-ref h (car path)) (hash))
                                (cdr path)
                                value))))

(define review-keys
  (foldl (lambda (b acc) (insert-path acc (car b) (string-append ":" (cadr b)))) (hash) review-bindings))

;; keymaps.scm looks docs up with a global `eval`, which cannot see this
;; module's commands while it loads, so attach the first doc line here.
(define review-docs
  (foldl (lambda (b acc) (hash-insert acc
                                    (cadr b)
                                    (trim (car (split-many (#%function-ptr-table-get #%function-ptr-table (caddr b)) "\n")))))
         (hash)
         review-bindings))

(define global-bindings (get-keybindings))
(merge-keybindings global-bindings (hash "normal" review-keys "select" review-keys))
(helix.keymaps.keymap-update-documentation! global-bindings review-docs)
(keybindings global-bindings)
