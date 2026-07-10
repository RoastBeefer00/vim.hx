(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "key-emulation.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/treesitter.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

;; l
;;@doc
;; Extend the selection right one character, stopping at end of line (l).
(define (extend-char-right-same-line)
  (define count (editor-count))
  (do-n-times count extend-char-right-same-line-impl))

(define (extend-char-right-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_right))))

;; h
;;@doc
;; Extend the selection left one character, stopping at start of line (h).
(define (extend-char-left-same-line)
  (define count (editor-count))
  (do-n-times count extend-char-left-same-line-impl))

(define (extend-char-left-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.extend_char_left))))

;; k
;;@doc
;; Extend the selection up one line (k).
(define (extend-line-up)
  (helix.static.extend_line_up)
  (extend-line-up-impl)
  (when (is-visual-line-mode?)
    (helix.static.extend_to_line_bounds)))

;; k in VIS-LINE steel mode — dispatched from vis-line-keymap, no flag check needed
;;@doc
;; Extend a linewise (VIS-LINE) selection up one line (k).
(define (extend-line-up-vis-line)
  (helix.static.extend_line_up)
  (extend-line-up-impl)
  (helix.static.extend_to_line_bounds))

(define (extend-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (extend-char-left-same-line-impl))))

;; j
;;@doc
;; Extend the selection down one line (j).
(define (extend-line-down)
  (helix.static.extend_line_down)
  (extend-line-down-impl)
  (when (is-visual-line-mode?)
    (helix.static.extend_to_line_bounds)))

;; j in VIS-LINE steel mode — dispatched from vis-line-keymap, no flag check needed
;;@doc
;; Extend a linewise (VIS-LINE) selection down one line (j).
(define (extend-line-down-vis-line)
  (helix.static.extend_line_down)
  (extend-line-down-impl)
  (helix.static.extend_to_line_bounds))

(define (extend-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (extend-char-left-same-line-impl))))

;; w
;;@doc
;; Extend the selection to the start of the next word (w).
(define (vim-extend-next-word-start)
  (define count (editor-count))
  (do-n-times count vim-extend-next-word-start-impl))

(define (vim-extend-next-word-start-impl)
  (get-next-word-start extend-right-n))

;; W
;;@doc
;; Extend the selection to the start of the next WORD (W).
(define (vim-extend-next-long-word-start)
  (define count (editor-count))
  (do-n-times count vim-extend-next-long-word-start-impl))

(define (vim-extend-next-long-word-start-impl)
  (get-next-long-word-start extend-right-n))

;; { (select)
;;@doc
;; Extend the selection to the start of the previous paragraph ({).
(define (vim-extend-to-prev-paragraph)
  (helix.static.extend_to_line_bounds)
  (helix.static.goto_prev_paragraph))

;; } (select)
;;@doc
;; Extend the selection to the start of the next paragraph (}).
(define (vim-extend-to-next-paragraph)
  (helix.static.extend_to_line_bounds)
  (helix.static.goto_next_paragraph))

(define (select-around-impl key)
  (helix.static.select_textobject_around)
  (trigger-on-key-callback key))

(define (select-inner-impl key)
  (helix.static.select_textobject_inner)
  (trigger-on-key-callback key))

;; vaw
;;@doc
;; Select a word and its surrounding whitespace (vaw).
(define (select-around-word)
  (select-around-impl w-key))

;; viw
;;@doc
;; Select a word (viw).
(define (select-inner-word)
  (select-inner-impl w-key))

;; vap
;;@doc
;; Select a paragraph and its surrounding blank lines (vap).
(define (select-around-paragraph)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))

  ;; Find paragraph boundaries
  (define para-start-line (find-paragraph-start rope cur-line))
  (define para-end-line (find-paragraph-end rope cur-line))

  ;; Find end of trailing blank lines
  (define blank-end-line (find-blank-lines-end rope (+ para-end-line 1)))

  ;; Use the blank line end if there are trailing blanks, otherwise use paragraph end
  (define actual-end-line (if (> blank-end-line para-end-line) blank-end-line para-end-line))

  ;; Convert line numbers to character positions
  (define start-char (rope-line->char rope para-start-line))
  (define end-line-start (rope-line->char rope actual-end-line))

  ;; Get the end of the last line
  (define end-line-rope (rope->line rope actual-end-line))
  (define end-line-len (rope-len-chars end-line-rope))
  (define end-char (+ end-line-start end-line-len))

  ;; Move to start and extend to end
  (move-to-position start-char)
  (extend-to-position (- end-char 1)))

;; vip
;;@doc
;; Select a paragraph (vip).
(define (select-inner-paragraph)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))

  ;; Find paragraph boundaries
  (define para-start-line (find-paragraph-start rope cur-line))
  (define para-end-line (find-paragraph-end rope cur-line))

  ;; Convert line numbers to character positions
  (define start-char (rope-line->char rope para-start-line))
  (define end-line-start (rope-line->char rope para-end-line))

  ;; Get the end of the last line in the paragraph
  (define end-line-rope (rope->line rope para-end-line))
  (define end-line-len (rope-len-chars end-line-rope))
  (define end-char (+ end-line-start end-line-len))

  ;; Move to start and extend to end
  (move-to-position start-char)
  (extend-to-position (- end-char 1)))

;; vaf
;;@doc
;; Select a function and its surrounding whitespace (vaf).
(define (select-around-function)
  (select-around-impl f-key))

;; vif
;;@doc
;; Select a function's body (vif).
(define (select-inner-function)
  (select-inner-impl f-key))

;; vac
;;@doc
;; Select a comment block (vac).
(define (select-around-comment)
  (select-around-impl c-key))

;; vic
;;@doc
;; Select a comment's contents (vic).
(define (select-inner-comment)
  (select-inner-impl c-key))

;; vae
;;@doc
;; Select a data-structure literal and its surrounding whitespace (vae).
(define (select-around-data-structure)
  (select-around-impl e-key))

;; vie
;;@doc
;; Select a data-structure literal's contents (vie).
(define (select-inner-data-structure)
  (select-inner-impl e-key))

;; ── Tag text objects via tree-walking ────────────────────────────────────────
;; Helix's native xml-element textobject only resolves when the cursor sits on
;; a captured node (the content), so `it`/`at` fail when the cursor is on the
;; tags or whitespace. Walk the tree to the enclosing element instead, so tag
;; motions work from anywhere inside the tag — in HTML-family grammars and rstml
;; (Leptos view!) alike, using the injection layer's tree at the cursor.

(define *tag-element-kinds* '("element" "element_node"))
(define *tag-open-kinds*    '("start_tag" "open_tag"))
(define *tag-close-kinds*   '("end_tag" "close_tag"))

(define (tag-find-ancestor node kinds)
  (let loop ([n node])
    (cond
      [(not n) #f]
      [(member (tsnode-kind n) kinds) n]
      [else (loop (tsnode-parent n))])))

(define (tag-find-child node kinds)
  (let loop ([cs (tsnode-named-children node)])
    (cond
      [(null? cs) #f]
      [(member (tsnode-kind (car cs)) kinds) (car cs)]
      [else (loop (cdr cs))])))

;; The element_node enclosing the cursor, using the injection layer's tree.
(define (enclosing-tag-element)
  (define doc-id (editor->doc-id (editor-focus)))
  (define rope (editor->text doc-id))
  (and rope
       (let* ([cb   (rope-char->byte rope (cursor-position))]
              [tree (document->tree-byte-range doc-id cb cb)])
         (and tree
              (let* ([root (tstree->root tree)]
                     [leaf (tsnode-named-descendant-byte-range root cb cb)])
                (tag-find-ancestor leaf *tag-element-kinds*))))))

;; Set the primary selection to the half-open char range [start, end).
(define (tag-select-range! start end)
  (helix.static.set-current-selection-object!
    (helix.static.range->selection (helix.static.range start end))))

;; vax / cat / dat / yat — select the whole enclosing element.
;;@doc
;; Select an HTML/JSX tag and its contents (vat).
(define (select-around-html-tag)
  (define el (enclosing-tag-element))
  (and el
       (let ([rope (editor->text (editor->doc-id (editor-focus)))])
         (tag-select-range!
           (rope-byte->char rope (tsnode-start-byte el))
           (rope-byte->char rope (tsnode-end-byte el)))
         #t)))

;; vix / cit / dit / yit — select the content between the open and close tags.
;;@doc
;; Select the contents of an HTML/JSX tag (vit).
(define (select-inner-html-tag)
  (define el (enclosing-tag-element))
  (and el
       (let* ([rope  (editor->text (editor->doc-id (editor-focus)))]
              [open  (tag-find-child el *tag-open-kinds*)]
              [close (tag-find-child el *tag-close-kinds*)])
         (and open close
              (begin
                (tag-select-range!
                  (rope-byte->char rope (tsnode-end-byte open))
                  (rope-byte->char rope (tsnode-start-byte close)))
                #t)))))

;; vit
;;@doc
;; Select a type definition and its surrounding whitespace (vax).
(define (select-around-type-definition)
  (select-around-impl t-key))

;; vit
;;@doc
;; Select a type definition's body (vix).
(define (select-inner-type-definition)
  (select-inner-impl t-key))

;; vaT
;;@doc
;; Select a test block and its surrounding whitespace (vaT).
(define (select-around-test)
  (select-around-impl T-key))

;; viT
;;@doc
;; Select a test block's body (viT).
(define (select-inner-test)
  (select-inner-impl T-key))

;; vaW
;;@doc
;; Select a WORD and its surrounding whitespace (vaW).
(define (select-around-long-word)
  (select-around-impl W-key))

;; viW
;;@doc
;; Select a WORD (viW).
(define (select-inner-long-word)
  (select-inner-impl W-key))

;; Helpers for linewise inner-brace detection (neovim parity)
(define (only-ws-after? rope pos)
  (let* ([line (rope-char->line rope pos)]
         [end  (+ (rope-line->char rope line) (rope-len-chars (rope->line rope line)))])
    (let loop ([p (+ pos 1)])
      (cond [(>= p (- end 1)) #t]
            [(is-whitespace? (rope-char-ref rope p)) (loop (+ p 1))]
            [else #f]))))

(define (only-ws-before? rope pos)
  (let ([start (rope-line->char rope (rope-char->line rope pos))])
    (let loop ([p start])
      (cond [(>= p pos) #t]
            [(is-whitespace? (rope-char-ref rope p)) (loop (+ p 1))]
            [else #f]))))

(define (brace-block-linewise? rope open-pos close-pos)
  (and (> (rope-char->line rope close-pos) (rope-char->line rope open-pos))
       (only-ws-after? rope open-pos)
       (only-ws-before? rope close-pos)))

;; vi{
;; vi[
;; vi(
;; vi"
;; vi'
;; vi<
(define (select-inner-bracket open-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (let ([pair (find-bracket-pair rope cur-pos open-ch)])
    (if (not pair)
        #f
        (let ([open-pos (min (car pair) (cdr pair))]
              [close-pos (max (car pair) (cdr pair))])
          (cond
            [(<= (- close-pos open-pos) 1) #f]
            [(brace-block-linewise? rope open-pos close-pos)
             (let* ([first-line (+ (rope-char->line rope open-pos) 1)]
                    [last-line  (- (rope-char->line rope close-pos) 1)])
               (if (> first-line last-line)
                   (begin (move-to-position (+ open-pos 1))
                          (extend-to-position (- close-pos 1)) #t)
                   (let* ([start-char (rope-line->char rope first-line)]
                          [ls (rope-line->char rope last-line)]
                          [llen (rope-len-chars (rope->line rope last-line))]
                          [last-content (+ ls (- llen 2))])
                     (move-to-position start-char)
                     (when (>= last-content start-char)
                       (extend-to-position last-content))
                     #t)))]
            [else (move-to-position (+ open-pos 1))
                  (extend-to-position (- close-pos 1)) #t])))))

;; va{
;; va[
;; va(
;; va"
;; va'
;; va<
(define (select-around-bracket open-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (let ([pair (find-bracket-pair rope cur-pos open-ch)])
    (if (not pair)
        #f
        (let ([open-pos (min (car pair) (cdr pair))]
              [close-pos (max (car pair) (cdr pair))])
          (move-to-position open-pos)
          (extend-to-position close-pos)
          #t))))

;; Collect positions of all quote chars on cur-line (left to right)
(define (quote-positions-on-line rope quote-ch cur-line)
  (define line-start (rope-line->char rope cur-line))
  (define line-end (+ line-start (rope-len-chars (rope->line rope cur-line))))
  (let loop ([p line-start] [acc '()])
    (cond
      [(>= p (- line-end 1)) (reverse acc)]
      [else
       (let ([ch (rope-char-at rope p)])
         (if (and ch (char=? ch quote-ch))
             (loop (+ p 1) (cons p acc))
             (loop (+ p 1) acc)))])))

;; Find the quote pair (open . close) that contains cur-pos or is first after it.
;; quotes must be in ascending order; pairs are (q0,q1),(q2,q3),...
(define (find-quote-pair quotes cur-pos)
  (let loop ([qs quotes])
    (cond
      [(or (null? qs) (null? (cdr qs))) #f]
      [else
       (let ([open (car qs)]
             [close (cadr qs)])
         (cond
           [(and (<= open cur-pos) (<= cur-pos close)) (cons open close)]
           [(< cur-pos open) (cons open close)]
           [else (loop (cddr qs))]))])))

(define (select-inner-quote quote-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))
  (define pair (find-quote-pair (quote-positions-on-line rope quote-ch cur-line) cur-pos))
  (if (not pair)
      #f
      (let ([open (car pair)] [close (cdr pair)])
        (if (<= (- close open) 1)
            #f
            (begin (move-to-position (+ open 1))
                   (extend-to-position (- close 1))
                   #t)))))

(define (select-around-quote quote-ch)
  (define rope (get-document-as-slice))
  (define cur-pos (cursor-position))
  (define cur-line (rope-char->line rope cur-pos))
  (define pair (find-quote-pair (quote-positions-on-line rope quote-ch cur-line) cur-pos))
  (if (not pair)
      #f
      (begin (move-to-position (car pair))
             (extend-to-position (cdr pair))
             #t)))

;; Public API functions
;;@doc
;; Select the contents of a { } block (vi{).
(define (select-inner-curly)
  (select-inner-bracket #\{))

;;@doc
;; Select a { } block, including the braces (va{).
(define (select-around-curly)
  (select-around-bracket #\{))

;;@doc
;; Select the contents of a ( ) block (vi().
(define (select-inner-paren)
  (select-inner-bracket #\())

;;@doc
;; Select a ( ) block, including the parens (va().
(define (select-around-paren)
  (select-around-bracket #\())

;;@doc
;; Select the contents of a [ ] block (vi[).
(define (select-inner-square)
  (select-inner-bracket #\[))

;;@doc
;; Select a [ ] block, including the brackets (va[).
(define (select-around-square)
  (select-around-bracket #\[))

;;@doc
;; Select the contents of a "double-quoted" string (vi").
(define (select-inner-double-quote)
  (select-inner-quote #\"))

;;@doc
;; Select a "double-quoted" string, including the quotes (va").
(define (select-around-double-quote)
  (select-around-quote #\"))

;;@doc
;; Select the contents of a 'single-quoted' string (vi').
(define (select-inner-single-quote)
  (select-inner-quote #\'))

;;@doc
;; Select a 'single-quoted' string, including the quotes (va').
(define (select-around-single-quote)
  (select-around-quote #\'))

;;@doc
;; Select the contents of a < > block (vi<).
(define (select-inner-arrow)
  (select-inner-bracket #\<))

;;@doc
;; Select a < > block, including the angle brackets (va<).
(define (select-around-arrow)
  (select-around-bracket #\<))

;; f(char)
;;@doc
;; Extend the selection to the next occurrence of a character (f in select mode).
(define (select-find-next-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-next-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\f char)))))))

(define (select-find-next-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (extend-right-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; F(char)
;;@doc
;; Extend the selection to the previous occurrence of a character (F in select mode).
(define (select-find-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\F char)))))))

(define (select-find-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (extend-left-n i)]
      [else (loop (+ i 1) next-char)]))

  (define (find-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (loop 1 next-char)
       (find-repeat (- count 1) next-char)]))

  (find-repeat count char))

;; t(char)
;;@doc
;; Extend the selection to just before the next occurrence of a character (t in select mode).
(define (select-find-till-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-find-till-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\t char)))))))

(define (select-find-till-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (extend-right-n i)
       (helix.static.extend_char_left)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (extend-right-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; T(char)
;;@doc
;; Extend the selection to just after the previous occurrence of a character (T in select mode).
(define (select-till-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (select-till-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\T char)))))))

(define (select-till-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (extend-left-n i)
       (helix.static.extend_char_right)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (extend-left-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; ,
;;@doc
;; Repeat the last character search, extending the selection (, in select mode).
(define (select-repeat-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (select-find-next-char-impl char count)]
    [(equal? action #\F) (select-find-prev-char-impl char count)]
    [(equal? action #\t) (select-find-till-char-impl char count)]
    [(equal? action #\T) (select-till-prev-char-impl char count)]))

;; ;
;;@doc
;; Repeat the last character search in reverse, extending the selection (; in select mode).
(define (select-reverse-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (select-find-prev-char-impl char count)]
    [(equal? action #\F) (select-find-next-char-impl char count)]
    [(equal? action #\t) (select-till-prev-char-impl char count)]
    [(equal? action #\T) (select-find-till-char-impl char count)]))

;;@doc
;; Exit VIS-LINE mode back to normal mode (Esc).
(define (exit-visual-line-mode)
  (when (is-visual-line-mode?)
    (set-visual-line-mode! #f))
  (helix.static.collapse_selection)
  (helix.static.normal_mode)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (define prev-char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (if (equal? #\newline char)
        ;; i don't want to import normal motions
        (unless (equal? #\newline prev-char)
          (helix.static.move_char_left)))))

(provide extend-char-right-same-line
         extend-char-left-same-line
         extend-line-up
         extend-line-up-vis-line
         extend-line-down
         extend-line-down-vis-line
         vim-extend-next-word-start
         vim-extend-next-long-word-start
         vim-extend-to-next-paragraph
         vim-extend-to-prev-paragraph
         select-around-word
         select-inner-word
         select-around-paragraph
         select-inner-paragraph
         select-around-function
         select-inner-function
         select-around-comment
         select-inner-comment
         select-around-data-structure
         select-inner-data-structure
         select-around-html-tag
         select-inner-html-tag
         select-around-type-definition
         select-inner-type-definition
         select-around-test
         select-inner-test
         select-inner-curly
         select-around-curly
         select-inner-paren
         select-around-paren
         select-inner-square
         select-around-square
         select-inner-double-quote
         select-around-double-quote
         select-inner-single-quote
         select-around-single-quote
         select-inner-arrow
         select-around-arrow
         select-around-long-word
         select-inner-long-word
         select-find-next-char
         select-find-prev-char
         select-find-till-char
         select-till-prev-char
         select-repeat-last-find
         select-reverse-last-find
         exit-visual-line-mode)
