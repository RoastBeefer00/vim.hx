(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "visual-motions.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/editor.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

;; ── Linewise register (vim yy/dd → p/P) ──────────────────────────────────────
;; Helix registers don't track vim's linewise-ness, so we track it here: yy
;; records the line text and marks the last yank linewise; every characterwise
;; yank clears the flag (via yank-selection-ranges). vim-paste-after/before then
;; paste linewise (on a new line) or fall back to the normal clipboard paste.
(define *yank-linewise* (box #f))
(define *linewise-text* (box ""))

(define (strip-trailing-newline s)
  (if (and (> (string-length s) 0)
           (char=? (string-ref s (- (string-length s) 1)) #\newline))
      (substring s 0 (- (string-length s) 1))
      s))

;; Yank flash: briefly highlight the yanked region as visual confirmation.
;; Requires set-document-highlights! / selection-char-ranges (helix-steel
;; script-highlights API). Uses eval so it degrades gracefully on older builds.

(define *yank-flash-scope* "ui.selection")
(define *yank-flash-delay* 350)

(define (yank-flash! ranges)
  (unless (null? ranges)
    (set-document-highlights! "yank" ranges *yank-flash-scope*)
    (enqueue-thread-local-callback-with-delay
     *yank-flash-delay*
     (lambda () (clear-document-highlights! "yank")))))

(define (yank-selection-ranges)
  ;; Any yank routed through here is characterwise unless the caller (yy) marks
  ;; it linewise afterward.
  (set-box! *yank-linewise* #f)
  (selection-char-ranges))

(define (yank-impl func)
  (when (func)
    (define ranges (yank-selection-ranges))
    (helix.static.yank_main_selection_to_clipboard)
    (helix.static.flip_selections)
    (helix.static.collapse_selection)
    (yank-flash! ranges)))

;; y (select)
(define (vim-yank-selection)
  (yank-impl helix.static.no_op)
  (exit-visual-line-mode))

;; yaw
(define (yank-around-word)
  (yank-impl select-around-word))

;; yiw
(define (yank-inner-word)
  (yank-impl select-inner-word))

;; yw
(define (yank-word)
  (vim-extend-next-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (define ranges (yank-selection-ranges))
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.flip_selections)
  (helix.static.collapse_selection)
  (yank-flash! ranges))

;; yW
(define (yank-long-word)
  (vim-extend-next-long-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (define ranges (yank-selection-ranges))
  (helix.static.yank_main_selection_to_clipboard)
  (helix.static.flip_selections)
  (helix.static.collapse_selection)
  (yank-flash! ranges))

;; yf{char}
(define (yank-find-char)
  (on-key-callback
   (lambda (key-event)
     (define char (on-key-event-char key-event))
     (when char
       (define rope (get-document-as-slice))
       (define start-pos (cursor-position))
       (define len (rope-len-chars rope))
       (let loop ([i 1])
         (define pos (+ start-pos i))
         (cond
           [(>= pos len) (void)]
           [(char=? (rope-char-ref rope pos) char)
            (extend-right-n i)
            (define ranges (yank-selection-ranges))
            (helix.static.yank_main_selection_to_clipboard)
            (helix.static.flip_selections)
            (helix.static.collapse_selection)
            (yank-flash! ranges)]
           [else (loop (+ i 1))]))))))

;; yt{char}
(define (yank-till-char)
  (on-key-callback
   (lambda (key-event)
     (define char (on-key-event-char key-event))
     (when char
       (define rope (get-document-as-slice))
       (define start-pos (cursor-position))
       (define len (rope-len-chars rope))
       (let loop ([i 1])
         (define pos (+ start-pos i))
         (cond
           [(>= pos len) (void)]
           [(char=? (rope-char-ref rope pos) char)
            (when (> i 1)
              (extend-right-n (- i 1))
              (define ranges (yank-selection-ranges))
              (helix.static.yank_main_selection_to_clipboard)
              (helix.static.flip_selections)
              (helix.static.collapse_selection)
              (yank-flash! ranges))]
           [else (loop (+ i 1))]))))))

;; yb
(define (yank-prev-word)
  (yank-impl helix.static.extend_prev_word_start))

;; yB
(define (yank-prev-long-word)
  (yank-impl helix.static.extend_prev_long_word_start))

;; y$
(define (yank-line-end)
  (yank-impl helix.static.extend_to_line_end))

;; y^
(define (yank-line-start)
  (yank-impl helix.static.extend_to_line_start))

;; y0
(define (yank-line-start-non-whitespace)
  (yank-impl helix.static.extend_to_first_nonwhitespace))

;; yy
(define (vim-yank-line)
  (define start-pos (cursor-position))
  (define count (editor-count))
  (when (> count 1)
    (set-editor-count! (- count 1))
    (helix.static.extend_line_down))
  (helix.static.extend_to_line_bounds)
  (define ranges (yank-selection-ranges))
  ;; Record the line text and mark this yank linewise (after yank-selection-ranges,
  ;; which resets the flag). Include a trailing newline so it's a whole line.
  (set-box! *linewise-text*
            (string-append (strip-trailing-newline (helix.static.current-highlighted-text!))
                           "\n"))
  (helix.static.yank_main_selection_to_clipboard)
  (set-box! *yank-linewise* #t)
  (helix.static.normal_mode)
  (helix.static.collapse_selection)
  (define current-pos (cursor-position))
  (define distance (- start-pos current-pos))
  (cond
    [(> distance 0) (move-right-n distance)]
    [(< distance 0) (move-left-n (- distance))])
  (yank-flash! ranges))

;; yap/yip
(define (yank-around-paragraph)        (yank-impl select-around-paragraph))
(define (yank-inner-paragraph)         (yank-impl select-inner-paragraph))

;; yaf/yif
(define (yank-around-function)         (yank-impl select-around-function))
(define (yank-inner-function)          (yank-impl select-inner-function))

;; yac/yic
(define (yank-around-comment)          (yank-impl select-around-comment))
(define (yank-inner-comment)           (yank-impl select-inner-comment))

;; yae/yie
(define (yank-around-data-structure)   (yank-impl select-around-data-structure))
(define (yank-inner-data-structure)    (yank-impl select-inner-data-structure))

;; yax/yix
(define (yank-around-html-tag)         (yank-impl select-around-html-tag))
(define (yank-inner-html-tag)          (yank-impl select-inner-html-tag))

;; yat/yit
(define (yank-around-type-definition)  (yank-impl select-around-type-definition))
(define (yank-inner-type-definition)   (yank-impl select-inner-type-definition))

;; yaT/yiT
(define (yank-around-test)             (yank-impl select-around-test))
(define (yank-inner-test)              (yank-impl select-inner-test))

;; ya{/yi{
(define (yank-around-curly)            (yank-impl select-around-curly))
(define (yank-inner-curly)             (yank-impl select-inner-curly))

;; ya[/yi[
(define (yank-around-square)           (yank-impl select-around-square))
(define (yank-inner-square)            (yank-impl select-inner-square))

;; ya(/yi(
(define (yank-around-paren)            (yank-impl select-around-paren))
(define (yank-inner-paren)             (yank-impl select-inner-paren))

;; ya"/yi"
(define (yank-around-double-quote)     (yank-impl select-around-double-quote))
(define (yank-inner-double-quote)      (yank-impl select-inner-double-quote))

;; ya'/yi'
(define (yank-around-single-quote)     (yank-impl select-around-single-quote))
(define (yank-inner-single-quote)      (yank-impl select-inner-single-quote))

;; ya</yi<
(define (yank-around-arrow)            (yank-impl select-around-arrow))
(define (yank-inner-arrow)             (yank-impl select-inner-arrow))

;; yaW/yiW
(define (yank-around-long-word)        (yank-impl select-around-long-word))
(define (yank-inner-long-word)         (yank-impl select-inner-long-word))

;; ── p / P — linewise-aware paste ─────────────────────────────────────────────
;; If the last yank was linewise (yy), paste the line on its own new line below
;; (p) or above (P), matching vim. Otherwise fall back to the normal clipboard
;; paste so characterwise yanks behave as before.

;; p
(define (vim-paste-after)
  (if (unbox *yank-linewise*)
      (let ([text (strip-trailing-newline (unbox *linewise-text*))])
        ;; insert_at_line_end puts the cursor *after* the last char (insert
        ;; mode); goto_line_end would leave it *on* the last char, so
        ;; insert_string would split the line.
        (helix.static.insert_at_line_end)
        (helix.static.insert_string (string-append "\n" text))
        (helix.static.normal_mode)
        (helix.static.goto_line_start))
      (helix.static.paste_clipboard_after)))

;; P
(define (vim-paste-before)
  (if (unbox *yank-linewise*)
      (let ([text (strip-trailing-newline (unbox *linewise-text*))])
        (helix.static.goto_line_start)
        (helix.static.insert_string (string-append text "\n"))
        (helix.static.move_line_up)
        (helix.static.goto_line_start))
      (helix.static.paste_clipboard_before)))

(provide vim-paste-after
         vim-paste-before
         vim-yank-selection
         yank-around-word
         yank-inner-word
         yank-around-paragraph
         yank-inner-paragraph
         yank-around-function
         yank-inner-function
         yank-around-comment
         yank-inner-comment
         yank-around-data-structure
         yank-inner-data-structure
         yank-around-html-tag
         yank-inner-html-tag
         yank-around-type-definition
         yank-inner-type-definition
         yank-around-test
         yank-inner-test
         yank-around-curly
         yank-inner-curly
         yank-around-square
         yank-inner-square
         yank-around-paren
         yank-inner-paren
         yank-around-double-quote
         yank-inner-double-quote
         yank-around-single-quote
         yank-inner-single-quote
         yank-around-arrow
         yank-inner-arrow
         yank-around-long-word
         yank-inner-long-word
         yank-word
         yank-long-word
         yank-prev-word
         yank-prev-long-word
         yank-find-char
         yank-till-char
         yank-line-end
         yank-line-start
         yank-line-start-non-whitespace
         vim-yank-line
         *yank-flash-delay*
         *yank-flash-scope*)
