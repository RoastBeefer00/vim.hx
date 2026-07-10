(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin helix/core/text)

(require "utils.scm")
(require "visual-motions.scm")

;; TODO: rethink everything about how repetition is implemented in delete and yank

(define (delete-impl func)
  (when (func)
    (helix.clipboard-yank)
    (helix.static.delete_selection)))

;; x (d in select-mode?)
;;@doc
;; Delete the current selection (x / d in select mode).
(define (vim-delete-selection)
  (helix.clipboard-yank)
  (helix.static.delete_selection)
  (exit-visual-line-mode))

;; dd
;;@doc
;; Delete the current line (dd).
(define (vim-delete-line)
  (define count (editor-count))
  (when (> count 1)
    (set-editor-count! (- count 1))
    (helix.static.extend_line_down))
  (helix.static.extend_to_line_bounds)
  (helix.clipboard-yank)
  (helix.static.delete_selection))

;; (orignal comment: broken - cuts off too much text in certain scenarios)
;; nacl -- I may have fixed this but can't know for sure unless
;; getting specific examples
;; dw
;;@doc
;; Delete to the start of the next word (dw).
(define (vim-delete-word)
  (vim-extend-next-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (helix.clipboard-yank)
  (helix.static.delete_selection))

;; (orignal comment: broken - cuts off too much text in certain scenarios)
;; nacl -- I may have fixed this but can't know for sure unless
;; getting specific examples
;; dW
;;@doc
;; Delete to the start of the next WORD (dW).
(define (vim-delete-long-word)
  (vim-extend-next-long-word-start)
  (set-editor-count! 1)
  (helix.static.extend_char_left)
  (helix.clipboard-yank)
  (helix.static.delete_selection))

;; this may not 100% work
;; db
;;@doc
;; Delete to the start of the previous word (db).
(define (vim-delete-prev-word)
  (vim-delete-prev-word-impl helix.static.extend_prev_word_start))

;; this may not 100% work
;; dB
;;@doc
;; Delete to the start of the previous WORD (dB).
(define (vim-delete-prev-long-word)
  (vim-delete-prev-word-impl helix.static.extend_prev_long_word_start))

(define (vim-delete-prev-word-impl func)
  (define pos (cursor-position))
  (func)
  (define start-pos (cursor-position))
  (move-to-position (- pos 1))
  (extend-to-position start-pos)
  (define new-pos (cursor-position))
  (when (not (equal? new-pos pos))
    (helix.clipboard-yank)
    (helix.static.delete_selection)))

;; de
;;@doc
;; Delete to the end of the current word (de).
(define (vim-delete-word-end)
  (delete-impl helix.static.extend_next_word_end))

;; dE
;;@doc
;; Delete to the end of the current WORD (dE).
(define (vim-delete-long-word-end)
  (delete-impl helix.static.extend_next_long_word_end))

;; d$
;;@doc
;; Delete to the end of the line (d$ / D).
(define (vim-delete-line-end)
  (delete-impl helix.static.extend_to_line_end))

;; d^
;;@doc
;; Delete to the first non-whitespace character of the line (d^).
(define (vim-delete-line-start)
  (delete-impl helix.static.extend_to_first_nonwhitespace))

;; d0
;;@doc
;; Delete to column 0 of the line (d0).
(define (vim-delete-line-col0)
  (delete-impl helix.static.extend_to_line_start))

;; daw
;;@doc
;; Delete a word and its surrounding whitespace (daw).
(define (vim-delete-around-word)
  (delete-impl select-around-word))

;; diw
;;@doc
;; Delete a word (diw).
(define (vim-delete-inner-word)
  (delete-impl select-inner-word))

;; daW
;;@doc
;; Delete a WORD and its surrounding whitespace (daW).
(define (vim-delete-around-long-word)
  (delete-impl select-around-long-word))

;; diW
;;@doc
;; Delete a WORD (diW).
(define (vim-delete-inner-long-word)
  (delete-impl select-inner-long-word))

;; dap
;;@doc
;; Delete a paragraph and its surrounding blank lines (dap).
(define (vim-delete-around-paragraph)
  (delete-impl select-around-paragraph))

;; dip
;;@doc
;; Delete a paragraph (dip).
(define (vim-delete-inner-paragraph)
  (delete-impl select-inner-paragraph))

;; daf
;;@doc
;; Delete a function and its surrounding whitespace (daf).
(define (vim-delete-around-function)
  (delete-impl select-around-function))

;; dif
;;@doc
;; Delete a function's body (dif).
(define (vim-delete-inner-function)
  (delete-impl select-inner-function))

;; dac
;;@doc
;; Delete a comment block (dac).
(define (vim-delete-around-comment)
  (delete-impl select-around-comment))

;; dic
;;@doc
;; Delete a comment's contents (dic).
(define (vim-delete-inner-comment)
  (delete-impl select-inner-comment))

;; dae
;;@doc
;; Delete a data-structure literal and its surrounding whitespace (dae).
(define (vim-delete-around-data-structure)
  (delete-impl select-around-data-structure))

;; die
;;@doc
;; Delete a data-structure literal's contents (die).
(define (vim-delete-inner-data-structure)
  (delete-impl select-inner-data-structure))

;; dax
;;@doc
;; Delete an HTML/JSX tag and its contents (dat).
(define (vim-delete-around-html-tag)
  (delete-impl select-around-html-tag))

;; dix
;;@doc
;; Delete the contents of an HTML/JSX tag (dit).
(define (vim-delete-inner-html-tag)
  (delete-impl select-inner-html-tag))

;; dat
;;@doc
;; Delete a type definition and its surrounding whitespace (dax).
(define (vim-delete-around-type-definition)
  (delete-impl select-around-type-definition))

;; dit
;;@doc
;; Delete a type definition's body (dix).
(define (vim-delete-inner-type-definition)
  (delete-impl select-inner-type-definition))

;; daT
;;@doc
;; Delete a test block and its surrounding whitespace (daT).
(define (vim-delete-around-test)
  (delete-impl select-around-test))

;; diT
;;@doc
;; Delete a test block's body (diT).
(define (vim-delete-inner-test)
  (delete-impl select-inner-test))

;; da{
;;@doc
;; Delete a { } block, including the braces (da{).
(define (vim-delete-around-curly)
  (delete-impl select-around-curly))

;; di{
;;@doc
;; Delete the contents of a { } block (di{).
(define (vim-delete-inner-curly)
  (delete-impl select-inner-curly))

;; da[
;;@doc
;; Delete a [ ] block, including the brackets (da[).
(define (vim-delete-around-square)
  (delete-impl select-around-square))

;; di[
;;@doc
;; Delete the contents of a [ ] block (di[).
(define (vim-delete-inner-square)
  (delete-impl select-inner-square))

;; di(
;;@doc
;; Delete the contents of a ( ) block (di().
(define (vim-delete-inner-paren)
  (delete-impl select-inner-paren))

;; da(
;;@doc
;; Delete a ( ) block, including the parens (da().
(define (vim-delete-around-paren)
  (delete-impl select-around-paren))

;; da"
;;@doc
;; Delete a "double-quoted" string, including the quotes (da").
(define (vim-delete-around-double-quote)
  (delete-impl select-around-double-quote))

;; di"
;;@doc
;; Delete the contents of a "double-quoted" string (di").
(define (vim-delete-inner-double-quote)
  (delete-impl select-inner-double-quote))

;; da'
;;@doc
;; Delete a 'single-quoted' string, including the quotes (da').
(define (vim-delete-around-single-quote)
  (delete-impl select-around-single-quote))

;; di'
;;@doc
;; Delete the contents of a 'single-quoted' string (di').
(define (vim-delete-inner-single-quote)
  (delete-impl select-inner-single-quote))

;; da<
;;@doc
;; Delete a < > block, including the angle brackets (da<).
(define (vim-delete-around-arrow)
  (delete-impl select-around-arrow))

;; di<
;;@doc
;; Delete the contents of a < > block (di<).
(define (vim-delete-inner-arrow)
  (delete-impl select-inner-arrow))

(provide vim-delete-selection
         vim-delete-line
         vim-delete-word
         vim-delete-long-word
         vim-delete-word-end
         vim-delete-long-word-end
         vim-delete-line-end
         vim-delete-line-start
         vim-delete-line-col0
         vim-delete-around-word
         vim-delete-inner-word
         vim-delete-around-long-word
         vim-delete-inner-long-word
         vim-delete-around-paragraph
         vim-delete-inner-paragraph
         vim-delete-prev-word
         vim-delete-prev-long-word
         vim-delete-around-function
         vim-delete-inner-function
         vim-delete-around-comment
         vim-delete-inner-comment
         vim-delete-around-data-structure
         vim-delete-inner-data-structure
         vim-delete-around-html-tag
         vim-delete-inner-html-tag
         vim-delete-around-type-definition
         vim-delete-inner-type-definition
         vim-delete-around-test
         vim-delete-inner-test
         vim-delete-around-curly
         vim-delete-inner-curly
         vim-delete-around-square
         vim-delete-inner-square
         vim-delete-inner-paren
         vim-delete-around-paren
         vim-delete-around-double-quote
         vim-delete-inner-double-quote
         vim-delete-around-single-quote
         vim-delete-inner-single-quote
         vim-delete-around-arrow
         vim-delete-inner-arrow)
