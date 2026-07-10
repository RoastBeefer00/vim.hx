(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "helix/editor.scm")

(require-builtin steel/time)

(require-builtin helix/core/text)

(require "visual-motions.scm")

(define (change-impl func)
  (when (func)
    (helix.clipboard-yank)
    (helix.static.change_selection)))

;; c (select)
;;@doc
;; Delete the current selection and enter insert mode (c in select mode).
(define (vim-change-selection)
  (helix.clipboard-yank)
  (helix.static.change_selection))

;; S
;;@doc
;; Delete the current line's contents and enter insert mode (cc / S).
(define (vim-change-line)
  (change-impl helix.static.extend_to_line_bounds))

;; cw
;;@doc
;; Change to the start of the next word (cw).
(define (vim-change-word)
  (change-impl helix.static.extend_next_word_end))

;; cW
;;@doc
;; Change to the start of the next WORD (cW).
(define (vim-change-long-word)
  (change-impl helix.static.extend_next_long_word_end))

;; cb
;;@doc
;; Change to the start of the previous word (cb).
(define (vim-change-prev-word)
  (change-impl helix.static.extend_prev_word_start))

;; cB
;;@doc
;; Change to the start of the previous WORD (cB).
(define (vim-change-prev-long-word)
  (change-impl helix.static.extend_prev_long_word_start))

;; c$
;;@doc
;; Change to the end of the line (c$ / C).
(define (vim-change-line-end)
  (change-impl helix.static.extend_to_line_end))

;; c^
;;@doc
;; Change to the first non-whitespace character of the line (c^).
(define (vim-change-line-start)
  (change-impl helix.static.extend_to_first_nonwhitespace))

;; c0
;;@doc
;; Change to column 0 of the line (c0).
(define (vim-change-line-col0)
  (change-impl helix.static.extend_to_line_start))

;; ce
;;@doc
;; Change to the end of the current word (ce).
(define (vim-change-word-end)
  (change-impl helix.static.extend_next_word_end))

;; cE
;;@doc
;; Change to the end of the current WORD (cE).
(define (vim-change-long-word-end)
  (change-impl helix.static.extend_next_long_word_end))

;; caw
;;@doc
;; Change a word and its surrounding whitespace (caw).
(define (vim-change-around-word)
  (change-impl select-around-word))

;; ciw
;;@doc
;; Change a word (ciw).
(define (vim-change-inner-word)
  (change-impl select-inner-word))

;; caW
;;@doc
;; Change a WORD and its surrounding whitespace (caW).
(define (vim-change-around-long-word)
  (change-impl select-around-long-word))

;; ciW
;;@doc
;; Change a WORD (ciW).
(define (vim-change-inner-long-word)
  (change-impl select-inner-long-word))

;; cap
;;@doc
;; Change a paragraph and its surrounding blank lines (cap).
(define (vim-change-around-paragraph)
  (change-impl select-around-paragraph))

;; cip
;;@doc
;; Change a paragraph (cip).
(define (vim-change-inner-paragraph)
  (change-impl select-inner-paragraph))

;; caf
;;@doc
;; Change a function and its surrounding whitespace (caf).
(define (vim-change-around-function)
  (change-impl select-around-function))

;; cif
;;@doc
;; Change a function's body (cif).
(define (vim-change-inner-function)
  (change-impl select-inner-function))

;; cac
;;@doc
;; Change a comment block (cac).
(define (vim-change-around-comment)
  (change-impl select-around-comment))

;; cic
;;@doc
;; Change a comment's contents (cic).
(define (vim-change-inner-comment)
  (change-impl select-inner-comment))

;; cae
;;@doc
;; Change a data-structure literal and its surrounding whitespace (cae).
(define (vim-change-around-data-structure)
  (change-impl select-around-data-structure))

;; cie
;;@doc
;; Change a data-structure literal's contents (cie).
(define (vim-change-inner-data-structure)
  (change-impl select-inner-data-structure))

;; cax
;;@doc
;; Change an HTML/JSX tag and its contents (cat).
(define (vim-change-around-html-tag)
  (change-impl select-around-html-tag))

;; cix
;;@doc
;; Change the contents of an HTML/JSX tag (cit).
(define (vim-change-inner-html-tag)
  (change-impl select-inner-html-tag))

;; cat
;;@doc
;; Change a type definition and its surrounding whitespace (cax).
(define (vim-change-around-type-definition)
  (change-impl select-around-type-definition))

;; cit
;;@doc
;; Change a type definition's body (cix).
(define (vim-change-inner-type-definition)
  (change-impl select-inner-type-definition))

;; caT
;;@doc
;; Change a test block and its surrounding whitespace (caT).
(define (vim-change-around-test)
  (change-impl select-around-test))

;; ciT
;;@doc
;; Change a test block's body (ciT).
(define (vim-change-inner-test)
  (change-impl select-inner-test))

;; ca{
;;@doc
;; Change a { } block, including the braces (ca{).
(define (vim-change-around-curly)
  (change-impl select-around-curly))

;; ci{
;;@doc
;; Change the contents of a { } block (ci{).
(define (vim-change-inner-curly)
  (change-impl select-inner-curly))

;; ca[
;;@doc
;; Change a [ ] block, including the brackets (ca[).
(define (vim-change-around-square)
  (change-impl select-around-square))

;; ci[
;;@doc
;; Change the contents of a [ ] block (ci[).
(define (vim-change-inner-square)
  (change-impl select-inner-square))

;; ci(
;;@doc
;; Change the contents of a ( ) block (ci().
(define (vim-change-inner-paren)
  (change-impl select-inner-paren))

;; ca(
;;@doc
;; Change a ( ) block, including the parens (ca().
(define (vim-change-around-paren)
  (change-impl select-around-paren))

;; ca"
;;@doc
;; Change a "double-quoted" string, including the quotes (ca").
(define (vim-change-around-double-quote)
  (change-impl select-around-double-quote))

;; ci"
;;@doc
;; Change the contents of a "double-quoted" string (ci").
(define (vim-change-inner-double-quote)
  (change-impl select-inner-double-quote))

;; ca'
;;@doc
;; Change a 'single-quoted' string, including the quotes (ca').
(define (vim-change-around-single-quote)
  (change-impl select-around-single-quote))

;; ci'
;;@doc
;; Change the contents of a 'single-quoted' string (ci').
(define (vim-change-inner-single-quote)
  (change-impl select-inner-single-quote))

;; ca<
;;@doc
;; Change a < > block, including the angle brackets (ca<).
(define (vim-change-around-arrow)
  (change-impl select-around-arrow))

;; ci<
;;@doc
;; Change the contents of a < > block (ci<).
(define (vim-change-inner-arrow)
  (change-impl select-inner-arrow))

(provide vim-change-selection
         vim-change-line
         vim-change-word
         vim-change-long-word
         vim-change-prev-word
         vim-change-prev-long-word
         vim-change-word-end
         vim-change-long-word-end
         vim-change-line-end
         vim-change-line-start
         vim-change-line-col0
         vim-change-around-word
         vim-change-inner-word
         vim-change-around-long-word
         vim-change-inner-long-word
         vim-change-around-paragraph
         vim-change-inner-paragraph
         vim-change-around-function
         vim-change-inner-function
         vim-change-around-comment
         vim-change-inner-comment
         vim-change-around-data-structure
         vim-change-inner-data-structure
         vim-change-around-html-tag
         vim-change-inner-html-tag
         vim-change-around-type-definition
         vim-change-inner-type-definition
         vim-change-around-test
         vim-change-inner-test
         vim-change-around-curly
         vim-change-inner-curly
         vim-change-around-square
         vim-change-inner-square
         vim-change-inner-paren
         vim-change-around-paren
         vim-change-around-double-quote
         vim-change-inner-double-quote
         vim-change-around-single-quote
         vim-change-inner-single-quote
         vim-change-around-arrow
         vim-change-inner-arrow)
