(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(require "utils.scm")
(require "key-emulation.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require "helix/editor.scm")

(require-builtin steel/time)
(require-builtin helix/core/text)

;; u
;;@doc
;; Undo the last change (u).
(define (vim-undo)
  (helix.static.undo)
  (helix.static.collapse_selection))

;; a
;;@doc
;; Enter insert mode after the cursor (a).
(define (vim-append-mode)
  ;; Move to insert mode
  (helix.static.insert_mode)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_right))))

;; l
;;@doc
;; Move right one character, stopping at end of line (l).
(define (move-char-right-same-line)
  (define count (editor-count))
  (do-n-times count move-char-right-same-line-impl))

(define (move-char-right-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (+ 1 pos)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_right))))

;; h
;;@doc
;; Move left one character, stopping at start of line (h).
(define (move-char-left-same-line)
  (define count (editor-count))
  (do-n-times count move-char-left-same-line-impl))

(define (move-char-left-same-line-impl)
  (set-editor-count! 1)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) (- pos 1)))
  (when char
    (unless (equal? #\newline char)
      (helix.static.move_char_left))))

;; k
;;@doc
;; Move up one line, preserving the target column (k).
(define (move-line-up)
  (helix.static.move_line_up)
  (move-line-up-impl))

;;@doc
;; Move up one line, preserving the target column (shared implementation).
(define (move-line-up-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (move-char-left-same-line-impl))))

;; j
;;@doc
;; Move down one line, preserving the target column (j).
(define (move-line-down)
  (helix.static.move_line_down)
  (move-line-down-impl))

;;@doc
;; Move down one line, preserving the target column (shared implementation).
(define (move-line-down-impl)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define char (rope-char-ref doc pos))
  (when char
    (when (char=? #\newline char)
      (move-char-left-same-line-impl))))

;; f(char)
;;@doc
;; Find and jump to the next occurrence of a character on the line (f).
(define (vim-find-next-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-next-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\f char)))))))

(define (vim-find-next-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char) (move-right-n i)]
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
;; Find and jump to the previous occurrence of a character on the line (F).
(define (vim-find-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\F char)))))))

(define (vim-find-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move left n times
      [(equal? char next-char) (move-left-n i)]
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
;; Jump to just before the next occurrence of a character on the line (t).
(define (vim-find-till-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-find-till-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\t char)))))))

(define (vim-find-till-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (+ i pos)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (move-right-n i)
       (helix.static.move_char_left)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (move-right-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; T(char)
;;@doc
;; Jump to just after the previous occurrence of a character on the line (T).
(define (vim-till-prev-char)
  (define count (editor-count))
  (on-key-callback (lambda (key-event)
                     (define char (on-key-event-char key-event))
                     (when char
                       (vim-till-prev-char-impl char count)
                       ;; NOTE: this will break if default key-bind is changed
                       (set-register! #\, (list (string #\@ #\T char)))))))

(define (vim-till-prev-char-impl char count)
  (define (loop i next-char)
    (define pos (cursor-position))
    (define doc (get-document-as-slice))
    (define char (rope-char-ref doc (- pos i)))
    (cond
      [(equal? char #\newline) void]
      ;; Move right n times
      [(equal? char next-char)
       (move-left-n i)
       (helix.static.move_char_right)]
      [else (loop (+ i 1) next-char)]))

  (define (find-till-repeat count next-char)
    (cond
      [(zero? count) void]
      [else
       (define pos (cursor-position))
       (define doc (get-document-as-slice))
       (define char (rope-char-ref doc pos))
       (cond
         [(equal? char next-char) (move-left-n 1)])
       (loop 0 next-char)
       (find-till-repeat (- count 1) next-char)]))

  (find-till-repeat count char))

;; NOTE: this is midly hacky, but the goal is to run the macro
;; stored in the , register, so whatever it takes...
;; ,
;;@doc
;; Repeat the last f/F/t/T character search (,).
(define (vim-repeat-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (helix.echo find-macro)
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (vim-find-next-char-impl char count)]
    [(equal? action #\F) (vim-find-prev-char-impl char count)]
    [(equal? action #\t) (vim-find-till-char-impl char count)]
    [(equal? action #\T) (vim-till-prev-char-impl char count)]))

;; ;
;;@doc
;; Repeat the last f/F/t/T character search in the opposite direction (;).
(define (vim-reverse-last-find)
  (define count (editor-count))
  (define find-macro (to-string (first (register->value #\,))))
  (define action (string-ref find-macro 1))
  (define char (string-ref find-macro 2))
  (cond
    [(equal? action #\f) (vim-find-prev-char-impl char count)]
    [(equal? action #\F) (vim-find-next-char-impl char count)]
    [(equal? action #\t) (vim-till-prev-char-impl char count)]
    [(equal? action #\T) (vim-find-till-char-impl char count)]))

;; G or (line-number)G
;;@doc
;; Go to the given count line, or the last line with no count (G).
(define (vim-goto-line-or-last)
  (define rope (get-document-as-slice))
  (define start-pos (cursor-position))

  (helix.static.goto_line)

  (define end-pos (cursor-position))

  ;; If we didn't move, no count was provided - go to last line
  (when (= start-pos end-pos)
    (helix.static.goto_last_line)))

;; e
;;@doc
;; Jump to the end of the next word (e).
(define (vim-next-word-end)
  (helix.static.move_next_word_end)
  (helix.static.collapse_selection))

;; E
;;@doc
;; Jump to the end of the next WORD (E).
(define (vim-next-long-word-end)
  (helix.static.move_next_long_word_end)
  (helix.static.collapse_selection))

;; b
;;@doc
;; Jump to the start of the previous word (b).
(define (vim-prev-word-start)
  (helix.static.move_prev_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (vim-prev-word-start)))

;; B
;;@doc
;; Jump to the start of the previous WORD (B).
(define (vim-prev-long-word-start)
  (helix.static.move_prev_long_word_start)
  (helix.static.collapse_selection)
  (define pos (cursor-position))
  (define doc (get-document-as-slice))
  (define cur-char (rope-char-ref doc pos))
  (when (and cur-char (char-whitespace? cur-char))
    (vim-prev-long-word-start)))

;; w
;;@doc
;; Jump to the start of the next word (w).
(define (vim-next-word-start)
  (define count (editor-count))
  (do-n-times count vim-next-word-start-impl))

(define (vim-next-word-start-impl)
  (get-next-word-start move-right-n))

;; W
;;@doc
;; Jump to the start of the next WORD (W).
(define (vim-next-long-word-start)
  (define count (editor-count))
  (do-n-times count vim-next-long-word-start-impl))

(define (vim-next-long-word-start-impl)
  (get-next-long-word-start move-right-n))

;; {
;;@doc
;; Jump to the start of the previous paragraph ({).
(define (vim-goto-prev-paragraph)
  (helix.static.goto_prev_paragraph)
  (helix.static.collapse_selection))

;; }
;;@doc
;; Jump to the start of the next paragraph (}).
(define (vim-goto-next-paragraph)
  (helix.static.goto_next_paragraph)
  (helix.static.collapse_selection))

;; V
;;@doc
;; Enter linewise visual (select) mode (V).
(define (visual-line-mode)
  (set-visual-line-mode! #t)
  (helix.static.select_mode)
  (helix.static.extend_to_line_bounds))

;; esc from normal
;;@doc
;; Exit insert mode back to normal mode (Esc / jk).
(define (vim-exit-insert-mode)
  (helix.static.collapse_selection)
  (helix.static.normal_mode)
  (define pos (cursor-position))
  (define char (rope-char-ref (get-document-as-slice) pos))
  (when char
    (when (equal? #\newline char)
      (move-char-left-same-line-impl)
      ;; BUG: for some reason, editor-count stacks, so I have to set it to 0 before continuing?
      ;; (e.g exiting insert mode from end of line -> 7k results in movement from 17k if below line isnt added)
      ;; TODO: look into Helix code to see why this happens
      (set-editor-count! 0))))

;; gg
;;@doc
;; Go to the first line of the file (gg).
(define (vim-goto-file-start)
  (helix.static.goto_file_start)
  (helix.static.collapse_selection))

;; I
;;@doc
;; Enter insert mode at the first non-whitespace character of the line (I).
(define (vim-insert-at-line-start)
  (helix.static.goto_first_nonwhitespace)
  (helix.static.collapse_selection)
  (helix.static.insert_mode))

;; J
;;@doc
;; Join the current line with the next, collapsing whitespace (J).
(define (vim-join-lines)
  (define count (editor-count))
  (set-editor-count! 1)
  (do-n-times (max 1 (- count 1)) (lambda () (helix.static.extend_line_down)))
  (helix.static.join_selections_space)
  (helix.static.collapse_selection))

;; ~
;;@doc
;; Toggle the case of the character under the cursor (~).
(define (vim-toggle-case)
  (define count (editor-count))
  (set-editor-count! 1)
  (if (> count 1)
      (begin
        (do-n-times (- count 1) (lambda () (helix.static.extend_char_right)))
        (helix.static.switch_case)
        (helix.static.flip_selections)
        (helix.static.collapse_selection))
      (begin
        (helix.static.switch_case)
        (helix.static.move_char_right)
        (helix.static.collapse_selection))))

;; zz
;;@doc
;; Scroll the view so the cursor line is centered (zz).
(define (vim-scroll-center) (helix.static.align_view_center))

;; zt
;;@doc
;; Scroll the view so the cursor line is at the top (zt).
(define (vim-scroll-top) (helix.static.align_view_top))

;; zb
;;@doc
;; Scroll the view so the cursor line is at the bottom (zb).
(define (vim-scroll-bottom) (helix.static.align_view_bottom))

;; H
;;@doc
;; Move the cursor to the top of the window (H).
(define (vim-window-top)
  (helix.static.goto_window_top)
  (helix.static.collapse_selection))

;; M
;;@doc
;; Move the cursor to the middle of the window (M).
(define (vim-window-middle)
  (helix.static.goto_window_center)
  (helix.static.collapse_selection))

;; L
;;@doc
;; Move the cursor to the bottom of the window (L).
(define (vim-window-bottom)
  (helix.static.goto_window_bottom)
  (helix.static.collapse_selection))

;; *
;;@doc
;; Search forward for the word under the cursor (*).
(define (vim-search-word-forward)
  (helix.static.search_selection_detect_word_boundaries)
  (helix.static.collapse_selection))

;; # — set word search then go to previous occurrence
;;@doc
;; Search backward for the word under the cursor (#).
(define (vim-search-word-backward)
  (helix.static.search_selection_detect_word_boundaries)
  (helix.static.search_prev)
  (helix.static.collapse_selection))

;; s
;;@doc
;; Delete the character under the cursor and enter insert mode (s).
(define (vim-substitute-char)
  (define count (editor-count))
  (set-editor-count! 1)
  (when (> count 1)
    (do-n-times (- count 1) (lambda () (helix.static.extend_char_right))))
  (helix.static.change_selection))

;; guu
;;@doc
;; Lowercase the current line (guu).
(define (vim-lowercase-line)
  (define count (editor-count))
  (set-editor-count! 1)
  (when (> count 1)
    (do-n-times (- count 1) (lambda () (helix.static.extend_line_down))))
  (helix.static.extend_to_line_bounds)
  (helix.static.switch_to_lowercase)
  (helix.static.collapse_selection))

;; gUU
;;@doc
;; Uppercase the current line (gUU).
(define (vim-uppercase-line)
  (define count (editor-count))
  (set-editor-count! 1)
  (when (> count 1)
    (do-n-times (- count 1) (lambda () (helix.static.extend_line_down))))
  (helix.static.extend_to_line_bounds)
  (helix.static.switch_to_uppercase)
  (helix.static.collapse_selection))

(provide vim-undo
         vim-append-mode
         move-char-right-same-line
         move-char-left-same-line
         move-line-up-impl
         move-line-up
         move-line-down-impl
         move-line-down
         vim-find-next-char
         vim-find-prev-char
         vim-find-till-char
         vim-till-prev-char
         vim-repeat-last-find
         vim-reverse-last-find
         vim-goto-line-or-last
         vim-next-word-start
         vim-next-word-end
         vim-prev-word-start
         vim-prev-long-word-start
         vim-next-long-word-start
         vim-next-long-word-end
         vim-goto-next-paragraph
         vim-goto-prev-paragraph
         visual-line-mode
         vim-exit-insert-mode
         vim-goto-file-start
         vim-insert-at-line-start
         vim-join-lines
         vim-toggle-case
         vim-scroll-center
         vim-scroll-top
         vim-scroll-bottom
         vim-window-top
         vim-window-middle
         vim-window-bottom
         vim-search-word-forward
         vim-search-word-backward
         vim-substitute-char
         vim-lowercase-line
         vim-uppercase-line)
