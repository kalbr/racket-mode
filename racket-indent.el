;;; racket-indent.el

;; Copyright (c) 2013-2017 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

(require 'cl-lib)
(require 'racket-custom)
(require 'racket-ppss)

;; The two top-level commands we care about are:
;;   1. `prog-indent-sexp' C-M-q
;;   2. `indent-region' C-M-\
;;
;; 1. `prog-indent-sexp' thinly wraps `indent-region'.
;;
;; 2. `indent-region' calls `indent-according-to-mode', which in turn
;; calls the mode-specific `indent-line-function'. In lisp-mode that's
;; `lisp-indent-line', which in turn calls `calculate-lisp-indent'.
;; That in turn calls the mode-specific `indent-function'; in
;; lisp-mode that's `lisp-indent-function'.
;;
;; However `calculate-lisp-indent' is complicated and doesn't always
;; behave the way we want. So we use a simplified version of that
;; (`racket--calculate-indent') in our `indent-line-function',
;; `racket-indent-line'.

(defun racket-indent-line (&optional whole-exp)
  "Indent current line as Racket code.

This behaves like `lisp-indent-line', except that whole-line
comments are treated the same regardless of whether they start
with single or double semicolons.

- Automatically indents forms that start with `begin` in the usual
  way that `begin` is indented.

- Automatically indents forms that start with `def` or `with-` in the
  usual way that `define` is indented.

- Has rules for many specific standard Racket forms.

To extend, use your Emacs init file to

    (put SYMBOL 'racket-indent-function INDENT)

where `SYMBOL` is the name of the Racket form (e.g. `'test-case`)
and `INDENT` is an integer or the symbol `'defun`. When `INDENT`
is an integer, the meaning is the same as for
`lisp-indent-function` and `scheme-indent-function`: Indent the
first `n` arguments specially and then indent any further
arguments like a body.

For example in your `.emacs` file you could use:

    (put 'test-case 'racket-indent-function 1)

to change the indent of `test-case` from this:

    (test-case foo
               blah
               blah)

to this:

    (test-case foo
      blah
      blah)

If `racket-indent-function` has no property for a symbol,
`scheme-indent-function` is also considered (although the with-x
indents defined by `scheme-mode` are ignored). This is only to
help people who may have extensive `scheme-indent-function`
settings, particularly in the form of file or dir local
variables. Otherwise prefer `racket-indent-function`."
  (interactive)
  (let ((indent (racket--calculate-indent))
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (when (listp indent)
        (setq indent (car indent)))
      (unless (zerop (- indent (current-column)))
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (when (< (point) (- (point-max) pos))
        (goto-char (- (point-max) pos))))))

(defun racket--calculate-indent ()
  "Simplified/modified version of `calculate-lisp-indent'.

Original doc string:

Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point    (point))
          (state           nil)
          (desired-indent  nil)
          (found-inner     nil)
          (last-sexp       nil)
          (containing-sexp nil))
      (let ((beginning-of-defun-function nil)) ;plain b-o-d
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and (not found-inner)
                  state
                  (< 0 (racket--ppss-paren-depth state)))
        (setq found-inner t)
        (setq last-sexp (racket--ppss-last-sexp state))
        (setq containing-sexp (racket--ppss-containing-sexp state))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (when (and last-sexp (< (point) last-sexp))
          ;; Yes, but is there a containing sexp after that?
          (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
            (when (racket--ppss-containing-sexp peek)
              (setq found-inner nil)
              (setq state peek)))))

      (when found-inner
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call racket-indent-function.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) last-sexp 0 t)
          ;; Note: The original lisp-mode code had a condition below
          ;; for (looking-at "\\s(") -- but that's wrong for Racket,
          ;; e.g. see issue #243.
          (cond ((< last-sexp
                    (save-excursion (forward-line 1) (point)))
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (= (point) last-sexp)
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point) last-sexp 0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) last-sexp 0 t)
                 (backward-prefix-chars)))))

      ;; Point is where to indent under, unless we are inside a
      ;; string. Call `racket-indent-function' unless desired
      ;; indentation has already been computed.
      (cond ((racket--ppss-string-p state)
             nil)
            ((and found-inner last-sexp)
             (racket-indent-function indent-point state))
            (desired-indent
             desired-indent)
            (t
             (current-column))))))

(defun racket-indent-function (indent-point state)
  "The function `racket--calculate-indent' calls this.

There is special handling for:
  - forms that begin with a #:keyword (as found in contracts)
  - forms like #hasheq()
  - data sequences when `racket-indent-sequence-depth' is > 0
  - {} forms when `racket-indent-curly-as-sequence' is not nil

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp form that has a non-nil
property `racket-indent-function' it specifies how to indent.  The property
value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the form
  has a name that begins with \"def\" or \"with-\");

* an integer N, meaning indent the first N arguments specially
  \(like ordinary function arguments), and then indent any
  further arguments like a body \(a value of 0 is used if there
  is no property and the form has a name that begins with
  \"begin\");

* a function to call that returns the indentation.

This function always returns either the indentation to use (never returns nil)."
  (goto-char (racket--ppss-containing-sexp state))
  (let ((body-indent (+ (current-column) lisp-body-indent)))
    (forward-char 1)
    (if (or (racket--hash-literal-or-keyword-p)
            (racket--data-sequence-p))
        (progn (backward-prefix-chars) (current-column))
      (let* ((head   (buffer-substring (point) (progn (forward-sexp 1) (point))))
             (method (racket--get-indent-function-method head)))
        (cond ((integerp method)
               (racket--indent-special-form method indent-point state))
              ((eq method 'defun)
               body-indent)
              (method
               (funcall method indent-point state))
              ((string-match (rx bos (or "def" "with-")) head)
               body-indent) ;just like 'defun
              ((string-match (rx bos "begin") head)
               (racket--indent-special-form 0 indent-point state))
              (t
               (racket--normal-indent indent-point state)))))))

(defun racket--hash-literal-or-keyword-p ()
  "Looking at things like #hash(___) or (#:keyword ___) ?
Latter occurs e.g. in Racket contract forms.
Returns nil for #% identifiers like #%app."
  (looking-at (rx ?\# (or ?\:
                          (not (any ?\%))))))

(defun racket--data-sequence-p ()
  "Looking at certain sequences where we align all with head item?

These include '() `() #() -- and {} if `racket-indent-curly-as-sequence'
is t -- but not #'() #`() ,() ,@().

To handle nested items, search `backward-up-list' up to
`racket-indent-sequence-depth' times."
  (and (< 0 racket-indent-sequence-depth)
       (save-excursion
         (ignore-errors
           (let ((answer 'unknown)
                 (depth racket-indent-sequence-depth))
             (while (and (eq answer 'unknown)
                         (< 0 depth))
               (backward-up-list)
               (setq depth (1- depth))
               (cond ((or
                       ;; a quoted '( ) or quasiquoted `( ) list --
                       ;; but NOT syntax #'( ) or quasisyntax #`( )
                       (and (memq (char-before (point)) '(?\' ?\`))
                            (eq (char-after (point)) ?\()
                            (not (eq (char-before (1- (point))) ?#)))
                       ;; a vector literal: #( )
                       (and (eq (char-before (point)) ?#)
                            (eq (char-after  (point)) ?\())
                       ;; { }
                       (and racket-indent-curly-as-sequence
                            (eq (char-after (point)) ?{)))
                      (setq answer t))
                     (;; unquote or unquote-splicing
                      (and (or (eq (char-before (point)) ?,)
                               (and (eq (char-before (1- (point))) ?,)
                                    (eq (char-before (point))      ?@)))
                           (eq (char-after (point)) ?\())
                      (setq answer nil))))
             (eq answer t))))))

(defun racket--normal-indent (indent-point state)
  ;; Credit: Substantially borrowed from clojure-mode
  (goto-char (racket--ppss-last-sexp state))
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match "[^[:blank:]]"
                               (buffer-substring (line-beginning-position)
                                                 (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point
      ;; is now at the function name), so the behaviour depends on
      ;; whether there's also an argument on this line.
      (when (and last-sexp-start
                 (< last-sexp-start (line-end-position)))
        ;; There's an arg after the function name, so align with it.
        (goto-char last-sexp-start))
      (current-column))))

(defun racket--indent-special-form (method indent-point state)
  ;; Credit: Substantially borrowed from clojure-mode
  (let ((containing-column (save-excursion
                             (goto-char (racket--ppss-containing-sexp state))
                             (current-column)))
        (pos -1))
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (forward-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the current sexp,
      ;; we detect that by catching the `scan-error'. In that case, we
      ;; should return the indentation as if there were an extra sexp
      ;; at point.
      (scan-error (cl-incf pos)))
    (cond ((= method pos)               ;first non-distinguished arg
           (+ containing-column lisp-body-indent))
          ((< method pos)               ;more non-distinguished args
           (racket--normal-indent indent-point state))
          (t                            ;distinguished args
           (+ containing-column (* 2 lisp-body-indent))))))

(defun racket--conditional-indent (indent-point state looking-at-regexp true false)
  (skip-chars-forward " \t")
  (let ((n (if (looking-at looking-at-regexp) true false)))
    (racket--indent-special-form n indent-point state)))

(defun racket--indent-maybe-named-let (indent-point state)
  ;; check for named let
  (racket--conditional-indent indent-point state
                              "[-a-zA-Z0-9+*/?!@$%^&_:~]" 2 1))

(defun racket--indent-for (indent-point state)
  "Indent function for all for/ and for*/ forms EXCEPT
for/fold and for*/fold."
  ;; check for either of:
  ;; - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  ;; - for/vector optional length, (for/vector #:length ([x xs]) x)
  (racket--conditional-indent indent-point state
                              "[:#]" 3 1))

(defun racket--indent-for/fold (indent-point state)
  "Indent function for for/fold and for*/fold."
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t\n")
  (if (looking-at ":")
      (racket--indent-special-form 4 indent-point state)
    (racket--indent-for/fold-untyped indent-point state)))

(defun racket--indent-for/fold-untyped (indent-point state)
  (let* ((containing-sexp-start  (racket--ppss-containing-sexp state))
         (_                      (goto-char containing-sexp-start))
         (containing-sexp-column (current-column))
         (containing-sexp-line   (line-number-at-pos))
         (body-indent            (+ containing-sexp-column lisp-body-indent))
         (clause-indent          nil))
    ;; Move to the open paren of the first, accumulator sexp
    (forward-char 1)    ;past the open paren
    (forward-sexp 2)    ;to the next sexp, past its close paren
    (backward-sexp 1)   ;back to its open paren
    ;; If the first, accumulator sexp is not on the same line as
    ;; `for/fold`, then this is simply specform 2.
    (if (/= (line-number-at-pos) containing-sexp-line) ;expensive?
        (racket--indent-special-form 2 indent-point state)
      (setq clause-indent (current-column))
      (forward-sexp 1)    ;past close paren
      ;; Now go back to the beginning of the line holding
      ;; the indentation point. Count the sexps on the way.
      (parse-partial-sexp (point) indent-point 1 t)
      (let ((n 1))
        (while (and (< (point) indent-point)
                    (ignore-errors
                      (cl-incf n)
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))))
        (list (if (= 1 n) clause-indent body-indent)
              containing-sexp-start)))))

(defun racket--get-indent-function-method (head)
  "Get property of racket- or scheme-indent-function.

Ignores certain with-xxx indents defined by scheme-mode --
because we automatically indent with- forms just like def forms.
However if a _user_ has defined their own legacy scheme-mode
indents for _other_ with- forms, those _will_ be used. We only
ignore a short list defined by scheme-mode itself."
  (let ((sym (intern-soft head)))
    (or (get sym 'racket-indent-function)
        (and (not (memq sym '(with-mode
                              with-input-from-file
                              with-input-from-port
                              with-output-to-file
                              with-output-to-port
                              with-input-from-string
                              with-output-to-string
                              with-values)))
             (get sym 'scheme-indent-function)))))

(defun racket--set-indentation ()
  "Set indentation for various Racket forms.

Note that `beg*`, `def*` and `with-*` aren't listed here because
`racket-indent-function' handles those.

Note that indentation is set for the symbol alone, and also with
a : suffix for legacy Typed Racket. For example both `let` and
`let:`. Although this is overzealous in the sense that Typed
Racket does not define its own variant of all of these, it
doesn't hurt to do so."
  (mapc (lambda (x)
          (put (car x) 'racket-indent-function (cadr x))
          (let ((typed (intern (format "%s:" (car x)))))
            (put typed 'racket-indent-function (cadr x))))
        '(;; begin* forms default to 0 unless otherwise specified here
          (begin0 1)
          (c-declare 0)
          (c-lambda 2)
          (call-with-input-file defun)
          (call-with-input-file* defun)
          (call-with-output-file defun)
          (call-with-output-file* defun)
          (case 1)
          (case-lambda 0)
          (catch 1)
          (class defun)
          (class* defun)
          (compound-unit/sig 0)
          (cond 0)
          ;; def* forms default to 'defun unless otherwise specified here
          (delay 0)
          (do 2)
          (dynamic-wind 0)
          (fn 1) ;alias for lambda (although not officially in Racket)
          (for 1)
          (for/list racket--indent-for)
          (for/vector racket--indent-for)
          (for/hash racket--indent-for)
          (for/hasheq racket--indent-for)
          (for/hasheqv racket--indent-for)
          (for/and racket--indent-for)
          (for/or racket--indent-for)
          (for/lists racket--indent-for/fold)
          (for/first racket--indent-for)
          (for/last racket--indent-for)
          (for/fold racket--indent-for/fold)
          (for/flvector racket--indent-for)
          (for/set racket--indent-for)
          (for/seteq racket--indent-for)
          (for/seteqv racket--indent-for)
          (for/sum racket--indent-for)
          (for/product racket--indent-for)
          (for* 1)
          (for*/list racket--indent-for)
          (for*/vector racket--indent-for)
          (for*/hash racket--indent-for)
          (for*/hasheq racket--indent-for)
          (for*/hasheqv racket--indent-for)
          (for*/and racket--indent-for)
          (for*/or racket--indent-for)
          (for*/lists racket--indent-for/fold)
          (for*/first racket--indent-for)
          (for*/last racket--indent-for)
          (for*/fold racket--indent-for/fold)
          (for*/flvector racket--indent-for)
          (for*/set racket--indent-for)
          (for*/seteq racket--indent-for)
          (for*/seteqv racket--indent-for)
          (for*/sum racket--indent-for)
          (for*/product racket--indent-for)
          (instantiate 2)
          (interface 1)
          (λ 1)
          (lambda 1)
          (lambda/kw 1)
          (let racket--indent-maybe-named-let)
          (let* 1)
          (letrec 1)
          (letrec-values 1)
          (let-values 1)
          (let*-values 1)
          (let+ 1)
          (let-syntax 1)
          (let-syntaxes 1)
          (letrec-syntax 1)
          (letrec-syntaxes 1)
          (letrec-syntaxes+values racket--indent-for/fold-untyped)
          (local 1)
          (let/cc 1)
          (let/ec 1)
          (match 1)
          (match* 1)
          (match-define defun)
          (match-lambda 0)
          (match-lambda* 0)
          (match-let 1)
          (match-let* 1)
          (match-let*-values 1)
          (match-let-values 1)
          (match-letrec 1)
          (match-letrec-values 1)
          (match/values 1)
          (mixin 2)
          (module 2)
          (module+ 1)
          (module* 2)
          (opt-lambda 1)
          (parameterize 1)
          (parameterize-break 1)
          (parameterize* 1)
          (quasisyntax/loc 1)
          (receive 2)
          (require/typed 1)
          (require/typed/provide 1)
          (send* 1)
          (shared 1)
          (sigaction 1)
          (splicing-let 1)
          (splicing-letrec 1)
          (splicing-let-values 1)
          (splicing-letrec-values 1)
          (splicing-let-syntax 1)
          (splicing-letrec-syntax 1)
          (splicing-let-syntaxes 1)
          (splicing-letrec-syntaxes 1)
          (splicing-letrec-syntaxes+values racket--indent-for/fold-untyped)
          (splicing-local 1)
          (splicing-syntax-parameterize 1)
          (struct defun)
          (syntax-case 2)
          (syntax-case* 3)
          (syntax-rules 1)
          (syntax-id-rules 1)
          (syntax-parse 1)
          (syntax-parser 0)
          (syntax-parameterize 1)
          (syntax/loc 1)
          (syntax-parse 1)
          (test-begin 0)
          (test-case 1)
          (unit defun)
          (unit/sig 2)
          (unless 1)
          (when 1)
          (while 1)
          ;; with- forms default to 1 unless otherwise specified here
          )))

(provide 'racket-indent)

;; racket-indent.el ends here
