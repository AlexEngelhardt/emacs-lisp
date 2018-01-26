# Emacs Lisp

- Run `M-x menu-bar-read-lispintro` within Emacs to get to the Lisp tutorial
- Run `M-x menu-bar-read-lispref` for the entire reference.

This folder consists of my code and notes while following along the `lispintro` tutorial.

`C-x C-e` evaluates the *last* sexp (symbolic expression), i.e. you must place your cursor exactly after the expression (i.e., the last closing parenthesis, in most cases).

```lisp
(+ 2 3)
(+ 111 (+ 111 123))  ; you can eval the inner list as well
fill-column  ; variable
```

### Lists, functions, variables

*s-expressions* consist of *atoms*

- '(quoted list)
  - This is the same as (quote (quoted list)) , just shorthand
  - https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Quoting
- (function)
- (function arg1 arg2)
- variable
- symbol
- "string"
- 'symbol  ; Really??

### TODO

- Find out what "special forms" are
- Find out what macros are:
  - E.g. `when`: It's a macro that computes an `if` expression without an else clause. `unless` is the opposite: this is an `if` expression with *only* the else clause.
  - Macros enable you to define new control constructs and other language features!
