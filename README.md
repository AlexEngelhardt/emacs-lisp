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

- '(quoted list)
- (function)
- (function arg1 arg2)
- variable
