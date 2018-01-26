;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy-paste (kill-yank in emacs lingo)

;; 8 - Cutting and Storing Text

;; The *kill ring* is a list from which you can retrieve with e.g. (car (nthcdr 1 mylist))

(defun zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
			 (search-forward (char-to-string char)
					 nil nil arg)
			 (point))))

;; progn evaluates all arguments in order and returns just the last one.
;; The expressions before are evaluated just for their *side effects*
;; Here, the progn expression is a *single argument* to kill-region

;;;;
;; Lisp macros
;; 'when' and 'unless' are macros that are expanded into if clauses:

(macroexpand '(when t (message "doot")))
(macroexpand '(unless (eq 3 (+ 1 2)) (error "METROPOLIS - THE WORLDS COLLIDE!")))


;;;;
;; The kill ring, appending to it, and creating new kills:

this-command
last-command  ; <- eval this one! Emacs stores the last executed command

(kill-new "Galileo Figaro!") ; Eval this and then C-y!
;; kill-new is not interactive

kill-word

(concat "new "
	(car '("first element" "second element")))

;;;;
;; The kill-new function (an excerpt):

(if (and replace kill-ring)
    (setcar kill-ring string)  ; setcar: replace first element
  (push string kill-ring)  ; similar to (setq kill-ring (cons string kill-ring))
                           ;  or (add-to-list 'kill-ring string)
  (if (> (length kill-ring) kill-ring-max)  ; still part of 'else' clause
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
(setq kill-ring-yank-pointer kill-ring)

;;

(setq doot '("one" "two" "three"))
doot
(push "zero" doot)
doot
(setq doot (cons "-one" doot))
doot
(add-to-list 'doot "-two")  ; note the quote
doot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 - Yanking

kill-ring-yank-pointer

kill-ring
