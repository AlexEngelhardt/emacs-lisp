;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file corresponds to sectons 02, 03, and 04 in the Lisp intro:
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html


(defun multiply-by-seven (number)
  "Multiply the argument 'number' by seven."
  (* 7 number))

;; Evaluating the definition "installs" the function
;; It returns the name of the function.

(multiply-by-seven 11)

;; Do C-h f "multiply-by-seven" to see the docstring

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

;; They are often called for their side effects (e.g. moving the cursor)
;; instead of their return value. They therefore don't print the return
;; value in the echo area.

(defun multiply-by-seven (number)       ; Interactive version.
  "Multiply NUMBER by seven."
  (interactive "p")
  ;; The "p" in the interactive call tells Emacs to pass a prefix argument
  ;; as a parameter to the function 
  (message "The result is %d" (* 7 number)))

(multiply-by-seven 123)

;; C-u 3 mu-s [RET]

(global-set-key "f" 'multiply-by-seven)

;; Now you can't type the letter f anymore :D
;; But `C-u 3 f` returns "The result is 21" in the echo area

(global-set-key "f" 'self-insert-command)  ; this lets you use 'f' again :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive shenanigans

;; Consider the function zap-to-char. Its interactive expression is
;;  (interactive "p\ncZap to char: ")
;; The \n separates the two arguments. 'p' and 'c' are important here.
;; See here for the doc:
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Interactive-Codes

;; When a function does not take arguments, interactive does not require any. 

;;;;


;; OMG look at this interactive statement.
;;  [ ] Why is the list created with a 'list' command instead of pure parentheses?
;;  [ ] How does that list magic thing work?
;; https://www.gnu.org/software/emacs/manual/html_mono/eintr.html#append-interactive

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
	 (region-beginning) (region-end)))
  (let* ((oldbuf (current-buffer))
         (append-to (get-buffer-create buffer))
         (windows (get-buffer-window-list append-to t t))
         point)
    (save-excursion
      (with-current-buffer append-to  ; This line is new; it's different in the tutorial
        (setq point (point))          ;  but it makes sense. Compare to `set-buffer`
        (barf-if-buffer-read-only)
        (insert-buffer-substring oldbuf start end)  ; this fct inserts in current and needs "source" buffer as argument.
        (dolist (window windows)
          (when (= (window-point window) point)
            (set-window-point window (point))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let - Local definitions

(setq capital "Berlin")
(setq name "Germany")
(message "%s is the capital of %s" capital name)

;; let introduces a local namespace, much like `with()` in R
(let ((capital "Munich")
      (name "Bavaria"))
  (message "%s is the capital of %s" capital name)
  )

(message "%s is the capital of %s" capital name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if-else

;; nil is false -- and the empty list: ()
;; anything else is true

t
nil

(> 5 4)
(< 5 4)

(if (> 5 4)
    (message "5 is greater than 4!")
  )

(if (> 5 4)
    "TRUE DAT"
  )
(if (> 5 4)
    'TRUE-DAT
  )

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the string \"fierce\",
     then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!")))  ; else nil

(type-of-animal "fierce")
(type-of-animal "striped")

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the string \"fierce\",
     then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!") 
    (message "All is well")
   ))

(type-of-animal "striped")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save-excursion

;; Similar to `progn`. But this one saves the cursor position ("point")

(defun doot ()
  (insert "DOOT")
  (message "I dooted!")
  )

(doot)DOOT  ; watch what happens to the cursor

(defun doot2 ()
  (save-excursion
    (insert "DOOT")
    (message "I dooted with save-excursion!")
    )
  )

(doot2)  ; the cursor stays where it is!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise

(defun dbl (num)
  "doubles its argument"
  (* num 2)
  )

(dbl 33)

(defun dbl-int (num)
  "doubles its argument"
  (interactive "nGief num: ")
  (setq result (* num 2))  ; result is alive after the function call. Use a 'let' block if you don't want this.
  (message "OMG IT'S %s" result)
  )

(dbl-int 33)  ; Still works
;; Try M-x dbl-int now.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional arguments

;; keyword @optional.
;; arguments are not named, so the order is important

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.6 Exercises

(defun simplified-end-of-buffer ()
  "sets point to end of buffer."
  (interactive)
  (goto-char (point-max))
  )

(global-set-key (kbd "C-z") 'simplified-end-of-buffer)

;; Now C-z goes to the end of your buffer!

;;;;
;; See if a buffer exists

(defun does-buffer-exist (buffer-name)
  "Checks if buffer exists and if so, messages it."
  (interactive "BBuffer name: ")
  (message "hello")
  (if (get-buffer buffer-name)
      (message "buffer %s exists!" buffer-name)
    (message "does not exist!")
      )
  )

;; M-x does-buffer-exist works too!
(does-buffer-exist "derp.el")
(does-buffer-exist "inbox.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 05 - A Few More Complex Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; https://www.gnu.org/software/emacs/manual/html_node/eintr/More-Complex.html


;; Review append-to-buffer:
(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
         (region-beginning) (region-end)))
  (let* ((oldbuf (current-buffer))
         (append-to (get-buffer-create buffer))
         (windows (get-buffer-window-list append-to t t))
         point)
    (save-excursion
      (with-current-buffer append-to
        (setq point (point))
        (barf-if-buffer-read-only)
        (insert-buffer-substring oldbuf start end)
        (dolist (window windows)
          (when (= (window-point window) point)
            (set-window-point window (point))))))))

;;;;
;; Now look at copy-to-buffer. It *overwrites* the second buffer:

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring oldbuf start end)))))


;;;;
;; This is an *old* version of insert-buffer, but it's used in the tutorial:

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
     Puts mark after the inserted text.
     BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer)  ; make sure 'buffer' is bound to an actual *buffer*, not just its name
      (setq buffer (get-buffer buffer)))  ; the 'or' is short for an 'if' clause here
  (let (start end newmark)  ; bind all to nil. Notice absence of parens around them
    (save-excursion
      (save-excursion
        (set-buffer buffer)  ; this guy needs an actual buffer, not its name
        (setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional arguments

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer.
With numeric arg N, put point N/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.

Push mark at previous position, unless either a \\[universal-argument] prefix
is supplied, or Transient Mark mode is enabled and the mark is active."
  (declare (interactive-only "use `(goto-char (point-min))' instead."))
  (interactive "^P")
  (or (consp arg)
      (region-active-p)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))  ; overflow-safe but not exactly precise
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))  ; precise,
		                 ; but a 10k buffer produces integers of say 70k here, ZOMG
		 (point-min))))
  (if (and arg (not (consp arg))) (forward-line 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.5 "optional" Argument Exercise

(defun booplesnoot (&optional xxx)
  "Write an interactive function with an optional argument that tests whether its argument, a number, is greater than or equal to, or else, less than the value of fill-column, and tells you which, in a message. However, if you do not pass an argument to the function, use 56 as a default value."
  (interactive "p")

  ;; The following won't work if called interactively. Then, the prefix argument is
  ;;  1 by default.
  (or xxx (setq xxx 56))  ; https://emacs.stackexchange.com/questions/14199/optional-parameter-defaults
  
  (message "boop %s" xxx)
  )

; M-x booplesnoot sets it to a prefix argument of 1, though.
(booplesnoot)
(booplesnoot 1)
(booplesnoot 56)
(booplesnoot -4)

