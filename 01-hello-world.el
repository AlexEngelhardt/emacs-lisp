(defun hello ()
  (insert "Hello thar!")
  (message (concat "What's " (number-to-string (+ fill-column 20)) " up?!"))
  ; how to return void so that the message doesn't appear twice in *Messages* ?
  )

(hello)

; Use `M-x load-file`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(a-function)
a-variable
"a-string"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "boop")

; Keep the *Messages* buffer open and check it

(message "this buffer is called %s" (buffer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set 'myname "Alex")
(message myname)
(setq myname "Alex")
myname

(set 'numbers '(one two three fo))
numbers

(setq numbers '(na fa fo na fo fa fa)
      letters '(A B C D E F G)
      )
numbers
letters

;; counting

(setq counter 0)  ; a function always returns a value
(setq counter (+ counter 1))
counter
; :D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move counter to hafway within the file, i.e. point-max / 2:

(goto-char (/ (point-max) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc shenanigans

(equal 3 3)  ; t

(equal '(1 2) '(1 2))  ; t

(equal (list 1 2) '(1 2))  ; t

(list 1 4)  ; (1 4)

'(1 8)  ; (1 8)

(equal (list 1 'doot) (list 1 "doot"))  ; nil
