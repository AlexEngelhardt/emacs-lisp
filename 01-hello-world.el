(defun hello ()
  (insert "Hello thar!")
  (message (concat "What's " (number-to-string (+ fill-column 20)) " up?!"))
  ; how to return void so that the message doesn't appear twice in *Messages* ?
  )

(hello)

; Use `M-x load-file`


