(require 'subr-x)
(defun phpdoc-function ()
  "print the phpdoc for function"
  (interactive)
  ;; expected the cursor on method name
  ;; find keyword "function"
  (end-of-line)
  (search-backward " function")

  ;; get method name ;;;;;;;;;;;;;;;;;;;;;;;;
  (right-word)
  (search-forward " ")
  ;; cursor position: " function |func(...)"
  ;; set function name start point
  (setq method-name-start (point))
  ;; move cursor to word end
  ;; " function func|(...)"
  (right-word)
  (setq method-name-end (point))
  (setq method-name (string-trim (buffer-substring-no-properties method-name-start method-name-end)))

  ;; get params ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (search-forward "(")
  (setq params-start (point))
  (search-forward ")")
  (left-char)
  (setq params-end (point))
  (setq params (buffer-substring-no-properties params-start params-end))

  (replace-regexp-in-string "$+" "" params)
  (setq params (split-string (replace-regexp-in-string ")" "" params) ","))

  ;; move cursor position to head of method
  (search-backward " function")

  (phpdoc-block-position)
  (setq inicio (point))
  (setq init-block-point (point))
  (phpdoc-insert-doc-start)     ; insert /**
  ;; (phpdoc-insert-new-line method-name) ; insert  * method-name
  ;; (phpdoc-insert-new-line)             ; insert  *
  (phpdoc-insert-params params) ; insert  * @param param
  (phpdoc-insert-new-line "@return ")  ; insert  * @return
  (phpdoc-insert-doc-end)       ; insert  */
  (indent-region inicio (point))
  (goto-char init-block-point)
  (message "Insert ther phpdoc for methood")
)


(defun php-generate-getter ()
  "create the setter for a variable"
  (interactive)
  (end-of-line)
  ;; move cursor to start of the var name
  (search-backward "$")
  (right-char)
  ;; save cursor point
  (setq var-name-start (point))
  ;; move cursor ot end of the var name
  (search-forward ";")
  (left-char)
  ;; save curosor point
  (setq var-name-end (point))
  (setq var-name (string-trim (setq params (buffer-substring-no-properties var-name-start var-name-end))))
  (end-of-line)
  (newline)
  (php-insert-getter var-name)
  )

(defun php-insert-getter (var-name)
  (newline-and-indent)
  (insert (concat (concat "public function get" (upcase-initials var-name)) "()"))
  (newline)

  (insert "{")
  (indent-for-tab-command)
  (newline)

  (insert (concat (concat "return $this->" var-name) ";"))
  (indent-for-tab-command)
  (newline)

  (insert "}")
  (indent-for-tab-command)
  (newline)
)


(defun phpdoc-block-position ()
  (previous-line)
  (beginning-of-line)
  (newline)
)

(defun phpdoc-insert-new-line (&optional phpdoc-str)
  (insert (concat "* " phpdoc-str))
  (newline)
)

(defun phpdoc-insert-doc-end ()
  (insert "*/")
)

(defun phpdoc-insert-doc-start ()
  (insert "/**")
  (newline)
)


(defun phpdoc-insert-params (param-list)
  (if (> (length param-list) 0)
  (while param-list
    (phpdoc-insert-new-line (concat "@param " (string-trim (car param-list))))
    (setq param-list (cdr param-list))
   )
  )
)
