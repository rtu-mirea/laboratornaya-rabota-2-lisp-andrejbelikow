(defun InsertAt (position lst value)
(if (= position 0) (cons value lst)
(cons (car lst) (InsertAt (1- position) (cdr lst) value))))

(defun NewDelete (n lst)
(cond ((null lst) nil)
((zerop n) (cdr lst))
(t (cons (car lst) (NewDelete (1- n) (cdr lst))))))

(defun Search (needle haystack)
(loop
for element in haystack
and position from 0
when (eql element needle)
collect position))

(defun OutputFile (path)
(let ((in (open path :if-does-not-exist nil)))
(when in
(loop for line = (read-line in nil)
while line do (format t "~a~%" line))
(close in))))

( (defun DeepDecompress (lst)
(if (zerop (car lst)) nil
(cons (second lst) (DeepDecompress (list (1- (car lst)) (second lst))))))

(defun Decompress (lst)
(cond ((null lst) nil)
((atom (car lst)) (cons (car lst) (Decompress (cdr lst))))
((consp (car lst)) (append (DeepDecompress (car lst)) (Decompress (cdr lst)))
)))

(defun CompareList (val acc)
(if (> acc 1) (list acc val) val))

(defun Accum (val acc lst)
(cond ((null lst) (cons (CompareList val acc) nil))
((eq val (car lst)) (Accum val (1+ acc) (cdr lst)))
(t (cons (CompareList val acc) (Accum (car lst) 1 (cdr lst))))))

(defun Compress (lst)
(cond ((null (cdr lst)) '())
(t (Accum (car lst) 1 (cdr lst)))))
