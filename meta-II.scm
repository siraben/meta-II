(include "parser.scm")
(use-modules (ice-9 match)
             (rnrs bytevectors))

;; Output stuff to the instruction field.  This is wrapped monadically
;; so it can be integrated with the doM* macro.
(define (out . args)
  (lambda (state)
    (format (current-output-port) "\t")
    (apply format (current-output-port) args)
    (newline)
    (cons args state)))

;; Output stuff to the label field.  This is wrapped monadically so it
;; can be integrated with the doM* macro.
(define (label . args)
  (lambda (state)
    (apply format (current-output-port) args)
    (newline)
    (cons args state)))

;; Read a space-delimited word (we're following META II convention
;; here so alpha-numeric characters only.
(define word (apply-p (many1 alpha-num)))

;; Parse a string.  Again, following META II convention, strings are
;; delimited by single quotes.
(define parse-string
  (doM* (char #\')
        (x <- (many (noneof "'")))
        (char #\')
        (return x)))

(define *gensym-count* 1)

(define (gensym)
  (let ((res (string->symbol (format #f "L~a" *gensym-count*))))
    (set! *gensym-count* (1+ *gensym-count*))
    res))

(define (reset-gensym-counter!)
  (set! *gensym-count* 1))


(define mstring (token parse-string))

(define id (token word))

;; Here's a fun challenge: is it possible to automatically generate
;; this code through a META II program that can convert META II
;; grammars into Scheme code?  If so, what does such a program look
;; like?  If not, why not?
(define out1
  (<:> (doM* (symb "*1")
             (out "GN1"))
       (doM* (symb "*2")
             (out "GN2"))
       (doM* (symb "*")
             (out "CI"))
       (doM* (x <- mstring)
             (out "CL '~a'" x))))

(define output
  (doM* (<:> (doM* (symb ".OUT")
                   (symb "(")
                   (many out1)
                   (symb ")"))
             (doM* (symb ".LABEL")
                   (out "LB")
                   out1))
        (out "OUT")))

(define ex3
  (<:> (doM* (x <- id)
             (out "CLL ~a" x))
       (doM* (x <- mstring)
             (out "TST '~a'" x))
       (doM* (symb ".ID")
             (out "ID"))
       (doM* (symb ".NUMBER")
             (out "NUM"))
       (doM* (symb ".STRING")
             (out "SR"))
       (doM* (symb "(")
             ex1
             (symb ")"))
       (doM* (symb ".EMPTY")
             (out "SET"))
       (doM* (symb "$")
             (*1 <- m-gensym)
             (label "~a" *1)
             ex3
             (out "BT ~a" *1)
             (out "SET"))))

(define ex2
  (doM* (*1 <- m-gensym)
        (<:> (doM* ex3
                   (out "BF ~a" *1))
             output)
        (many (<:> (doM* ex3
                         (out "BE"))
                   output))
        (label "~a" *1)))

(define ex1
  (doM* ex2
        (*1 <- m-gensym)
        (many (doM* (symb "/")
                    (out "BT ~a" *1)
                    ex2))
        (label "~a" *1)))

(define st
  (doM* (* <- id)
        (label "~a" *)
        (symb "=")
        ex1
        (symb ".,")
        (out "R")))

(define program
  (doM* (symb ".SYNTAX")
        (* <- id)
        (out "ADR ~a" *)
        (many st)
        (symb ".END")
        (out "END")))

(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))

(define (parse-meta-II-file filename)
  (reset-gensym-counter!)
  (let ((msg (read-file-string filename)))
    (program msg)))

(define (meta-II-to-masm metaII-file out-file)
  (reset-gensym-counter!)
  (let ((msg (read-file-string metaII-file))
        (old-port (current-output-port)))
    (call-with-output-file out-file
      (lambda (port)
        (set-current-output-port port)
        (program msg)))
    (set-current-output-port old-port)))
