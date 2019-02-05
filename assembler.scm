;; Assemble meta-II output to our bytecode format.
(include "parser.scm")
(include "meta-II.scm")

(use-modules (rnrs io ports))

;; set! this to #t to see debugging information.  Note that `lookup`
;; will complain a lot but generally it's fine.
(define verbose? #f)

(define 16-bit-regs
  '((af . #b11)
    (bc . #b00)
    (de . #b01)
    (hl . #b10)
    (sp . #b11)))

(define (lookup key alist)
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
        (begin
          ;; Verbose
          (if verbose? (format #t "Failed to lookup: ~a\n" key))
          #f))))

(define-record-type <inst>
  (make-inst-rec length generator)
  inst?
  (length inst-length)
  (generator inst-generator))

(define-syntax make-inst
  (syntax-rules ()
    ((_ length generator)
     (make-inst-rec length (delay generator)))))

(define (gen-inst inst)
  (force (inst-generator inst)))

(define (unsigned-nat? x)
  (and (integer? x) (>= x 0)))

(define (8-bit-imm? x)
  (and (unsigned-nat? x)
       (> (ash 1 8) x)))

(define label? symbol?)
(define (16-bit-imm-or-label? x)
  (and (unsigned-nat? x)
       (> (ash 1 16) x)))

(define (16-bit-imm? x)
  (and (unsigned-nat? x)
       (> (ash 1 16) x)))

(define (num->binary n)
  (format #f "~8,'0b" n))

(define (num->hex n)
  (format #f "~2,'0x" n))

;; Least significant byte.
(define (lsb n)
  (logand n 255))

;; Most significant byte.
(define (msb n)
  (ash n -8))

(define (resolve-label label-or-imm16)
  (if (16-bit-imm? label-or-imm16)
      label-or-imm16
      (or (lookup label-or-imm16 *labels*)
          (error (format #f "Label not found: ~a" label-or-imm16)))))

(define (simple-op? op) (lookup op simple-ops))

;; Operations that don't receive arguments or have specific ones.
(define simple-ops
  '((ID    #x1)
    (NUM   #x2)
    (SR    #x3)
    (R     #x5)
    (SET   #x6)
    (BE    #xA)
    (CI    #xC)
    (GN1   #xD)
    (GN2   #xE)
    (LB    #xF)
    (OUT   #x10)
    (END   #x11)
    ))

(define (assemble-simple a)
  (let ((res (lookup a simple-ops)))
    (if res
        (make-inst (length res) res)
        (error (format #f "Operation not found: ~a" a)))))

(define (add-label! name val)
  (if (assv name *labels*)
      (error (format #f "Cannot add another label of ~a" name))
      (begin
        (if verbose?
            (format #t "Adding label ~a with value 0x~4,'0x\n" name val))
        (set! *labels* `((,name . ,val) . ,*labels*)))))

(define (advance-pc! count)
  (set! *pc* (+ *pc* count)))

(define (assemble-label name)
  (add-label! name *pc*)
  '())

(define (assemble-org new-pc)
  (set! *pc* new-pc)
  '())

(define (string->bytes s)
  `(,@(map char->integer (string->list s)) 0))

(define (assemble-test str)
  (make-inst (+ 2 (string-length str))
             `(0 ,@(string->bytes str))))

(define (assemble-copy str)
  (make-inst (+ 2 (string-length str))
             `(#xb ,@(string->bytes str))))


(define (assemble-call label)
  (make-inst 3
             (let ((imm16 (resolve-label label)))
               `(4
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-branch label)
  (make-inst 3
             (let ((imm16 (resolve-label label)))
               `(#x7
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-branch-true label)
  (make-inst 3
             (let ((imm16 (resolve-label label)))
               `(#x8
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-branch-false label)
  (make-inst 3
             (let ((imm16 (resolve-label label)))
               `(#x9
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-expr expr)
  ;; Pattern match EXPR against the valid instructions and dispatch to
  ;; the corresponding sub-assembler.
  (match expr
    (((? simple-op? a)) (assemble-simple a))
    (`(LABEL ,a)        (assemble-label a))
    (`(TST ,a)          (assemble-test a))
    (`(CLL ,a)          (assemble-call a))
    (`(B   ,a)          (assemble-branch a))
    (`(ADR ,a)          (assemble-branch a))
    (`(BT  ,a)          (assemble-branch-true a))
    (`(BF  ,a)          (assemble-branch-false a))
    (`(CL  ,a)          (assemble-copy a))
    (_
     (error (format #f "Unknown expression: ~s\n" expr))))
  )

(define *pc* 0)

(define *labels* 0)

(define (reset-pc!)
  (set! *pc* 0))
(define (reset-labels!)
  
  (set! *labels* '()))

(define (write-bytevector-to-file bv fn)
  (let ((port (open-output-file fn)))
    (put-bytevector port bv)
    (close-port port)))

(define (flatten l)
  (if (null? l)
      '()
      (append (car l) (flatten (cdr l)))))

(define (all-sat? p l)
  (cond ((null? l) #t)
        ((p (car l)) (all-sat? p (cdr l)))
        (else #f)))

(define (pass1 exprs)
  ;; Check each instruction for correct syntax and produce code
  ;; generating thunks.  Meanwhile, increment PC accordingly and build
  ;; up labels.
  
  (reset-labels!)
  (reset-pc!)
  (format #t "Pass one...\n")

  ;; Every assembled instruction, or inlined procedure should return a
  ;; value.  A value of () indicates that it will not be included in
  ;; pass 2.
  (filter
   (lambda (x) (not (null? (car x))))
   (map-in-order
    (lambda (expr)
      (if (procedure? expr)
          ;; Evaluate an inlined procedure (could do anything(!)).
          (let ((macro-val (expr)))
            ;; But that procedure has to return () or an instruction
            ;; record.
            (if (not (or (null? macro-val)
                         (inst? macro-val)))
                (error (format #f
                               "Error during pass one: macro did not return an instruction record: instead got ~a.  PC: ~a\n"
                               macro-val
                               *pc*))
                (begin (if (inst? macro-val)
                           ;; This macro generated an instruyction
                           ;; record, so advance the program counter.
                           (advance-pc! (inst-length macro-val)))
                       ;; Return a "tagged" result, where the original
                       ;; expression is preserved.
                       (cons macro-val expr))))

          ;; Assemble a normal instruction.
          (let ((res (assemble-expr expr)))
            (if (inst? res)
                (advance-pc! (inst-length res)))
            ;; Return a "tagged" result, where the original expression
            ;; is preserved, for debugging purposes.
            (cons res expr))))
    exprs)))

(define (pass2 insts)
  (reset-pc!)
  (format #t "Pass two...\n")
  ;; Force the code generating thunks.  All labels should be resolved by now.
  (map-in-order
   (lambda (x)
     (if (not (inst? (car x)))
         (error (format #f "Error during pass two: not an instruction record: ~a. PC: ~a." (car x) (num->hex *pc*))))
     (advance-pc! (inst-length (car x)))
     (let ((res (gen-inst (car x))))
       (if verbose?
           (format #t "PC: ~a ~a\n" (num->hex *pc*) (cdr x)))
       (cond
        ;; Check consistency of declared instruction length and actual
        ;; length.
        ((not (= (inst-length (car x)) (length res)))
         (error (format #f "Instruction length declared does not match actual: Expected length ~a, got length ~a of expression ~a\n PC: ~a" (inst-length (car x)) (length res) res *pc*)))
        ;; Check that everything is an 8-bit unsigned number.
        ((not (all-sat? 8-bit-imm? res))
         (error (format #f "Invalid byte at ~4'0x: ~a" *pc* res)))
        (else
         ;; We're ok.
         res))))
   insts)
  )

(define (assemble-prog prog)
  (pass2 (pass1 prog)))

(define (assemble-to-binary prog)
  (map num->binary (flatten (assemble-prog prog))))

(define (assemble-to-hex prog)
  (map num->hex (flatten (assemble-prog prog))))

(define (assemble-to-file prog filename)
  (write-bytevector-to-file
   (u8-list->bytevector (flatten (assemble-prog prog)))
   filename))


;; Take n elements from a list.
(define (take n list)
  (if (or (zero? n) (null? list))
      '()
      (cons (car list)
            (take (1- n) (cdr list)))))

;; For debugging purposes.  Assemble the program and find the
;; instruction that is at the specified byte address.
(define (assemble-find-instr-byte byte prog context)
  (let ((partial-asm (pass1 prog)))
    (let loop ((pc 0)
               (rest-insts partial-asm))
      (cond ((null? rest-insts) (error "Reached end of program before specified byte address."))
            ((>= pc byte)
             (map cdr (take context rest-insts)))
            (else
             (loop (+ pc (inst-length (caar rest-insts)))
                   (cdr rest-insts)))))))

(define (one-of-strings strings)
  (if (null? strings)
      fail
      (+++ (str (car strings)) (one-of-strings (cdr strings)))))

(define masm-label
  (doM* (x <- (many1 alpha-num))
        (return `(LABEL ,x))))

(define masm-simple-command
  (doM* (x <- (one-of-strings '("ID" "NUM" "SR" "R" "SET" "BE" "CI" "GN1" "GN2" "LB" "OUT" "END")))
        (return `(,(string->symbol x)))))

(define masm-complex-command-label
  (doM* (command-name <- (one-of-strings '("CLL" "BT" "BF" "BT" "ADR")))
        (many (char #\space))
        (label <- (many1 alpha-num))
        (return `(,(string->symbol command-name) ,label))))

(define masm-complex-command-str
  (doM* (command-name <- (one-of-strings '("TST" "CL")))
        (many (char #\space))
        (string <- parse-string)
        (return `(,(string->symbol command-name) ,string))))

(define masm-complex-command
  (<:> masm-complex-command-label
       masm-complex-command-str))

(define masm-command
  ;; Accepts tabs or eight spaces.
  (doM* (<:> (char #\tab) (str "        "))
        (<:> masm-simple-command
             masm-complex-command)))

(define masm
  (sepby-n (<:> masm-command masm-label)
           (char #\newline)))

(define (parse-masm-file filename)
  (let ((msg (read-file-string filename)))
    (masm msg)))

(define (compile-masm-to-bytecode masm-filename bytecode-filename)
  (let* ((parse-res (parse-masm-file masm-filename)))
    (if (string=? (cdr parse-res) "\n")
        (assemble-to-file (car parse-res) bytecode-filename)
        (format #t
                "Warning: File ~a has unconsumed input ~a"
                masm-filename
                (cdr parse-res)))))

(define (compile-meta-II metaII-filename masm-filename bytecode-filename)
  (meta-II-to-masm metaII-filename masm-filename)
  (compile-masm-to-bytecode masm-filename bytecode-filename))
