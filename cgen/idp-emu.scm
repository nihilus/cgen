
; Instruction support.

; Return list of all instructions to use for scache engine.
; This is all real insns plus the `invalid' and `cond' virtual insns.
; It does not include the pbb virtual insns.

(define (scache-engine-insns)
  (non-multi-insns (non-alias-pbb-insns (current-insn-list)))
)

; Hardware descriptions support code.

; Default cxmake-get method.

(method-make!
 <hardware-base> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
  (error "cxmake-emu-get not overridden:" self))
)

; PC support

; 'gen-set-quiet helper for PC values.
; NEWVAL is a <c-expr> object of the value to be assigned.
; If OPTIONS contains #:direct, set the PC directly, bypassing semantic
; code considerations.
; ??? OPTIONS support wip.  Probably want a new form (or extend existing form)
; of rtx: that takes a variable number of named arguments.
; ??? Another way to get #:direct might be (raw-reg h-pc).

(define (/hw-gen-emu-set-quiet-pc self estate mode index selector newval order . options)
  (if (not (send self 'pc?)) (error "Not a PC:" self))
  (string-append
    "{ " (mode:c-type mode) " val = " (cx:c newval) "; "
    "if (valid) ua_add_cref(0, val, InstrIsSet(cmd.itype, CF_CALL) ? fl_CN : fl_JN); }\n"
  )
)

(method-make! <hw-pc> 'gen-emu-set-quiet /hw-gen-emu-set-quiet-pc)

; Ignore PC skips

(method-make!
 <hw-pc> 'cxmake-skip
 (lambda (self estate yes?)
   (error "PC skip not implemented"))
)

; Registers.

; If any register is read, we "taint" the value produced and essentially 
; force the emu function to return and not do any more processing

(define (/hw-cxmake-emu-get hw estate mode index selector order)
  (cx:make mode
    "[&valid](){ valid = 0; return 0; }()"
  )
)

(method-make! <hw-register> 'cxmake-emu-get /hw-cxmake-emu-get)
(method-make! <hw-register> 'cxmake-emu-get-raw /hw-cxmake-emu-get)

; Utilities to generate C code to assign a variable to a register.

(define (/hw-gen-emu-set-quiet hw estate mode index selector newval order)
  (string-append (cx:c newval) ";\n") ; ignore register writes
)

(method-make! <hw-register> 'gen-emu-set-quiet /hw-gen-emu-set-quiet)
(method-make! <hw-register> 'gen-emu-set-quiet-raw /hw-gen-emu-set-quiet)

; Memory support.
; We create xref only for memory accesses that uses a single register (offset 
; reference) or accesses that uses only immediate values.

; Create xref for memory reads

(method-make!
 <hw-memory> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
   (let ((mode (if (mode:eq? 'DFLT mode) (hw-mode self) mode))
      (default-selector? (hw-selector-default? selector)))
       (if (not default-selector?) (error "selector not implemented"))
       (cx:make mode
         (string-append 
            "[&valid](){ "
            (mode:c-type mode)
            " val = " 
            (/gen-hw-index index estate)
            "; if (valid) ua_add_dref(0, val, dr_R); return 0; }()\n"
         )
       )
   )
 )
)

; Create xref for memory writes

(method-make!
 <hw-memory> 'gen-emu-set-quiet
 (lambda (self estate mode index selector newval order)
   (let ((mode (if (mode:eq? 'DFLT mode) (hw-mode self) mode))
      (default-selector? (hw-selector-default? selector)))
       (if (not default-selector?) (error "selector not implemented"))
       (string-append 
          "{ " (mode:c-type mode) " val = "
          (/gen-hw-index index estate)
          "; if (valid) ua_add_dref(0, val, dr_W); }\n"
       )
   )
 )
)

; Immediates, addresses.


(method-make!
 <hw-immediate> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
  (let ((opstr (string-append "cmd.Op" (number->string (+ order 1)))))
    (cx:make mode
      (string-append
         opstr ".type == o_imm ? " opstr ".value : " opstr ".addr"
      )
    )
  )
 )
)

; TODO: implement this

(method-make!
 <hw-address> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
   (error "hw-address cxmake-emu-get not implemented")
 )
)

; TODO: implement this

(method-make!
 <hw-iaddress> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
   (error "hw-iaddress cxmake-emu-get not implemented")
 )
)

; Hardware index support code.

; TODO: implement this

(method-make!
 <hw-index> 'cxmake-emu-get
 (lambda (self estate mode order)
   (error "hw-index cxmake-emu-get not implemented")
 )
)

; Instruction operand support code.

; Extra pc operand methods.

(method-make!
 <pc> 'cxmake-emu-get
 (lambda (self estate mode index selector order)
   (error "pc operand not supported"))
)

(method-make!
 <pc> 'cxmake-skip
 (lambda (self estate yes?)
   (error "pc operand skip not implemented"))
)

; Return <c-expr> object to get the value of an operand.
; ESTATE is the current rtl evaluator state.
; If INDEX is non-#f use it, otherwise use (op:index self).
; This special handling of #f for INDEX is *only* supported for operands
; in cxmake-get, gen-set-quiet, and gen-set-trace.
; Ditto for SELECTOR.

(method-make!
 <operand> 'cxmake-get
 (lambda (self estate mode index selector)
   (let* ((mode (if (mode:eq? 'DFLT mode)
        (send self 'get-mode)
        mode))
    (hw (op:type self))
    (index (if index index (op:index self)))
    (idx (if index (/gen-hw-index index estate) ""))
    (order (insn-op-order (estate-owner estate) (op:sem-name self)))
    (idx-args (if (equal? idx "") "" (string-append ", " idx)))
    (selector (if selector selector (op:selector self)))
    (delayval (op:delay self))
    (md (mode:c-type mode))
    (name (if 
     (eq? (obj:name hw) 'h-memory)
     (string-append md "_memory")
     (gen-c-symbol (obj:name hw))))
    (getter (op:getter self))
    (def-val (cond ((obj-has-attr? self 'RAW)
        (send hw 'cxmake-emu-get-raw estate mode index selector order))
       (getter
        (let ((args (car getter))
        (expr (cadr getter)))
          (rtl-c-expr mode
          (obj-isa-list self)
          (if (= (length args) 0) nil
              (list (list (car args) 'UINT index)))
          expr
          #:rtl-cover-fns? #t
          #:output-language (estate-output-language estate))))
       (else
        (let ()
        (if (= order -1) (logit 3 "operand: " (op:sem-name self) "\n"))
        (send hw 'cxmake-emu-get estate mode index selector order)))))
        )

     (logit 4 "<operand> cxmake-get self=" (obj:name self) " mode=" (obj:name mode)
      " index=" (obj:name index) " selector=" selector "\n")

     (if delayval
   (cx:make mode (string-append "lookahead ("
              (number->string delayval)
              ", tick, " 
              "buf." name "_writes, " 
              (cx:c def-val) 
              idx-args ")"))
   def-val)))
)

; Return C code to set the value of an operand.
; NEWVAL is a <c-expr> object of the value to store.
; If INDEX is non-#f use it, otherwise use (op:index self).
; This special handling of #f for INDEX is *only* supported for operands
; in cxmake-get, gen-set-quiet, and gen-set-trace.
; Ditto for SELECTOR.

(define (/op-gen-emu-set-quiet op estate mode index selector newval order)
  (send (op:type op) 'gen-emu-set-quiet estate mode index selector newval order)
)

(method-make!
 <operand> 'gen-set-quiet
 (lambda (self estate mode index selector newval)
   (let ((mode (if (mode:eq? 'DFLT mode)
       (send self 'get-mode)
       mode))
       (order (insn-op-order (estate-owner estate) (op:sem-name self)))
   (index (if index index (op:index self)))
   (selector (if selector selector (op:selector self))))
     (cond ((obj-has-attr? self 'RAW)
      (send (op:type self) 'gen-emu-set-quiet-raw estate mode index selector newval order))
     (else
      (/op-gen-emu-set-quiet self estate mode index selector newval order)))))
)

; Since we wish to map all possible code paths, we strip out all conditionals 
; and turn them into sequences. The user must manually remove unwanted paths.

(define (disable-gen-conditionals!)
  (let ((saved '(s-if s-cond s-case s-c-call)))
    (set! s-if (lambda (estate mode cond then . else)
      (apply s-sequence
            (cons estate
                  (cons mode
                  (cons nil (cons then else))))) ; ignore the cond
    ))

    (set! s-cond (lambda (estate mode . cond-code-list)
      (apply s-sequence
            (cons estate
                  (cons mode
                  (cons nil
                  (map (lambda (cond) (cadr cond)) cond-code-list)))))
    ))

    (set! s-case (lambda (estate mode test . case-list)
      (apply s-sequence
            (cons estate
                  (cons mode
                  (cons nil
                  (map (lambda (cond) (test cadr cond)) case-list)))))
    ))

    ; Similarily, disable all c-calls

    (set! s-c-call (lambda (estate mode name . args)
      (cx:make mode 
        ; If the mode is VOID, this is a statement.
        ; Otherwise it's an expression.
        ; ??? Bad assumption!  VOID expressions may be used
        ; within sequences without local vars, which are translated
        ; to comma-expressions.
        (if (or (mode:eq? 'DFLT mode) ;; FIXME: can't get DFLT anymore
          (mode:eq? 'VOID mode))
          ""
          "[&valid](){ valid = 0; return 0; }()" ; using the result taints our analysis
        )
      )
    ))
    saved
  )
)

(define (restore-conditionals! saved)
  (set! s-if (car saved))
  (set! s-cond (cadr saved))
  (set! s-case (caddr saved))
  (set! s-c-call (cadddr saved))
)

(define (s-c-raw-call estate mode name . args)
  (s-c-call estate mode name args)
)

; Return C code to perform the semantics of INSN.

(define (gen-emu-code insn)
  (let ((saved (disable-gen-conditionals!))
    (res (cond ((insn-compiled-semantics insn)
     => (lambda (sem)
          (rtl-c++-parsed VOID sem
              #:for-insn? #f ; disable tracing
              #:rtl-cover-fns? #t
              #:owner insn)))
    ((insn-canonical-semantics insn)
     => (lambda (sem)
          (rtl-c++-parsed VOID sem
              #:for-insn? #f ; disable tracing
              #:rtl-cover-fns? #t
              #:owner insn)))
    (else
     (context-error (make-obj-context insn #f)
        "While generating emu code"
        "semantics of insn are not canonicalized")))))
    (restore-conditionals! saved)
    res
  )
)

; Return definition of C function to perform INSN.
; This version handles the with-scache case.

(define (/gen-scache-emu-fn insn)
  (logit 2 "Processing semantics for " (obj:name insn) ": \"" (insn-syntax insn) "\" ...\n")
  (let ((cti? (insn-cti? insn))
  (insn-len (insn-length-bytes insn)))
    (string-list
     "// ********** " (obj:name insn) ": " (insn-syntax insn) "\n\n"
     "static int\n"
     "@prefix@_emu_" (gen-sym insn)
     " (void)\n"
     "{\n"
     ; The address of this insn, needed by extraction and emu code.
     ; Note that the address recorded in the cpu state struct is not used.
     ; For faster engines that copy will be out of date.
     "  ea_t pc = cmd.ea;\n"
     "  int valid = 1;\n"
     "\n"
     (gen-emu-code insn)
     "\n"
     "  return " (number->string insn-len) ";\n"
     "}\n\n"
     ))
)

; Generate all emu functions

(define (/gen-all-emu-fns insns)
  (logit 2 "Processing semantics ...\n")
  (if (with-scache?)
    (map /gen-scache-emu-fn insns)
    (error "must specify `with-scache'"))
)

; Generate the entry function

(define (/gen-emu-entry-fn insns)
  (append
    (string-list
      "// Emulator entry\n"
      "int idaapi emu(void)\n"
      "{\n"
      "  int len;"
      "  switch (cmd.itype)\n"
      "  {\n"
    )
    (map (lambda (insn)
        (string-append
          "    case " (gen-cpu-insn-enum (current-cpu) insn) ": "
          "len = @prefix@_emu_" (gen-sym insn) "(); break;\n"
        )
      )
      insns
    )
    (string-list
      "    default: len = 0; break;\n"
      "  }\n"
      "  if (len && !InstrIsSet(cmd.itype, CF_STOP))\n"
      "  {\n"
      "    ua_add_cref(0, cmd.ea+len, fl_F);\n"
      "  }\n"
      "  return 1;\n"
      "}\n"
    )
  )
)

; Entry point.

(define (emu.cpp)
  (logit 1 "Generating emu.cpp ...\n")
  (assert-keep-one)

  (sim-analyze-insns!)

  ; Turn parallel execution support on if cpu needs it.
  (set-with-parallel?! (state-parallel-exec?))

  ; Tell the rtx->c translator we are the simulator.
  (rtl-c-config! #:rtl-cover-fns? #t)

  (let ((insns (scache-engine-insns)))
    (map analyze-insn-op-order! insns)
    (string-write
     (gen-c-copyright "IDP emulator for @prefix@."
        copyright-red-hat package-red-hat-simulators)
     "\

#include \"@arch@.hpp\"
\n"
     (/gen-all-emu-fns insns)
     "\n"
     (/gen-emu-entry-fn insns)
    )
  )
)
