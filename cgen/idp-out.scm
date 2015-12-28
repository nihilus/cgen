
(define (-gen-getop-access insn operand)
  (let ((opnum (insn-op-order insn (op:sem-name operand))))
    (string-append
      (if (= opnum -1)
        (string-append "    out_cgen_operand(x, @ARCH@_OPERAND_" (string-upcase (gen-sym operand)) ", cmd.ea);\n" )
        (string-append "    out_one_operand(" (number->string opnum) ");\n")
      )
    )
  )
)

(define (-gen-mnem-out insn operand)
  (if operand
    (error "Postfix instructions unimplemented")
    "    OutMnem();\n"
  )
)

(method-make!
 <hw-memory> 'gen-print
 (lambda (self operand)
   (error "gen-print of memory not supported yet")
 )
)

; Hardware support.

; For registers, use the indices field.  Ignore values.
; ??? Not that that will always be the case.

;(method-make-forward! <hw-register> 'indices '(gen-print))
(method-make!
 <hw-register> 'gen-print
  (lambda (self operand)
    (string-append
      "      out_register(ph.regNames[x.reg]);\n"
    )
  )
)

; For immediates, use the values field.  Ignore indices.
; ??? Not that that will always be the case.

;(method-make-forward! <hw-immediate> 'values '(gen-print))
(method-make!
 <hw-immediate> 'gen-print
  (lambda (self operand)
    (let ((fn (send operand 'gen-function-name 'print))) 
      (if fn
        (string-append
          "      out_" fn "(x, pc);\n"
        )
        (string-append
          "      OutValue(x, "
          (if (eq? (mode:class (elm-get (hw-values self) 'mode)) 'INT)
            "OOF_SIGNED|"
            ""
          )
          (if (or (obj-has-attr? operand 'PCREL-ADDR) (obj-has-attr? operand 'ABS-ADDR))
            "OOF_ADDR|OOFS_NOSIGN|"
            "OOF_NUMBER|"
          )
          "OOFW_IMM);\n"
        )
      )
    )
  )
)

; For addresses, use the values field.  Ignore indices.

;(method-make-forward! <hw-address> 'values '(gen-print))
(method-make!
 <hw-address> 'gen-print
 (lambda (self operand)
   (error "gen-print of address not implemented")
 )
)

; Strip the mnemonic part from SYNTAX.
; (ie: everything up to but not including the first space or '$')
; If STRIP-MNEM-OPERANDS?, strip them too.

(define (strip-mnemonic strip-mnem-operands? syntax)
  (let ((space (string-index syntax #\space)))
    (if strip-mnem-operands?
  (if space
      (string-drop space syntax)
      "")
  (let loop ((syn syntax))
    (if (= (string-length syn) 0)
        ""
        (case (string-ref syn 0)
    ((#\space) syn)
    ((#\\) (loop (string-drop 2 syn)))
    ((#\$) syn)
    (else (loop (string-drop1 syn))))))))
)

; Compute the sequence of syntax bytes for SYNTAX.
; STRIP-MNEMONIC? is #t if the mnemonic part is to be stripped off.
; STRIP-MNEM-OPERANDS? is #t if any mnemonic operands are to be stripped off.
; SYNTAX is a string of text and operands.
; OP-MACRO is the macro to call that computes an operand's value.
; The resulting syntax is expressed as a sequence of bytes.
; Values < 128 are characters that must be matched.
; Values >= 128 are 128 + the index into the operand table.

(define (-gen-insn-out insn syntax isa-name-list)
  (let ((context (make-prefix-context "idp out generation"))
    (syntax (strip-mnemonic #f syntax)))

    (let loop ((syn syntax) (result "") (first? #t))
      (cond 
        ((and (= (string-length syn) 0) first?) (-gen-mnem-out insn #f)) ; no operands

        ((= (string-length syn) 0) result) ; base case

        ((char=? #\\ (string-ref syn 0))
          (if (= (string-length syn) 1)
            (parse-error context "missing char after '\\'" syntax))
          (let ((escaped-char (string-ref syn 1))
            (remainder (string-drop 2 syn)))
              (if (char=? #\\ escaped-char)
                (loop remainder (string-append result "    out_symbol('\\\\');\n") #f)
                (loop remainder (string-append result "    out_symbol('" (string escaped-char) "');\n") #f)
              )
          )
        )

        ((char=? #\$ (string-ref syn 0))
          ; Extract the symbol from the string, which will be the name of
          ; an operand.  Append it to the result.
          (if (= (string-length syn) 1)
          (parse-error context "missing operand name" syntax))
          ; Is it $foo or ${foo}?
          (if (char=? (string-ref syn 1) #\{)
            (let ((n (chars-until-delimiter syn #\})))
              ; Note that 'n' includes the leading ${.
              ; FIXME: \} not implemented yet.
              (case n
                ((0) (parse-error context "empty operand name" syntax))
                ((#f) (parse-error context "missing '}'" syntax))
                (else 
                  (let* ((operand (string->symbol (substring syn 2 n)))
                    (op (current-op-lookup operand isa-name-list)))
                    (if (not op)
                      (parse-error context "undefined operand " operand syntax)
                    )
                    (loop (string-drop (+ n 1) syn)
                      (string-append result (if first?
                        (-gen-mnem-out insn op)
                        (-gen-getop-access insn op)
                      ))
                      #f
                    )
                  )
                )
              )
            )
            (let ((n (id-len (string-drop1 syn))))
              (if (= n 0)
                (parse-error context "empty or invalid operand name" syntax)
              )
              (let* ((operand (string->symbol (substring syn 1 (1+ n))))
                (op (current-op-lookup operand isa-name-list)))
                (if (not op)
                  (parse-error context "undefined operand " operand syntax)
                )
                (loop (string-drop (1+ n) syn)
                  (string-append result (if first?
                    (-gen-mnem-out insn op)
                    (-gen-getop-access insn op)
                  ))
                  #f
                )
              )
            )
          )
        )

        ; comma becomes comma-space for readability
        ((char=? #\, (string-ref syn 0))
          (loop (string-drop1 syn)
            (string-append
              result
              "    out_symbol('" (string-take1 syn) "');\n"
              "    OutChar(' ');\n"
            )
            #f
          )
        )

        ; Append the character to the result.
        (else (loop (string-drop1 syn)
            (string-append 
              result
              (if first? 
                (-gen-mnem-out insn #f)
                (string-append "    out_symbol('" (string-take1 syn) "');\n")
              )
            )
            #f
          )
        )
      )
    )
  )
)

; Generate special printing stubs

(define (-gen-special-print-stubs)
  (remove-duplicates
    (apply append
      (string-list-map
        (lambda (ops)
          (string-list-map
            (lambda (op)
              (let ((fn (send op 'gen-function-name 'print)))
                (if fn
                  (string-append "extern void out_" fn "(op_t &x, ea_t pc);\n")
                  ""
                )
              )
            )
            ops
          )
        )
        (op-sort (find (lambda (op)
              (and (not (has-attr? op 'SEM-ONLY))
                (not (anyof-operand? op))
                (not (derived-operand? op))
              )
            )
            (current-op-list)
          )
        )
      )
    )
  )
)

; Generate @arch@_data() function
; TODO: put code here

(define (-gen-outdata)
  (string-list
    "void idaapi @arch@_data(ea_t ea)\n"
    "{\n"
    "  gl_name = 1;\n"
    "  intel_data(ea);\n"
    "}\n\n"
  )
)

; Generate outop() function

(define (-gen-outop)
  (string-list
    "static bool cgen_outop(op_t &x, uint16 opindex, ea_t pc)\n"
    "{\n"
    "  switch (opindex)\n"
    "  {\n"
    (gen-switch 'print)
    "    default: return 0;\n"
    "  }\n"
    "  return 1;\n"
    "}\n\n"
    "bool idaapi outop(op_t &x)\n"
    "{\n"
    "  return cgen_outop(x, x.cgen_optype, cmd.ea);\n"
    "}\n\n"
  )
)

; Generate out() function

(define (-gen-out insn-list)
  (append
    (string-list
      "void idaapi out(void)\n"
      "{\n"
      "  char buf[MAXSTR];\n"
      "  init_output_buffer(buf, sizeof(buf));\n"
      "  switch (cmd.itype)\n"
      "  {\n"
    )
    (map (lambda (insn)
        (string-append
          "case " (gen-cpu-insn-enum (current-cpu) insn) ":\n"
          (-gen-insn-out insn (insn-syntax insn) (obj-isa-list insn))
          "    break;\n"
        )
      ) insn-list
    )
    (string-list
      "    default: break;\n"
      "  }\n"
      "  term_output_buffer();\n"
      "  gl_comm = 1;\n"
      "  MakeLine(buf);\n"
      "}\n"
    )
  )
)

; Entry point.

(define (out.cpp)
  (logit 1 "Generating out.cpp ...\n")
  (sim-analyze-insns!)
  (let* ((all-insn (real-insns (current-insn-list))))
    (map analyze-insn-op-order! all-insn)

    ; Turn parallel execution support on if cpu needs it.
    (set-with-parallel?! (state-parallel-exec?))

    ; Tell the rtx->c translator we are the simulator.
    (rtl-c-config! #:rtl-cover-fns? #t)

    (string-write
     (gen-c-copyright "@ARCH@ IDP output"
        CURRENT-COPYRIGHT CURRENT-PACKAGE)
      "\
#include \"@arch@.hpp\"

  \n"
     -gen-special-print-stubs
     "\n"
     -gen-outdata
     -gen-outop
     (-gen-out all-insn)
     )
  )
)
