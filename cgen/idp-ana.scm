
; Instruction field extraction support cont'd.
; Hardware support.

; gen-extract method.
; For the default case we use the ifield as is, which is output elsewhere.

(method-make!
 <hardware-base> 'gen-extract
 (lambda (self op sfmt local?)
   "")
)

; Extract the necessary fields into ARGBUF.

(method-make!
 <hw-register> 'gen-extract
 (lambda (self op sfmt local?)
  (let* ((order (op:order op))
    (cmd-op (string-append "cmd.Op" (number->string (+ (op:order op) 1)))))
    (if (>= order 0)
      (string-append 
        "    " cmd-op ".type = o_reg;\n"
        "    " cmd-op ".reg = "
        (gen-sym (op:type op)) " + "
        (gen-extracted-ifld-value (op-ifield op))
        ";\n")
      ""
    )
  )
 )
)

; Extract the necessary fields into ARGBUF.

(method-make!
 <hw-address> 'gen-extract
 (lambda (self op sfmt local?)
   (string-append "  " (number->string (op:order op))
      (if local?
          (gen-hw-index-argbuf-name (op:index op))
          (gen-hw-index-argbuf-ref (op:index op)))
      " = "
      (gen-extracted-ifld-value (op-ifield op))
      ";\n"))
)

; Extract iaddress

(method-make!
 <hw-iaddress> 'gen-extract
 (lambda (self op sfmt local?)
   (error "iaddr extraction unimplemented"))
)

; Extract immediate

(method-make!
 <hw-immediate> 'gen-extract
 (lambda (self op sfmt local?)
  (let* ((order (op:order op))
    (cmd-op (string-append "cmd.Op" (number->string (+ (op:order op) 1)))))
    (if (>= order 0)
      (string-append 
        "    " cmd-op ".type = o_imm;\n"
        "    " cmd-op ".dtyp = dt_dword; // TODO: change to actual size\n"
        "    " cmd-op ".value = "
        (gen-extracted-ifld-value (op-ifield op))
        ";\n")
      ""
    )
  )
 )
)

; Extract pc

(method-make!
 <hw-pc> 'gen-extract
 (lambda (self op sfmt local?)
   (error "pc extraction unimplemented"))
)

; Return C code that extracts the fields of <sformat> SFMT.
;
; Extraction is based on formats to reduce the amount of code generated.
; However, we also need to emit code which records the hardware elements used
; by the semantic code.  This is currently done by recording this information
; with the format.

(define (/gen-extract-case sfmt)
  (logit 2 "Processing extractor for \"" (sfmt-key sfmt) "\" ...\n")
  (string-list
   " extract_" (gen-sym sfmt) ":\n"
   "  {\n"
   (if (> (length (sfmt-iflds sfmt)) 0)
       (string-append
  "    CGEN_INSN_WORD insn = "
  (if (adata-integral-insn? CURRENT-ARCH)
      "entire_insn;\n"
      "base_insn;\n"))
       "")
   ;(gen-define-field-macro sfmt)
   (gen-define-ifields (sfmt-iflds sfmt) (sfmt-length sfmt) "    " #f)
   "\n"
   (gen-extract-ifields (sfmt-iflds sfmt) (sfmt-length sfmt) "    " #f)
   "\n"
   (/gen-record-args sfmt)
   "\n"
   ;(gen-undef-field-macro sfmt)
   "    cmd.size = " (number->string (/ (sfmt-length sfmt) 8)) ";\n"
   "    return " (number->string (/ (sfmt-length sfmt) 8)) ";\n"
   "  }\n\n"
   )
)

; Generate top level ana function.
; INITIAL-BITNUMS is a target supplied list of bit numbers to use to
; build the first decode table.  If nil, we compute 8 bits of it (FIXME)
; ourselves.
; LSB0? is non-#f if bit number 0 is the least significant bit.

(define (/gen-ana-fn insn-list initial-bitnums lsb0?)

  ; Compute the initial DECODE-BITSIZE as the minimum of all insn lengths.
  ; The caller of @prefix@_decode must fetch and pass exactly this number of bits
  ; of the instruction.
  ; ??? Make this a parameter later but only if necessary.

  (let ((decode-bitsize (apply min (map insn-base-mask-length insn-list)))
    (max-bitsize (apply max (map insn-base-mask-length insn-list))))

    ; Compute INITIAL-BITNUMS if not supplied.
    ; 0 is passed for the start bit (it is independent of lsb0?)
    (if (null? initial-bitnums)
  (set! initial-bitnums (decode-get-best-bits insn-list nil
                0 ; startbit
                8 ; max
                decode-bitsize
                lsb0?)))

    ; All set.  gen-decoder does the hard part, we just print out the result. 
    (let ((decode-code (gen-decoder insn-list initial-bitnums
            decode-bitsize
            "    " lsb0?
            (current-insn-lookup 'x-invalid #f)
            #f)))

      (string-write
       "\
/* Analyze the current instruction.  */

int idaapi ana( void )
{
  /* Result of decoder.  */
  @PREFIX@_INSN_TYPE itype;

  {
    CGEN_INSN_WORD insn;
    CGEN_INSN_WORD entire_insn;
    get_data_value(cmd.ea, &insn, " (number->string (/ decode-bitsize 8)) ");
    get_data_value(cmd.ea, &entire_insn, " (number->string (/ max-bitsize 8)) ");
\n"

       decode-code

       "\
  }
\n"

       (if (with-scache?)
           (string-list "\
  /* The instruction has been decoded, now extract the fields.  */\n\n"
            /gen-all-extractors)
            (error "must enable scache")
        )

       "\
}\n"
       )))
)

; Entry point.

(define (ana.cpp)
  (logit 1 "Generating ana.cpp ...\n")
  (sim-analyze-insns!)
  (let* ((all-insn (real-insns (current-insn-list))))
    (map set-insn-operand-order! all-insn)

    ; Turn parallel execution support on if cpu needs it.
    (set-with-parallel?! (state-parallel-exec?))

    ; Tell the rtx->c translator we are the simulator.
    (rtl-c-config! #:rtl-cover-fns? #t)

    (string-write
     (gen-c-copyright "@ARCH@ IDP instructions"
        CURRENT-COPYRIGHT CURRENT-PACKAGE)
      "\
#include <bytes.hpp>
#include \"@arch@.hpp\"
#include \"cgen.h\"

#define FLD // no need for this macro here
#ifdef CGEN_TRACE_EXTRACT
#undef CGEN_TRACE_EXTRACT
#endif
#define CGEN_TRACE_EXTRACT(cpu, abuf, args) \
do { \
  DEBUG_TRACE args ; \
} while (0)
#define DEBUG_TRACE(cpu, pc, args...) \
do { \
} while (0)
  \n"
     (lambda () (/gen-ana-fn all-insn
              (state-decode-assist)
              (current-arch-insn-lsb0?)))
     )
  )
)
