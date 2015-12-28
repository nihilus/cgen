
; Instruction field extraction support cont'd.
; Hardware support.

; gen-extract method.
; For the default case we use the ifield as is, which is output elsewhere.

(method-make!
 <hardware-base> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
   "")
)

; Extract the necessary fields into ARGBUF.

(method-make!
 <hw-register> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
  (let* ((order (insn-op-order insn (op:sem-name op)))
    (cmd-op (string-append "cmd.Op" (number->string (+ order 1)))))
    (if (>= order 0)
      (string-append 
        "    " cmd-op ".type = o_reg;\n"
        "    " cmd-op ".reg = REGS_"
        (string-upcase (hw-enum self)) "_BASE + "
        (gen-extracted-ifld-value (op-ifield op))
        ";\n"
        "    " cmd-op ".cgen_optype = @ARCH@_OPERAND_"
        (string-upcase (gen-sym op))
        ";\n"
      )
      ""
    )
  )
 )
)

; Extract the necessary fields into ARGBUF.

(method-make!
 <hw-address> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
   (error "addr extraction unimplemented"))
)

; Extract iaddress

(method-make!
 <hw-iaddress> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
   (error "iaddr extraction unimplemented"))
)

; Extract immediate

(method-make!
 <hw-immediate> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
  (let* ((mode (hw-mode self))
    (order (insn-op-order insn (op:sem-name op)))
    (cmd-op (string-append "cmd.Op" (number->string (+ order 1))))
    (pcrel? (obj-has-attr? op 'PCREL-ADDR))
    (absaddr? (obj-has-attr? op 'ABS-ADDR))
    )
    (if (>= order 0)
      (string-append 
        "    " cmd-op ".type = "
        (if pcrel? "o_near" (if absaddr? "o_mem" "o_imm"))
        ";\n"
        "    " cmd-op ".dtyp = get_dtyp_by_size("
        (number->string (mode:bytes mode))
        ");\n"
        "    " cmd-op (if (or pcrel? absaddr?) ".addr = " ".value = ")
        (gen-extracted-ifld-value (op-ifield op))
        ";\n"
        "    " cmd-op ".cgen_optype = @ARCH@_OPERAND_"
        (string-upcase (gen-sym op))
        ";\n"
      )
      ""
    )
  )
 )
)

; Extract pc

(method-make!
 <hw-pc> 'gen-ana-extract
 (lambda (self insn op sfmt local?)
   (error "pc extraction unimplemented"))
)

; Stub for extracting operands

(define (/gen-op-ana-extract insn op sfmt local?)
  (send (op:type op) 'gen-ana-extract insn op sfmt local?)
)

; Instruction field extraction support cont'd.
; Emit extraction section of decode function.

; Return C code to record insn field data for <sformat> SFMT.
; This is used when with-scache.

(define (/gen-ana-record-args insn sfmt)
  (let ((operands (sfmt-extracted-operands sfmt))
  (iflds (sfmt-needed-iflds sfmt)))
    (string-list
     "    /* Record the operands  */\n"
     (string-list-map (lambda (op) (/gen-op-ana-extract insn op sfmt #f))
          operands)
     ))
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
   (/gen-ana-record-args (sfmt-eg-insn sfmt) sfmt)
   "\n"
   ;(gen-undef-field-macro sfmt)
   "    cmd.itype = itype;\n"
   "    cmd.size = " (number->string (/ (sfmt-length sfmt) 8)) ";\n"
   "    return " (number->string (/ (sfmt-length sfmt) 8)) ";\n"
   "  }\n\n"
   )
)

; For each format, return its extraction function.

(define (/gen-all-extractors)
  (logit 2 "Processing extractors ...\n")
  (string-list-map /gen-extract-case (current-sfmt-list))
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
    (max-bitsize (apply max (map insn-base-mask-length insn-list)))
    (saved-fn state-base-insn-bitsize))

    ; Compute INITIAL-BITNUMS if not supplied.
    ; 0 is passed for the start bit (it is independent of lsb0?)
    (if (null? initial-bitnums)
  (set! initial-bitnums (decode-get-best-bits insn-list nil
                0 ; startbit
                8 ; max
                decode-bitsize
                lsb0?)))

    ; idk why I have to do this
    (set! state-base-insn-bitsize
      (lambda () max-bitsize)
    )

    ; because yay hard coding stuff
    (set! APPLICATION 'SID-SIMULATOR)

    ; All set.  gen-decoder does the hard part, we just print out the result. 
    (let* ((decode-code (gen-decoder insn-list initial-bitnums
            decode-bitsize
            "    " lsb0?
            (current-insn-lookup 'x-invalid #f)
            #f))
      (out (string-write
       "\
/* Split the instruction into chunks. stolen from binutils */

static inline uint64_t get_bits (const void *p, int bits, int big_p)
{
  const unsigned char *addr = (const unsigned char *) p;
  uint64_t data;
  int i;
  int bytes;

  if (bits % 8 != 0)
    abort ();

  data = 0;
  bytes = bits / 8;
  for (i = 0; i < bytes; i++)
    {
      int addr_index = big_p ? i : bytes - i - 1;

      data = (data << 8) | addr[addr_index];
    }

  return data;
}

static inline CGEN_INSN_WORD get_insn_value(unsigned char *buf, int length)
{
  int big_p = " (if (equal? 'big (cpu-insn-endian (current-cpu))) "1" "0") ";
  int insn_chunk_bitsize = " (number->string (cpu-insn-chunk-bitsize (current-cpu))) ";
  CGEN_INSN_WORD value = 0;

  if (insn_chunk_bitsize != 0 && insn_chunk_bitsize < length)
    {
      /* We need to divide up the incoming value into insn_chunk_bitsize-length
   segments, and endian-convert them, one at a time. */
      int i;

      /* Enforce divisibility. */ 
      if ((length % insn_chunk_bitsize) != 0)
  abort ();

      for (i = 0; i < length; i += insn_chunk_bitsize) /* NB: i == bits */
  {
    int bit_index;
    uint64_t this_value;

    bit_index = i; /* NB: not dependent on endianness; opposite of cgen_put_insn_value! */
    this_value = get_bits (& buf[bit_index / 8], insn_chunk_bitsize, big_p);
    value = (value << insn_chunk_bitsize) | this_value;
  }
    }
  else
    {
      value = get_bits (buf, length, big_p);
    }

  return value;
}

/* Analyze the current instruction.  */

int idaapi ana( void )
{
  /* temporary buffer */
  unsigned char buffer[" (number->string (quotient max-bitsize 8)) "];

  /* Result of decoder.  */
  @PREFIX@_INSN_TYPE itype;

  CGEN_INSN_WORD insn;
  CGEN_INSN_WORD entire_insn;
  ea_t pc;
  get_data_value(cmd.ea, (uval_t *)buffer, " (number->string (quotient max-bitsize 8)) ");
  insn = get_insn_value(buffer, " (number->string decode-bitsize) ");
  entire_insn = get_insn_value(buffer, " (number->string max-bitsize) ");
  pc = cmd.ea;
  {
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
    ; restore saved state
    (set! state-base-insn-bitsize saved-fn)
    (set! APPLICATION 'IDP)

    out
  ))
)

; Entry point.

(define (ana.cpp)
  (logit 1 "Generating ana.cpp ...\n")
  (sim-analyze-insns!)
  (let* ((all-insn (real-insns (current-insn-list))))
    (map analyze-insn-op-order! all-insn)

    ; Turn parallel execution support on if cpu needs it.
    (set-with-parallel?! (state-parallel-exec?))

    ; Tell the rtx->c translator we are the simulator.
    (rtl-c-config! #:rtl-cover-fns? #t)

    (string-write
     (gen-c-copyright "@ARCH@ IDP instructions"
        CURRENT-COPYRIGHT CURRENT-PACKAGE)
      "\
#include \"@arch@.hpp\"

 /* The size of an \"int\" needed to hold an instruction word.
   This is usually 32 bits, but some architectures needs 64 bits.  */
typedef CGEN_INSN_INT CGEN_INSN_WORD;
  \n"
     (lambda () (/gen-ana-fn all-insn
              (state-decode-assist)
              (current-arch-insn-lsb0?)))
     )
  )
)
