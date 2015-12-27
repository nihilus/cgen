
; Given a register index, get the name if defined
(define (-get-reg-name hw idx)
  (let ((indices (hw-indices hw)))
    (if (and indices (class-instance? <keyword> indices))
      (let ((kwd (find-first (lambda (value)
          (and (number? (cadr value)) (= (cadr value) idx))) 
        (kw-values indices))))
        (string-append
          "  \""
          (if kwd
            (->string (car kwd))
            ""
          )
          "\", \n"
        )
      )
      "  \"\", \n"
    )
  )
)

; Default generate

(method-make!
 <hardware-base> 'gen-reg-list
 (lambda (self) '())
)

; Generate for registers

(method-make!
  <hw-register> 'gen-reg-list
    (lambda (self)
      (if (scalar? (hw-type self))
        '("  \"\", \n") ; single register, no keyword
        (map (lambda (idx) (-get-reg-name self idx)) (iota (hw-num-elms self)))
      )
    )
)

(define (-gen-all-reg-list hw-list)
  (let ((counter 0))
    (apply append
      (map (lambda (hw)
          (send hw 'gen-reg-list)
        )
      hw-list)
    )
  )
)

(define (-gen-reg-base-defs hw-list)
  (let ((counter 0)
    (defs '()))
      (append
        (map (lambda (hw)
          (let* ((decls (send hw 'gen-reg-list)))
            (if (not (null? decls))
              (let ((len (length decls))
                (idx counter))
                (logit 3 "found register list " (->string decls) "\n")
                (set! counter (+ counter len))
                (string-append "#define REGS_" (string-upcase (hw-enum hw)) "_BASE " (number->string idx) "\n")
              )
              ""
            )
          ))
          hw-list)
        (string-list "#define REGS_COUNT " (number->string counter) "\n")
      )
  )
)

; Entry point.

(define (reg.cpp)
  (logit 1 "Generating reg.cpp ...\n")

  ;(sim-analyze-insns!)

  ; Turn parallel execution support on if cpu needs it.
  ;(set-with-parallel?! (state-parallel-exec?))

  ; Tell the rtx->c translator we are the simulator.
  ;(rtl-c-config! #:rtl-cover-fns? #t)

  (string-write
   (gen-c-copyright "@ARCH@ IDP instructions"
      CURRENT-COPYRIGHT CURRENT-PACKAGE)
   "\
/* This file is only to be included by whoever defines LPH! */

static const char *const RegNames[] =
{\n"
    (-gen-all-reg-list (current-hw-list))
    "};\n"
   )
)

(define (arch.hpp)
  (logit 1 "Generating @arch@.hpp ...\n")

  ;(sim-analyze-insns!)

  ; Turn parallel execution support on if cpu needs it.
  ;(set-with-parallel?! (state-parallel-exec?))

  ; Tell the rtx->c translator we are the simulator.
  ;(rtl-c-config! #:rtl-cover-fns? #t)

  (string-write
   (gen-c-copyright "@ARCH@ IDP hardware defines"
      CURRENT-COPYRIGHT CURRENT-PACKAGE)
   "\
#ifndef __@ARCH@_HPP
#define __@ARCH@_HPP

#include <ida.hpp>
#include <idp.hpp>
#include <ua.hpp>
#include <name.hpp>
#include <auto.hpp>
#include <bytes.hpp>
#include <queue.hpp>
#include <lines.hpp>
#include <loader.hpp>
#include <offset.hpp>
#include <segment.hpp>
#include <kernwin.hpp>
#include \"ins.hpp\"

/* needed for cgen.h */
#define CGEN_ARCH @arch@
#define CGEN_SYM(s) @arch@##_cgen_##s

/* for referring to operand type in cmd */
#define cgen_optype specval_shorts.low

/* Offsets for register names by cgen hw name */
\n"
    (-gen-reg-base-defs (current-hw-list))
    gen-hw-decls
    gen-operand-decls
    "\
/* cgen.h must be included after all that decls */
#include \"cgen.h\"

/* IDP exports */

int  idaapi ana( void );
int  idaapi emu( void );
void idaapi out( void );
bool idaapi outop( op_t &op );
void idaapi @arch@_data(ea_t ea);

#endif\n"
   )
)
