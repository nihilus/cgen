
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

(define (-gen-all-reg-list)
  (let ((counter 0))
    (apply append
      (map (lambda (hw)
        (let* ((decls (send hw 'gen-reg-list)))
          (if (not (null? decls))
            (let ((len (length decls)))
              (logit 3 "found register list " (->string decls) "\n")
              (set! decls (append (string-list "#define REGS_" (string-upcase (hw-enum hw)) "_BASE " (number->string counter) "\n") decls))
              (set! counter (+ counter len))
              decls
            )
            '()
          )
        ))
      (current-hw-list))
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
#include <ida.hpp>
#include <idp.hpp>

static const char *const RegNames[] =
{\n"
    -gen-all-reg-list
    "};\n"
   )
)
