; IDP module generator for IDA Pro
; Copyright (C) 2000, 2009 Red Hat, Inc.
; This file is part of CGEN.
;
; This is a standalone script, we don't load anything until we parse the
; -s argument (keeps reliance off of environment variables, etc.).

; Load the various support routines.

(define (load-files srcdir)
  (load (string-append srcdir "/read.scm"))
  (load (string-append srcdir "/desc.scm"))
  (load (string-append srcdir "/desc-cpu.scm"))
  (load (string-append srcdir "/idp.scm"))
  (load (string-append srcdir "/idp-ins.scm"))
)

(define opc-arguments
  (list
   (list "-A" "file" "generate ana.cpp in <file>"
	 #f
	 (lambda (arg) (file-write arg ana.cpp)))
   (list "-E" "file" "generate emu.cpp in <file>"
	 #f
	 (lambda (arg) (file-write arg emu.cpp)))
   (list "-I" "file" "generate ins.cpp in <file>"
	 #f
	 (lambda (arg) (file-write arg ins.cpp)))
   (list "-J" "file" "generate ins.hpp in <file>"
	 #f
	 (lambda (arg) (file-write arg ins.hpp)))
   (list "-H" "file" "generate $arch.hpp in <file>"
	 #f
	 (lambda (arg) (file-write arg arch.hpp)))
   (list "-O" "file" "generate out.cpp in <file>"
	 #f
	 (lambda (arg) (file-write arg out.cpp)))
   (list "-R" "file" "generate reg.cpp in <file>"
	 #f
	 (lambda (arg) (file-write arg reg.cpp)))
   )
)

; Kept global so it's available to the other .scm files.
(define srcdir ".")

; Scan argv for -s srcdir.
; We can't process any other args until we find the cgen source dir.
; The result is srcdir.
; We assume "-s" isn't the argument to another option.  Unwise, yes.
; Alternatives are to require it to be the first argument or at least preceed
; any option with a "-s" argument, or to put knowledge of the common argument
; set and common argument parsing code in every top level file.

(define (find-srcdir argv)
  (let loop ((argv argv))
    (if (null? argv)
	(error "`-s srcdir' not present, can't load cgen"))
    (if (string=? "-s" (car argv))
	(begin
	  (if (null? (cdr argv))
	      (error "missing srcdir arg to `-s'"))
	  (cadr argv))
	(loop (cdr argv))))	
)

; Main routine, parses options and calls generators.

(define (cgen-idp argv)
  (let ()

    ; Find and set srcdir, then load all Scheme code.
    ; Drop the first argument, it is the script name (i.e. argv[0]).
    (set! srcdir (find-srcdir (cdr argv)))
    (set! %load-path (cons srcdir %load-path))
    (load-files srcdir)

    (display-argv argv)

    (cgen #:argv argv
	  #:app-name "idp"
	  #:arg-spec opc-arguments
	  #:init idp-init!
	  #:finish idp-finish!
	  #:analyze idp-analyze!)
    )
)

(cgen-idp (program-arguments))
