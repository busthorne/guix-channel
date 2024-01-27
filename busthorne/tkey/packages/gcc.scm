(define-module (busthorne tkey packages gcc)
  #:use-module (srfi srfi-26)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages check)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages bootloaders))

(define-public riscv32-unknown-elf-binutils
  (let ((xbinutils (cross-binutils "riscv32-unknown-elf")))
    (package
      (inherit xbinutils)
      (name "riscv32-unknown-elf-binutils")
      (version "2.38")
      (arguments
       (substitute-keyword-arguments (package-arguments xbinutils)
         ((#:tests? tests
           '())
          #f)
         ((#:configure-flags flags)
          #~(cons* "--enable-multilib" "--disable-gdb" "--disable-sim"
                   #$flags))))
      (native-inputs (append (list texinfo bison flex gmp dejagnu)
                             (package-native-inputs xbinutils)))
      (home-page "")
      (synopsis "binutils for riscv32"))))

(define-public gcc-riscv32-unknown-elf-12
  (let ((xgcc (cross-gcc "riscv32-unknown-elf"
                         #:xgcc gcc-12
                         #:xbinutils riscv32-unknown-elf-binutils)))
    (package
      (inherit xgcc)
      (name "gcc-riscv32-unknown-elf")
      (native-inputs (modify-inputs (package-native-inputs xgcc)
                       (prepend python-wrapper)))
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-before 'configure 'fix-genmultilib
                (lambda* (#:key inputs #:allow-other-keys)
                  (patch-shebang "gcc/genmultilib")
                  ;; (substitute* "gcc/config/riscv/multilib-generator"
                  ;; (("#!/usr/bin/env python") (string-append (assoc-ref inputs "python") "/bin/python3" )))
                  ))
              (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((gcc (assoc-ref inputs "gcc")))
                    ;; Remove the default compiler from CPLUS_INCLUDE_PATH to
                    ;; prevent header conflict with the GCC from native-inputs.
                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join (delete (string-append gcc
                                                  "/include/c++")
                                                 (string-split (getenv
                                                                "CPLUS_INCLUDE_PATH")
                                                               #\:)) ":"))
                    (format #t
                     "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                     (getenv "CPLUS_INCLUDE_PATH")) #t)))))
         ((#:configure-flags flags)
          #~(append (list "--target=riscv32-unknown-elf"
                     "--disable-threads"
                     "--enable-languages=c"
                     "--with-system-zlib"
                     "--enable-tls"
                     "--enable-multilib"
                     "--with-native-system-header-dir=/include"
                     "--disable-libmudflap"
                     "--disable-libssp"
                     "--disable-libquadmath"
                     "--disable-libgomp"
                     "--disable-nls"
                     "--disable-tm-clone-registry"
                     "--with-multilib-generator='rv32i-ilp32--;rv32ic-ilp32--;rv32iczmmul-ilp32--;rv32im-ilp32--;rv32imc-ilp32--'"
                     "--with-abi=ilp32"
                     "--with-arch=rv32ic"
                     "--with-isa-spec=2.2")
                    (delete "--disable-multilib"
                            #$flags)))
         ((#:make-flags flags)
          #~(append '("CFLAGS_FOR_TARGET=-g -O2")
                    #$flags))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("riscv32-unknown-elf/include")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("riscv32-unknown-elf/lib")))))
      (home-page "https://gcc.gnu.org")
      (synopsis "GCC"))))

(define (riscv32-unknown-elf-toolchain xgcc)
  (package
    (name "riscv32-unknown-elf-toolchain")
    (version (package-version xgcc))
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union)
                  (guix build utils))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union)
                                (guix build utils))
                   (let ((out (assoc-ref %outputs "out")))
                     (mkdir-p out)
                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build (string-append out "/riscv32-unknown-elf")
                                     directories) #t))))))
    (propagated-inputs `(("binutils" ,riscv32-unknown-elf-binutils)
                         ("gcc" ,xgcc)))
    (synopsis "Complete GCC toolchain for riscv32")
    (description "GCC")
    (home-page (package-home-page xgcc))
    (license (package-license xgcc))))
	
(define-public riscv32-unknown-elf-toolchain-12
  (riscv32-unknown-elf-toolchain gcc-riscv32-unknown-elf-12))
