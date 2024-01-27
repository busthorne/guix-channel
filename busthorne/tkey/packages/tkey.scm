(define-module (busthorne tkey packages tkey)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (busthorne tkey packages gcc))

(define-public tkey-libs
  (let* ((commit "296b5c578c56740c4762bb7bfe9149da1a59cf75")
         (revision "1"))
    (package
      (name "tkey-libs")
      (version (git-version "1.0.0" revision commit))
      (build-system gnu-build-system)
      (source
       (origin
         (method url-fetch)
         (uri "file:///home/iskrim/guix-master/src.tar.gz")
         (sha256
          (base32 "0{52}"))))
      (arguments
       `(#:tests? #f ;No tests exist
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (add-before 'build 'setenv
                      (lambda* (#:key inputs #:allow-other-keys)
                        (setenv "CC" "riscv32-unknown-elf-gcc")
                        (setenv "CXX" "riscv32-unknown-elf-g++")
                        (setenv "AS" "riscv32-unknown-elf-gcc")
                        (setenv "AR" "riscv32-unknown-elf-ar")
                        (setenv "OBJCOPY" "riscv32-unknown-elf-objcopy")
                        #t))
                    (replace 'install
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (libcommon (string-append out "/libcommon"))
                               (libcrt0 (string-append out "/libcrt0"))
                               (libcryptonite (string-append out
                                                             "/libcryptonite"))
                               (include (string-append out "/include")))
                          (mkdir-p libcommon)
                          (mkdir-p libcrt0)
                          (mkdir-p libcryptonite)
                          (mkdir-p include)

                          (install-file "libcommon/libcommon.a" libcommon)
                          (install-file "libcrt0/libcrt0.a" libcrt0)
                          (install-file "cryptonite/libcryptonite.a"
                                        libcryptonite)
                          (install-file "app.lds" out)
                          (for-each (lambda (file)
                                      (let* ((target-path (string-append
                                                           include "")))
                                        (mkdir-p (dirname target-path))
                                        (install-file file target-path)))
                                    (find-files "include" ".*"))
                          (for-each (lambda (file)
                                      (let* ((target-path (string-append
                                                           include
                                                           "/cryptonite")))
                                        (mkdir-p (dirname target-path))
                                        (install-file file target-path)))
                                    (find-files "cryptonite" ".*"))
                          #t))))))
      (native-inputs `(("binutils" ,riscv32-unknown-elf-binutils)
                       ("gcc" ,gcc-riscv32-unknown-elf-12)))
      (inputs '())
      (synopsis "tkey-libs with cryptonite ported to riscv32")
      (description "")
      (home-page "https://github.com/busthorne/tkey-libs")
      (license license:gpl2+))))

(define* (make-tkey-demo name
                         origin
                         #:key (version "0.0")
                         (synopsis "")
                         (description "")
                         (home-page "example.org")
                         (license license:gpl2+))
  (package
    (name name)
    (version version)
    (build-system gnu-build-system)
    (source
     origin)
    (arguments
     `(#:tests? #f ;No tests exist
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'setenv
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((tkey-libs (assoc-ref inputs "tkey-libs")))
                        (setenv "CC" "riscv32-unknown-elf-gcc")
                        (setenv "CXX" "riscv32-unknown-elf-g++")
                        (setenv "AS" "riscv32-unknown-elf-gcc")
                        (setenv "OBJCOPY" "riscv32-unknown-elf-objcopy")
                        (setenv "LIBDIR" tkey-libs)) #t))
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (install-file "device-demo/app.bin" out) #t))))))
    (native-inputs `(("binutils" ,riscv32-unknown-elf-binutils)
                     ("gcc" ,gcc-riscv32-unknown-elf-12)
                     ("tkey-libs" ,tkey-libs)))
    (synopsis synopsis)
    (description description)
    (home-page home-page)
    (license license)))

(define-public dstu4145-demo
  (make-tkey-demo "dstu4145-demo"
                  (origin
                    (method url-fetch)
                    (uri "dstu4145-demo.tar.gz")
                    (sha256 (base32
                             "0000000000000000000000000000000000000000000000000000")))))

(define-public dstu28147-demo
  (make-tkey-demo "dstu28147-demo"
                  (origin
                    (method url-fetch)
                    (uri "dstu28147-demo.tar.gz")
                    (sha256 (base32
                             "0000000000000000000000000000000000000000000000000000")))))

(define-public dstu7564-demo
  (make-tkey-demo "dstu7564-demo"
                  (origin
                    (method url-fetch)
                    (uri "dstu7564-demo.tar.gz")
                    (sha256 (base32
                             "0000000000000000000000000000000000000000000000000000")))))
