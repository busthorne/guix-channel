(define-module (busthorne tkey packages tkey)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (guix build-system gnu)
  #:use-module (busthorne tkey packages gcc))

(define-public tkey-libs
  (let ((commit "296b5c578c56740c4762bb7bfe9149da1a59cf75")
        (revision "1"))
    (package
      (name "tkey-libs")
      (version (git-version "1.0.0" revision commit))
      (build-system gnu-build-system)
      (source (origin
        (method git-fetch)
           (uri (git-reference
                      (url "https://github.com/busthorne/tkey-libs/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1v2ybd9xpc9q9dgzrd3f25mgvkwq42b02d99jnzsa9mni7b7psky"))))
      (arguments
       `(#:tests? #f                   ; No tests exist
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'setenv
             (lambda _
               (setenv "CC" "riscv32-unknown-elf-gcc")
               (setenv "CXX" "riscv32-unknown-elf-g++")
               (setenv "AS" "riscv32-unknown-elf-as")
	       (setenv "OBJCOPY" "riscv32-unknown-elf-objcopy")
               #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libcommon (string-append out "/libcommon"))
		    (libcrt0 (string-append out "/libcrt0"))
		    (libcryptonite (string-append out "/libcryptonite")))
               (mkdir-p libcommon)
               (mkdir-p libcrt0)
               (mkdir-p libcryptonite)

               (install-file "libcommon/libcommon.a" libcommon)
               (install-file "libcrt0/libcrt0.a" libcrt0)
               (install-file "cryptonite/libcryptonite.a" libcryptonite)
               #t))))))
    (native-inputs
     `(("binutils" ,riscv32-unknown-elf-binutils)
       ("gcc" ,gcc-riscv32-unknown-elf-12)))
    (inputs
     `())
    (synopsis "tkey-libs with cryptonite ported to riscv32")
    (description "")
    (home-page "https://github.com/busthorne/tkey-libs")
    (license license:gpl2+))))
