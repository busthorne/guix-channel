(define-module (busthorne vault)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module (guix build utils)
  #:use-module (guix build-system go)
  #:use-module (guix git-download))

(define-public vault
  (package
    (name "vault")
    (version (git-version "1.15.0-beta" "0" "5fd5cc11aeaad38293a516c7f2dcae64d4ee1971"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hovertank3d/vault")
                    (commit "5fd5cc11aeaad38293a516c7f2dcae64d4ee1971")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wxhjkq02lmlrzas5dp92xb2hj19psacyknyiq0jb01hj0m88678"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/hashicorp/vault"
       #:go ,go-1.20))
    (home-page "https://github.com/hashicorp/vault")
    (synopsis "Vault")
    (description
     "@@strong{Please note}: We take Vault's security and our users trust very
seriously.  If you believe you have found a security issue in Vault, by
contacting us at @@url{mailto:security@@hashicorp.com,security@@hashicorp.com}.")
    (license license:mpl2.0)))
