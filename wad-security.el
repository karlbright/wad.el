;;; wad-security.el -*- lexical-binding: t; -*-

;; Ensure some sensible defaults for security. We pull in packages from the
;; the internet and we should be a little careful, if possible.
;;
;; `gnutls-min-prime-bits' is based on recommendations from
;; https://www.keylength.com/en/4/.

(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
	(concat "SECURE128:+SECURE192:-VERS-ALL"
		(if (and (not wad/is-windows)
			 (not (version< emacs-version "26.3"))
			 (>= libgnutls-version 30605))
		    ":+VERS-TLS1.3")
		":+VERS-TLS1.2"))
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    "gnutls-cli -p %p %h"))

(provide 'wad-security)
