(hall-description
 (name "byggsteg")
 (prefix "")
 (version "0.1")
 (author "Josep Bigorra")
 (copyright (2024))
 (synopsis "")
 (description "")
 (home-page "")
 (license gpl3+)
 (dependencies `())
 (skip ())
 (files (libraries ((scheme-file "byggsteg") (directory "byggsteg" ())))
        (tests ((directory "tests" ())))
        (programs ((directory "scripts" ())))
        (documentation
         ((org-file "README")
          (symlink "README" "README.org")
          (text-file "HACKING")
          (text-file "COPYING")
          (directory "doc" ((texi-file "byggsteg")))))
        (infrastructure
         ((scheme-file "guix") (text-file ".gitignore") (scheme-file "hall")))))
