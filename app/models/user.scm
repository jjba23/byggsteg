(import (artanis mvc model))

(create-artanis-model user
                      (:deps)
                      (id integer
                          (#:primary-key #:auto-increment))
                      (username char-field
                                (#:not-null #:maxlen 256))
                      (password char-field
                                (#:not-null #:maxlen 512))
                      (salt char-field
                            (#:not-null #:maxlen 8))
                      (email char-field
                             (#:not-null #:maxlen 256)))
