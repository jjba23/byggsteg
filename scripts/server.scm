(use-modules (web server))

(load "../byggsteg/main.scm")
(use-modules (byggsteg-main))


(run-server byggsteg-http-server)
