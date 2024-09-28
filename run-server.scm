(use-modules (ice-9 popen))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format))
(use-modules (ice-9 string-fun))
(use-modules (ice-9 iconv))


(load "byggsteg/preferences.scm")
(load "byggsteg/process.scm")
(load "byggsteg/base16.scm")
(load "byggsteg/job.scm")
(load "byggsteg/log.scm")
(load "byggsteg/html.scm")
(load "byggsteg/server.scm")
(load "byggsteg/main.scm")

(use-modules (web server))
(use-modules (byggsteg-main))


(run-server byggsteg-http-server)


