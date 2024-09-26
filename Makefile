prepare-log-dir:
	sudo mkdir /var/log/byggsteg || true
	sudo chown -R joe:users /var/log/byggsteg
run:
	/usr/bin/env guile -s byggsteg/main.scm
