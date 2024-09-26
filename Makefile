prepare-log-dir:
	sudo mkdir /var/log/byggsteg || true
	sudo chown -R joe:users /var/log/byggsteg
server:
	/usr/bin/env guile -s scripts/server.scm
