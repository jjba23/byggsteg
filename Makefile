prepare-log-dir:
	sudo mkdir /var/log/byggsteg || true
	sudo chown -R joe:users /var/log/byggsteg
dev:
	watchexec -r -e scm -- make server
server:
	guile -s scripts/server.scm
