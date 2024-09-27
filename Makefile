prepare-log-dir:
	sudo mkdir -p /var/log/byggsteg || true
	sudo mkdir -p /var/log/byggsteg/job-log || true
	sudo mkdir -p /var/log/byggsteg/job-failure || true
	sudo mkdir -p /var/log/byggsteg/job-success || true
	sudo mkdir -p /var/log/byggsteg/job-request || true
	sudo chown -R joe:users /var/log/byggsteg
dev:
	watchexec -r -e scm -- make server
server:
	GUILE_AUTO_COMPILE=0 guile -s scripts/server.scm
