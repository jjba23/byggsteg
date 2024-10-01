
prepare-dirs:
	mkdir -p /var/log/byggsteg || true
	mkdir -p /var/log/byggsteg/job-log || true
	mkdir -p /var/log/byggsteg/job-failure || true
	mkdir -p /var/log/byggsteg/job-clone || true
	mkdir -p /var/log/byggsteg/job-success || true
	mkdir -p /var/log/byggsteg/job-detail || true
clean-dirs:
	sudo rm -rfv /var/log/byggsteg/* || true
dev:
	watchexec -r -e scm -- make server
server:
	GUILE_AUTO_COMPILE=0 guile run-server.scm

