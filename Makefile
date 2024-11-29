
prepare-dirs:
	sudo mkdir -p /var/log/byggsteg || true
	sudo mkdir -p /var/log/byggsteg/job-log || true
	sudo mkdir -p /var/log/byggsteg/job-failure || true
	sudo mkdir -p /var/log/byggsteg/job-clone || true
	sudo mkdir -p /var/log/byggsteg/job-success || true
	sudo mkdir -p /var/log/byggsteg/job-detail || true
	sudo mkdir -p /var/log/byggsteg/profile || true
clean-dirs:
	sudo rm -rfv /var/log/byggsteg/* || true
server:
	art work
