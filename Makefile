.PHONY: prepare-dirs clean-dirs create server

prepare-dirs:
	mkdir -p /var/log/byggsteg || true
	mkdir -p /var/log/byggsteg/job-log || true
	mkdir -p /var/log/byggsteg/job-failure || true
	mkdir -p /var/log/byggsteg/job-clone || true
	mkdir -p /var/log/byggsteg/job-success || true
	mkdir -p /var/log/byggsteg/job-detail || true
	mkdir -p /var/log/byggsteg/profile || true
clean-dirs:
	rm -rfv /var/log/byggsteg/* || true
create:
	sudo mkdir /var/log/byggsteg || true
	sudo chown $(whoami) /var/log/byggsteg	
server:
	art work
