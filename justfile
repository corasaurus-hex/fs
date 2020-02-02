# Local Variables:
# mode: makefile
# End:
# vim: set ft=make :

# test in clj in docker
default:
	just docker-test clj

# build docker images
build:
	docker-compose build

# test in <type:clj|bb> outside of docker
test type:
	script/{{type}}/test

# test in <type:clj|bb> in docker
docker-test type:
	docker-compose run --rm test-{{type}}

# start the clj nrepl server outside of docker
nrepl-server:
	script/clj/nrepl-server

# start the clj nrepl server in docker
docker-nrepl-server:
	docker-compose up -d nrepl-server

# list just tasks
ls:
	just --list
