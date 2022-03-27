# https://stackoverflow.com/a/5907409 how-does-make-app-know-default-target-to-build-if-no-target-is-specified
.DEFAULT_GOAL := list

# https://github.com/containers/podman/issues/11541 clock problem
# for now: podman machine ssh sudo hwclock --hctosys
build: # doc
	cp ~/.abclrc files/dot-abclrc
	bin/sync-machine-time
	podman  build -f Dockerfile -t "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)" .
	podman tag  "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)" lsw2/lisp:latest

squashed: 
	podman build --squash  -t "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)-s" .
	podman tag  "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)" lsw2/lisp:latest

run:
	bin/sync-machine-time
# This is broken in podman, or rather it doesn't do what you think it does. the host volume mounted is from the podman machine.
#	podman run -it -v `pwd`:"/local" lsw2/lisp
# CHOSTNAME in the container will be ip address of the host
#	podman run -it --publish-all --privileged=true -e CHOSTNAME=`ifconfig en0 | grep "inet " | cut -d " " -f 2`  -v /mnt/Users:/Users lsw2/lisp   # -p 10.2.0.2:12345:22/tcp
	podman run -it --privileged=true -e CHOSTNAME=`ifconfig en0 | grep "inet " | cut -d " " -f 2`  -v /mnt/Users:/Users lsw2/lisp   

# https://www.thegeekstuff.com/2010/07/bash-string-manipulation/
export-checkpoint:
	ID=`podman container list | grep lsw | cut -d " " -f 1`; sudo podman container checkpoint --file-locks --tcp-established $$ID -e lsw-checkpoint-$$ID.tar.gz

# If we are checkpointing, get rid of the old exited containers
checkpoint:
	podman container ls -f "status=exited" | grep lsw2 | cut -d " " -f 1 | xargs podman rm
	ID=`podman container list | grep lsw | cut -d " " -f 1`; sudo podman container checkpoint --keep --file-locks --tcp-established $$ID 

prune:
	podman container prune -f
	podman image prune -f


restore-from-export:
	bin/sync-machine-time
	podman container prune -f
	sudo podman container restore --keep --file-locks --tcp-established --import `ls -t -1 lsw-checkpoint-*.tar.gz | head -1`

resume:
	bin/sync-machine-time
	ID=`podman container list --all | grep lsw | cut -d " " -f 1`; sudo podman container restore  --keep --file-locks --tcp-established $$ID; podman container attach $$ID

init:
	podman machine init --cpus 8 --memory 16384 --rootful -v /Users:/mnt/Users

qcow-size:
	ls -l -h /Users/alanr/.local/share/containers/podman/machine/qemu/*.qcow2 

# http://www.microhowto.info/howto/suppress_echoing_of_commands_in_a_makefile.html
# https://stackoverflow.com/questions/4219255/how-do-you-get-the-list-of-targets-in-a-makefile
.PHONY: list
list:
	@echo Targets:
	@LC_ALL=C $(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'
