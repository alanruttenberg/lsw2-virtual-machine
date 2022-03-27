## General

- machine needs to be run rootful. Specify that with --rootful=true when doing podman machine run, or after the fact podman machine set --rootful=true
- The set of images and containers visible in rootful  is different than the ones visible when running as core (non-rootful)
- podman machine init --cpus 8 --memory=16384 ; so that the podman machine has some oomph
- podman qcow2 file gets too big. Not shrinking.
- To use a new version of podman, temporarily https://github.com/containers/podman/issues/13543#issuecomment-1071175619

## Checkpoint

- checkpoint export and restore must be with sudo
- If you are going to restore via import make sure you do podman container prune first otherwise you will get a duplicate container id failure on restore. That's because when you have checkpointed the container it's still there. So when you import it, you are importing it to the same ID and: bang!
- podman machine set -rootful=true ; to allow checkpointing. Or include --rootful=true in the init command above
- checkpoint/restore without export is very fast. However the constraint on being able to restart over and over is that no file in the container can change size. First thing is to run java with -XX:-UsePerfData, which prevents it from writing a file that will land up changing. But you also need to be careful about other places a write might occur. See https://github.com/containers/podman/issues/13566 for a discussion. It is probably worth thinking about having all the software on a separate volume, although we then need to see whether the files reopen on container restart.

## Host files

- The best way to work it, at the moment, is to mount a volume inside the podman machine, which can then be mounted inside the container. See https://github.com/containers/podman/pull/13453 . For example, use -v /Users:/mnt/Users when doing machine init, then -v /mnt/Users:/Users when running. Note: If you want to write then run the container with --privileged=true and, within the container, sudo to write, since the mount is owned by root:root and is chmod u+rw,g+r,o+r . TBD: Look into how write without having to do sudo. There are security issues to worry about here since by this method you can potentially write anywhere you mount. What may be the best way would be to only mount one's home directory in the machine.

Other notes
- To mount local files, see https://dalethestirling.github.io/Macos-volumes-with-Podman/ but instead of core, use root since we are rootful
-- https://github.com/containers/podman/issues/13256 says to rebuilt qemu, but it is a brew formula. Existing qemu is at /opt/local/var/macports/software/qemu
-- Discussion https://github.com/containers/podman/issues/8016
--- https://github.com/afbjorklund/homebrew-core/blob/qemu-9p-darwin/Formula/qemu.rb the brew formula for a qemu with virtio 

- You can podman run --publish-all --privileged=true lsw2/lisp ; Seems you need --privileged=true to mount samba
-- then  sudo mount -t cifs -o username=alanr,password=XXXX //192.168.0.39/repos /mnt/. But you have to dismount it before checkpoint!

-- You can SMB mount to get access e.g.: sudo mount -t cifs -o username=<user>,password=<password> //$CHOSTNAME/repos /mnt/

## Port mapping
-- port mapping using -p 127.0.0.1:80:12345/tcp or --publish-all . Haven't been able to get port mapping working. Are the mappings to the machine?

## LSW
- lsw image is large - 1gig. gc doesn't help
- Checkpoint / restore inside machine is fast, few seconds. 
- Checkpoint export/import is slow, 10-20 seconds. Not sure why the time to do this changes.
- There's a suggestion to use --compress=none. Some other ideas in https://github.com/containers/podman/issues/13566

## Linux
- Inside the image lsof -c java shows the open files for java only, not all its threads. Need to install util-linux for lsof. Other useful packages are:
-- util-linux for mount and lslocks
-- net-utils for netstat
-- iproute2 for ip 
-- inetutils-ping for ping

## Workarounds
- the podman machine loses time when the laptop is asleep. That's why a container you just created shows as being hours old
-- https://github.com/containers/podman/issues/11541#issuecomment-1038436498 solved it with sleepwatcher and chronyc. See ~/.wakeup
-- CHOSTNAME is set with container run -e CHOSTNAME=`ifconfig en0 | grep "inet " | cut -d " " -f 2`  

## Building
- Any time you apt-get install use --no-install-recommends and add, on the same RUN link && apt-get clean to get rid of the temporary files. 
