## General

- machine needs to be run rootful. Specify that with --rootful=true when doing podman machine run, or after the fact podman machine set --rootful=true
- The set of images visible in rootful  is different than the ones visible when running as core (non-rootful)
- podman machine init --cpus 8 --memory=16384 ; so that the podman machine has some oomph
- podman qcow2 file gets too big. Not shrinking.

## Checkpoint

- checkpoint export and restore must be with sudo
- If you are going to restore make sure you do podman container prune first otherwise you will get a duplicate container id failure on restore
- podman machine set -rootful=true ; to allow checkpointing. Or include --rootful=true in the init command above
- checkpoint/restore without export is very fast but has a bug https://github.com/containers/podman/issues/13566

## Host files

- To mount local files, see https://dalethestirling.github.io/Macos-volumes-with-Podman/ but instead of core, use root since we are rootful
-- See https://github.com/containers/podman/pull/13453 suggests using -v /Users:/mnt/Users but this fails 
-- https://github.com/containers/podman/issues/13256 says to rebuilt qemu, but it is a brew formula. Existing qemu is at /opt/local/var/macports/software/qemu
-- Discussion https://github.com/containers/podman/issues/8016
--- https://github.com/afbjorklund/homebrew-core/blob/qemu-9p-darwin/Formula/qemu.rb the brew formula for a qemu with virtio 
- You can podman run --publish-all --privileged=true lsw2/lisp ; Seems you need --privileged=true to mount samba
-- then  sudo mount -t cifs -o username=alanr,password=XXXX //192.168.0.39/repos /mnt/
-- But you have to dismount it before checkpoint!
-- port mapping using -p 127.0.0.1:80:12345/tcp or --publish-all . Haven't been able to get port mapping working. Are the mappings to the machine?

## LSW
- lsw image is large - 1gig. gc doesn't help
- Checkpoint / restore inside machine is fast, few seconds. 
- Checkpoint export/import is slow, 10-20 seconds. Not sure why the time to do this changes.

## Linux
- Inside the image lsof -c java shows the open files for java only, not all its threads. Need to install util-linux for lsof

## Workarounds
- the podman machine loses time when the laptop is asleep. That's why a container you just created shows as being hours old
-- https://github.com/containers/podman/issues/11541#issuecomment-1038436498 solved it with sleepwatcher and chronyc. See ~/.wakeup
-- You can SMB mount to get access e.g.: sudo mount -t cifs -o username=<user>,password=<password> //$CHOSTNAME/repos /mnt/
-- CHOSTNAME is set with container run -e CHOSTNAME=`ifconfig en0 | grep "inet " | cut -d " " -f 2`  
