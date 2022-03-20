# Virtualizing LSW

You must install [podman](https://podman.io/_ first, at least version 4. On a mac I use [MacPorts](https://www.macports.org/)```sudo port install podman``` 

## Build the image
```
make image
```

## Run the image
To run LSW and interact with a repl
```
make run
```
You will be put into a shell. Run ```./lsw``` and wait for the prompt.

## Save the image
To save the image to a file, for instance to upload it somewhere
```
make save
```

## Checkpoint 
Checkpoint saves the state of a running LSW. When at the prompt, in another terminal, 
```
make checkpoint
```
This will make a file called lsw-checkpoint-<container-id>.tar.gz

Note, this is [broken](https://github.com/containers/podman/issues/12053) for the moment.

## Resume a checkpoint 
```
make restore
```
This will resume the most recent saved checkpoint. Note that you can have only one copy running. 
If the container is stopped but not pruned you will get an error. In that case use 
```
make resume
```
Otherwise you can get rid of old non-running containers with 
```
podman container prune 
```

## Inside emacs

(instructions out of date)

To run LSW from docker image inside your local emacs:
 - put https://github.com/emacs-pe/docker-tramp.el somewhere, add the path to the emacs load-path, and (require 'docker-tramp)
 - clone https://github.com/daewok/slime-docker and add the path you cloned in to load-path, and (require 'slime-docker)
 - Configure slime-docker with 
 ```lisp
 (setq slime-docker-implementations '((lsw ("/home/lsw/repos/lsw2/bin/lsw") :image-name "lsw2/lisp")))
 ```
 - add slime-tramp as one of your slime-contribs

Run it with M-x slime-docker

This is relatively new tech for me. Please contact me if try but have trouble with these instructions.

### Notes

- http://kartoza.com/en/blog/how-to-run-a-linux-gui-application-on-osx-using-docker/ gives instructions on how to set
  things up so that X windows will open on a mac running Xquartz, so show-classtree works. Summary,
  (assuming you've installed brew)
```bash
    brew install socat
    socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\"&  # (do that only once per login session)
```
in .emacs
```lisp
(setq slime-docker-implementations `((lsw ("/home/lsw/repos/lsw2/bin/lsw") :image-name "lsw2/lisp" :env (("DISPLAY" . ,(concat (get-ip-address) ":0"))))))
```
I added the below definition of get-ip-address to my .emacs, which seems to work. YMMV 
```lisp
(defun get-ip-address ()
   (substring (shell-command-to-string
       "ifconfig | grep 'inet ' | grep -v 127.0.0.1 | head -1 | sed 's/.*inet \\(\\([0-9\\.]\\)*\\).*/\\1/'")
	     0 -1))
```

## Of note

The CRaC (Coordinated Restore at Checkpoint) Project researches coordination of Java programs with mechanisms to checkpoint (make an image of, snapshot) a Java instance while it is executing. Restoring from the image could be a solution to some of the problems with the start-up and warm-up times. The primary aim of the Project is to develop a new standard mechanism-agnostic API to notify Java programs about the checkpoint and restore events. Other research activities will include, but will not be limited to, integration with existing checkpoint/restore mechanisms and development of new ones, changes to JVM and JDK to make images smaller and ensure they are correct.

https://github.com/CRaC
https://github.com/CRaC/docs/blob/master/STEP-BY-STEP.md
