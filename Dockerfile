FROM ubuntu:16.04

# see files/keyboard file for documentation
COPY files/keyboard /etc/default/keyboard

# Use Zulu openjdk distribution to get java17
# https://unix.stackexchange.com/questions/263801/apt-get-fails-the-method-driver-usr-lib-apt-methods-https-could-not-be-found
# https://docs.azul.com/core/zulu-openjdk/install/debian
RUN apt-get -q update
RUN apt-get -yq install --no-install-recommends gnupg curl apt-transport-https ca-certificates && apt-get clean
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0xB1998361219BD9C9
RUN curl -O https://cdn.azul.com/zulu/bin/zulu-repo_1.0.0-3_all.deb
RUN apt-get install ./zulu-repo_1.0.0-3_all.deb

# Install Java 17 and other software. We need at least git, maven, and
# ant to build ABCL and checkout LSW source trees.  ladr4-apps, prover9,
# and z3 are reasoners

# Note that prover9, ladr4-apps are available in ubuntu 16 but not in
# ubuntu 20. I need to figure that out if I want to upgrade the base.

# util-linux for mount lslocks. mount for SMB, lslocks to understand why
# we need --file-locks on checkpoint.  net-utils is for netstat,
# iproute2 is for ip, inetutils-ping. To debug network issues.

RUN apt-get update && apt-get install --no-install-recommends -y zulu17-jdk-headless zulu17-jre-headless prover9 ladr4-apps z3 git maven ant openssh-server lsof sudo util-linux net-tools inetutils-ping iproute2 && apt-get clean 

# There's 30m to be gained by deleting /var/lib/apt/lists/ but then you can't use apt again. Do don't
# && rm -rf /var/lib/apt/lists/* 

# Add a user so we're not root. 
RUN useradd -ms /bin/bash lsw

# Dummy password, so what's the point?
RUN echo "lsw:lsw" | chpasswd

# Add groups sudo if you want to do any sudo. Added root in attempt to  write mounted volume without sudo, but failed.
RUN sudo usermod -aG sudo,root lsw

# Copy over separately built vampire. use --chmod vs chmod in another line as the latter lands up copying the file to the new layer.
COPY --chmod=555 docker-reasoners/vampire /usr/local/bin/

# Download the required repositories: abcl,lsw2,lilith,slime
# Remove the .git directories to save space, since we're not going to update
USER lsw
WORKDIR /home/lsw
RUN mkdir /home/lsw/repos/

# https://stackoverflow.com/questions/36996046/how-to-prevent-dockerfile-caching-git-clone

## ABCL
ADD https://api.github.com/repos/alanruttenberg/abcl/git/refs/heads/stage abcl-version.json
RUN cd repos && git clone --branch stage --depth 1 https://github.com/alanruttenberg/abcl.git && cd /home/lsw/repos/abcl && ant abcl-aio.jar && rm -rf build .git
## LSW
ADD https://api.github.com/repos/alanruttenberg/lsw2/git/refs/heads/owlapiv4 lsw2-version.json
RUN cd repos && git clone --branch owlapiv4 --depth 1 https://github.com/alanruttenberg/lsw2.git && cd lsw2/owl2/lib && rm -rf  Chainsaw-1.0-SNAPSHOT.jar  pelletcli/  Konclude*  factpp-native-1.6.4/  prefuse/  LICENSE-README/  jfact/  sparqldl-api1.0.0/  jfact-1.2.3.jar  telemetry-1.0.0.jar  org.semanticweb.hermit-1.3.8.413.jar  owlapi-4.2.6-dependencies/  uncommons-maths-1.2.2.jar  elk-owlapi-standalone-0.5.0-SNAPSHOT-bin.jar  owlapi-distribution-4.2.6.jar  explanation/  owlapitools/ && cd ../.. && rm -rf protege virtual-machine .git

## slime
ADD https://api.github.com/repos/alanruttenberg/slime/git/refs/heads/beta-2021-11 slime-version.json
RUN cd repos && git clone --branch beta-2021-11 --depth 1 https://github.com/alanruttenberg/slime.git && rm -rf slime/.git 

## lilith 
ADD https://api.github.com/repos/alanruttenberg/lilith/git/refs/heads/master lilith-version.json
RUN cd repos && git clone --branch master --depth 1 https://github.com/alanruttenberg/lilith.git && rm -rf lillith/.git

# Copy some dot files and emacs basics. Re-evaluate which, at some point
# Emacs dubious since we're not installing emacs yet.
COPY files/dot-emacs  /home/lsw/.emacs
COPY files/slime-init.el /home/lsw/emacs/slime-init.el
COPY files/font-indent.el /home/lsw/emacs/font-indent.el

# .abclrc is needed
COPY files/dot-abclrc  /home/lsw/.abclrc

# For x-windows. Not using X at the moment, so consider not
COPY files/dot-Xresources /home/lsw/.Xresources

# For some aliases, like alias h=history. Need to write one specific for
#  the container instead of just copying mine.
COPY files/dot-bashrc /home/lsw/.bashrc

# Link all the asdf system definitions into asdf-source-registry, which
# is where .abclrc says to look for them. Note that there are a couple
# of duplicates, which fail on the second time. Fortunately the right
# ones link first.

RUN mkdir /home/lsw/repos/asdf-source-registry
RUN find . -name \*asd -exec ln -s `pwd`/{} /home/lsw/repos/asdf-source-registry/ \; 
 
# Add LSW's bin to PATH
ENV PATH="/home/lsw/repos/lsw2/bin:${PATH}"

# Don't really need to do this, since abcl and lsw are on PATH
WORKDIR /home/lsw/repos/lsw2/bin

# Install quicklisp. Add the (sleep 2) at the end otherwise lisp quits EOF before quicklisp installed
RUN ./abcl -- --foreground --no-quicklisp --eval '(require :quicklisp-abcl)' --eval '(sleep 2)'

# Run LSW so everything gets compiled. Use --foreground so that quicklisp gets setup up properly
RUN lsw -- --foreground

# compile swank
RUN abcl -- --foreground --load /home/lsw/repos/slime/swank-loader.lisp --eval '(progn (swank-loader::load-swank) (quit))'

#ENTRYPOINT lsw --load /home/lsw/repos/slime/swank-loader.lisp --eval '(swank-loader::load-swank)' --eval '(progn (in-package :logic) (setq *print-case* :downcase))' --eval '(when (probe-file "/local/init.lisp") (load "/local/init.lisp"))'
#ENTRYPOINT lsw -- --foreground
USER lsw
ENTRYPOINT /bin/bash