FROM ubuntu:16.04

# see files/keyboard file for documentation
COPY files/keyboard /etc/default/keyboard

# Use Zulu openjdk distribution to get java17
# https://unix.stackexchange.com/questions/263801/apt-get-fails-the-method-driver-usr-lib-apt-methods-https-could-not-be-found
# https://docs.azul.com/core/zulu-openjdk/install/debian
RUN apt-get -q update
RUN apt-get -yq install gnupg curl apt-transport-https ca-certificates
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0xB1998361219BD9C9
RUN curl -O https://cdn.azul.com/zulu/bin/zulu-repo_1.0.0-3_all.deb
RUN apt-get install ./zulu-repo_1.0.0-3_all.deb

# Install Java 17
RUN apt-get update
RUN apt-get install --no-install-recommends -y zulu17-jdk-headless zulu17-jre-headless

# We need at least git, maven, and ant as well as the reasoners prover9 and z3
# Note that prover9, ladr4-apps are available in ubuntu 16 but need to figure out how to do it with ubuntu 20
# util-linux for mount lslocks
# net-utils for netstat
# iproute2 for ip 
RUN apt-get install --no-install-recommends -y prover9 ladr4-apps z3 git maven ant openssh-server lsof sudo util-linux net-utils iproute2 && apt-get clean

# Don't do this if you want to install later
# && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Add a user so we're not root
RUN useradd -ms /bin/bash lsw
RUN echo "lsw:lsw" | chpasswd
RUN sudo usermod -aG sudo lsw

# Copy over separately built vampire
COPY docker-reasoners/vampire /usr/local/bin/
RUN chmod a+x /usr/local/bin/vampire

# Download the required repositories: abcl,lsw2,lilith,slime
USER lsw
WORKDIR /home/lsw
RUN mkdir /home/lsw/repos/
# https://stackoverflow.com/questions/36996046/how-to-prevent-dockerfile-caching-git-clone
ADD https://api.github.com/repos/alanruttenberg/abcl/git/refs/heads/stage abcl-version.json
RUN cd repos && git clone --branch stage --depth 1 https://github.com/alanruttenberg/abcl.git 
RUN cd /home/lsw/repos/abcl && ant abcl-aio.jar
ADD https://api.github.com/repos/alanruttenberg/lsw2/git/refs/heads/owlapiv4 lsw2-version.json
RUN cd repos && git clone --branch owlapiv4 --depth 1 https://github.com/alanruttenberg/lsw2.git 
ADD https://api.github.com/repos/alanruttenberg/slime/git/refs/heads/beta-2021-11 slime-version.json
RUN cd repos && git clone --branch beta-2021-11 --depth 1 https://github.com/alanruttenberg/slime.git
ADD https://api.github.com/repos/alanruttenberg/lilith/git/refs/heads/master lilith-version.json
RUN cd repos && git clone --branch master --depth 1 https://github.com/alanruttenberg/lilith.git

# Copy some dot files and emacs basics. Re-evaluate which, at some point
COPY files/dot-emacs  /home/lsw/.emacs
COPY files/dot-abclrc  /home/lsw/.abclrc
COPY files/dot-Xresources /home/lsw/.Xresources
COPY files/slime-init.el /home/lsw/emacs/slime-init.el
COPY files/font-indent.el /home/lsw/emacs/font-indent.el
COPY files/dot-bashrc /home/lsw/.bashrc

# Link all the asdf sysdefs into asdf-source-registry, which is where .abclrc says to look for them
RUN mkdir /home/lsw/repos/asdf-source-registry
RUN find . -name \*asd -exec ln -s `pwd`/{} /home/lsw/repos/asdf-source-registry/ \; 

# We don't need ant any more so remove it. Later: See if we can get rid of anything else.
USER 0
RUN apt-get remove -y --auto-remove ant
RUN rm -rf /usr/local/share/doc

# Remove some of the reasoners not generally used
USER lsw
WORKDIR /home/lsw/repos/lsw2/owl2/lib
RUN rm -rf  Chainsaw-1.0-SNAPSHOT.jar  pelletcli/  Konclude*  factpp-native-1.6.4/  prefuse/  LICENSE-README/  jfact/  sparqldl-api1.0.0/  jfact-1.2.3.jar  telemetry-1.0.0.jar  org.semanticweb.hermit-1.3.8.413.jar  owlapi-4.2.6-dependencies/  uncommons-maths-1.2.2.jar  elk-owlapi-standalone-0.5.0-SNAPSHOT-bin.jar  owlapi-distribution-4.2.6.jar  explanation/  owlapitools/ 
 
# Remove stuff we don't need. We're not doing dev so remove the .git directoris as well as the abcl build files
# don't remove dist as lsw soft links to the abcl-aio.jar there
RUN rm -rf /home/lsw/repos/abcl/build /home/lsw/repos/lsw2/slime-alan /home/lsw/repos/lsw2/protege /home/lsw/repos/lsw2/virtual-machine /home/lsw/repos/*/.git

# Add LSW's bin to PATH
ENV PATH="/home/lsw/repos/lsw2/bin:${PATH}"

# Run LSW so everything gets compiled
RUN lsw -- --eval '(cl-user::quit)'

# compile swank
RUN abcl -- --foreground --load /home/lsw/repos/slime/swank-loader.lisp --eval '(progn (swank-loader::load-swank) (asdf::load-system "logic") (quit))'

WORKDIR /home/lsw/repos/lsw2/bin
#ENTRYPOINT lsw --load /home/lsw/repos/slime/swank-loader.lisp --eval '(swank-loader::load-swank)' --eval '(progn (in-package :logic) (setq *print-case* :downcase))' --eval '(when (probe-file "/local/init.lisp") (load "/local/init.lisp"))'
#ENTRYPOINT lsw -- --foreground
USER lsw
ENTRYPOINT /bin/bash