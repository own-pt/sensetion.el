FROM fedora:29

RUN dnf install -y git emacs curl wordnet

RUN useradd -ms /bin/bash sensetioner

USER sensetioner

WORKDIR /home/sensetioner

COPY --chown=sensetioner:sensetioner .emacs.d/ /home/sensetioner/.emacs.d/

RUN rm "$HOME/.emacs" && \
        git clone https://github.com/own-pt/sensetion.el.git "$HOME/sensetion.el" && \
        curl http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.gz > wn3.tar.gz && \
        tar xvf wn3.tar.gz && \
        emacs --batch --load="$HOME/.emacs.d/init.el" --kill

ENV WNSEARCHDIR="/home/sensetioner/WordNet-3.0/dict"
