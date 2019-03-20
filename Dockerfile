FROM fedora:29

RUN dnf install -y git emacs curl

RUN useradd -ms /bin/bash sensetioner

USER sensetioner

WORKDIR /home/sensetioner

COPY --chown=sensetioner:sensetioner .emacs.d/ /home/sensetioner/.emacs.d/

RUN rm "$HOME/.emacs" && \
        git clone https://github.com/own-pt/sensetion.el.git "$HOME/sensetion.el" && \
        emacs --batch --load="$HOME/.emacs.d/init.el" --kill
