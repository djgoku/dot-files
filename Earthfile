emacs:
    FROM silex/emacs:master
    WORKDIR /

deps:
    FROM +emacs
    RUN apt update
    RUN DEBIAN_FRONTEND=noninteractive apt install git --yes

build:
    FROM +deps
    COPY emacs/early-init.el /.config/emacs/

early-init:
    FROM +build
    WORKDIR /
    COPY early-init.el .
    RUN emacs --batch -l early-init.el

test:
    FROM +early-init
    COPY emacs/init.el /.config/emacs/
    COPY emacs/config.org /.config/emacs/
    COPY emacs/custom.el /.config/emacs/
    COPY test.el .
    RUN emacs --batch -l test.el
