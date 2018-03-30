FROM nixorg/nix:v1.1.0

ENV NB_USER jovyan
ENV NB_UID 1000
ENV HOME /home/${NB_USER}

RUN nix-shell \
    -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/d16ffa1509008b53891412b095cea259dd73d869.tar.gz \
    -p busybox \
    --run "adduser --disabled-password --gecos 'Default user' --uid ${NB_UID} ${NB_USER}"

COPY . ${HOME}
USER root
WORKDIR ${HOME}

RUN nix-build default.nix
RUN chown -R ${NB_UID} ${HOME}
RUN chown -R ${NB_UID} /tmp

USER ${NB_USER}

CMD ["result/bin/ihaskell-notebook", "--ip", "0.0.0.0"]
