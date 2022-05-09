# Source: https://github.com/testcab/docker-yay

FROM archlinux:latest

USER root

# COPY ./init.el /init.el
# COPY ./early-init.el /early-init.el

RUN pacman -Syyu --noconfirm --noprogressbar
RUN pacman -S --noconfirm --noprogressbar base-devel sudo git lsb-release

ARG USER=user

RUN useradd --system --create-home $USER && \
    echo "$USER ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER

USER $USER

ENV HOME_DIR /home/$USER
ENV EMACS_DIR $HOME_DIR/.emacs.d

WORKDIR /home/$USER

COPY ./init.el $EMACS_DIR/init.el
COPY ./early-init.el $EMACS_DIR/early-init.el

RUN git clone https://aur.archlinux.org/yay.git
RUN cd ./yay && ls -al && \
    makepkg -sirc --needed --noconfirm && \
    cd && \
    rm -rf .cache yay

RUN yay -S --noconfirm --cleanafter emacs-native-comp-git-enhanced
RUN emacs -e "(kill-emacs)"
