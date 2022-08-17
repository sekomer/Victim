FROM ubuntu:20.04

RUN apt update
RUN apt install openssh-server sudo -y
RUN useradd -rm -d /home/jarbay51 -s /bin/bash -g root -G sudo -u 1000 test 
RUN usermod -aG sudo test
RUN service ssh start
RUN echo 'test:test' | chpasswd

RUN apt update
RUN apt install build-essential -y
RUN apt install gdb -y

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git jq bc make automake libnuma-dev \
    && apt-get install -y rsync htop curl build-essential \
    && apt-get install -y pkg-config libffi-dev libgmp-dev \
    && apt-get install -y libssl-dev libtinfo-dev libsystemd-dev \
    && apt-get install -y zlib1g-dev make g++ wget libncursesw5 libtool autoconf


WORKDIR "/root"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN git clone https://github.com/Sekomer/Victim
WORKDIR "/root/Victim"

RUN /bin/bash -c "source /root/.ghcup/env && cabal install -O2 --overwrite-policy=always"


EXPOSE 22
CMD ["/usr/sbin/sshd","-D"]
