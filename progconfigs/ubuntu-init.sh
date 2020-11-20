#!/usr/bin/env bash
# upgrade first
sudo apt update
sudo apt upgrade

# openssh, nvim, ifconfig
sudo apt-get install ssh neovim net-tools
# docker repos
sudo apt install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic test"
sudo apt update
sudo apt upgrade
sudo apt-get install docker-ce docker-ce-cli containerd.io
