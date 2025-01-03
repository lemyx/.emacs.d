#!/bin/bash

if command -v xelatex > /dev/null 2> /dev/null; then
    echo "TeX Live is already installed."
else
    echo "ERROR! TeX Live has not been installed. Make sure it's installed and environment variables are configured correctly."
    echo "Depends on the TeX Live edition, append corresponding environments variables into ~/.zshrc"
    echo 'export PATH="/usr/local/texlive/2024/bin/x86_64-linux:$PATH"'
    echo 'export MANPATH="/usr/local/texlive/2024/texmf-dist/doc/man:$MANPATH"'
    echo 'export INFOPATH="/usr/local/texlive/2024/texmf-dist/doc/info:$INFOPATH"'
    exit 1
fi

# emacs
sudo pacman -S emacs

# fonts
yay -S nerd-fonts-sarasa-term
sudo pacman -S ttf-nerd-fonts-symbols

# lsp-bridge
## dependency
sudo pacman -S python-orjson python-six python-setuptools python-paramiko python-rapidfuzz python-watchdog python-packaging
yay -S python-epc python-sexpdata python-pynput
## lsp
sudo pacman -S npm
sudo npm install -g yaml-language-server
sudo npm install -g bash-language-server
sudo npm install -g vscode-langservers-extracted
sudo npm install -g pyright
yay -S python-lsp-ruff
sudo pacman -S texlab

# vterm
sudo pacman -S cmake

# aider
export PATH="$PATH:$HOME/.local/bin"
python -m pip install aider-install --break-system-packages
aider-install

# rime
sudo pacman -S librime

# fetch submodules
git submodule update --init
