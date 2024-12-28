#!/bin/bash

if command -v xelatex > /dev/null 2> /dev/null; then
    echo "TeX Live is already installed."
else
    echo "ERROR! TeX Live has not been installed. Make sure it's installed and environment variables are configured correctly."
    echo "Depends on the TeX Live edition, append corresponding environments variables into ~/.zshrc"
    echo 'export PATH="/usr/local/texlive/2023/bin/x86_64-linux:$PATH"'
    echo 'export MANPATH="/usr/local/texlive/2023/texmf-dist/doc/man:$MANPATH"'
    echo 'export INFOPATH="/usr/local/texlive/2023/texmf-dist/doc/info:$INFOPATH"'
    exit 1
fi

# emacs
cd
sudo bash ~/.emacs.d/install/build-emacs-ubuntu.sh

# fonts
wget https://github.com/laishulu/Sarasa-Term-SC-Nerd/releases/download/v2.3.0/SarasaTermSCNerd.ttf.tar.gz && && tar -zxvf SarasaTermSCNerd.ttf.tar.gz && mv SarasaTermSCNerd*.ttf ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip && unzip NerdFontsSymbolsOnly && mv SymbolsNerdFont*.ttf ~/.local/share/fonts
fc-cache -fv

# lsp-bridge
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz pynput pyobjc --break-system-packages
sudo apt install npm
sudo npm install -g yaml-language-server
sudo npm install -g bash-language-server
sudo npm install -g vscode-langservers-extracted
sudo npm install -g pyright
pip3 install ruff-lsp --break-system-package
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
. "$HOME/.cargo/env"
git clone https://github.com/latex-lsp/texlab.git && cd texlab
cargo build --release

# vterm
sudo apt install cmake libtool-bin libvterm-dev

# fetch submodules
git submodule update --init
