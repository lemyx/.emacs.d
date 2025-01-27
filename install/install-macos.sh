#!/bin/bash

# LaTeX
brew install texlive

# emacs
brew tap d12frosted/emacs-plus
brew uninstall emacs-plus
brew install emacs-plus --with-xwidgets --with-imagemagick --with-dbus --with-ctags
brew services restart d12frosted/emacs-plus/emacs-plus@29
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'

# fonts
brew tap laishulu/homebrew
brew install --cask font-sarasa-nerd
brew install --cask font-symbols-only-nerd-font

# lsp-bridge
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz pynput pyobjc --break-system-packages
brew install node
npm install -g yaml-language-server
npm install -g bash-language-server
npm install -g vscode-langservers-extracted
brew install texlab
brew install basedpyright
brew install ruff
brew install beancount-language-server

# aider
brew install aider

# rime
brew install --cask squirrel
brew install librime
git clone https://github.com/iDvel/rime-ice.git && cd rime-ice
sed -i '' 's/page_size: 5/page_size: 9/' default.yaml
wget https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram
cat <<EOL > rime_ice.custom.yaml
patch:
  grammar:
    language: wanxiang-lts-zh-hans.gram
    collocation_max_length: 5
    collocation_min_length: 2
  translator/contextual_suggestions: true
  translator/max_homophones: 7
  translator/max_homographs: 7
EOL
cd ..
cp -r ./rime-ice/* ~/.config/fcitx/rime/
cp -r ./rime-ice/* ~/Library/Rime
rm -rf rime-ice
brew install --cask switchkey

# org-pomodoro
brew install terminal-notifier

# fetch submodules
git submodule update --init
