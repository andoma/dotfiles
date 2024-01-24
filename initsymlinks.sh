#!/bin/bash

ln -sf ${PWD}/emacs ${HOME}/.emacs
ln -sf ${PWD}/gitconfig ${HOME}/.gitconfig
ln -sf ${PWD}/bash_aliases ${HOME}/.bash_aliases

if [ ! -f ${HOME}/.profile ]; then
    echo >${HOME}/.profile "source ~/.bash_aliases"
fi
