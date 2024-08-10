#!/bin/bash

if test -f ~/.emacs | test -L ~/.emacs; then
    echo -e "\e[36m.emacs found in home directory. This action will delete it."
    echo -e -n "Do yo want to create a backup? y/n \e[0m"
    
    read backup
    if [[ $backup == "y" ]]; then
        cp -P ~/.emacs ~/my.emacs.backup
        echo "cp ~/.emacs ~/my.emacs.backup"
        
    elif [[ $backup != "n" ]]; then
        echo -e "\e[31maborted"
        exit
    fi
    
    rm ~/.emacs
    echo "rm ~/.emacs"
fi

ln -s $(pwd)/emacs.el ~/.emacs
echo "ln -s "$(pwd)"/emacs.el ~/.emacs"
echo -e "\e[35mdone!"


# ln -s "$(pwd)"/eshell.desktop ~/.local/share/applications/eshell.desktop
