#!/bin/bash -e

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export EMACS_BREWFILE="~/.emacs.d/data/Brewfile"

# trap "rm -rf ~/.emacs.d" EXIT

pwd=`pwd`

if ! test -e /usr/local/bin/emacs; then
    {
        >&2 echo "------- installing dependencies -------"
        brew bundle --file=$EMACS_BREWFILE
    } > /dev/null
fi

