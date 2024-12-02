# ~/.profile: executed by the command interpreter for login shells.
# Not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples.

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/opt/homebrew/bin" ] ; then
    export PATH="/opt/homebrew/bin:$PATH"
fi

if [ -d "/opt/homebrew/sbin" ] ; then
    export PATH="/opt/homebrew/sbin:$PATH"
fi

if [ -d "/snap/bin" ] ; then
    export PATH="/snap/bin:$PATH"
fi

if [ -d "$HOME/.nvm" ] ; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # load nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # nvm bash_completion
fi

if [ -d "/opt/homebrew/opt/gnu-tar/libexec" ] ; then
    # Prefer GNU tar
    export PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:$PATH"    
fi

# Source environment secrets
if [ -d "$HOME/.env" ] ; then
    source "$HOME/.env"
    echo $PING_ENV
fi

scheme() {
  chez "$@"
}
export -f scheme

PROMPT_COMMAND='PS1_PATH=sh:$(sed "s:\([^/\.]\)[^/]*/:\1/:g" <<< ${PWD/#$HOME/u/et})'
export PS1='$PS1_PATH>'

echo "Sourced ~/.profile"
#serv

