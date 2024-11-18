PROMPT_COMMAND='PS1_PATH=sh:$(sed "s:\([^/\.]\)[^/]*/:\1/:g" <<< ${PWD/#$HOME/u/et})' 
export PS1='$PS1_PATH> '

export PATH="$PATH:/opt/homebrew/bin"
export PATH="$PATH:/opt/homebrew/sbin"
export PATH="$PATH:~/.local/bin"
export PATH="$PATH:~/.ghcup/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$PATH:$NVM_DIR/versions/node/v18.19.0/bin"

scheme() {
  chez "$@"
}
export -f scheme

source "$HOME/.env"
echo $PING_ENV
