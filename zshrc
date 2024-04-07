# # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# # Initialization code that may require console input (password prompts, [y/n]
# # confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#
# THE BEST THEME so far is fwalch
# gallifrey is good
#
ZSH_THEME="fwalch"
#ZSH_THEME="gallifrey"

# TEMP ALIASES
#
alias mtorrents='/home/deni/dotfiles/scripts/mount_torrents.sh'
alias mbackups='/home/deni/dotfiles/scripts/mount_backups.sh'
alias utorrents='umount /media/torrents'
alias ubackups='umount /media/backups'

# kube
alias check-kubectl-version='echo $(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)'
alias fetch-latest-kubectl='curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl'
alias k='kubectl --namespace=${KUBECTL_NAMESPACE:-default}'
alias h='helm --namespace=${KUBECTL_NAMESPACE:-default}'

# Force gpg2
# alias gpg='gpg2'

# Keymaps
# Xmonad map is for switching Caps Lock for Control
alias setkben='setxkbmap us && xmodmap ~/.Xmodmap'
alias setkbhr='setxkbmap hr && xmodmap ~/.Xmodmap'

# docker related aliases
#
alias c='docker-compose'
alias m='docker-machine'
# Delete all stoped/exited containers except data containers which will have the naming scheme
# someNameData or someNameDataContainer - We have to use camel case because docker-compose.yml doesn't allow for
# dashes or underscores
# TODO: figure out how to use labels for this (need inverted query)
alias dclean='docker ps -a -f status=exited | grep -vi "datacontainer" | tail -n +2 | cut -c1-12 | xargs --no-run-if-empty docker rm -v'
# remove all images tagged as <none>
alias dcleanimages='docker images -q -f dangling=true | xargs --no-run-if-empty docker rmi'
alias dcleanvolumes='docker volume ls -q | xargs docker volume rm'
alias dstoplast='docker ps -l -q | xargs docker stop -t 1'
alias dps='docker ps'
alias di='docker images | awk "{ print \$1}" | tail -n +2 | sort | uniq'

# development related aliases
alias p='python3 -c "import IPython; IPython.terminal.ipapp.launch_new_instance()"'
# alias debug='python -m pdb manage.py runserver'
# alias idebug='python -m ipdb manage.py runserver --pm'

# other aliases
alias sudo='sudo '
alias dk='cd /home/deni/work'
alias ll='ls -l'
alias lln='ls -l --color=never'
alias lls='ls -l | less'
alias la='ls -la'
alias noack='ack --ignore-file=ext:json --ignore-dir=.devenv --ignore-dir=.next --ignore-dir=node_modules  --ignore-dir=.terraform --ignore-dir=static --ignore-dir=__data --ignore-dir=migrations --ignore-dir=.stack-work --ignore-dir=.stack --ignore-dir=.ghc --ignore-dir=.ghcjs --ignore-dir=.spago --ignore-file=ext:sql'
alias duh='du -h --max-depth=1 | sort -h'
alias ctl='systemctl'
alias t='/home/deni/.local/bin/todo'
alias t-check-tags='grep -E -o "@.*" ~/Dropbox/todo/todo.txt  | sort | uniq'
alias get-subtitles='subliminal download -l en .'
alias copy-path='pwd | tr -d "\n" | xsel -b'
alias asciicast2gif='docker run -v $PWD:/data asciinema/asciicast2gif'
alias gci='google-chrome --profile-directory=\"Default\" --incognito'
alias pdf="zathura"

# tmux
alias tmux='tmux -2'

# eid
alias eid-client='/usr/lib/akd/eidmiddleware/Client'
alias eid-signer='/usr/lib/akd/eidmiddleware/Signer'

# haskell
alias hoogle='hoogle --count=100'

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"
#
WORKON_HOME=/home/deni/.virtualenvs

# Use python3
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3

export PYENV_ROOT="$HOME/.pyenv"

#ZVM_INIT_MODE=sourcing

timezsh() {
  shell=${1-$SHELL}
  for i in $(seq 1 10); do time $shell -i -c exit; done
}

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    todo
    denv
    git
    virtualenvwrapper
    docker
    cabal
    python
    stack
    vault
    pass
    fzf
    zsh-vi-mode
    zsh-fzf-history-search
)

source $ZSH/oh-my-zsh.sh

export PATH="/home/deni/.local/bin:$PATH"

unsetopt correct_all

# Since the default initialization mode, this plugin will overwrite the previous key
# bindings, this causes the key bindings of other plugins (i.e. fzf, zsh-autocomplete, etc.)
# to fail. See here: https://github.com/jeffreytse/zsh-vi-mode#execute-extra-commands
# Define an init function and append to zvm_after_init_commands
function my_init() {
    bindkey "^[[A" history-beginning-search-backward
    bindkey "^[[B" history-beginning-search-forward
    autoload -U history-search-end
}
zvm_after_init_commands+=(my_init)

# 10ms for key sequences
# https://www.johnhawthorn.com/2012/09/vi-escape-delays/
export KEYTIMEOUT=1

share-session () {
    if (( $# == 0 ))
    then
        echo Usage: $0 WORKDIR;
    else
        docker run --rm -it \
            -v $1:/opt/workdir \
            -v /home/deni/.vim:/home/user/.vim \
            -v /home/deni/dotfiles/vim/vimrc:/home/user/.vimrc \
            -e LOCAL_USER_ID=$(id -u $USER) denibertovic/tmate tmate
    fi;
}

# I have no idea why I did this
export SSH_AUTH_SOCK=/run/user/$(id -u)/gnupg/S.gpg-agent.ssh

# log last cmd to file
# useful for documenting steps
note () {
    if [[ $# -eq 1  ]]; then
        echo "Logging: `fc -ln -1`"
        echo `fc -ln -1` >> "$HOME/Dropbox/notes/$1-`date +"%Y-%m-%d"`.md" ;
    else
        echo "Logging: `fc -ln -1`"
        echo `fc -ln -1` >> "$HOME/Dropbox/notes/note-`date +"%Y-%m-%d"`.md" ;
    fi
}

function kubectlgetall {
  for i in $(kubectl api-resources --verbs=list --namespaced -o name | grep -v "events.events.k8s.io" | grep -v "events" | sort | uniq); do
    echo "Resource:" $i
    kubectl -n ${KUBECTL_NAMESPACE:-default} get ${i}
  done
};

# use denv
eval "$(denv hook ZSH)"

# use direnv
#eval "$(direnv hook zsh)"

## To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
