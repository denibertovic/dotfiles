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

# TEMP ALIASES
#
alias met='cd /home/deni/meetup/docker'
alias mtorrents='/home/deni/scripts/mount_torrents.sh'
alias mbackups='/home/deni/scripts/mount_backups.sh'
alias utorrents='umount /media/varys/torrents'
alias ubackups='umount /media/varys/backups'

# Force gpg2
alias gpg='gpg2'

# Keymaps
# Xmonad map is for switching Caps Lock for Control
alias setkben="setxkbmap en_US && xmodmap ~/.Xmodmap"
alias setkbhr="setxkbmap hr && xmodmap ~/.Xmodmap"

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
alias dgc='docker run --rm -v /var/run/docker.sock:/var/run/docker.sock -v /etc:/etc spotify/docker-gc'
alias dstoplast='docker ps -l -q | xargs docker stop -t 1'
alias dps='docker ps'
alias di="docker images | awk '{ print \$1}' | tail -n +2 | sort | uniq"

# development related aliases
alias p="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"
alias shell='./manage.py shell'
alias debug='python -m pdb manage.py runserver'
alias idebug='python -m ipdb manage.py runserver --pm'
alias clojure='docker run -e LEIN_ROOT=1 -i -t clojure /bin/bash -c "lein repl"'
# alias ghci=ghci-color

# other aliases
alias sudo='sudo '
alias dk='cd /home/deni/work'
alias ll='ls -l'
alias la='ls -la'
alias noack='ack --ignore-file=ext:json --no-css --no-js --ignore-dir=static --ignore-dir=__data --ignore-dir=migrations --ignore-dir=.stack-work --ignore-file=ext:sql'
alias duh='du -h --max-depth=1'
alias proxy='ssh -D 9999 magrathea.kset.org -p 80'
# alias psc='ps xawf -eo pid,user,cgroup,args'
alias ctl='systemctl'
alias t='/usr/bin/todo-txt'
alias get-subtitles='subliminal download -l en .'
alias copy-path="pwd | tr -d '\n' | xsel -b"

# tmux
alias tmux='tmux -2'

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

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git virtualenvwrapper docker cabal python keybase stack)

source $ZSH/oh-my-zsh.sh

export PATH="/usr/local/heroku/bin:/home/deni/programs/appengine:/opt/scala/bin:/home/deni/.local/bin:/usr/local/bin/terraform:$PATH"


#export PYTHONPATH="$PYTHONPATH":/usr/lib/python2.7/dist-packages/wx-2.8-gtk2-unicode/
unsetopt correct_all

# added by travis gem
[ -f /home/deni/.travis/travis.sh ] && source /home/deni/.travis/travis.sh

# added by Nix Installer
[ -e /home/deni/.nix-profile/etc/profile.d/nix.sh  ] && source /home/deni/.nix-profile/etc/profile.d/nix.sh;

# RPROMPT="\$(cabal_sandbox_info) $RPROMPT"

# VIM mode
bindkey -v

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]% %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

# use all 3 monitors at work
workmon () {
    killall trayer
    xrandr --output eDP-1 --primary --below DP-2-2 --output DP-2-2 --right-of DP-2-1 --auto --output DP-2-1 --rotate left --auto
    nohup trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --tint 0x222222 --heighttype pixel --height 36  --monitor 2 &

}

# set single monitor
singlemon () {
    killall trayer
    xrandr --output DP-2-2 --off --output DP-2-1 --off
    nohup trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --tint 0x222222 --heighttype pixel --height 36 &
}

export SSH_AUTH_SOCK=/run/user/$(id -u)/gnupg/S.gpg-agent.ssh
