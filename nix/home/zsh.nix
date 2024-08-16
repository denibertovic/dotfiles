{ config, pkgs, lib, ... }:
{
  programs.zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "fwalch";
      # manually copied completeions here
      custom = "/home/deni/.oh-my-zsh/custom";
      plugins = [
        # "todo"
        "denv"
        "git"
        # TODO: fix this error /home/deni/.nix-profile/bin/virtualenvwrapper_lazy.sh:21: unmatched '
        # "virtualenvwrapper"
        "docker"
        "cabal"
        "python"
        "stack"
        "vault"
        "pass"
        "fzf"
      ];
    };
    shellAliases = {
        # TEMP ALIASES
        mtorrents="/home/deni/dotfiles/scripts/mount_torrents.sh";
        mbackups="/home/deni/dotfiles/scripts/mount_backups.sh";
        utorrents="umount /media/torrents";
        ubackups="umount /media/backups";

        # kube
        check-kubectl-version="echo $(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)";
        fetch-latest-kubectl="curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl";
        k="kubectl --namespace=\${KUBECTL_NAMESPACE:-default}";
        h="helm --namespace=\${KUBECTL_NAMESPACE:-default}";

        # Force gpg2
        # gpg="gpg2";

        # Keymaps
        # Xmonad map is for switching Caps Lock for Control
        setkben="setxkbmap us && xmodmap ~/.Xmodmap";
        setkbhr="setxkbmap hr && xmodmap ~/.Xmodmap";

        # docker related aliases
        #
        c="docker-compose";
        m="docker-machine";
        # Delete all stoped/exited containers except data containers which will have the naming scheme
        # someNameData or someNameDataContainer - We have to use camel case because docker-compose.yml doesn't allow for
        # dashes or underscores
        # TODO: figure out how to use labels for this (need inverted query)
        dclean="docker ps -a -f status=exited | grep -vi \"datacontainer\" | tail -n +2 | cut -c1-12 | xargs --no-run-if-empty docker rm -v";
        # remove all images tagged as <none>
        dcleanimages="docker images -q -f dangling=true | xargs --no-run-if-empty docker rmi";
        dcleanvolumes="docker volume ls -q | xargs docker volume rm";
        dstoplast="docker ps -l -q | xargs docker stop -t 1";
        dps="docker ps";
        di="docker images | awk \"{ print \\$1 }\" | tail -n +2 | sort | uniq";

        # development related aliases
        p="python -c \"import IPython; IPython.terminal.ipapp.launch_new_instance()\"";
        # alias debug="python -m pdb manage.py runserver"
        # alias idebug="python -m ipdb manage.py runserver --pm"

        # other aliases
        sudo="sudo ";
        dk="cd /home/deni/work";
        ll="ls -l";
        lln="ls -l --color=never";
        lls="ls -l | less";
        la="ls -la";
        noack="ack --ignore-file=ext:json --ignore-dir=.devenv --ignore-dir=.next --ignore-dir=node_modules  --ignore-dir=.terraform --ignore-dir=static --ignore-dir=__data --ignore-dir=migrations --ignore-dir=.stack-work --ignore-dir=.stack --ignore-dir=.ghc --ignore-dir=.ghcjs --ignore-dir=.spago --ignore-file=ext:sql";
        duh="du -h --max-depth=1 | sort -h";
        ctl="systemctl";
        t="/home/deni/.local/bin/todo";
        t-check-tags="grep -E -o \"@.*\" ~/Dropbox/todo/todo.txt  | sort | uniq";
        get-subtitles="subliminal download -l en .";
        copy-path="pwd | tr -d \"\n\" | xsel -b";
        asciicast2gif="docker run -v \$PWD:/data asciinema/asciicast2gif";
        gci="google-chrome-stable --profile=\"Default\" --incognito";
        pdf="zathura";

        # tmux
        tmux="tmux -2";

        # eid
        eid-client="/usr/lib/akd/eidmiddleware/Client";
        eid-signer="/usr/lib/akd/eidmiddleware/Signer";

        # haskell
        hoogle="hoogle --count=100";

        # speeds up MC
        mc="mc --nosubshell";
      };

    localVariables = {};
    initExtra = ''
      WORKON_HOME=/home/deni/.virtualenvs
      DISABLE_AUTO_UPDATE="true"
      DISABLE_CORRECTION="true"
      export PATH="/home/deni/.local/bin:''${PATH}"
      unsetopt correct_all
      # Since the default initialization mode, this plugin will overwrite the previous key
      # bindings, this causes the key bindings of other plugins (i.e. fzf, zsh-autocomplete, etc.)
      # to fail. See here: https://github.com/jeffreytse/zsh-vi-mode#execute-extra-commands
      # Define an init function and append to zvm_after_init_commands
      # useful for documenting steps
      function my_init() {
          bindkey "^[[A" history-beginning-search-backward
          bindkey "^[[B" history-beginning-search-forward
          autoload -U history-search-end
      }
      zvm_after_init_commands+=(my_init)

      # 10ms for key sequences
      # https://www.johnhawthorn.com/2012/09/vi-escape-delays/
      export KEYTIMEOUT=1

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
          kubectl -n ''${KUBECTL_NAMESPACE:-default} get $i
        done
      };

      # use denv
      eval "$(denv hook ZSH)"

      # use direnv
      eval "$(direnv hook zsh)"

      # aws-vault GetSessionToken duration
      export AWS_SESSION_TOKEN_TTL=12h
      export AWS_CHAINED_SESSION_TOKEN_TTL=12h
      export AWS_VAULT_PROMPT=terminal
    '';
    zplug = {
      enable = true;
      plugins = [
        { name = "jeffreytse/zsh-vi-mode"; }
      ];
    };
  };
}
