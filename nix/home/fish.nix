{ config, pkgs, lib, ... }:
{

  programs.dircolors = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      OTHER_WRITABLE = "30;46";
      ".sh" = "01;32";
      ".csh" = "01;32";
    };
  };

  programs.fish = {
    enable = true;
    shellAliases = {
        # TEMP ALIASES
        mtorrents="/home/deni/dotfiles/scripts/mount_torrents.sh";
        mbackups="/home/deni/dotfiles/scripts/mount_backups.sh";
        utorrents="umount /media/torrents";
        ubackups="umount /media/backups";

        # kube
        check-kubectl-version="echo $(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)";
        fetch-latest-kubectl="curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl";
        k="kubectl --namespace=(test -n \"$KUBECTL_NAMESPACE\"; and echo \"$KUBECTL_NAMESPACE\"; or echo \"default\")";
        h="helm --namespace=(test -n \"$KUBECTL_NAMESPACE\"; and echo \"$KUBECTL_NAMESPACE\"; or echo \"default\")";

        # Force gpg2
        # gpg="gpg2";

        # Keymaps
        # Xmonad map is for switching Caps Lock for Control
        setkben="setxkbmap us";
        setkbhr="setxkbmap hr";

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
        get-subtitles="op run -- subliminal --opensubtitles $OPENSUBTITLES_USERNAME $OPENSUBTITLES_PASSWORD download -l en .";
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

    shellInit = ''
      set WORKON_HOME /home/deni/.virtualenvs
      set -x PATH "/home/deni/.local/bin:$PATH"

      function note
        set cmd (history --show-time="%Y-%m-%d" | tail -n1 | string split " " | tail -n+2 | string join " ")
        echo "Logging: $cmd"

        if test (count $argv) -eq 1
          set filename "$HOME/Dropbox/notes/$argv[1]-(date '+%Y-%m-%d').md"
        else
          set filename "$HOME/Dropbox/notes/note-(date '+%Y-%m-%d').md"
        end

        echo $cmd >> $filename
      end

      function kubectlgetall
        set namespace (test -n "$KUBECTL_NAMESPACE"; and echo "$KUBECTL_NAMESPACE"; or echo "default")

        kubectl api-resources --verbs=list --namespaced -o name | grep -v "events.events.k8s.io" | grep -v "events" | sort | uniq | while read -l resource
          echo "Resource: $resource"
          kubectl -n $namespace get $resource
        end
      end

      # use denv
      denv hook FISH | source

      # use direnv
      direnv hook fish | source

      # aws-vault GetSessionToken duration
      set -x AWS_SESSION_TOKEN_TTL 12h
      set -x AWS_CHAINED_SESSION_TOKEN_TTL 12h
      set -x AWS_VAULT_PROMPT terminal
    '';
  };
}
