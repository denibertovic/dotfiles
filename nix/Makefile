.PHONY: help

DEFAULT_GOAL: help system user

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## build system with 'nixos-reuild switch --flake .'
system:
	@sudo nixos-rebuild switch --flake .#kanta

## build home with 'home-manager --flake .#your-username@your-hostname'
user:
	@home-manager switch -b backup-`date +"%Y-%m-%d-%H%M%S"` --flake .#deni@kanta --impure ${NIX_BUILDER_OPTS}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

