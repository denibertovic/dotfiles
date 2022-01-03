.PHONY = all common enable-multi-arch dependencies help tag
.DEFAULT_GOAL := help


# paswordless sudo has to be set up
## Install dependencies before Ansible is run
dependencies:
	@sudo apt-get install libssl-dev git vim sudo python3-pip python3-dev
	@sudo pip install ansible

## Run the entire Ansible playbook and prerequisites
all: enable-multi-arch dependencies
	@cd ansible && ansible-playbook -vvv -i inventory/localhost site.yml

## Run Ansible playbook for 'common' tag
common:
	@cd ansible && ansible-playbook -i inventory/localhost site.yml --tags=common

## Run Ansible playbook with single tag. TAG=keybase make tag
tag:
	@cd ansible && ansible-playbook -vvv -i inventory/localhost site.yml --tags=${TAG} ${OPTS}

## Enable i386 arch
enable-multi-arch:
	@sudo dpkg --add-architecture i386

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

