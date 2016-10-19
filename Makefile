.PHONY = all common enable-multi-arch dependencies
.DEFAULT_GOAL := all


# paswordless sudo has to be set up
dependencies:
	@sudo apt-get install libssl-dev git vim sudo python-pip python-dev
	@sudo pip install ansible

all: enable-multi-arch dependencies
	@cd ansible && ansible-playbook -i inventory/localhost site.yml

common:
	@cd ansible && ansible-playbook -i inventory/localhost site.yml --tags=common

enable-multi-arch:
	@sudo dpkg --add-architecture i386

