.PHONY = all common enable-multi-arch
.DEFAULT_GOAL := all

all:
	@cd ansible && ansible-playbook -i inventory/localhost site.yml

common:
	@cd ansible && ansible-playbook -i inventory/localhost site.yml --tags=common

enable-multi-arch:
	@dpkg --add-architecture i386

