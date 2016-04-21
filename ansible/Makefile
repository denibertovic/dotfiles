.PHONY = build common

build:
	@ansible-playbook -i inventory/localhost site.yml

common:
	@ansible-playbook -i inventory/localhost site.yml --tags=common
