NAMESPACE=mcandre
BASENAME=fewer-buildbot-debian
BOX=$(BASENAME).box
NAME_QUALIFIED=$(NAMESPACE)/$(BASENAME)

.PHONY: down-up

down-up:
	-vagrant destroy -f
	vagrant up

$(BOX): Vagrantfile bootstrap.sh down-up
	vagrant package --output $(BOX)

import: $(BOX)
	vagrant box add --name $(NAME_QUALIFIED) $(BOX)
