NAME=fewer-buildbot-debian
BOX=$(NAME).box

.PHONY: down-up

down-up:
	-vagrant destroy -f
	vagrant up

$(BOX): Vagrantfile bootstrap.sh down-up
	vagrant package --output $(BOX)

import: $(BOX)
	vagrant box add --name $(NAME) $(BOX)
