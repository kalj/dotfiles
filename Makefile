### Makefile.tpl -
##
## Author: Karl Ljungkvist

TIMESTAMP=$(shell date +"%Y-%m-%d_%X")

HOMEFILES=.bashrc .dircolors .emacs .inputrc .screenrc .templates \
		.xmonad .vimperatorrc .vimrc .viper .Xresources \
		.gitconfig .aspell.en.pws .aspell.en.prepl .aspell.sv.pws \
		.ncmpcpp

EMACSFILES=plugins
VIMFILES=plugin

install:
	@python setup.py -timestamp ${TIMESTAMP} $(HOMEFILES)
	@python setup.py -timestamp ${TIMESTAMP} -dir ".emacs.d" $(EMACSFILES)
	@python setup.py -timestamp ${TIMESTAMP} -dir ".vim" $(VIMFILES)

LOCALBRANCH=$(shell hostname)-local

update:
	git stash && \
		git checkout master && \
		git pull &&  \
		git checkout ${LOCALBRANCH} && \
		git rebase master && \
		git stash pop

ifndef FIRSTCOMMIT
FIRSTCOMMIT=weorijfoiclkasdf
endif

cp-and-rebase:
	@ask "FIRSTCOMMIT is ${FIRSTCOMMIT} -- okay?" && git stash && \
		git checkout master && \
		git cherry-pick ${FIRSTCOMMIT}^..${LOCALBRANCH} && \
		git co ${LOCALBRANCH} && \
		git rebase master && \
		git stash pop

### Makefile ends here
