### Makefile.tpl -
##
## Author: Karl Ljungkvist

TIMESTAMP=$(shell date +"%Y-%m-%d_%T")

HOMEFILES=.bashrc .dircolors .inputrc .screenrc .templates \
		.xmonad .vimperatorrc .vimrc .viper .Xresources \
		.gitconfig .aspell.en.pws .aspell.en.prepl .aspell.sv.pws \
		.aspell.sv.prepl .ncmpcpp .gdbinit .gdbscripts \
		.offlineimaprc .offlineimap.py

EMACSFILES=init.el lisp plugins snippets
VIMFILES=plugin

install:
	@# Emacs ignores ~/.emacs.d/init.el while ~/.emacs exists, so drop the
	@# symlink left over from when the config was a single ~/.emacs.
	@if [ -L ~/.emacs ] && [ "$$(readlink ~/.emacs)" = "$$HOME/.dotfiles/.emacs" ]; then \
		rm ~/.emacs && echo "Removed old ~/.emacs symlink"; fi
	@python3 setup.py -timestamp ${TIMESTAMP} $(HOMEFILES)
	@python3 setup.py -timestamp ${TIMESTAMP} -dir ".emacs.d" $(EMACSFILES)
	@python3 setup.py -timestamp ${TIMESTAMP} -dir ".vim" $(VIMFILES)

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
