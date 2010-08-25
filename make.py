#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @(#)make.py
# @author Karl Ljungkvist


import os
import errno
import shutil
import time

HOME=os.path.expanduser('~')
DOTDIR=HOME+"/.dotfiles"
OLDDIR=HOME+"/dotfiles_old_"+time.strftime('%Y-%m-%d_%X')


def setup(dir, nodes):    


    if dir == None:
        bup_dir=OLDDIR
    else:
        bup_dir=OLDDIR+"/"+dir
        link_dir = HOME + "/" + dir

        if not os.path.isdir(link_dir):
            os.makedirs(link_dir)

        
    for node in nodes:

        if dir == None:
            node_path=node
        else:
            node_path="%s/%s" % (dir,node)
            
        full_bup_path="%s/%s" % (bup_dir,node)
        full_src_path="%s/%s" % (DOTDIR,node_path)
        full_link_path="%s/%s" % (HOME,node_path)

        if os.path.exists(full_link_path):
            if os.path.islink(full_link_path):
                continue

            # Back up old config file

            print "Backing up %s" % node_path

            if not os.path.isdir(bup_dir):
                os.makedirs(bup_dir)

            shutil.move(full_link_path, full_bup_path)

        print "Creating symlink for %s" % node_path
        os.symlink(full_src_path, full_link_path)


if __name__ == '__main__':

    os.chdir(DOTDIR)

    # regular files
    homefiles=['.bashrc',
               '.dircolors',
               '.emacs',
               '.inputrc',
               '.screenrc',
               '.templates', # note, this is not a file, but we want all of this
               # directory versioned
               '.vimperatorrc',
               '.viper',
               '.Xresources',
               '.gitconfig']

    setup(None,homefiles)

    # emacs.d
    emacsfiles=[ 'template.el', 'matlab.el', 'plugins']
    setup(".emacs.d",emacsfiles)
    
    # xmonad
    xmonadfiles=["xmonad.hs", "xmobar.hs"]
    setup(".xmonad",xmonadfiles)

