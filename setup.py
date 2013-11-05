#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @(#)setup.py
# @author Karl Ljungkvist


import os
import argparse
import errno
import shutil
import time

def setup(dir, nodes, HOME, DOTDIR, OLDIR):

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

    parser = argparse.ArgumentParser()
    parser.add_argument('nodes',type=str,nargs='+')
    parser.add_argument('-dir',type=str,action='store')
    parser.add_argument('-timestamp',type=str,action='store')
    args=parser.parse_args()

    if args.timestamp == None:
        raise Exception('Must supply timestamp')

    HOME=os.path.expanduser('~')
    DOTDIR=HOME+"/.dotfiles"
    OLDDIR=HOME+"/dotfiles_old_"+args.timestamp

    # print 'timestamp: '+args.timestamp
    # print 'files: '+', '.join(args.nodes)
    # if args.dir != None:
    #     print 'in dir: '+args.dir

    setup(args.dir,args.nodes,HOME,DOTDIR,OLDDIR)


