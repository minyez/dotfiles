#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Setup dotfiles by creating symlinks
"""
import os
import subprocess as sp
from argparse import ArgumentParser

SRC_DEST_PAIR = {
        "doom.d/"       : "~/.doom.d",
        "oh-my-zsh/"    : "~/.oh-my-zsh",
        "neovim/"       : "~/.config/nvim",
        "vim-dir/"      : "~/.vim",
        "lyx/"          : "~/.lyx",
        "ssh.config"    : "~/.ssh/config",
        "vimrc"         : "~/.vimrc",
        "alacritty.yml" : "~/.alacritty.yml",
        "zshrc"         : "~/.zshrc",
        "bashrc"        : "~/.bashrc",
        "gnuplotrc"     : "~/.gnuplot",
        "mushroomrc.py"     : "~/.mushroomrc",
        "nutshare_rules": "~/.nutstore/db/customExtRules.conf",
        "../latex/latex-cls-and-sty/": "/Library/TeX/Local/tex/latex",
        }

def symlink_dotfile(src, dest, debug=False):
    """symlink dotfile at `src` to `dest`

    TODO:
        create directories when not found
    """
    if src.startswith("/"):
        pass
    elif src.startswith("~"):
        src = os.path.expanduser(src)
    else:
        src = os.path.join(os.path.dirname(os.path.abspath(__file__)), src)
    if dest.startswith("/"):
        pass
    elif dest.startswith("~"):
        dest = os.path.expanduser(dest)
    else:
        dest = os.path.join(os.environ["HOME"], dest)
    if debug:
        print(src, dest)
        return
    if os.path.isdir(dest):
        print("found directory at %s (skip)" % dest)
    elif os.path.islink(dest):
        print("found link at %s (skip)" % dest)
    elif os.path.isfile(dest):
        print("found file at %s (skip)" % dest)
    else:
        print("Symlinking %s to %s" % (src, dest))
        try:
            sp.check_output(["ln", "-s", src, dest])
        except sp.CalledProcessError:
            print("Error in symlinking %s to %s (skip)" % (src, dest))

def main():
    """main stream"""
    parser = ArgumentParser(__doc__)
    parser.add_argument("-D", dest="debug", action="store_true", help="debug mode")
    args = parser.parse_args()
    for k, v in SRC_DEST_PAIR.items():
        if not os.path.exists(k):
            raise KeyError("Required dotfile not found: %s" % k)
        symlink_dotfile(k, v, debug=args.debug)

if __name__ == "__main__":
    main()
