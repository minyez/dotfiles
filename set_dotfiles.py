#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Setup dotfiles by creating symlinks
"""
import os
import subprocess as sp
from argparse import ArgumentParser

SRC_DEST_PAIR = {
        "doom.d/"   : ".doom.d",
        "oh-my-zsh/": ".oh-my-zsh",
        "neovim/"   : ".config/nvim",
        "vim-dir/"  : ".vim",
        "ssh.config": ".ssh/config",
        "vimrc"     : ".vimrc",
        "zshrc"     : ".zshrc",
        "bashrc"    : ".bashrc",
        "gnuplotrc" : ".gnuplot",
        }

def symlink_dotfile(src, dest):
    """symlink dotfile at `src` to `dest`"""
    if not dest.startswith("/"):
        dest = os.path.join(os.environ["HOME"], dest)
    _src = src
    if not _src.startswith("/"):
        _src = os.path.join(os.path.dirname(os.path.abspath(__file__)), src)
    if os.path.isdir(dest):
        print("found directory at %s (skip)" % dest)
    elif os.path.islink(dest):
        print("found link at %s (skip)" % dest)
    elif os.path.isfile(dest):
        print("found file at %s (skip)" % dest)
    else:
        print("Symlinking %s to %s" % (src, dest))
        sp.check_output(["ln", "-s", _src, dest])

def main():
    """main stream"""
    parser = ArgumentParser()
    parser.add_argument("-D", action="store_true", help="debug mode")
    args = parser.parse_args()
    for k, v in SRC_DEST_PAIR.items():
        if not os.path.exists(k):
            raise KeyError("Required dotfile not found: %s" % k)
        symlink_dotfile(k, v)

if __name__ == "__main__":
    main()
