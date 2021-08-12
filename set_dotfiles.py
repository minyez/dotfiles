#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Setup dotfiles by creating symlinks

First, the platform is decided from methods in os.
Then the public linking setup and the one for the platform
is loaded by decoding the JSON file, 'public.json' and 'platform.json',
respectively.

Currently the platform is completed decided by the lower case of os.uname.sysname

The JSON has entries with one of the following structure:
    dotfile : link-target
    dotdir/ : link-target
"""
import os
import json
import subprocess as sp
from argparse import ArgumentParser, RawDescriptionHelpFormatter

## a more advanced platform naming, but not necessary for now
## maybe also need a dict with name:identifier
#from pwd import getpwuid
#from socket import gethostname
#
#_user = getpwuid(os.geteuid()).pw_name
#_host = gethostname()
#user_at_host = "{}-{}".format(_user, _host)
#del _user, _host, getpwuid, gethostname

def decode_src(src):
    """decode the path of dotfile source"""
    if src.startswith("/"):
        return src
    if src.startswith("~"):
        return os.path.expanduser(src)
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), src)

def decode_target(target):
    """decode the path of symlink target"""
    if target.startswith("/"):
        return target
    if target.startswith("~"):
        return os.path.expanduser(target)
    return os.path.join(os.environ["HOME"], target)

def remove_symlink(target, debug=False):
    """remove symlink dotfile at `target`
    """
    target = decode_target(target)
    if debug:
        print(target)
        return
    if os.path.islink(target):
        try:
            os.unlink(target)
            print("removed {} (symlink to {})".format(target, os.path.realpath(target)))
        except PermissionError:
            print("fail removing {} due to permission (symlink to {})"
                  .format(target, os.path.realpath(target)))
    else:
        print("%s is not symlink, skip removing for safety" % target)

def symlink_dotfile(src, target, debug=False):
    """symlink dotfile at `src` to `target`

    TODO:
        create directories when not found
    """
    src = decode_src(src)
    target = decode_target(target)
    if debug:
        print(src, target)
        return
    if os.path.isdir(target):
        print("found directory at %s (skip)" % target)
    elif os.path.islink(target):
        print("found link at %s (skip)" % target)
    elif os.path.isfile(target):
        print("found file at %s (skip)" % target)
    else:
        print("Symlinking %s to %s" % (src, target))
        try:
            os.symlink(src, target)
            #sp.check_output(["ln", "-s", src, target])
        except PermissionError:
            print("Permission denied in symlinking %s to %s (skip)" % (src, target))

def load_setup_json():
    """load the link setup from json files"""
    d = {}
    jsonfns = ("public", os.uname().sysname.lower())
    for jfn in jsonfns:
        try:
            with open(jfn+".json", 'r') as h:
                d.update(json.load(h))
                print("Loaded link setup from {}.json".format(jfn))
        except FileNotFoundError:
            print("link setup {}.json is not found, skip".format(jfn))
    return d

def main():
    """main stream"""
    parser = ArgumentParser(__doc__, formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument("-D", dest="debug", action="store_true", help="debug mode")
    parser.add_argument("-r", dest="remove", action="store_true",
                        help="remove the symlinks")
    args = parser.parse_args()

    src_target_pair = load_setup_json()

    for k, v in src_target_pair.items():
        if not os.path.exists(k):
            print("Warning!! Requested dotfile not found: %s" % k)
            continue
        if args.remove:
            remove_symlink(v, debug=args.debug)
        else:
            symlink_dotfile(k, v, debug=args.debug)


if __name__ == "__main__":
    main()

