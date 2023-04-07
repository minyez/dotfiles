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
# import subprocess as sp
from pwd import getpwuid
from socket import gethostname
from argparse import ArgumentParser, RawDescriptionHelpFormatter


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


def remove_symlink(target, dry=False):
    """remove symlink dotfile at `target`
    """
    target = decode_target(target)
    if dry:
        print(target)
        return
    if os.path.islink(target):
        try:
            os.unlink(target)
            print("removed {} (symlink to {})".format(
                target, os.path.realpath(target)))
        except PermissionError:
            print("fail removing {} due to permission (symlink to {})".format(
                target, os.path.realpath(target)))
            print("Please do manually symlink by")
            print("    sudo rm -f %s" % target)
    else:
        print("%s is not symlink, skip removing for safety" % target)


def symlink_dotfile(src, target, dry=False, verbose=False):
    """symlink dotfile at `src` to `target`

    TODO:
        create directories when not found
    """
    src = decode_src(src)
    target = decode_target(target)
    if dry:
        print(src, target)
        return
    if os.path.isdir(target):
        if verbose:
            print("found directory at %s (skip)" % target)
    elif os.path.islink(target):
        if verbose:
            print("found link at %s (skip)" % target)
    elif os.path.isfile(target):
        if verbose:
            print("found file at %s (skip)" % target)
    else:
        print("Symlinking %s to %s" % (src, target))
        try:
            os.symlink(src, target)
            # sp.check_output(["ln", "-s", src, target])
        except PermissionError:
            print("Permission denied in symlinking %s to %s (skip)" %
                  (src, target))
            print("  Please do manually symlink by")
            print("    sudo ln -s %s %s" % (src, target))
        except FileNotFoundError:
            print(
                "Directory of target missing when symlinking %s to %s (skip)" %
                (src, target))


def load_setup_json(*jsonfns):
    """load the link setup from json files"""
    d = {}
    for jfn in jsonfns:
        try:
            with open(jfn + ".json", 'r') as h:
                d.update(json.load(h))
                print("Loaded link setup from {}.json".format(jfn))
        except FileNotFoundError:
            print("link setup {}.json is not found, skip".format(jfn))
    return d


def main():
    """main stream"""
    AVAILABLE_SETUPS = ("public", "darwin", "taiyi", "y9kfed", "amdfed", "iopcas")

    parser = ArgumentParser(__doc__,
                            formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument("--dry", action="store_true", help="dry mode")
    parser.add_argument("--verbose", action="store_true", help="verbose mode")
    parser.add_argument("-r",
                        dest="remove",
                        action="store_true",
                        help="remove the symlinks")
    parser.add_argument("--showname",
                        action="store_true",
                        help="show the OS name and files to link, then exit")
    parser.add_argument("--setups", nargs="+", type=str, default=None,
                        choices=AVAILABLE_SETUPS, help="manually chosen setups")
    args = parser.parse_args()

    # identifier from name and host
    iden_jsonfn = {
        "stevezhang-stevezhangMacBook-Pro.local": "darwin",
        "stevezhang-localhost": "darwin",
        "stevezhang-stevezhangMBP.lan": "darwin",
        "minyez-myz-amd-fedora": "amdfed",
        "minyez-iopcas-fedora": "iopcas",
        "minyez-y9kfed": "y9kfed",
        "minyez-taiyi": "taiyi",
    }
    jsonfns = []
    if args.setups is None:
        _user = getpwuid(os.geteuid()).pw_name
        _host = gethostname()
        iden = "{}-{}".format(_user, _host)
        jsonfns = ["public", ]
        try:
            jfn = iden_jsonfn[iden]
            print("Set JSON '{}' for identifier: {}".format(jfn, iden))
            jsonfns.append(jfn)
        except KeyError:
            print("No JSON set for identifier: {}".format(iden))
            print("Public only")
    else:
        jsonfns = args.setups
    src_target_pair = load_setup_json(*jsonfns)

    if args.showname:
        for s, t in src_target_pair.items():
            print(" ", s, "->", t)
        return

    for k, v in src_target_pair.items():
        if not os.path.exists(k):
            print("Warning!! Requested dotfile not found: %s" % k)
            continue
        if args.remove:
            remove_symlink(v, dry=args.dry)
        else:
            symlink_dotfile(k, v, dry=args.dry, verbose=args.verbose)


if __name__ == "__main__":
    main()
