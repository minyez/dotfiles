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


MACHINE_JSON_FILE = "machine.json"


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


def load_setup(machine_identity):
    """load the link setup for a particular machine"""
    if not os.path.isdir(machine_identity):
        raise FileNotFoundError("directory {}/ not found".format(machine_identity))
    jsonfile = os.path.join(machine_identity, "setup.json")
    if not os.path.exists(jsonfile):
        raise FileNotFoundError("setup file {} not found".format(jsonfile))
    d = {}
    with open(jsonfile, 'r') as h:
        setup = json.load(h)
    for k, v in setup.items():
        if k.startswith("/"):
            d[k] = v
        else:
            d[os.path.join(machine_identity, k)] = v
    print("Loaded link setup from {}".format(jsonfile))
    return d


def get_user_machine():
    _user = getpwuid(os.geteuid()).pw_name
    _host = gethostname()
    return "{}-{}".format(_user, _host)


def main():
    """main stream"""
    try:
        with open(MACHINE_JSON_FILE, 'r') as h:
            machine_json = json.load(h)
    except FileNotFoundError as e:
        raise FileNotFoundError("Required file {} is not found".format(MACHINE_JSON_FILE)) from e
    AVAILABLE_SETUPS = set(machine_json.values())

    parser = ArgumentParser(__doc__,
                            formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument("--dry", action="store_true", help="dry mode")
    parser.add_argument("-v", "--verbose", action="store_true", help="verbose mode")
    parser.add_argument("-r",
                        dest="remove",
                        action="store_true",
                        help="remove the symlinks")
    parser.add_argument("--showname",
                        action="store_true",
                        help="show the OS name and files to link, then exit")
    parser.add_argument("--setup", type=str, default=None,
                        choices=AVAILABLE_SETUPS, help="manually chosen setup")
    args = parser.parse_args()

    # machine name
    identity = args.setup
    if identity is None:
        try:
            user_machine = get_user_machine()
            identity = machine_json[user_machine]
            print("Find machine '{}' for {}".format(identity, user_machine))
        except KeyError as e:
            raise KeyError("No machine found for {}. Check {}".format(user_machine, MACHINE_JSON_FILE)) from e

    src_target_pair = load_setup(identity)

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
