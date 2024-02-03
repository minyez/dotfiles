My personal configuration files.

`set_dotfiles.py` is used to create symlinks for files to appropriate paths.

Files included:

- [x] Vim
- [x] NeoVim
- [ ] [helix](https://github.com/helix-editor/helix) (WIP)
    Okay with 0.5 release.
    Not able to install the latest commit, possibly due to incomplete submodule clone.
    Also check issue 773 therein
- [x] Doom Emacs. Both lisp and org-mode [literate configuration](https://github.com/minyez/doom) (as a submodule).
- [x] Bash Shell
- [x] Z shell
- [x] git excludes and ignore
- [x] SSH config (as a symlink to the real private config)
- [x] gnuplot
- [x] ~~NutShare excluding rules~~ (not working any more)
- [x] wine registery files
- [x] ~~Clash config directory~~ (ignored for private usage)

More functionalities

- [x] JSON metadata for source-target pair
- [x] Platform-specific linking
- [ ] installing script (WIP)

Current machines:

- `darwin` : MBP 15 (2016), Intel i7-6820HQ, AMD Radeon Pro 455, 16G + 512G, macOS Catalina 10.15.7
- `amdfed` : DIY PC, AMD R5 5600X, AMD Radeon RX 6700XT, 32G + (500G + 2T), Fedora 34 Gnome (dual boot Windows 10)
- `iopcas` : Dell workstation, i9-11900, Radeon RX 640, 32G + (500G + 2T), Fedora 34 Gnome
- `y9kfed` : Y9000P, i7-12700H, RTX 3060, 16G + 1T, Fedora 38 KDE (dual boot Windows 11, 500G)
- `taiyi`  : MBP 14 (2023), M2 Max, 64G + 2T, macOS Sonoma

Some directories:
- `public`: rc files that is not quite machine-specific
- `common`: common ingredients that may be linked into machine-specific directories

Usage
```shell
python set_dotfiles.py
```

To use Doom Emacs literate configuration, make sure Doom works
```shell
git submodule update --init --recursive
cd taiyi/doom-literate
make
```
