My personal configuration files.

`set_dotfiles.py` is used to create symlinks for files to appropriate paths.

Files included:

- [x] Vim
- [x] NeoVim
- [x] Doom Emacs
- [x] Bash Shell
- [x] Z shell
- [x] git excludes and ignore
- [x] ~~SSH config~~
- [x] gnuplot
- [x] ~~NutShare excluding rules~~ (not working any more)
- [x] wine registery files
- [x] ~~Clash config directory~~ (ignored for private usage)
- [ ] rime (WIP)

More functionalities

- [x] JSON metadata for source-target pair
- [ ] Platform-specific linking (WIP)
- [ ] installing script (WIP)

Current machines:

- `darwin`: MBP 15 (2016), Intel i7-6820HQ, AMD Radeon Pro 455, 16G + 512G, macOS Catalina 10.15.7
- `amdfed`: DIY PC, AMD R5 5600X, AMD Radeon RX 6700XT, 32G + (500G + 2T), Fedora 34 (dual boot Windows 10)
- `iopcas`: Dell workstation, i9-11900, Radeon RX 640, 32G + (500G + 2T), Fedora 34

Some directories:
- `public`: rc files that is not quite machine-specific and directly loaded by the script
- `common`: common ingredients that may be linked into machine-specific directories, hence only indirectly loaded.

