# `install_tmcstu`

This is a set of scripts to install basic tools for research purpose
on a new student PC (Fedora 30+) in the TMC group.

## Usage

`install_tmcstu.sh` is the driver script.
Basically, you only need to run with the subcommand `init`.

```bash
chmod +x install_tmcstu.sh
./install_tmcstu.sh init
```

This will install some tools from the Fedora repository by `using dnf`,
hence you may need to enter password for sudo.
For more instruction, use

```bash
./install_tmcstu.sh help
```

## Customization

See `custom.sh`

## Extension

One may need to extend the list of online repositories to be downloaded,
or retrieve more packages from the TMC workstation.
To these ends, they can modify arrays in `extern_repos.sh` and `tmcws_pkgs.sh`, respectively.
See either script for more guidance.

Automatic installtion of the extended sources requires more efforts.

