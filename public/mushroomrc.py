#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""configurable parameters for mushroom"""
import os

# logging
# log_level = "notset"
log_level = "info"
logfile_mode = "w"
stream_level = "info"

# custom color map
color_map = (
    (140, 0, 0, "pkured"),
    (0, 63, 126, "ghblue"),
    (204, 12, 32, "Red"),  # #CC0C20
    # (30, 30, 101, "Blue"),  # the old Blue was too dark
    (46, 97, 207, "Blue"),  # #2E61CF
    (18, 147, 210, "ProcessBlue"),
    (66, 176, 193, "SkyBlue"),
    (18, 139, 57, "Green"),  # same to green4
    (120, 120, 120, "Gray"),
    (254, 208, 10, "Gold"),
    (209, 149, 26, "Goldenrod"),
    (204, 204, 216, "PalePurple"),
    (128, 72, 133, "Purple")
)
prefer_gracecolors = ["Red", "Blue", "orange", "SkyBlue", "Goldenrod", "Green"]

# numeric precision
symprec = 1.0e-5

try:
    db_cell_path = os.path.join(os.environ["M"], "Database", "crystal_structures")
except KeyError:
    db_cell_path = None

# workflow platform header
sbatch_headers = {
    # "wm": ["-A hpc0006175276",
    #        "--get-user-env",
    #        "--nodes=1",
    #        "--mail-type=end",
    #        "--mail-user=stevezhang@pku.edu.cn",
    #        "-n 32",
    #        "-x a7u03n10",
    #        "--qos=low",
    #        "--partition=C032M0128G",
    #        "-J test",
    #        "-o jobid%j-%N.out",],
    # "sk": ["-A hpc0006175276",
    #        "--get-user-env",
    #        "--nodes=1",
    #        "--mail-type=end",
    #        "--mail-user=stevezhang@pku.edu.cn",
    #        "-n 32",
    #        "--qos=low",
    #        "--partition=C032M0128G",
    #        "-J test",
    #        "-o jobid%j-%N.out",],
    # "bd": ["-A hpc0006175276",
    #        "--get-user-env",
    #        "--nodes=1",
    #        "--mail-type=end",
    #        "--mail-user=stevezhang@pku.edu.cn",
    #        "-n 24",
    #        "-x c22",
    #        "--qos=low",
    #        "--partition=C028M256G",
    #        "-J test",
    #        "-o jobid%j-%N.out",],
    "iophr": ["--partition=640",
              "--cpus-per-task=1",
              "--nodes=1",
              "--ntasks-per-node=48",
              "--mem=180000",
              "-J test",
              "-e err.slurm.%j-%N",
              "-o out.slurm.%j-%N",],
    "raven": ["--cpus-per-task=1",
              "--nodes=1",
              "--ntasks-per-node=72",
              "--mem=240000",
              "-J test",
              "-t 24:00:00",
              "-e tjob.err.%j",
              "-o tjob.out.%j",],
    "cobra": ["--partition=640",
              "--cpus-per-task=1",
              "--nodes=1",
              "--ntasks-per-node=40",
              "--mem=185000",
              "-J test",
              "-t 24:00:00",
              "-e tjob.err.%j",
              "-o tjob.out.%j",],
}

pbs_headers = {}

# key: `whoami`@`uname -n`
# value: (platform: str, use_pbs: bool)
uname_platforms = {
    # "1501210186@login01.hpc.pku.edu.cn": ("wm", False),
    # "1501210186@sk1-login01": ("sk", False),
    # "1501210186@bd-login01": ("bd", False),
    "zhangmy@mgmt": ("iophr", False),
    "minyez@raven01": ("raven", False),
    "minyez@raven02": ("raven", False),
    "minyez@cobra01": ("cobra", False),
    "minyez@cobra02": ("cobra", False),
}

aims_paths = [
    os.path.expanduser("~/software/FHIaims-latest-master-3/species_defaults"),
    os.path.expanduser("~/software/FHIaims-develop/species_defaults"),
    os.path.expanduser("~/programs/FHIaims-latest-master/species_defaults"),
    os.path.expanduser("~/programs/FHIaims-master/species_defaults"),
]
aims_species_defaults = None
for p in aims_paths:
    if (os.path.exists(p)):
        aims_species_defaults = p
        break

# remote servers for rsync distribution
dist_remotes = {
    # "tmcws": "/home/zhangmy/scripts/",
    # "iophr": "/home/zhangmy/",
    # "tmcpc": "/home/stevezhang/codes/project/",
    # "phbd": "/gpfs/share/home/1501210186/",
    # "phwm": "/gpfs/share/home/1501210186/",
    # "raven": "/u/minyez/",
    # "phsk": "/gpfs/share/home/1501210186/",
}
