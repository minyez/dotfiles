#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""configurable parameters for mushroom"""

# logging
log_level = "debug"
stream_level = "info"

# custom color map for grace
color_map = (
    (140, 0, 0, "pkured"),
    (0, 63, 126, "ghblue"),
    (204, 12, 32, "Red"),
    (30, 30, 101, "Blue"),
    (18, 147, 210, "ProcessBlue"),
    (66, 176, 193, "SkyBlue"),
    (18, 139, 57, "Green"),
    (120, 120, 120, "Gray"),
    (254, 208, 10, "Gold"),
    )
prefer_gracecolors = ["Red", "blue", "orange", "Green", "Gold", "SkyBlue"]

# numeric precision
symprec = 1.0e-5

# workflow platform header
sbatch_headers = {
    "wm": ["-A hpc0006175276",
           "--get-user-env",
           "--nodes=1",
           "--mail-type=end",
           "--mail-user=stevezhang@pku.edu.cn",
           "-n 32",
           "-x a7u03n10",
           "--qos=low",
           "--partition=C032M0128G",
           "-J test",
           "-o jobid%j-%N.out",],
    "sk": ["-A hpc0006175276",
           "--get-user-env",
           "--nodes=1",
           "--mail-type=end",
           "--mail-user=stevezhang@pku.edu.cn",
           "-n 32",
           "--qos=low",
           "--partition=C032M0128G",
           "-J test",
           "-o jobid%j-%N.out",],
    "bd": ["-A hpc0006175276",
           "--get-user-env",
           "--nodes=1",
           "--mail-type=end",
           "--mail-user=stevezhang@pku.edu.cn",
           "-n 24",
           "-x c22",
           "--qos=low",
           "--partition=C028M256G",
           "-J test",
           "-o jobid%j-%N.out",],
}

pbs_headers = {}

# key: `whoami`@`uname -n`
# value: (platform: str, use_pbs: bool)
uname_platforms = {
        "1501210186@login01.pku.edu.cn": ("wm", False),
        "1501210186@sk-login01": ("sk", False),
        "1501210186@bd-login01": ("bd", False),
        }

# remote servers for rsync distribution 
dist_remotes = {
    "tmcws": "/home/zhangmy/scripts/",
    "stevezhang@222.29.156.87": "/home/stevezhang/codes/project/",
    "1501210186@162.105.133.134": "/gpfs/share/home/1501210186/",
    "1501210186@162.105.133.164": "/gpfs/share/home/1501210186/",
    "1501210186@115.27.161.31": "/gpfs/share/home/1501210186/",
    }
