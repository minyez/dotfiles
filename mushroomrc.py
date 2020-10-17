#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""configurable parameters for mushroom"""

# logging
log_level = "debug"
stream_level = "info"

# numeric precision
symtol = 1.0e-5

# workflow platform header
sbatch_headers = {
    "pkuhpc": ["-A hpc0006175276",
               "--get-user-env",
               "--nodes=1",
               "-n 32",
               "--qos=low",
               "--partition=C032M0128G",
               "-J test",
               "-o jobid%j-%N.out",]
}

pbs_headers = {}

dist_remotes = {
    "tmcws": "/home/zhangmy/scripts/",
    "stevezhang@222.29.156.87": "/home/stevezhang/codes/project/",
    "1501210186@162.105.133.134": "/gpfs/share/home/1501210186/",
    "1501210186@162.105.133.164": "/gpfs/share/home/1501210186/",
    }
