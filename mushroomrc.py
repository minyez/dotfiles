#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""configurable parameters for mushroom"""

# logging
loglevel = "debug"
stream_level = "debug"

# numeric precision
symtol = 1.0e-5

# workflow platform header
sbatch_header = {
    "pkuhpc": ["-A hpc0006175276",
               "--get-user-env",
               "--nodes=1",
               "-n 32",
               "--qos=low",
               "--partition=C032M0128G",
               "-J test",
               "-o jobid%j-%N.out",]
}

