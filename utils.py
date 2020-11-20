# -*- coding: utf-8 -*-
"""helper facilities"""
from os import geteuid
from pwd import getpwuid
from socket import gethostname

_user = getpwuid(geteuid()).pw_name
_host = gethostname()
user_at_host = "{}@{}".format(_user, _host)
del _user, _host, geteuid, getpwuid, gethostname

