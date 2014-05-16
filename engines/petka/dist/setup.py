from cx_Freeze import setup, Executable

import cx_Freeze.util

import sys, os, struct

# Dependencies are automatically detected, but it might need
# fine tuning.
buildOptions = dict(packages = ["re", "io", "PIL", "traceback", "zlib", "gzip", "argparse", "struct", "binascii"], \
    excludes = ["_posixsubprocess"],
    include_files = [],
    compressed = True, silent = True,\
    optimize = 2, copy_dependent_files = True, \
    create_shared_zip = True, include_in_shared_zip = True)

executables = [
    Executable('p12explore.py',
        base = 'Win32GUI',
        targetName = "p12explore.exe")
]

setup(name='p12explore',
      version = '0.2',
      description = 'Petka 1&2 explorer',
      author = "romiq.kh@gmail.com, https://bitbucket.org/romiq/p12simtran",
      options = dict(build_exe = buildOptions),
      executables = executables)
