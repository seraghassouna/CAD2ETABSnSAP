from py2exe.build_exe import py2exe
from distutils.core import setup
import numpy
datafiles = [('',['icon_csssol.ico']),('',['facilitate.xlsm'])]
setup(windows = [{"script":"CAD2ETABSnSAP_1_0_0.py","icon_resources":[(0,"icon_csssol.ico")]}],data_files=datafiles,
      name="CAD2ETABSnSAP_1_0_0",author="Serag Hassouna",maintainer="Serag Hassouna")
