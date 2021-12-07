from distutils.core import setup
from Cython.Build import cythonize

setup(name='cycpu', ext_modules=cythonize("cycpu.pyx"))
