from distutils.core import setup
from distutils.extension import Extension
from Pyrex.Distutils import build_ext
setup(
  name = "permute_c",
  ext_modules=[ 
    Extension("permute_c", ["permute_c.pyx"])
    ],
  cmdclass = {'build_ext': build_ext}
)

