- started work updating ckmeans to modern python
  - cloning straight from [the author's source](https://github.com/cran/Ckmeans.1d.dp/blob/f7f2920fc9aabab184a2acff29e7965ce4f90173/src/Ckmeans.1d.dp.cpp), it has changed a lot in the intervening years
  - making it into typed python
  - going to try implementing it in plain python, and maybe then in numpy to see what the performance difference is
    - want to compare it to the old version too. Hopefully will be faster! It's definitely shaping up to be clearer
  - work in progress [here](https://github.com/llimllib/ckmeans/blob/aa1f44bde6d2c0ba9ca920dd626052719ad6f94a/ckmeans2.py)
