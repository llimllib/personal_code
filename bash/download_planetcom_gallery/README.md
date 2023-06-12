# Download planet.com gallery images

I like to use the images from the [planet.com
gallery](https://www.planet.com/gallery/) as backgrounds for my computer. This
folder contains a script to download the largest-resolution versions of those
images, in parallel, with curl and jq.

The script is well commented, read it to find out how.

I assume you have `jq` and `curl` installed; `curl` has to be decently recent
since it only gained parallel download support in version 7.66

Execute it with `./dl.sh` and it will create an `images` directory and download
a bunch of large satellite images to it.
