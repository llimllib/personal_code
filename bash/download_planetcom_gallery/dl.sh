#!/usr/bin/env bash
# Download images from the planet.com gallery. requires modern versions of jq and curl

# First, get the json file containing information about all the images. the
# request appears to not require any cookies of any sort
if [ ! -f res.json ]; then
    curl 'https://api.planet.com/gallery/v1/posts/' \
        --compressed \
        -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/114.0'  \
        -H 'Accept: */*'  \
        -H 'Accept-Language: en-US,en;q=0.5'  \
        -H 'Accept-Encoding: gzip, deflate, br'  \
        -H 'Referer: https://www.planet.com/'  \
        -H 'Origin: https://www.planet.com'  \
        -H 'Connection: keep-alive'  \
        -H 'Sec-Fetch-Dest: empty'  \
        -H 'Sec-Fetch-Mode: cors'  \
        -H 'Sec-Fetch-Site: same-site'  \
        -H 'If-None-Match: W/"1325c4-RkX4YiJHuKnycMdAycfHyxszmV8"' |
        jq > res.json
fi

# make a file to store the URLs contained within res.json
URLS=$(mktemp)

# get all the images arrays, and select the objects within where there is a
# "full" key and it is a URL, and save them in curl config format.
jq -r '.[].images[] | select(.full | startswith("https")) | "url = \(.full)\n"' res.json > "$URLS"

# make the output dir if it doesn't exist
mkdir -p images

# download:
# - with the remote file name in place
# - to the `images` dir
# - in parallel, with a max of 5 at a time
# - only download if the file is incomplete
# - output the URL and response code
# - from the curl config file we created above
#
# notes: 
# - you need "--remote-name-all" instead of just "--remote-name", which
#   I think would only apply for the first given URL
# - curl has no way to say "if the filename exists, don't download" (which is
#   annoying!) so instead we pass `--continue-at -`, which will check the
#   server to see if we have the full file. This is more comprehensive - it
#   protects against partial downloads - but it's robustness we don't really
#   need here. I wish curl had an equivalent of wget's --no-clobber.
curl --remote-name-all --output-dir images \
    --parallel --parallel-immediate --parallel-max 5 \
    --continue-at - \
    --write-out "%{response_code} -> %{filename_effective}\n" \
    --config "$URLS"
