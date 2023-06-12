# #!/usr/bin/env
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
# "full" key and it is a URL, and save them in curl command format. jq
# apparently can't output a newline? so output a pipe char and replace it with
# a newline with `tr`. I verified manually that there were no pipe characters
# in the URLs
jq -r '.[].images[] | select(.full | startswith("https")) | "url = \(.full)|"' res.json |
    tr '|' '\n' > "$URLS"

# make the output dir if it doesn't exist
mkdir -p images

# download:
# - with the remote file name in place
# - to the `images` dir
# - in parallel, with a max of 5 at a time
# - from the curl config file we created above
#
# note that you need "--remote-name-all" instead of just "--remote-name", which
# I think would only apply for the first given URL
curl --remote-name-all --output-dir images \
    --parallel --parallel-immediate --parallel-max 5 \
    --no-clobber \
    --config "$URLS"
