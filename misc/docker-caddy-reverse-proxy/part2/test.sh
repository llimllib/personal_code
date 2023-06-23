#!/usr/bin/env bash

curl http://localhost/uuid
# should return something like:
# {
#   "uuid": "9a0470c3-5a24-46ea-9da8-aa0e38104cea"
# }
#
# try `curl http://localhost/get?abraham=lincoln` or visit http://localhost/ in
# your browser to see lots more endpoints you can play with
