from mod_python import apache

def authenhandler(req):

    pw = req.get_basic_auth_pw()
    user = req.user

    if user == "spam" and pw == "eggs":
       return apache.OK
    else:
       return apache.HTTP_UNAUTHORIZED

def handler(req):
    req.content_type = "text/plain"
    for x in dir(req): req.write(str(x) + "\n")
    req.write(req.uri)
    return apache.OK
