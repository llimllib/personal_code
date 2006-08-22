#!/usr/bin/python
import os, sys

def extract(filename, len, extract_cmd):
    dirname = filename[:-len]
    cmd = "mkdir " + dirname
    print "making directory..."
    os.system(cmd)
    cmd = "mv " + filename + " " + dirname
    print cmd, filename, dirname + os.sep + filename
    print "moving file into directory..."
    print os.environ.get('USERNAME', 'None. Nope no username ------------')
    print os.access(dirname, os.F_OK), os.access(dirname, os.W_OK)
    old, new = filename, dirname + '/' + filename
    os.rename(old, new)
    #os.system(cmd)
    #cmd = extract_cmd + filename
    #print cmd
    cmd = extract_cmd + new
    import pdb; pdb.set_trace()
    print "untarring file..."
    os.chdir(dirname)
    os.system(cmd)

if __name__ == '__main__':
    helpmsg = """
    Usage: extract.py filename
    """
    #if len(sys.argv) != 2:
    #    print helpmsg
    #    print "wrong len " + str(len(sys.argv))
    #    sys.exit(1)
    print sys.argv
    f = sys.argv[-1]
    ext = sys.argv[1][-7:]
    exts = {"tar.bz2": "tar -xjsf ",
        "tar.gz": "tar -xzsf ",
        "tgz": "tar -xzsf ",
        "gz": "gunzip ",
        ".zip": "unzip -d %s " % f[:-4],
        ".jar": "jar xf ",
        ".tar": "tar -xsf ",
        ".tar.Z": "tar -xZsf "}
    if ext == "tar.bz2":
        extract(f, 8, 'tar -xjsf ')
    elif ext == ".tar.gz":
        extract(f, 7, 'tar -xzsf ')
    elif ext[-4:] == ".tgz":
        extract(f, 4, 'tar -xzsf ')
    elif ext[-3:] == ".gz":
        extract(f, 3, 'gunzip ')
    elif ext[-4:] == ".zip":
        extract(f, 4, 'unzip -d %s ' % f[:-4])
    elif ext[-4:] == ".jar":
        extract(f, 4, 'jar xf ')
    elif ext[-4:] == ".tar":
        extract(f, 4, 'tar -xsf ')
    elif ext[-6:] == ".tar.Z":
        extract(f, 6, 'tar -xZsf ')
