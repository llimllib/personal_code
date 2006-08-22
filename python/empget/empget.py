#!/usr/bin/python
#
# Copyright (C) 2002-2003 by Konstantin Riabitsev
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  
# 02111-1307, USA.
#
# $Id: empget.py,v 1.5 2003/02/11 22:41:29 icon Exp $
#
# @Author Konstantin Riabitsev <icon@duke.edu> ($Author: icon $)
# @Version $Date: 2003/02/11 22:41:29 $
#

import ConfigParser
import emusic
import getopt
import os.path
import re
import sys

VERSION = '0.9.5'

class EConfig:
    """
    This class represents the configuration for empget, placed into the
    user homedir: ~/.empget
    """
    def __init__(self):
        config = ConfigParser.ConfigParser()
        config.read(os.path.expanduser('~/.empget'))
        try:
            self.whitespace = config.getboolean('main', 'fix_whitespace')
        except:
            self.whitespace = 1
        try:
            template = config.get('main', 'filename_scheme')
            self.scheme = self.__mkscheme(template)
        except:
            self.scheme = '%(tracknum)02d.%(filename)s'
        try:
            self.prefix = config.get('main', 'download_prefix')
        except:
            self.prefix = os.path.expanduser('~/music/emusic')

    def __mkscheme(self, scheme):
        """
        __mkscheme:
            This method accepts a template, and makes a sprintf'able
            scheme out of it.
            E.g.:
            '[tracknum] - [title].mp3' => '%(tracknum)02d - %(title)s.mp3'
        """
        scheme = re.sub(re.compile('\[tracknum\]'), '%(tracknum)02d', scheme)
        scheme = re.sub(re.compile('\[title\]'), '%(title)s', scheme)
        scheme = re.sub(re.compile('\[album\]'), '%(album)s', scheme)
        scheme = re.sub(re.compile('\[artist\]'), '%(artist)s', scheme)
        scheme = re.sub(re.compile('\[filename\]'), '%(filename)s', scheme)
        return scheme

def showRep(blockcount, blocksize, totalsize):
    """
    showRep(blockcount, blocksize, totalsize):
        This function draws nice progress bars and is being called by the
        urllib.urlretrieve() method. See that for more info.
    """
    ##
    # Grab the tracktitle from global, since we have no way of getting to
    # it from here.
    #
    global tracktitle
    title = tracktitle
    ##
    # Do some nifty calculations to present the bar
    #
    downsize = blockcount * blocksize
    totalmb = float(totalsize)/1024/1024
    if len(title) > 50:
        title = title[:50]
    title = "%s [%0.1fMb]" % (title, totalmb)
    barwidth = 70 - len(title) - 2
    barmask = "[%-" + str(barwidth) + "s]"
    bardown = int(barwidth*(float(downsize)/float(totalsize)))
    bar = barmask % ("=" * bardown)
    sys.stdout.write("\r%s: %s\r" % (title, bar))
        
def err(fatal, message):
    """
    err(fatal, message):
        This function will print a message, and die if fatal = 1.
    """
    print "ERROR: %s" % message
    if fatal:
        sys.exit(1)

def usage(myname):
    print """Usage: %s [-f] [-p path] [-w] filename.emp
     -f:
         Overwrite the tracks already existing on the disk.
         Default is no.

     -p path:
         Specify a path where to download the music. Specifying it on
         the command line will override the setting in ~/.empget
         Defaults to ~/music/emusic/.

     -w:
         Preserve the whitespace as it is. By default, empget will
         replace any whitespace characters in the filenames into
         underscores. Specifying '-w' will turn this off. The
         command-line setting overrides the one in ~/.empget.

     -V:
         Print version and exit.
         
     Example:
         %s -p /xtmp/emusic -w The_Lost_Souls.emp

     More info:
         See man empget(8).
    """ % (myname, myname)
    sys.exit(1)

def doempfile(empfile, empcfg):
    """
    doempfile(empfile, empcfg):
        Processes a given empfile.
    """
    ##
    # Let the magic begin!
    #
    try:
        empget = emusic.EmpGet(empfile, empcfg)
    except emusic.ParserError:
        err(0, 'Error parsing the EMP file "%s!"' % empfile)
        return
    except IOError, e:
        err(0, 'Could not read the EMP file "%s!"' % empfile)
        return

    print 'Getting "%s" from "%s"' % (empget.title, empget.provider.name)
    while empget.nextTrack():
        ##
        # Get the title and make it global so repfunc can get to it
        #
        global tracktitle
        tracktitle = empget.getTitle()
        ##
        # Download the file only if:
        #     1: We don't have it
        #     2: We have it, but -f was specified
        #
        if ((empget.trackFileExists() and empcfg.force_reget) or
            not empget.trackFileExists()):
                try:
                    ##
                    # Get it!
                    #
                    empget.downloadTrack(showRep)
                    print
                except emusic.NetworkError, e:
                    ##
                    # Network errors are not fatal. Show the error, but
                    # do not die.
                    #
                    print
                    err(0, e)
                except IOError, e:
                    ##
                    # This means trouble saving the file, or somesuch.
                    # Die.
                    print
                    err(1, e)
                except emusic.TrackIndexError, e:
                    ##
                    # Mmph. This should never happen. Something very bizarre
                    # is going on if this is ever thrown here.
                    #
                    print
                    err(1, 'Mantra overflow: %s' % e)
        else:
            ##
            # No need to download it again.
            #
            print 'Track "%s" already downloaded, skipping' % tracktitle
    print 'Done with "%s"' % empget.title

def main(args):
    cmdargs = args[1:]
    ##
    # Parse the command line arguments
    #
    force_reget = 0
    prefix = None
    whitespace = None
    try:
        gopts, cmds = getopt.getopt(cmdargs, 'VfwWp:', [])
        for o,a in gopts:
            if o == '-V':
                print 'EMPget version %s' % VERSION
                sys.exit(0)
            if o == '-f':
                force_reget = 1
            elif o == '-p':
                prefix = a
            elif o == '-w':
                whitespace = 0
            elif o == '-W':
                whitespace = 2
    except getopt.error, e:
        usage(args[0])

    if len(cmds) < 1:
        usage(args[0])

    empfiles = cmds
    empcfg = EConfig()

    ##
    # See if prefix was specified on the command line and stick it into
    # the config object.
    #
    if prefix is not None:
        empcfg.prefix = prefix
    ##
    # Same with whitespace
    #
    if whitespace is not None:
        empcfg.whitespace = whitespace
    ##
    # Stick force_reget into the config object so we don't have to
    # explicitly pass it.
    #
    empcfg.force_reget = force_reget

    ##
    # Process the EMP files.
    #
    for empfile in empfiles:
        doempfile(empfile, empcfg)
    if len(empfiles) > 1:
        print 'Finished with all files.'
    sys.exit(0)

if __name__ == "__main__":
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        ##
        # Trap keyboard interrupts nicely.
        #
        print
        print 'Interrupted by user'
        sys.exit(2)
