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
# $Id: emusic.py,v 1.6 2003/02/11 22:41:29 icon Exp $
#
# @Author Konstantin Riabitsev <icon@duke.edu> ($Author: icon $)
# @Version $Date: 2003/02/11 22:41:29 $
#

import exceptions
import os
import re
import shutil
import urllib
import xml.dom.minidom

def getTextVal(node):
    """
    getTextVal(node):
        The node is an element node containing text nodes. This function
        will concatenate the text nodes together and return them as one
        string. Any non-text nodes will be ignored.
    """
    val = ''
    for childnode in node.childNodes:
        if childnode.nodeType == childnode.TEXT_NODE:
            val = val + childnode.data
    return val

def getChildDict(node):
    """
    getChildDict(node):
        This will take a node full of other simple element nodes and
        return a dictionary with the element name as the key and the
        value as value of each child node. Really useful with simple
        xml documents.
    """
    valdict = {}
    for eltnode in node.childNodes:
        if eltnode.nodeType == eltnode.ELEMENT_NODE:
            name = eltnode.nodeName
            val = getTextVal(eltnode)
            valdict[name] = val
    return valdict

class NetworkError(exceptions.Exception):
    """
    exception NetworkError:
        Exception occurring when the software encounters a problem downloading
        a file off the network. Can be a 404, problem finding host, etc. Not
        very specific.
    """
    def __init__(self, args = None):
        self.args = args

class ParserError(exceptions.Exception):
    """
    exception ParserError:
        This exception is thrown whenever there are problems parsing the EMP
        file.
    """
    def __init__(self, args = None):
        self.args = args

class TrackIndexError(exceptions.Exception):
    """
    exception TrackIndexError:
        This exception is thrown whenever someone attempts to access
        a track that does not exist.
    """
    def __init__(self, args = None):
        self.args = args

class EmpGet:
    """
    class EmpGet:
        attributes:
            conf (obj):
                A config object passed to __init__
            title (str):
                The title of the package (PACKAGE->TITLE)
            provider (obj):
                the Provider object
            tracks[] (tuple):
                a tuple full of Track objects
            curtrack (int):
                index of the current track in the tracks[] tuple
    """
    curtrack = -1
    
    def __init__(self, empfileurl, empcfg):
        """
        __init__:
            accepts the following parameters:
                empfileurl: the url of the EMP file to process
                empcfg: the config object. It can be anything, as long
                        as it has the following attributes:
                            whitespace: substitute whitespace for underscores
                                        1 - make substitution
                                        0 - leave it like it is
                            prefix: the path where to put files
                        additional attributes may be added as API changes
        """
        try:
            ##
            # Get the emp file. It can be a URL, too, for all we care.
            #
            (empfile, hdr) = urllib.urlretrieve(empfileurl)
        except:
            raise IOError('Could not get .emp file from "%s"' % empfileurl)

        ##
        # Store the config in the object for later reference.
        #
        self.conf = empcfg

        ##
        # Now we parse the EMP file. If there are any errors, we just
        # bail out.
        #
        try:
            eDom = xml.dom.minidom.parse(empfile)
            packageNode = eDom.getElementsByTagName('PACKAGE')[0]
            ##
            # Set the title
            #
            self.title = getTextVal(
                packageNode.getElementsByTagName("TITLE")[0])
            ##
            # Set the provider object
            #
            self.provider = Provider(eDom)
            ##
            # Populate the tracks[] tuple
            #
            tlNode = eDom.getElementsByTagName("TRACKLIST")[0]
            tracks = []
            for trackNode in tlNode.getElementsByTagName("TRACK"):
                newtrack = Track(trackNode, self.conf.scheme)
                ##
                # This is where we take care of those pesky whitespaces
                #
                if self.conf.whitespace:
                    newtrack.nixSpaces()
                tracks.append(newtrack)
            ##
            # Done. Stick it into the object.
            #
            self.tracks = tracks
        except:
            ##
            # Bail out.
            #
            raise ParserError

    def nextTrack(self):
        """
        nextTrack():
            Advances to the next track and returns 0 if there are no more
            tracks.
        """
        self.curtrack = self.curtrack + 1
        if self.curtrack >= len(self.tracks):
            return 0
        else:
            return 1

    def rewind(self):
        """
        rewind():
            Sets the current track back to the beginning. NOTE that it will
            make the current index -1, so you will need to use nextTrack()
            or setTrackByTrackid() in order to make the index to actually
            point at something useful.
        """
        self.curtrack = -1

    def setTrackByTrackid(self, trackid):
        """
        setTrackByTrackid(trackid):
            Sets the current track by the trackid specified
            (PACKAGE->TRACKLIST->TRACK->TRACKID). This is the only real
            unique identifier to be used safely. Throws TrackIndexError
            exception if the track is not found.
        """
        for index in range(0, len(self.tracks)):
            if self.tracks[index].trackid == trackid:
                self.curtrack = index
                return 1
        raise TrackIndexError('Track with id "%s" not found' % trackid)
    
    def getCurrentTrack(self):
        """
        getCurrentTrack():
            This simply returns the current track object to whoever asks
            for it. Useful for UIs. Throws TrackIndexError exception if
            it tries to get a track that doesn't exist.
        """
        self.__sanityCheck()
        return self.tracks[self.curtrack]

    def getArtwork(self):
        """
        getArtwork():
            Returns the path to the artwork of the current track.
            None if download failed, or no artwork was specified.
        """
        self.__sanityCheck()
        return self.tracks[self.curtrack].getart()

    def getTitle(self):
        """
        getTitle():
            Returns the title of the current track.
        """
        self.__sanityCheck()
        return self.tracks[self.curtrack].title

    def trackFileExists(self):
        """
        trackFileExists():
           This checks if the current track has already been downloaded.
           Since there is no sane way to check if the file is the same or
           different, this just checks fro the file presense in the specified
           prefix. Returns 1 if file is already there, or 0 if not.
        """
        self.__sanityCheck()
        if self.tracks[self.curtrack].exists(self.conf.prefix):
            return 1
        return 0
        
    def downloadTrack(self, repfunc = None):
        """
        downloadTrack(repfunc):
            This method will download the current track and use the function
            passed as repfunc as the callback function to the urlretrieve.
            If you are writing an UI, you should make use of this to show the
            progress of the download. For more info see urllib.urlretrieve().
        """
        self.__sanityCheck()
        if repfunc == None:
            ##
            # Use the dummy instead
            #
            repfunc = self.__repdummy
        ##
        # Fetch the bugger.
        #
        self.tracks[self.curtrack].fetch(self.provider, self.conf.prefix,
                                         repfunc)
    def __repdummy(self):
        """
        __repdummy():
            A dummy method in case no repfunc was provided for downloading.
            Doesn't do anything.
        """
        pass

    def __sanityCheck(self):
        """
        __sanityCheck():
            This method checks if the track index is out of bounds and
            raises TrackIndexError exception if it is.
        """
        if self.curtrack < 0 or self.curtrack >= len(self.tracks):
            raise TrackIndexError('Track index out of range')
        

class Provider:
    """
    Provider:
        Class hoding server information.
        Attributes:
            protocol (str):
                always 'http'
            name (str):
                the name of the provider (PACKAGE->PROVIDER->NAME)
            copyright (str):
                copyright information (PACKAGE->PROVIDER->COPYRIGHT)
            url (str):
                the URL of the provider (PACKAGE->PROVIDER->URL)
            host (str):
                the host where we'll be getting our music
                (PACKAGE->SERVER->NETNAME)
            pathmask (str):
                The '/%fid/%f' mask. Does it ever change?
                (PACKAGE->SERVER->LOCATION)
    """
    protocol = 'http'
    
    def __init__(self, eDom):
        """
        __init__:
            Takes the document node (PACKAGE) as constructor argument.
        """
        try:
            provNode = eDom.getElementsByTagName("PROVIDER")[0]
            valdict = getChildDict(provNode)
            self.name = valdict['NAME']
            self.copyright = valdict['COPYRIGHT']
            self.url = valdict['URL']
            
            serverNode = eDom.getElementsByTagName("SERVER")[0]
            valdict = getChildDict(serverNode)
            self.host = valdict['NETNAME']
            self.pathmask = valdict['LOCATION']
        except IndexError:
            ##
            # Doesn't seem like a valid EMP file to me.
            #
            raise ParserError

class Track:
    """
    class Track:
        This class represents each track in the tracklist.
        Attributes directly reflect the element nodes of the TRACK
        node, except they are lowercased. Additional attributes are:
        trackpath (str):
            the relative path of the track consisting of
            Artist/Album/trackname.mp3
    """
    def __init__(self, tracknode, scheme):
        """
        __init__:
           Initialize a Track object from a TRACK node passed to it during
           constructor initialization. The prefix is taken directly from the
           config object passed to EmpGet during init.
        """
        try:
            valdict = getChildDict(tracknode)
            self.trackid = valdict['TRACKID']
            self.tracknum = int(valdict['TRACKNUM'])
            self.title = valdict['TITLE']
            self.album = valdict['ALBUM']
            self.artist = valdict['ARTIST']
            self.genre = valdict['GENRE']
            self.filename = valdict['FILENAME']
            self.format = valdict['FORMAT']
            self.quality = int(valdict['QUALITY'])
            self.channels = int(valdict['CHANNELS'])
            self.duration = int(valdict['DURATION'])
            self.arturl = valdict['ALBUMART']
        except IndexError, e:
            raise ParserError

        ##
        # Generate a relative trackpath
        #
        self.trackpath = self.__mktrackpath(scheme)
            
    def nixSpaces(self):
        """
        nixSpaces():
            This method will remove all spaces from the localpath
            attribute so these files are easier to access via the
            command line.
        """
        self.trackpath = re.sub(re.compile('\s'), '_', self.trackpath)
        
    def exists(self, prefix):
        """
        exists(prefix):
            This function checks if this track has already been downloaded
            into the prefix provided. It just checks if the file exists, it
            can't do any more in-depth checking, like comparing if the file
            is corrupted or incomplete.
        """
        localpath = self.__localpath(prefix)
        if os.access(localpath, os.F_OK):
            return 1
        return 0

    def fetch(self, provider, prefix, repfunc):
        """
        fetch(provider, prefix, repfunc):
            This will download the track to the local disk. This method takes
            the following parameters:
            provider (obj):
                The Provider object with all server info
            prefix (str):
                Where to put all the files.
            repfunc (function):
                This is what is going to be used as a callback function for
                urllib.urlretrieve(). See reference for urllib for more
                information.
        """
        try:
            (trackfile, hdr) = urllib.urlretrieve(self.__mkurl(provider),
                                                  reporthook = repfunc)
        except IOError:
            raise NetworkError('Download failed from "%s"' % provider.host)

        ##
        # Check if we have 'Content-Length' in the header. If not, then
        # this was likely a 404 or some other boo-boo.
        #
        if hdr == None or not hdr.has_key('Content-Length'):
            raise NetworkError('No joy getting track "%s" from "%s"'
                               % (self.title, provider.host))
        ##
        # We seem to have gotten the file ok. Now let's put it where it
        # belongs.
        self.__copytrack(trackfile, prefix)

    def getart(self):
        """
        getart():
            Downloads the artwork and places it somewhere in a temporary
            location. Returns the path to that location.
        """
        try:
            (artfile, hdr) = urllib.urlretrieve(self.arturl)
        except IOError:
            ##
            # Ah, failed. Oh well, return None.
            #
            return None

        ##
        # Check if we have 'Content-Length' in the header. If not, then
        # this was likely a 404 or some other boo-boo.
        #
        if hdr == None or not hdr.has_key('Content-Length'):
            return None
        ##
        # Ok. Return the artfile.
        #
        return artfile


    def __localpath(self, prefix):
        """
        __localpath(prefix):
            Look at the prefix and the trackpath and join them together,
            making a spanking good full path to the file. Yeah, baby!
        """
        localpath = os.path.join(prefix, self.trackpath)
        return localpath

    def __copytrack(self, trackfile, prefix):
        """
        __copytrack(trackfile, prefix):
            This copies the freshly downloaded track from the temporary
            location into the prefix specified. This method will create the
            directories as necessary and trow IOError if it is unsuccessful
            in doing anything.
        """
        localpath = self.__localpath(prefix)
        dirpath = os.path.dirname(localpath)
        ##
        # Check if the directory exists, and create it if necessary
        #
        if not os.path.isdir(dirpath):
            try:
                os.makedirs(dirpath)
            except OSError, e:
                raise IOError('Error trying to create directory "%s"'
                              % dirpath)
        try:
            ##
            # Copy the file already!
            #
            shutil.copyfile(trackfile, localpath)
        except:
            raise IOError('Could not move downloaded track "%s" into "%s"'
                          % (trackfile, localpath))

    def __mkurl(self, provider):
        """
        __mkurl(provider):
            This will take a look at the provider object, local trackid,
            and local filename, and figure out how to make a url where
            we get our music.
        """
        path = provider.pathmask
        path = re.sub(re.compile('%fid', re.M), self.trackid, path)
        path = re.sub(re.compile('%f', re.M), self.filename, path)
        trackurl = "%s://%s%s" % (provider.protocol, provider.host, path)
        return trackurl

    def __nixSlashes(self, dirty):
        """
        __nixSlashes(dirty):
            Removes forward slashes from the string, since they will confuse
            the os.path module when creating directories.
        """
        clean = re.sub(re.compile('/'), '_', dirty)
        return clean


    def __mktrackpath(self, scheme):
        """
        __mktrackpath(scheme):
            Makes a trackpath based on a passed filename scheme.
            The scheme is a sprintf-able string ready to accept a
            dictionary consisting of five keys:
            tracknum, filename, artist, album, title
        """
        nm = {}
        nm['tracknum'] = self.tracknum
        nm['filename'] = self.__nixSlashes(self.filename)
        nm['artist'] = self.__nixSlashes(self.artist)
        nm['album'] = self.__nixSlashes(self.album)
        nm['title'] = self.__nixSlashes(self.title)
        try:
            trackfile = scheme % nm
        except:
            ##
            # Don't fail for stupid schemes. Use the default.
            #
            trackfile = '%(tracknum)02d.%(filename)s' % nm
        trackpath = os.path.join(nm['artist'], nm['album'], trackfile)
        return trackpath
