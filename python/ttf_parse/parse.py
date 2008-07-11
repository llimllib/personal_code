from struct import unpack, calcsize

buf = file('Vera.ttf').read()

#uint32  	scaler type  	A tag to indicate the OFA scaler to be used to rasterize this font; see the note on the scaler type below for more information.
#uint16 	numTables 	number of tables
#uint16 	searchRange 	(maximum power of 2 <= numTables)*16
#uint16 	entrySelector 	log2(maximum power of 2 <= numTables)
#uint16 	rangeShift 	numTables*16-searchRange

header = ">I4H"
scaler, numTables, searchRange, entrySelector, rangeShift =  \
    unpack(header, buf[:calcsize(header)])
ptr = calcsize(header)
print scaler, numTables, searchRange, entrySelector, rangeShift

assert scaler in (0x10000, 0x74727565)
assert numTables > 8 #there are 9 required tables
from math import log, floor
assert searchRange == (2 ** floor(log(numTables, 2))) * 16
assert entrySelector == floor(log(numTables, 2))
assert rangeShift == numTables * 16 - searchRange

#Type  	Name  	Description
#uint32 	tag 	4-byte identifier
#uint32 	checkSum 	checksum for this table
#uint32 	offset 	offset from beginning of sfnt
#uint32 	length 	length of this table in byte (actual length not padded length)

table_directory = {}
table_entry = ">4s3I"
for i in range(numTables):
    entry = unpack(table_entry, buf[ptr:ptr+calcsize(table_entry)])
    table_directory[entry[0]] = entry[1:]
    ptr += calcsize(table_entry)
print table_directory

#let's calculate a checksum:
#The table directory includes checkSum, a number
#which can be used to verify the identity of and the integrity of the data in
#its associated tagged table. Table checksums are the unsigned sum of the longs
#in a table. The following C function can be used to determine the checksum of a
#given table:
#
#uint32 CalcTableChecksum(uint32 *table, uint32 numberOfBytesInTable)
#    { 
#    uint32 sum = 0; 
#    uint32 nLongs = (numberOfBytesInTable + 3) / 4;
#
#    while (nLongs-- > 0) sum += *table++;
#
#    return sum; 
#    }

#so some of these pass, others fail...
#how is it possible to have a table length of 139? It's not divisible by 4?
#  that's why they calculate nLongs and then work off that... so just
#  use nLongs * 4 as your length
#    tests no longer fail, but neither did we gain a true
#the checksums that we're calculating are quite a bit larger than the ones
#given by the font
#  perhaps nlongs is being calculated improperly?
#    subtracting one made the two true vales negative by a small amount, but 
#    didn't affect many of the false values and the ones it did affect are
#    still *way* too large
#for key in table_directory:
#    try:
#        checksum, offset, length = table_directory[key]
#        nlongs = (length + 3) / 4 #will fail in py3k
#        mysum = sum(unpack(">%sI" % nlongs, buf[offset:offset+(nlongs*4)]))
#        print key, checksum, mysum, checksum == mysum or mysum - checksum
#        i = 0
#        #while mysum > checksum:
#        #    nlongs -= 1
#        #    mysum = sum(unpack(">%sI" % nlongs, buf[offset:offset+(nlongs*4)]))
#        #    i += 1
#        #if i: print "it took %s tries to get to %s" % (i, mysum)
#    except Exception, e:
#        print str(e) + ", got %s" % length

#hmmm.... well that didn't work well. We got correct checksums for only two tables.
#let's try and read some data from the cmap table and see if it gives any hints:
#UInt16  	version         	Version number (Set to zero)
#UInt16 	numberSubtables 	Number of encoding subtables
ptr = table_directory['cmap'][1]
header = ">2H"
version, numberSubtables = unpack(header, buf[ptr:ptr+calcsize(header)])
print "version: %s subtables: %s" % (version, numberSubtables)
ptr += calcsize(header)

#next up come the "encoding subtables"
#UInt16  	platformID          Platform identifier
#UInt16 	platformSpecificID 	Platform-specific encoding identifier
#UInt32 	offset 	            Offset of the mapping table
header = ">HHI"
sz = calcsize(header)
subtables = [unpack(header, buf[ptr+sz*i:ptr+sz*(i+1)])
             for i
             in range(numberSubtables)]
print subtables

#next we read a short to determine what format we've got:
ptr = table_directory['cmap'][1]
#the first font table is a type 0:
#UInt16  	format  	Set to 0
#UInt16 	length 	Length in bytes of the subtable (set to 262 for format 0)
#UInt16 	language 	Language code for this encoding subtable, or zero if language-independent
#UInt8 	glyphIndexArray[256] 	An array that maps character codes to glyph index values

#the second is type 4:
#UInt16  	format  	Format number is set to 4  	 
#UInt16 	length 	Length of subtable in bytes 	 
#UInt16 	language 	Language code for this encoding subtable, or zero if language-independent 	 
#UInt16 	segCountX2 	2 * segCount 	 
#UInt16 	searchRange 	2 * (2**FLOOR(log2(segCount))) 	 
#UInt16 	entrySelector 	log2(searchRange/2) 	 
#UInt16 	rangeShift 	(2 * segCount) - searchRange 	 
#UInt16 	endCode[segCount] 	Ending character code for each segment, last = 0xFFFF. 	
#UInt16 	reservedPad 	This value should be zero 	
#UInt16 	startCode[segCount] 	Starting character code for each segment 	
#UInt16 	idDelta[segCount] 	Delta for all character codes in segment 	 
#UInt16 	idRangeOffset[segCount] 	Offset in bytes to glyph indexArray, or 0 	 
#UInt16 	glyphIndexArray[variable] 	Glyph index array
#for now we'll just load those first 7 variables to see what's going on
cmap_tables = []
for i, table in enumerate(subtables):
    _, _, offset = table
    start = ptr + offset * i
    #sometimes format is not a short. We'll ignore these cases, especially since they
    #make use of a datatype whose representation is not specified, and I have no examples
    #from which to test.
    format = unpack(">H", buf[start:start+2])[0]
    if format == 0:
        fmt = ">3H256B"
        data = unpack(fmt, buf[start:start+calcsize(fmt)])
        data = tuple(data[:3]) + (tuple(data[3:]), )
        cmap_tables.append(data)
        print "table %s type 0:\n%s" % (i, data)
    elif format == 4:
        fmt = ">7H"
        sz = calcsize(fmt)
        data = unpack(fmt, buf[start:start + sz])
        format, length, language, segCountX2, searchRange, entrySelector, rangeShift = data
        segCount = data[3] / 2
        cur = start + sz


        def getShortArray(ptr, buf, segCount):
            fmt = ">%sH" % segCount
            sz = calcsize(fmt)
            shortArr = unpack(fmt, buf[cur:cur + sz])
            return shortArr, cur + sz
            
        endCode, cur = getShortArray(cur, buf, segCount)
        assert endCode[-1] == 0xFFFF

        assert unpack(">H", buf[cur:cur+2])[0] == 0
        cur += 2

        startCode, cur = getShortArray(cur, buf, segCount)
        assert startCode[-1] == 0xFFFF

        idDelta, cur = getShortArray(cur, buf, segCount)
        idRangeOffset, cur = getShortArray(cur, buf, segCount)
        glyphIndexArray, _ = getShortArray(cur, buf, (start + length - cur) / 2)

        #To use these arrays, it is necessary to search for the first endCode that is
        #greater than or equal to the character code to be mapped. If the corresponding
        #startCode is less than or equal to the character code, then use the
        #corresponding idDelta and idRangeOffset to map the character code to the glyph
        #index. Otherwise, the missing character glyph is returned. To ensure that the
        #search will terminate, the final endCode value must be 0xFFFF. This segment
        #need not contain any valid mappings. It can simply map the single character
        #code 0xFFFF to the missing character glyph, glyph 0.  
        assert len(endCode) == len(startCode) == len(idDelta) == len(idRangeOffset) 
        for i in range(len(endCode)):
            #start must be <= end; I don't understand why they can be equal
            #I'd also like to make stronger assertions, but I'm still not real clear
            #on what's allowed to go in the idDelta and idRangeOffset fields
            assert endCode[i] >= startCode[i]

        cmap_tables.append(data + (endCode, startCode, idDelta, idRangeOffset, glyphIndexArray))

        print "table %s type 4:\n%s" % (i, cmap_tables[-1]) 
    else:
        print "unable to read format %s of table %s" % (format, i)

#well, I'm still not quite sure how the mapping works, and I know that my format 4 table
#reading is incomplete because I ignore the final element of the table, because the spec
#does not make it clear how many elements are in it. Nonetheless, let's press on to 
#reading some glyphs.

#here is the spec for a glyph header:
#int16 	numberOfContours 	If the number of contours is positive or zero, it is a single glyph;
#                           If the number of contours is -1, the glyph is compound
#FWord 	xMin 	Minimum x for coordinate data
#FWord 	yMin 	Minimum y for coordinate data
#FWord 	xMax 	Maximum x for coordinate data
#FWord 	yMax 	Maximum y for coordinate data

#and for a simple glyph:
#uint16 	endPtsOfContours[n] 	Array of last points of each contour; n is the number of 
#                                   contours; array entries are point indices
#uint16 	instructionLength 	Total number of bytes needed for instructions
#uint8 	instructions[instructionLength] 	Array of instructions for this glyph
#uint8 	flags[variable] 	Array of flags
#uint8 or int16 	xCoordinates[] 	Array of x-coordinates; the first is relative to (0,0), 
#                                   others are relative to previous point
#uint8 or int16 	yCoordinates[] 	Array of y-coordinates; the first is relative to (0,0),
#                                   others are relative to previous point

