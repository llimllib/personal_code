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
for key in table_directory:
    try:
        checksum, offset, length = table_directory[key]
        nlongs = (length + 3) / 4 #will fail in py3k
        mysum = sum(unpack(">%sI" % nlongs, buf[offset:offset+(nlongs*4)]))
        print key, checksum, mysum, checksum == mysum or mysum - checksum
        i = 0
        while mysum > checksum:
            nlongs -= 1
            mysum = sum(unpack(">%sI" % nlongs, buf[offset:offset+(nlongs*4)]))
            i += 1
        if i: print "it took %s tries to get to %s" % (i, mysum)
    except Exception, e:
        print str(e) + ", got %s" % length
