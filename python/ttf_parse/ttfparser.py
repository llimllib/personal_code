#!/usr/bin/python

# from: http://two.pairlist.net/pipermail/reportlab-users/2003-October/002329.html

# Spike TrueType/OpenType subsetting

# Data Type     Description
# ------------- -------------------------------------------------------------
# BYTE          8-bit unsigned integer.
# CHAR          8-bit signed integer.
# USHORT        16-bit unsigned integer.
# SHORT         16-bit signed integer.
# ULONG         32-bit unsigned integer.
# LONG          32-bit signed integer.
# Fixed         32-bit signed fixed-point number (16.16)
# FUNIT         Smallest measurable distance in the em space.
# F2DOT14       16-bit signed fixed number with the low 14 bits of fraction (2.14).
# LONGDATETIME  Date represented in number of seconds since 12:00 midnight, January 1, 1904. The value is represented as a signed 64-bit integer.
# Tag           Array of four uint8s (length = 32 bits) used to identify a script, language system, feature, or baseline
# GlyphID       Glyph index number, same as uint16(length = 16 bits)
# Offset        Offset to a table, same as uint16 (length = 16 bits), NULL offset = 0x0000
#
# NOTE: All numbers are big-endian

# Font file begins with an offset table:
#   Fixed       sfnt version    0x00010000 for TrueType outlines, 'OTTO' for OpenType with CFF outlines (not relevant here)
#   USHORT      numTables       number of tables
#   USHORT      searchRange     16 * max(2^n <= numTables)
#   USHORT      entrySelector   max(n: 2^n <= numTables)
#   USHORT      rangeShift      numTables * 16 - searchRange
#   ------------------------------ (12 bytes)
# Table directory follows.  Each entry is 12 bytes.  Entries are sorted by
# tag in lexicographical order.  Offsets are from the start of the font file.
# Entry format:
#   ULONG       tag             4-byte identifier
#   ULONG       checkSum        CheckSum for this table
#   ULONG       offset          Offset from beginning of font file
#   ULONG       length          length of this table

# Checksum calculation:
#   ULONG
#   CalcTableChecksum(ULONG *Table, ULONG Length)
#   {
#   ULONG Sum = 0L;
#   ULONG *Endptr = Table+((Length+3) & ~3) / sizeof(ULONG);
#
#   while (Table < EndPtr)
#       Sum += *Table++;
#       return Sum;
#   }
#
# Note: This function implies that the length of a table must be a multiple of
# four bytes. In fact, a font is not considered structurally proper without the
# correct padding. All tables must begin on four byte boundries, and any
# remaining space between tables is padded with zeros. The length of all tables
# should be recorded in the table directory with their actual length (not their
# padded length).

ttf_tables = {
# Required Tables
    'cmap': "Character to glyph mapping",
    'head': "Font header",
    'hhea': "Horizontal header",
    'hmtx': "Horizontal metrics",
    'maxp': "Maximum profile",
    'name': "Naming table",
    'OS/2': "OS/2 and Windows specific metrics",
    'post': "PostScript information",
# Tables Related to TrueType Outlines
    'cvt ': "Control Value Table",
    'fpgm': "Font program",
    'glyf': "Glyph data",
    'loca': "Index to location",
    'prep': "CVT program",
# Tables Related to PostScript Outlines
    'CFF ': "PostScript font program (compact font format)",
# Obsolete Multiple Master support
    'fvar': "obsolete",
    'MMSD': "obsolete",
    'MMFX': "obsolete",
# Advanced Typographic Tables
    'BASE': 'Baseline data',
    'GDEF': 'Glyph definition data',
    'GPOS': 'Glyph positioning data',
    'GSUB': 'Glyph substitution data',
    'JSTF': 'Justification data',
# Tables Related to Bitmap Glyphs
    'EBDT': 'Embedded bitmap data',
    'EBLC': 'Embedded bitmap location data',
    'EBSC': 'Embedded bitmap scaling data',
# Other OpenType Tables
    'DSIG': 'Digital signature',
    'gasp': 'Grid-fitting/Scan-conversion',
    'hdmx': 'Horizontal device metrics',
    'kern': 'Kerning',
    'LTSH': 'Linear threshold data',
    'PCLT': 'PCL 5 data',
    'VDMX': 'Vertical device metrics',
    'vhea': 'Vertical Metrics header',
    'vmtx': 'Vertical Metrics',
    'VORG': 'Vertical Origin',
}

class TTFParser:

    def __init__(self, file):
        "Creates a TrueType font file parser.  File can be a file name, or a file object."
        if type(file) == type(""):
            file = open(file, "rb")
        self.file = file
        version = self.read_ulong()
        if version == 0x4F54544F:
            raise 'TTFError', 'OpenType fonts with PostScript outlines are not supported'
        if version != 0x00010000:
            raise 'TTFError', 'Not a TrueType font'
        self.numTables = self.read_ushort()
        self.searchRange = self.read_ushort()
        self.entrySelector = self.read_ushort()
        self.rangeShift = self.read_ushort()

        self.table = {}
        self.tables = []
        for n in range(self.numTables):
            record = {}
            record['tag'] = self.read_tag()
            record['checkSum'] = self.read_ulong()
            record['offset'] = self.read_ulong()
            record['length'] = self.read_ulong()
            self.tables.append(record)
            self.table[record['tag']] = record

    def get_table_pos(self, tag):
        tag = (tag + "    ")[:4]
        offset = self.table[tag]['offset']
        length = self.table[tag]['length']
        return (offset, length)

    def get_table(self, tag):
        offset, length = self.get_table_pos(tag)
        self.file.seek(offset)
        return self.file.read(length)

    def tell(self):
        return self.file.tell()

    def seek(self, pos):
        self.file.seek(pos)

    def skip(self, delta):
        self.file.seek(pos, 1)

    def seek_table(self, tag, offset_in_table = 0):
        pos = self.get_table_pos(tag)[0] + offset_in_table
        self.file.seek(pos)
        return pos

    def read_tag(self):
        return self.file.read(4)

    def read_ushort(self):
        s = self.file.read(2)
        return (ord(s[0]) << 8) + ord(s[1])
    def read_short(self):
        us = self.read_ushort()
        if us >= 0x8000:
            return us - 0x10000
        else:
            return us

    def read_ulong(self):
        s = self.file.read(4)
        return (ord(s[0]) << 24) + (ord(s[1]) << 16) + (ord(s[2]) << 8) + ord(s[3])

    def debug_printHeader(self):
        print "sfnt version: 1.0"
        print "numTables: %d" % self.numTables
        print "searchRange: %d" % self.searchRange
        print "entrySelector: %d" % self.entrySelector
        print "rangeShift: %d" % self.rangeShift

    def debug_printIndex(self):
        print "Tag   Offset       Length    Checksum"
        print "----  -----------  --------  ----------"
        for record in self.tables:
            print "%(tag)4s  +0x%(offset)08X  %(length)8d  0x%(checkSum)08x" % record,
            if ttf_tables.has_key(record['tag']):
                print "", ttf_tables[record['tag']],
            print

if __name__ == "__main__":
    import sys
    if len(sys.argv) == 2:
        ttf = TTFParser(sys.argv[1])
        ttf.debug_printHeader()
        print
        ttf.debug_printIndex()
    elif len(sys.argv) == 3 and sys.argv[2] == "--perms":
        ttf = TTFParser(sys.argv[1])
        fsType = ttf.get_table("OS/2")[8:10]
        fsType = (ord(fsType[0]) << 8) + ord(fsType[1])
##      print "fsType: %04X" % fsType
        if fsType == 0:
            print "0000 - Installable embedding"
        if fsType & 0x0001:
            print "0001 - Reserved"
        if fsType & 0x0002:
            print "0002 - Restricted license embedding (CANNOT EMBED)"
        if fsType & 0x0004:
            print "0004 - Preview & print embedding"
        if fsType & 0x0008:
            print "0008 - Editable embedding"
        for i in range(4, 8):
            if fsType & (1 << i):
                print "%04X - Reserved" % (1 << i)
        if fsType & 0x0100:
            print "0100 - No subsetting"
        if fsType & 0x0200:
            print "0200 - Bitmap embeding only"
        for i in range(10, 16):
            if fsType & (1 << i):
                print "%04X - Reserved" % (1 << i)
    elif len(sys.argv) == 3 and sys.argv[2] == "--cmap":
        ttf = TTFParser(sys.argv[1])
        start = ttf.seek_table("cmap")
        version = ttf.read_ushort()
        print "cmap version: %d" % version
        numTables = ttf.read_ushort()
        print
        print "Plat.  Enc.   Fmt.   Offset"
        print "-----  -----  -----  -----------"
        for n in range(numTables):
            platform = ttf.read_ushort()
            encoding = ttf.read_ushort()
            offset = ttf.read_ulong()
            pos = ttf.tell()
            ttf.seek(start + offset)
            format = ttf.read_ushort()
            ttf.seek(pos)
            print "%5d  %5d  %5d  +0x%08X" % (platform, encoding, format, offset)
    elif len(sys.argv) == 3:
        ttf = TTFParser(sys.argv[1])
        sys.stdout.write(ttf.get_table(sys.argv[2]))
    else:
        print "Usage: %s filename               -- print TTF info" % sys.argv[0]
        print "       %s filename tag > output  -- extract TTF table" % sys.argv[0]

