import numarray as na

#docs = n = artists, terms = m = users

def read_sdd(fin):
    """Read an SDD output file into memory

    The SDD output file starts with 2 comment lines, followed by a line whose
    contents are C{k n m}. C{k} is the user's selected value for truncation
    of the matrices. C{n} is the number of rows in the original matrix, and
    C{m} is the number of columns in the original matrix.

    Following this line are C{k} values which represent B{D}, a diagonal matrix.
    The next n*k values are in the set C{{-1, 0, 1}}, and represent the X
    matrix. Following this, logically, are m*k values representng the Y matrix.
    If you're confused, read the code; it's pretty simple. (although not
    documented anywhere, grumble, grumble)

    The odd C{if buf} statements in the read loops are necessary because
    SDDPACK seems not to write any lines longer than 96 characters. Thus, each
    row may or may not be on the same line.

    @param fin: open sdd file
    @type fin: File

    @returns Tuple (X, D, Y), the three SDD matrices
    """
    for i in range(2): fin.readline() #ignore comments at top of sdd file
    k, n, m = fin.readline().split()
    k = int(k)
    n = int(n)
    m = int(m)

    D = na.zeros((k,k), type="Float32")
    print "reading D matrix from SDD file"
    for i in range(int(k)):
        D[i,i] = float(fin.readline())

    X = na.zeros((n, k), type="Int8")   #the values are in {-1, 0, 1}
    buf = fin.readline().split()
    print "reading X matrix from SDD file"
    for i in xrange(k):
        for j in xrange(n):
            if buf:
                X[j,i] = int(buf.pop(0))  #do I want j, i reversed? (think so)
            else:
                buf = fin.readline().split()
                X[j,i] = int(buf.pop(0))
    assert buf == []  #verify we don't have data left over

    Y = na.zeros((m, k), type="Int8")   #vals in {-1, 0, 1} still
    buf = fin.readline().split()
    print "reading Y matrix from SDD file"
    for i in xrange(k):
        for j in xrange(m):
            if buf:
                Y[j,i] = int(buf.pop(0))
            else:
                buf = fin.readline().split()
                Y[j,i] = int(buf.pop(0))
    assert fin.readline() == '' and buf == [] #verify we done got everything
    return (X, D, Y)

def do_aa_similarity(X, D):
    """compute the term-term similarity matrix

    Does this even work with SDD? It might be SVD only! (SDD matrices aren't
    orthonormal, so order of multiplication does matter!)
    
    @param X: The SDD X array
    @type X: numarray.array
    
    @param Y: The SDD diagonal array
    @type Y: numarray.array
    
    @returns: X * D^2 * transpose(D)
    """
    T = na.transpose
    m = na.matrixmultiply
    return m(X, m(D, D)

X, D, Y = read_sdd(file('big.sdd'))
