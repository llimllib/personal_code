def factorial( n ):
    if n <= 1:
        return 1
    else:
        return n * factorial( n-1 )

class Permutation:
    def __init__( self, items ):
        seq = list( items[:] )
        n = len( seq )
        self.sequence = seq
        self.length = n
        self.count = factorial( n )

    def __getitem__( self, key ):
        result = []
        sequence = self.sequence[:]
        N = self.count
        index = key
        for i in range( self.length, 0, -1):
            N = N / i
            choice, index = index // N, index % N
            result += [ sequence.pop(choice) ]
        return result

class NPermutation( Permutation ):

    def __init__( self, n ):
        Permutation.__init__( self, range( 1, n+1 ) )
        self.reset()

    def reset( self ):
        list = [ [-1,i] for i in range(self.length+2) ]
        list[0][1] = self.length+1
        #eg. n=3 -> list = [[-1,4], [-1,1], [-1,2], [-1,3], [-1,4]]
        self.__current = list
        self.__index = 0

    def __iter__( self ):
        return self

    def next( self ):
        if self.__index == self.count:
            self.reset()
            raise StopIteration
        elif self.__index > 0:
            j = self.__get_index_of_highest_mobile()
            #remember the mobile itself before you move it
            mobile = self.__current[j][1]
            #swap the mobile with the element it 'sees'
            self.__move_mobile_at_index( j )
            #switch the direction of elements greater than mobile
            if mobile < self.length:
                self.__reorient_list_elements( mobile )
        self.__index += 1
        return [ a[1] for a in self.__current[ 1:self.length+1 ] ]

    def __get_index_of_highest_mobile( self ):
        high_value = 0
        high_index = 0
        for i in range( 1, self.length+1 ):
            direction = self.__current[i][0]
            value = self.__current[i][1]
            if value > high_value and value > self.__current[i+direction][1]:
                high_value = value
                high_index = i
        return high_index

    def __move_mobile_at_index( self, index ):
            direction = self.__current[index][0]
            value = self.__current[index][1]
            self.__current[index] = self.__current[index+direction]
            self.__current[index+direction] = [direction,value]

    def __reorient_list_elements( self, mobile ):
        for i in range( 1, self.length+1 ):
            if self.__current[i][1] > mobile:
                self.__current[i][0] = -self.__current[i][0]

if __name__ == "__main__":
    x = NPermutation( 6 )

    print 'loop with __getitem__'
    print '---------------'
    for i in range( x.count ):
        print x[i]

    print 'loop with __iter__'
    print '---------------'
    for perm in x:
        print perm
