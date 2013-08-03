class Dictionary:
    __slots__ = ('hash_table',)

    def init(self):
        self.hash_table = `(MAKE-HASH-TABLE :TEST 'EQUALP)`

    def setitem(self, key, value):
        `(SETF (GETHASH key (SLOT-VALUE self 'hash_table)) value)`

    def getitem(self, key):
        return `(GETHASH key (SLOT-VALUE self 'hash_table))`


class List(SEQUENCE, `STANDARD-OBJECT`):
    __slots__ = ('array',)
    def init(self):
        self.array = `(MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T)`

    def setitem(self, key, value):
        `(SETF  (SLOT-VALUE self 'array) value)`

    def getitem(self, key):
        `(AREF (SLOT-VALUE self 'array) key)`

    def append(self, val):
        `(VECTOR-PUSH-EXTEND val (SLOT-VALUE self 'array))`

    def `SEQUENCE:LENGTH`(self):
        return LENGTH(self.array)

    def `SEQUENCE:ELT`(self, index):
        return self.getitem(index)

    def `MAKE-SEQUENCE-ITERATOR`(self, `FROM-END`=None, START=None, END=None):
        return `MAKE-SEQUENCE-ITERATOR`(self.array, `FROM-END`=`FROM-END`, START=START, END=END)

def foo():
    l = List()

    for x in range(10):
        l.append(x)

    for x in l:
        PRINT(x)

TIME(foo())
