import llvmlite.ir as ll

int32 = ll.IntType(32)
int64 = ll.IntType(64)
int32p = ll.PointerType(int32)
int64p = ll.PointerType(int64)


def Struct:
    _fields = tuple()





def PyObj(Struct):
    _fields = (('refcount', int64),
               (
