

class IR:
    def __str__(self):
        return 'IR '


class TypedName(IR):
    def __init__(self, name, type):
        self.name = name
        self.type = type


class Structure(IR):
    pass


class Define(IR):
    pass


class Type(IR):
    pass


class Assign(IR):
    pass


class Function(IR):
    def __init__(self, name, args, body=None):
        self.name = name
        self.args = args
        self.body = body
