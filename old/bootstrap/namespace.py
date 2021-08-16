

class Namespace(object):
    def __init__(self,
                 class_top_level=False,
                 inside_form=False):
        self.s = set()
        self.type_map = dict()
        self.class_top_level = class_top_level
        self.inside_form = inside_form

    def add(self, name, ):
        self.s.add(name, type=None)
        if type is not None:
            self.set_type(name, type)

    @property
    def names(self):
        return self.s

    def type(self, name):
        return self.type_map.get(name)

    def set_type(self, name, type):
        self.type_map[name] = type

    def __contains__(self, name):
        return name in self.s

    def __repr__(self):
        return '<NAMESPACE class_top_level=%s %s>' % (
            self.class_top_level,
            ', '.join(self.s))


class LinkedNamespace:
    def __init__(self, parent=None):
        self.parent = parent
        self.local = Namespace()

    def make_child(self):
        return LinkedNamespace(parent=self)

    def locals(self):
        return self.local.names

    def globals(self):
        d = dict(self.parent.globals())
        d.update(self.locals())

    def __contains__(self, name):
        if name in self.local:
            return True
        if self.parent is not None and name in self.parent:
            return True
        return False

    def type(self, name):
        if name in self.local:
            return self.local.type(name)
        if self.parent is not None:
            return self.parent.type(name)

    def add(self, name, type=None):
        self.local.add(name)
        if type:
            self.local.set_type(name, type)

    def __repr__(self):
        return '<LinkedNamespace depth=%s local=%s parent=%s>' % (
            self.depth,
            self.local,
            self.parent)
