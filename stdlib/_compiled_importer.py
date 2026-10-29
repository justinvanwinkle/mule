"""Importlib policy for modules already compiled into the native image."""

__speed__ = 3
__mule_builtin_implementation__ = True

from _mule_native import compiled_module_info
from _mule_native import load_compiled_module
from _mule_native import create_compiled_module
from _mule_native import exec_compiled_module
from _mule_native import invalidate_compiled_caches
from importlib._bootstrap import ModuleSpec


class CompiledLoader:
    def create_module(self, spec):
        return create_compiled_module(spec.name)

    def exec_module(self, module):
        exec_compiled_module(module.__spec__.name, module)

    def load_module(self, fullname):
        return load_compiled_module(fullname)


class CompiledFinder:
    @staticmethod
    def find_distributions(*args, **kwargs):
        from importlib.metadata import MetadataPathFinder

        return MetadataPathFinder.find_distributions(*args, **kwargs)

    @staticmethod
    def find_spec(fullname, path=None, target=None):
        info = compiled_module_info(fullname)
        if info is None:
            return None
        filename, locations = info
        spec = ModuleSpec(
            fullname, CompiledLoader(), origin=filename, is_package=bool(locations)
        )
        if locations:
            spec.submodule_search_locations = list(locations)
        spec.has_location = filename is not None
        return spec

    @staticmethod
    def invalidate_caches():
        invalidate_compiled_caches()
