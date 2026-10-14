"""Importlib policy for modules already compiled into the native image."""

from _mule_native import compiled_module_info as _info
from _mule_native import load_compiled_module as _load
from _mule_native import create_compiled_module as _create
from _mule_native import exec_compiled_module as _execute
from importlib._bootstrap import ModuleSpec


class CompiledLoader:
    def create_module(self, spec):
        return _create(spec.name)

    def exec_module(self, module):
        _execute(module.__spec__.name, module)

    def load_module(self, fullname):
        return _load(fullname)


class CompiledFinder:
    @staticmethod
    def find_distributions(*args, **kwargs):
        from importlib.metadata import MetadataPathFinder

        return MetadataPathFinder.find_distributions(*args, **kwargs)

    @staticmethod
    def find_spec(fullname, path=None, target=None):
        info = _info(fullname)
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
        pass
