"""General constructor forwarding, compiled alongside generated argument binders."""

# Physical allocation, method lookup and borrowed-view invocation are primitives.
# ruff: noqa: F821


def construct(cls, view):
    instance = _allocate_instance(cls)
    initializer = _init_method(instance)
    if initializer is None:
        if _binding_count(view) != 0 or _binding_keyword_count(view) != 0:
            raise TypeError("instance constructor takes no arguments")
    else:
        result = _forward_call(initializer, view)
        if result is not None:
            raise TypeError("__init__ must return None")
    return instance
