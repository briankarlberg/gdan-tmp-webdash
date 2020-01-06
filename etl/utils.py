import os
import inspect
import typing
import threading
from contextlib import suppress
from functools import wraps


def ensure_directory(*args):
    path = os.path.join(*args)
    if os.path.isfile(path):
        raise Exception(
            "Emitter output directory %s is a regular file", path)

    if not os.path.exists(path):
        os.makedirs(path)


def enforce_types(callable):
    """
    From:
    https://stackoverflow.com/questions/50563546/validating-detailed-types-in-python-dataclasses
    """
    spec = inspect.getfullargspec(callable)

    def check_types(*args, **kwargs):
        parameters = dict(zip(spec.args, args))
        parameters.update(kwargs)
        # allow thread to control if check skipped
        try:
            if threading.local().skip_check_types:
                return
        except AttributeError:
            pass
        for name, value in parameters.items():
            # Assume un-annotated parameters can be any type
            with suppress(KeyError):
                type_hint = spec.annotations[name]
                if isinstance(type_hint, typing._SpecialForm):
                    # No check for typing.Any, typing.Union, typing.ClassVar
                    # (without parameters)
                    continue
                try:
                    actual_type = type_hint.__origin__
                except AttributeError:
                    actual_type = type_hint
                if isinstance(actual_type, typing._SpecialForm):
                    # case of typing.Union[…] or typing.ClassVar[…]
                    actual_type = type_hint.__args__

                if not isinstance(value, actual_type):
                    raise TypeError(
                        "Unexpected type for '{}' (expected {} but found {})".
                        format(name, type_hint, type(value))
                    )

    def decorate(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            check_types(*args, **kwargs)
            return func(*args, **kwargs)
        return wrapper

    if inspect.isclass(callable):
        callable.__init__ = decorate(callable.__init__)
        return callable

    return decorate(callable)
