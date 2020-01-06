import os


def ensure_directory(*args):
    path = os.path.join(*args)
    if os.path.isfile(path):
        raise Exception(
            "Emitter output directory %s is a regular file", path)

    if not os.path.exists(path):
        os.makedirs(path)
