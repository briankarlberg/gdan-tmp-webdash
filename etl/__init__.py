import sys
import jsonschema
import pkg_resources
from copy import deepcopy
from functools import partial
from dictionaryutils import DataDictionary, load_schemas_from_dir

from etl.gid import edge_gid, vertex_gid


class CustomDataDictionary(DataDictionary):
    """
    Modified from:
    https://github.com/uc-cdis/dictionaryutils/blob/42bf330d82bf084141c0f21b9815cc7e34bf5287/dictionaryutils/__init__.py#L112
    """

    def __init__(
            self,
            root_dir,
            definitions_paths=None,
            metaschema_path=None
    ):
        self.root_dir = root_dir
        self.metaschema_path = ""
        self.definitions_paths = ""
        self.exclude = (["_gids.yaml"])
        self.schema = dict()
        self.resolvers = dict()
        self.metaschema = dict()
        self.load_data(directory=self.root_dir, url=None)

    def load_data(self, directory=None, url=None):
        """Load and reslove all schemas from directory or url"""
        yamls, resolvers = load_schemas_from_dir(pkg_resources.resource_filename(__name__, "schema"))
        yamls, resolvers = load_schemas_from_dir(directory,
                                                 schemas=yamls,
                                                 resolvers=resolvers)

        self.settings = yamls.get(self.settings_path) or {}
        self.resolvers.update(resolvers)

        schemas = {
            schema["id"]: self.resolve_schema(schema, deepcopy(schema))
            for path, schema in yamls.items()
            if path not in self.exclude
        }
        self.schema.update(schemas)


class ClassInstance:

    def __init__(self, **kwargs):
        self.__dict__["_props"] = {}
        if "properties" in self._schema:
            for k in self._schema["properties"].keys():
                if k in ["label", "backref"]:
                    continue
                elif k in ["gid", "from", "to"]:
                    k = "_%s" % k
                self.__dict__["_props"][k] = None

        for k, v in kwargs.items():
            if k in ["gid", "from", "to"]:
                k = "_%s" % k
            self.__setattr__(k, v)

    def props(self, preserve_null=False, exclude=[]):
        data = {}
        for k, v in self._props.items():
            if k.startswith("_"):
                continue
            if k in exclude:
                continue
            data[k] = v
        if not preserve_null:
            nulls = [k for k in data if data[k] is None]
            for k in nulls:
                del data[k]
        return data

    def schema(self):
        return self._schema

    def validate(self):
        data = deepcopy(self.props())
        data["gid"] = self.gid()
        data["label"] = self.label()
        if "_from" in self._props:
            data["from"] = self._from
        if "_to" in self._props:
            data["to"] = self._to
        try:
            jsonschema.validate(data, self.schema())
        except Exception as e:
            print(data, file=sys.stderr)
            raise e

    def label(self):
        return self._label

    def gid(self):
        return self._gid

    def __repr__(self):
        return '<%s(%s)>' % (self.__class__.__name__,
                             self.props(preserve_null=True))

    def __setattr__(self, key, item):
        if key == "_label":
            raise KeyError("setting _label is not permitted")
        if key not in self._props:
            raise KeyError("object does not contain key '{}'".format(key))
        self._props[key] = item

    def __getattr__(self, key):
        return self._props[key]

    def __getitem__(self, k):
        return self.__getattr__(k)

    def __setitem__(self, k, v):
        return self.__setattr__(k, v)


class Vertex:
    pass


class Edge:
    pass


_schemaPath = pkg_resources.resource_filename(__name__, "schema")
_schema = CustomDataDictionary(root_dir=_schemaPath)


__all__ = ['Vertex', 'Edge']
for k, schema in _schema.schema.items():
    cls_name = schema["id"]
    label = schema['properties']['label']['const']

    is_vertex = True
    if 'from' in schema['properties'] and 'to' in schema['properties']:
        is_vertex = False

    if is_vertex:
        cls = type(
            cls_name,
            (ClassInstance, Vertex),
            {
                '_schema': schema,
                '_label': label,
                'make_gid': partial(vertex_gid, label)
            }
        )
    else:
        cls = type(
            cls_name,
            (ClassInstance, Edge),
            {
                '_schema': schema,
                '_label': label,
                'gid': lambda self: "(%s)--%s->(%s)" % (self._from, self._label, self._to)
            }
        )
        cls._backref = None
        cls.backref = lambda self: None

        if 'backref' in schema['properties']:
            parts = cls_name.split('_')
            if len(parts) != 2:
                raise ValueError('Unexpected id format for edge')
            backref_name = '%s_%s' % (parts[1], parts[0])
            backref = schema['properties']['backref']['const']
            backref_schema = deepcopy(schema)
            backref_schema['id'] = backref_name
            backref_schema['properties']['backref']['const'] = label
            backref_schema['properties']['label']['const'] = backref
            backref_schema['properties']['from'] = schema['properties']['to']
            backref_schema['properties']['to'] = schema['properties']['from']
            backref_cls = type(
                backref_name,
                (ClassInstance, Edge),
                {
                    '_schema': backref_schema,
                    '_label': backref,
                    'gid': lambda self: "(%s)--%s->(%s)" % (self._from, self._label, self._to)
                }
            )
            cls._backref = backref_cls
            cls.backref = lambda self: self._backref(
                _from=self._to,
                _to=self._from,
                **self.props()
            )
            backref_cls._backref = cls
            cls.backref = lambda self: self._backref(
                _from=self._to,
                _to=self._from,
                **self.props()
            )
            globals()[backref_name] = backref_cls
            __all__.append(backref_name)

    globals()[cls_name] = cls
    __all__.append(cls_name)
