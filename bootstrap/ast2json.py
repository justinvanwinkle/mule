from _ast import AST
from ast import parse
import codecs
import json

BUILTIN_PURE = (int, float, bool)
BUILTIN_BYTES = (bytearray, bytes)
BUILTIN_STR = (str,)


def decode_str(value):
    return value


def decode_bytes(value):
    try:
        return value.decode('utf-8')
    except Exception:
        return codecs.getencoder('hex_codec')(value)[0].decode('utf-8')


def ast2json(node):
    assert isinstance(node, AST)
    to_return = dict()
    to_return['_type'] = node.__class__.__name__
    for attr in dir(node):
        if attr.startswith("_"):
            continue
        to_return[attr] = get_value(getattr(node, attr))
    return to_return


def str2json(string):
    return ast2json(parse(string))


def get_value(attr_value):
    if attr_value is None:
        return attr_value
    if isinstance(attr_value, BUILTIN_PURE):
        return attr_value
    if isinstance(attr_value, BUILTIN_BYTES):
        return decode_bytes(attr_value)
    if isinstance(attr_value, BUILTIN_STR):
        return decode_str(attr_value)
    if isinstance(attr_value, complex):
        return str(attr_value)
    if isinstance(attr_value, list):
        return [get_value(x) for x in attr_value]
    if isinstance(attr_value, AST):
        return ast2json(attr_value)
    else:
        raise Exception("unknown case for '%s' of type '%s'" % (
            attr_value, type(attr_value)))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Run training')
    parser.add_argument('source_fn',
                        help="fn")

    args = parser.parse_args()
    print(json.dumps(ast2json(parse(open(args.source_fn, "r").read())),
                     indent=4))
