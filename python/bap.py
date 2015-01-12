#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, time, atexit
import requests
from subprocess import Popen
from mmap import mmap
from urlparse import urlparse, parse_qs
from tempfile import NamedTemporaryFile
import json
import adt, arm, asm, bil


__all__ = ["disasm", "image"]

DEBUG_LEVEL = ["Critical", "Error"]

instance = None

def del_instance():
    if instance is not None:
        instance.close()

def get_instance(**kwargs):
    global instance
    if 'server' in kwargs or instance is None:
        if instance is not None:
            instance.close()
        args = kwargs.get('server', {})
        instance = Bap(args)
    return instance

atexit.register(del_instance)

def disasm(obj, **kwargs):
    r""" disasm(obj) disassembles provided object.
    Returns a generator object yield instructions.

    """
    def ret(obj):
        return get_instance(**kwargs).insns(obj)
    if isinstance(obj, Id):
        return ret(obj)
    elif isinstance(obj, Resource):
        return ret(obj.ident)
    else:
        return ret(load_chunk(obj, **kwargs))

def image(f, **kwargs):
    bap = get_instance(**kwargs)
    if os.path.isfile(f) and not os.path.isabs(f):
        f = os.path.abspath(f)
    return Image(bap.load_file(f), bap)

def load_chunk(s, **kwargs):
    return get_instance(**kwargs).load_chunk(s, **kwargs)


class Resource(object):
    def __init__(self, name, ident, bap):
        self.ident = Id(ident)
        self.bap = bap
        self.msg = None
        self._name = name

    def load(self):
        if self.msg is None:
            self.msg = self.bap.get_resource(self.ident)
            if not self._name in self.msg:
                if 'error' in msg:
                    raise ServerError(response)
                else:
                    msg = "Expected {0} msg but got {1}".format(
                        self._name, msg)
                    raise RuntimeError(msg)

    def get(self, child):
        self.load()
        return self.msg[self._name].get(child)


class Image(Resource):
    def __init__(self, ident, bap):
        super(Image,self).__init__('image', ident, bap)

    def load_sections(self):
        ss = self.get('sections')
        self.sections = [Section(s, self) for s in ss]

    def get_symbol(self, name, d=None):
        for sec in self.sections:
            sym = sec.get_symbol(name, d)
            if sym is not d:
                return sym
        return d

    def __getattr__(self, name):
        if name == 'sections':
            self.load_sections()
            return self.sections
        else:
            return self.get(name)

class Section(Resource):
    def __init__(self, ident, parent):
        super(Section, self).__init__('section', ident, parent.bap)
        self.parent = parent

    def load_symbols(self):
        self.symbols = [Symbol(s, self) for s in self.get('symbols')]

    def get_symbol(self, name, d=None):
        try:
            return (s for s in self.symbols if s.name == name).next()
        except StopIteration:
            return d

    def __getattr__(self, name):
        if name == 'symbols':
            self.load_symbols()
            return self.symbols
        elif name == 'addr' or name == 'size':
            return self.get('memory')[name]
        elif name == 'memory':
            return Memory(self.get('memory'), self)
        else:
            return self.get(name)

class Symbol(Resource):
    def __init__(self, ident, parent):
        super(Symbol, self).__init__('symbol', ident, parent.bap)
        self.parent = parent

    def load_chunks(self):
        self.chunks = [Memory(s, self) for s in self.get('chunks')]

    def __getattr__(self, name):
        if name == 'chunks':
            self.load_chunks()
            return self.chunks
        elif name == 'addr':
            return self.chunks[0].addr
        else:
            return self.get(name)

class Memory(object):
    def __init__(self, mem, parent):
        self.__dict__.update(mem)
        self.parent = parent


    def load_data(self):
        try:
            url = (urlparse(url) for url in self.links
                   if urlparse(url).scheme == 'mmap').next()
            qs = parse_qs(url.query)
            length = int(qs['length'][0])
            offset = int(qs['offset'][0])
            with open(url.path, "rw+b") as f:
                mm = mmap(f.fileno(), length=0)
                mm.seek(offset)
                self.data = mm.read(length)
                mm.close()
        except StopIteration:
            self.data = None

    def __getattr__(self, name):
        if name == 'data':
            self.load_data()
            return self.data
        raise AttributeError(name)


class ServerError(Exception):
    def __init__(self, err):
        self.msg = str(Error(err))

    def __str__(self):
        return self.msg

class Error(object):
    def __init__(self, err):
        self.__dict__.update(err)
        self.__dict__.update(err['error'])

    def __str__(self):
        return "{severity}: {description}".format(**self.error)

class Id(object):
    def __init__(self, r):
        self.value = r
    def __str__(self):
        return str(self.value)

RETRIES = 10

class Bap(object):
    def __init__(self, server={}):
        if isinstance(server, dict):
            self.__dict__.update(spawn_server(**server))
        else:
            self.url = server

        self.last_id = 0
        for attempt in range(RETRIES):
            try:
                self.capabilities = self.call({'init' : {
                    'version' : '0.1'}})
            except Exception:
                if attempt + 1 == RETRIES:
                    raise
                else:
                    time.sleep(0.1 * attempt)

        if not "capabilities" in self.__dict__:
            raise RuntimeError("Failed to connect to BAP server")

        self.data = {}
        self.temp = NamedTemporaryFile('rw+b')

    def insns(self, src):
        res = self.call({'get_insns' : {'resource' : src}})
        for msg in res:
            if 'error' in msg:
                err = Error(msg)
                if err.severity in DEBUG_LEVEL:
                    print err
            else:
                return (parse_insn(js) for js in msg['insns'])

    def close(self):
        self.__exit__()

    def load_file(self, name):
        return self._load_resource({'load_file' : {
            'url' : 'file://' + name}})

    def get_resource(self, name):
        return self.call({'get_resource' : name}).next()

    def load_chunk(self, data, **kwargs):
        kwargs.setdefault('url', self.mmap(data))
        kwargs.setdefault('arch', 'x86_32')
        kwargs.setdefault('address', bil.Int(0,32))
        kwargs.setdefault('endian', bil.LittleEndian())
        return self._load_resource({'load_memory_chunk' : kwargs})

    def __exit__(self):
        if 'server' in self.__dict__:
            self.server.terminate()
        self.temp.close()

    def call(self, data):
        def dumps(dic):
            self.last_id += 1
            dic['id'] = Id(self.last_id)
            return json.dumps(dic, default=str)

        if isinstance(data, dict):
            method = requests.post
            if 'get_insns' or 'get_resource' in data:
                method = requests.get
            return jsons(method(self.url, data=dumps(data)))
        else:
            gen = (dumps(msg) for msg in data)
            return jsons(requests.post(self.uri, data=gen))

    def mmap(self, data):
        url = "mmap://{0}?offset=0&length={1}".format(
            self.temp.name, len(data))
        os.ftruncate(self.temp.fileno(), len(data))
        mm = mmap(self.temp.fileno(), len(data))
        mm.write(data)
        mm.close()
        return url

    def _load_resource(self, res):
        rep = self.call(res).next()
        if 'error' in rep:
            raise ServerError(rep)
        return Id(rep['resource'])

def spawn_server(**kwargs):
    port = kwargs.get('port', 8080)
    name = kwargs.get('name', 'bap-server')
    server = Popen([name, '--port=' + str(port)])
    return {
        'server' : server,
        'url' : "http://127.0.0.1:{0}".format(port)
    }

def jsons(r, p=0):
    dec = json.JSONDecoder(encoding='utf-8')
    while True:
        obj,p = dec.scan_once(r.text,p)
        yield obj

def parse_target(js):
    if 'target' in js:
        return arm.loads(js['target'])
    else:
        return None

def parse_bil(js):
    if 'bil' in js:
        return [bil.loads(s) for s in js['bil']]
    else:
        return None

def parse_insn(js):
    js.update(js['memory'], bil=parse_bil(js), target=parse_target(js))
    return asm.Insn(**js)

##### Examples

def demo_chunk():
    data = b"\x48\x83\xec\x08"
    insns = disasm(data, arch="x86_64")
    print list(insns)

def demo_image():
    img = image("coreutils_O0_ls")
    print "Arch: {1}".format(img.name, img.arch)
    print "Sections:"
    for sec in img.sections:
        print "\t{0}\t{1}".format(sec.name, "".join(sec.perm))
        print "\t\tSymbols:"
        for sym in sec.symbols:
            print "\t\t\t{0}".format(sym.name)
    sym = img.get_symbol('to_uchar')
    print ' '.join(x.encode('hex') for x in sym.chunks[0].data)
    print "Diassembly of the `{0}` function:".format(sym.name)
    for insn in disasm(sym):
        print insn.asm

if "__main__" == __name__:
    demo_chunk()
    demo_image()
