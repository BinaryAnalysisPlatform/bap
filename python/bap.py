#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, time, atexit
from signal import signal, SIGTERM
import requests
from subprocess import Popen
from mmap import mmap
from urlparse import urlparse, parse_qs
from tempfile import NamedTemporaryFile
import json
import adt, arm, asm, bil

import threading

from pprint import pprint


__all__ = ["disasm", "image"]

DEBUG_LEVEL = ["Critical", "Error"]

storage = threading.local()
servers = dict()
server_lock = threading.Lock()
requests_lock = threading.Lock()
request = None

def init_requests():
    global request
    with requests_lock:
        if request == None:
            request = requests.Session()
            adapter = requests.adapters.HTTPAdapter(
                    pool_connections=1000,
                    pool_maxsize=1000,
                    max_retries=10,
                    pool_block=True)
            request.mount('http://', adapter)

init_requests()


def del_instance():
    instance = getattr(storage, 'instance', None)
    if instance is not None:
        instance.close()

def get_instance(**kwargs):
    instance = getattr(storage, 'instance', None)
    if instance is None:
        args = kwargs.get('server', {})
        storage.instance = Bap(args)
    return storage.instance

atexit.register(del_instance)
signal(SIGTERM, lambda x,y: del_instance)


def spawn_server(**kwargs):
    port = str(kwargs.get('port', 8080))
    name = kwargs.get('name', 'bap-server')
    with server_lock:
        if port in servers:
            return servers[port]
        else:
            process = Popen([name, '--port=' + port])
            server  = {
                'server' : process,
                'url' : "http://127.0.0.1:{0}".format(port)
            }
            servers[port] = server
            return server


def disasm(obj, **kwargs):
    r""" disasm(obj) disassembles provided object.
    Returns a generator object yield instructions.
    """
    def run(obj):
        return get_instance(**kwargs).insns(obj, **kwargs)
    if isinstance(obj, Id):
        return run(obj)
    elif isinstance(obj, Resource):
        return run(obj.ident)
    else:
        return run(load_chunk(obj, **kwargs))

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

    def load_segments(self):
        ss = self.get('segments')
        self.segments = [Segment(s, self) for s in ss]

    def get_symbol(self, name, d=None):
        for sec in self.segments:
            sym = sec.get_symbol(name, d)
            if sym is not d:
                return sym
        return d

    def __getattr__(self, name):
        if name == 'segments':
            self.load_segments()
            return self.segments
        else:
            return self.get(name)

class Segment(Resource):
    def __init__(self, ident, parent):
        super(Segment, self).__init__('segment', ident, parent.bap)
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
            self.memory = Memory(self.get('memory'), self)
            return self.memory
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
            self.load_chunks()
            return self.chunks[0].addr
        else:
            return self.get(name)

class Memory(object):
    def __init__(self, mem, parent):
        self.parent = parent
        self.size = int(mem['size'])
        self.addr = int(mem['addr'])
        self.links = mem['links']

    def load_data(self):
        try:
            url = (urlparse(url) for url in self.links
                   if urlparse(url).scheme == 'mmap').next()
            qs = parse_qs(url.query)
            offset = int(qs['offset'][0])
            with open(url.path, "rw+b") as f:
                mm = mmap(f.fileno(), length=0)
                mm.seek(offset)
                self.data = mm.read(self.size)
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
                    'version' : '0.1'}}).next()['capabilities']
                break
            except Exception:
                if attempt + 1 == RETRIES:
                    raise
                else:
                    time.sleep(0.1 * attempt)

        if not "capabilities" in self.__dict__:
            raise RuntimeError("Failed to connect to BAP server")
        self.data = {}
        self.temp = NamedTemporaryFile('rw+b', prefix="bap-")

    def insns(self, src, **kwargs):
        req = {'resource' : src}
        req.update(kwargs)
        res = self.call({'get_insns' : req})
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
        kwargs.setdefault('arch', 'i386')
        kwargs.setdefault('addr', 0)
        addr = kwargs['addr']
        if isinstance(addr, str):
            addr = long(addr, 0)
        kwargs['addr'] = '0x{0:x}'.format(addr)

        return self._load_resource({'load_memory_chunk' : kwargs})

    def __exit__(self):
        if 'server' in self.__dict__:
            self.server.terminate()
        self.temp.close()

    def dumps(self,dic):
        self.last_id += 1
        dic['id'] = Id(self.last_id)
        return json.dumps(dic, default=str)

    def call(self, data):
        if isinstance(data, dict):
            method = request.post
            return jsons(method(self.url, data=self.dumps(data)))
        else:
            gen = (self.dumps(msg) for msg in data)
            return jsons(request.post(self.uri, data=gen))


    def mmap(self, data):
        url = "mmap://{0}?offset=0&length={1}".format(
            self.temp.name, len(data))
        os.ftruncate(self.temp.fileno(), 4096)
        mm = mmap(self.temp.fileno(), 4096)
        mm.write(data)
        mm.close()
        return url

    def _load_resource(self, res):
        rep = self.call(res).next()
        if 'error' in rep:
            raise ServerError(rep)
        return Id(rep['resource'])


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

def hexs(data):
    return ' '.join(x.encode('hex') for x in data)

##### Examples

def demo_chunk():
    data = b"\x48\x83\xec\x08"
    insns = disasm(data, arch="x86_64")
    print list(insns)

def demo_image():
    img = image("coreutils_O0_ls")
    print "Arch: {1}".format(img.name, img.arch)
    print "Segments:"
    for sec in img.segments:
        print "\t{0}\t{1}".format(sec.name, "".join(sec.perm))
        print "\t\tSymbols:"
        for sym in sec.symbols:
            print "\t\t\t{0}".format(sym.name)
    sym = img.get_symbol('to_uchar')
    print "Disassembly of the `{0}` function:".format(sym.name)
    for insn in disasm(sym):
        print insn.asm

if "__main__" == __name__:
    demo_chunk()
    demo_image()
