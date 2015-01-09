#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, time
import requests

from subprocess import Popen
from mmap import mmap
from tempfile import NamedTemporaryFile
import json
import adt, arm, asm, bil

instance = None

def disasm(obj, **kwargs):
    global instance
    if 'server' in kwargs:
        if instance is not None:
            instance.close()
        instance = Disassembler(kwargs['server'])
    if instance is None:
        instance = Disassembler()

    return instance.run(obj, **kwargs)

class ServerError(Exception):
    def __init__(self, err):
        self.msg = str(Error(err))

    def __str__(self):
        return self.msg

class Error(object):
    def __init__(self, err):
        self.__dict__.update(err)

    def __str__(self):
        return "{severity}: {description}".format(**self.error)

class Id(object):
    def __init__(self, r):
        self.value = r
    def __str__(self):
        return str(self.value)

class Disassembler(object):
    def __init__(self, server={}):
        if isinstance(server, dict):
            self.__dict__.update(spawn_server(**server))
        else:
            self.url = server

        self.last_id = 0
        for attempt in range(10):
            try:
                self.capabilities = self.post({'init' : {
                    'version' : '0.1'}})
            except Exception:
                time.sleep(0.1 * attempt)

        if not "capabilities" in self.__dict__:
            raise RuntimeError("Failed to connect to BAP server")

        self.data = {}
        self.temp = NamedTemporaryFile('rw+b')

    def run(self, src, **kwargs):
        def get_insns(res):
            res = self.post({'get-insns' : {'resource' : res}})
            for msg in res:
                if 'error' in msg:
                    print Error(msg)
                else:
                    return (parse_insn(js) for js in msg['insns'])

        if isinstance(src, file):
            return get_insns(self.load_file(src.name))
        elif isinstance(src, Id):
            return get_insns(src)
        else:
            return get_insns(self.load_chunk(src,**kwargs))

    def close(self):
        self.__exit__()

    def load_file(self, name):
        return self._load_resource({'load-file' : {
            'url' : 'file://' + name}})

    def load_chunk(self, data, **kwargs):
        kwargs.setdefault('url', self.mmap(data))
        kwargs.setdefault('arch', 'x86_32')
        kwargs.setdefault('address', bil.Int(0,32))
        kwargs.setdefault('endian', bil.LittleEndian())
        return self._load_resource({'load-memory-chunk' : kwargs})

    def __exit__(self):
        print "closing connection to BAP"
        if 'server' in self.__dict__:
            self.server.terminate()
        self.temp.close()

    def post(self, data):
        def dumps(dic):
            self.last_id += 1
            dic['id'] = Id(self.last_id)
            return json.dumps(dic, default=str)

        if isinstance(data, dict):
            return jsons(requests.post(self.url, data=dumps(data)))
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
        rep = self.post(res).next()
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



if "__main__" == __name__:
    for x in xrange(1000):
        demo_chunk()
