#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import sys
import time

from pprint import pprint

import requests
import json

import asm, arm, bil

path = "file:///home/ivg/factory/arm-binaries/coreutils/coreutils_O0_ls"

session = [
    { 'id' : '0', 'init' : {'version' : '0.1'}},
    { 'id' : '1',
      'load-memory-chunk' : {
          'url' : path,
          'arch' : 'ARM',
          'address' : 'Int(0,32)',
          'endian' : 'LittleEndian()'
      }
  }, {
      "id" : "3",
      "load-file" : {'url' : path}
  }, {
      "id" : "4",
      "get-resource" : "2"
  }, {
      "id" : "5",
      "get-resource" : "3"
  }, {
      "id" : "6",
      "get-resource" : "4"
  },
    { 'id' : '7',
      'get-insns' :
      {
          'resource' : '1'
      }
  }
]


def json_objects(s, p=0):
    dec = json.JSONDecoder(encoding='utf-8')
    while True:
        obj,p = dec.scan_once(s,p)
        yield obj


def parse_target(js):
    if 'target' in js:
        return arm.loads(js['target'])
    else:
        return None

def parse_bil(js):
    if 'bil' in js:
        return [arm.loads(s) for s in js['bil']]
    else:
        return None

def parse_insn(js):
    "accepts json object and returns a lifted instruction"
    js.update(js['memory'], bil=parse_bil(insn), target=parse_target(insn))

    return asm.Insn(**js)

def reqs():
    for msg in session:
        print("sending: %s" % json.dumps(msg))
        yield json.dumps(msg)

if __name__ == "__main__":
    r = requests.post("http://127.0.0.1:8080", data=reqs(), stream=True)
    for obj in json_objects(r.text):
        if 'insns' in obj:
            for insn in obj['insns']:
                try:
                    ins = parse_insn(insn)
                    pprint(ins, width=1)

                    print('.', end=None)
                except Exception as exn:
                    print("skipping, \n{1}\n Error:\n{0}".format(insn, exn))
