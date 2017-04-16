## Instructions

### 0. Prepare the `data` subdirectory

TBD

### 1. Start a web server to serve `viz`

Our recommended method is to start a simple Python web server, i.e.,

```bash
$ python -m SimpleHTTPServer [port-number=8000]
```

### 2. Launch browser

Assuming the web server serving `viz` is running on port 8000 and the directory
`data/target` exists, you can simply go to:

http://localhost:8000/decompiler.html?bin=target

to see the visualization of `target`.
