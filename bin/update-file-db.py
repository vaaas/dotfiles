import os
import lzma
from pathlib import Path

prefix = 'a:\\code\\'

exclude= {
	'node_modules',
	'.git',
	'public',
	'vendor',
}

def walk(root, exclude=set()):
	for x in os.listdir(root):
		if x in exclude: continue
		x = os.path.join(root,x)
		if os.path.isdir(x): yield from walk(x, exclude)
		else: yield x

def write(path, mode, data):
	x = open(path, mode)
	x.write(data)
	x.close()

x = walk(prefix, exclude=exclude)
x = (x.replace('\\', '/') for x in x)
x = '\n'.join(x)
x = bytes(x, 'utf-8')
x = lzma.compress(x)
write(os.path.join(Path.home(), 'filedb.xz'), 'wb', x)
print('done!')
