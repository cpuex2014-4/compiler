#!/usr/bin/python
import sys

def add_init(out):
    out.write('start:\n')
    out.write('\taddiu\t$sp, $zero, 10000\n')  # set $sp 
    out.write('\tj\tmain\n')
    return

def concat(lib, f, out):
    lib_data = lib.read()
    file_data = f.read()
    out.write(lib_data)
    out.write(file_data)
    return

if __name__ == '__main__':
    file_name = sys.argv[1]
    lib_name = sys.argv[2]
    out_name = sys.argv[3]
    with open(file_name,'r') as f:
        with open(lib_name,'r') as lib:
            with open(out_name,'w') as out:
                add_init(out)
                concat(lib, f, out)
