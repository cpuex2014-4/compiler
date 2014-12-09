#!/usr/bin/python
import sys

def add_init(out):
    out.write('start:\n')
    out.write('\tli\t$sp, 0x00400000\n')  # set $sp 
    out.write('\tli\t$gp, 0x00200000\n')   # set $gp
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
    out_name = sys.argv[2]
    lib_name = '../kake/library/arrayLib.s'
    flib_name = '../kake/library/floatLib.s'
    with open(out_name,'w') as out:
        add_init(out)
        with open(lib_name,'r') as lib:
            out.write(lib.read())
        with open(flib_name, 'r') as flib:
            out.write(flib.read())
        with open(file_name,'r') as f:
            out.write(f.read())
