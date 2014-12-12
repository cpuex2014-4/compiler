#!/usr/bin/python
import sys

def add_init(out):
    out.write('start:\n')
    out.write('\tli\t$sp, 0x00400000\n')  # set $sp 
    out.write('\tli\t$gp, 0x00200000\n')   # set $gp
    out.write('\tj\tmain\n')
    return

if __name__ == '__main__':
    file_name = sys.argv[1]
    out_name = sys.argv[2]
    with open(out_name,'w') as out:
        add_init(out)
        for i in sys.argv[3:]:
            with open(i,'r') as lib:
                out.write(lib.read())
        with open(file_name, 'r') as f:
            out.write(f.read())
