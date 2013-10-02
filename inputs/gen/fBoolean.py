#!/usr/bin/env python

def torf(bo):
    if (bo):
        res = '.TRUE. '
    else:
        res = '.FALSE.'
    
    return res

if __name__ == "__main__":
    import sys
    torf(sys.argv[1])
