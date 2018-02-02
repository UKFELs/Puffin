# Copyright (c) 2012-2018, University of Strathclyde
# Authors: Lawrence T. Campbell
# License: BSD-3-Clause

def torf(bo):
    if (bo):
        res = '.TRUE. '
    else:
        res = '.FALSE.'
    
    return res

if __name__ == "__main__":
    import sys
    torf(sys.argv[1])
