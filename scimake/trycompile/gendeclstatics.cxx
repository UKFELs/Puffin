/**
 * $Id: gendeclstatics.cxx 792 2015-04-17 14:07:44Z jrobcary $
 *
 * Copyright &copy; 2012-2015, Tech-X Corporation, Boulder, CO.
 * See LICENSE file (EclipseLicense.txt) for conditions of use.
 */

template <class TYPE>
class X {
  public:

    static int r;
};

template <class TYPE>
int X<TYPE>::r = 0;

int main (int argc, char* argv[]) {
  X<double> x;
  int rr = x.r + X<float>::r;
}

