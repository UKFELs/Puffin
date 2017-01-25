/**
 * $Id: gendeclstatics.cxx 975 2016-01-09 20:04:17Z cary $
 *
 * Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
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
// cppcheck not seeing above static member initialization
// cppcheck-suppress uninitStructMember
  int rr = x.r + X<float>::r;
}

