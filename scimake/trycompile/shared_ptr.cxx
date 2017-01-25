/**
 * $Id: shared_ptr.cxx 975 2016-01-09 20:04:17Z cary $
 *
 * Copyright &copy; 2013-2016, Tech-X Corporation, Boulder, CO.
 * See LICENSE file (EclipseLicense.txt) for conditions of use.
 */

#include <sci_shared_ptr>

struct S {
  int i;
};

int main(int argc, char** argv) {
  sci_shared_ptr<S> sptr;
  return 0;
}

