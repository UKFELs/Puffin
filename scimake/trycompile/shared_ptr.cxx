/**
 * $Id: shared_ptr.cxx 792 2015-04-17 14:07:44Z jrobcary $
 *
 * Copyright &copy; 2013-2015, Tech-X Corporation, Boulder, CO.
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

