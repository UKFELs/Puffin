/**
 * $Id: stdabsdbl.cxx 975 2016-01-09 20:04:17Z cary $
 *
 * Copyright &copy; 2012-2016, Tech-X Corporation, Boulder, CO.
 * See LICENSE file (EclipseLicense.txt) for conditions of use.
 *
 * Determine whether the compiler knows std::abs<double>.
 */

#include <cmath>

int main(int argc, char** argv) {
  double a = 0;
  double b = std::abs(a);
  return 0;
}

