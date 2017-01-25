SciMake CONVENTIONS FOR CMAKE FILES AND MODULES

$Id: README.txt 1081 2016-09-10 15:44:42Z cary $

Copyright &copy; 2014-2015, Tech-X Corporation

The scimake document is at
https://ice.txcorp.com/support/wiki/CmakeCodingStandards
This needs to be rewritten for the new structure.

Links for conventions
http://www.cmake.org/pipermail/cmake/2004-July/005283.html
http://www.phy.bnl.gov/~bviren/lbne/code/ai/external/build/LCG/cmake-2.6.4/Modules/readme.txt

To commit changes:

  svn switch --relocate svn://svn.code.sf.net/p/scimake/code/trunk https://SOURCEFORGE_USERNAME@svn.code.sf.net/p/scimake/code/trunk

SHARED FLAGS

We are currently (20131201) using the following Booleans:
USE_PYC_LIBS
USE_SHARED_LIBS
BUILD_SHARED_LIBS
ENABLE_SHARED
BUILD_WITH_SHARED_RUNTIME
BUILD_WITH_PYCSH_RUNTIME
along with package specific booleans.
The above are overlapping and it is not clear what they mean. To regularize:

0) Keep USE_PYC_LIBS and USE_SHARED_LIBS.  These define what one
   links with.

1) Remove ENABLE_SHARED.  This is a holdover from autotools.  The
   corresponding CMake variable is BUILD_SHARED_LIBS:
   http://www.cmake.org/pipermail/cmake/2003-December/004586.html

2) Remove BUILD_WITH_PYCSH_RUNTIME.  This means no more than
   BUILD_WITH_SHARED_RUNTIME, which means to add the /MD flags on Windows.

So now only the following variables should be used:
USE_PYC_LIBS: Look for installations in order: pycsh, pycst, sersh,
  sermd (on Windows), ser
USE_SHARED_LIBS: Look for installations in order: sersh, sermd (on Windows),
  ser
Otherwise: just look for static libs
BUILD_SHARED_LIBS: CMake var meaning default libs to produce are shared. If
  USE_SHARED_LIBS is not defined, it is set to true.
BUILD_WITH_SHARED_RUNTIME: Add /MD to compiler flags on Windows

The logic is implemented in the SciGetInstSubdirs function in
SciFindPackage.cmake.  The USE_PYC_LIBS variable and other booleans
must be set on the cmake command line, with
e.g. -DUSE_PYC_LIBS:BOOL=TRUE.  When using Bilder this flag should
be set via a bilder script.

Target properties
  http://www.kitware.com/blog/home/post/390

