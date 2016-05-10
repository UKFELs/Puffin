# SDDS python module
import os,sys

# Define platform dependent parameters
if (sys.platform == "win32" or sys.platform == "cygwin"):  # on a Windows or cygwin port
  cr = '\r\n'
else:                                                      # else on POSIX box
  cr = '\n'

def getParameter(file,parameter):
  cmd = 'sdds2stream '+file+' -parameters='+parameter
  result =[dataIn.strip(cr) for dataIn in os.popen(cmd).readlines()]
  return result[0]

def getParameterList(file):
  cmd = 'sddsquery '+file+' -parameterList'
  result =[dataIn.strip(cr) for dataIn in os.popen(cmd).readlines()]
  return result
