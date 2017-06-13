import sys
import numpy as np
from numpy import pi
from numpy import arange
import matplotlib.pyplot as plt
from matplotlib.pyplot import specgram


def showBFields():

  npts = 500;

  x = np.linspace(0,10*pi,npts);

  bx = np.cos(x);
  bx2 = np.sin(x);
  
  by = np.cos(x);
  by2 = np.sin(x);

##############################

  bx[x < pi/2] = 0;
  bx[(x > pi/2)] = 1./3./pi * (x[x > pi/2] - pi/2) * np.cos(x[x > pi/2]);
  bx[x > 4*pi - pi/2] = np.cos(x[x > 4*pi - pi/2]);


  bx2[x < pi/2] = 0.;
  bx2[(x > pi/2)] = 1./3./pi * (x[x > pi/2] - pi/2);
  bx2[x > 4*pi - pi/2] = 1.;


##############################

  by[x <= 4.*pi] = 1./4./pi * x[x <= 4.*pi] * np.sin(x[x <= 4.*pi]);
  by[x > 4.*pi] = np.sin(x[x > 4.*pi]);


  by2[x <= 4.*pi] = 1./4./pi * x[x <= 4.*pi];
  by2[x > 4.*pi] = 1.;


###############################


  #np.where(x<pi/2, np.zeros(500,1), np.sin(x));
  #y = np.sin(x);


  axis_font = {'fontname':'Arial', 'size':'20'}
  title_font = {'fontname':'Arial', 'size':'24'}

  plt.plot(x/(2.*pi), bx, label='$b_x$');
  plt.plot(x/(2.*pi), bx2, '--', label='$b_x$ envelope');
  
  plt.plot(x/(2.*pi), by, label='$b_y$');
  plt.plot(x/(2.*pi), by2, '--', label='$b_y$ envelope');


  plt.axis([0, 5, -1.3, 1.3])

  plt.xlabel('Periods', **axis_font)
  plt.ylabel('$b$-fields', **axis_font)
  # plt.title('b-fields')
  plt.legend();
  plt.grid(True);

  plt.savefig("b-fields.png")
  
  plt.clf();




  plt.plot(x/(2.*pi), bx, label='$b_x$');
  plt.plot(x/(2.*pi), bx2, '--', label='$b_x$ envelope');
  
  plt.axis([0, 5, -1.3, 1.3])

  plt.xlabel('Periods', **axis_font)
  plt.ylabel('$b$-field', **axis_font)
  plt.title('$x$ field', **title_font)
  plt.legend();
  plt.grid(True);
  
  plt.savefig("bx-field.png")

  plt.clf();
  #plt.plot(x, x);



  plt.plot(x/(2.*pi), by, label='$b_y$');
  plt.plot(x/(2.*pi), by2, '--', label='$b_y$ envelope');
  
  plt.axis([0, 5, -1.3, 1.3])

  plt.xlabel('Periods', **axis_font)
  plt.ylabel('$b$-field', **axis_font)
  plt.title('$y$ field', **title_font)
  plt.legend();
  plt.grid(True);
  
  plt.savefig("by-field.png")




#  plt.show();




if __name__ == '__main__':
    #h5fname=sys.argv[1]
    showBFields();
    