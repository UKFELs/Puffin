
import numpy as np
import warnings

### Some utility functions

def getBeta(std_pos, emittance):
    ''' Calculates a simple approximation of the Beta
    function for a slice given standard deviation of position and
    slice emittance'''

    calc = np.power(std_pos , 2)
    calc = np.divide(calc, emittance)

    return calc


def getAlpha(xpx_cor, beta):
    ''' Calculates a simple approximation of the alpha
    function for a slice given x-px correlation and beta function'''

    calc = -1. * xpx_cor * beta

    return calc



class SUBeam:
    
    def __init__(self, fname = false, SU_data = false):
        if (not fname):
            if (not SU_data):
                warnings.warn('SUBeam class must be initialized with a filename or particle data')
            else:
                self.SU_data = SU_data
        else:
            x, px, y, py, z, pz, wghts = SUF.readSUF(fnamein)
            self.SU_data = numpy.empty(len(x), 7)
            self.SU_data[:,0] = x
            self.SU_data[:,1] = px
            self.SU_data[:,2] = y
            self.SU_data[:,3] = py
            self.SU_data[:,4] = z
            self.SU_data[:,5] = pz
            self.SU_data[:,6] = wghts

        
    def calc_emittance(self):
        '''Returns the global (integrated) emittance'''

        m_POSx = self.SU_data[:, 0]
        m_mOmx = self.SU_data[:, 1] / self.SU_data[:, 5]
        m_POSy = self.SU_data[:, 2]
        m_mOmy = self.SU_data[:, 3] / self.SU_data[:, 5]
        wts = self.SU_data[:, 6]


        x_2 = ((np.sum(wts*m_POSx*m_POSx))/np.sum(wts))-(np.average(m_POSx, weights=wts))**2.0                     #
        px_2 = ((np.sum(wts*m_mOmx*m_mOmx))/np.sum(wts))-(np.average(m_mOmx, weights=wts))**2.0                    #
        xpx = np.sum(wts*m_POSx*m_mOmx)/np.sum(wts)-np.sum(wts*m_POSx)*np.sum(wts*m_mOmx)/(np.sum(wts))**2 #
                                                                                                       #
        y_2 = ((np.sum(wts*m_POSy*m_POSy))/np.sum(wts))-(np.average(m_POSy, weights=wts))**2.0                     #
        py_2 = ((np.sum(wts*m_mOmy*m_mOmy))/np.sum(wts))-(np.average(m_mOmy, weights=wts))**2.0                    #
        ypy = np.sum(wts*m_POSy*m_mOmy)/np.sum(wts)-np.sum(wts*m_POSy)*np.sum(wts*m_mOmy)/(np.sum(wts))**2 #

        exf = np.sqrt((x_2*px_2)-(xpx*xpx))
        eyf = np.sqrt((y_2*py_2)-(ypy*ypy))

        return exf, eyf


    def calc_xpxypy(self):
        '''returns the global x-xp and y-yp correlations'''

        m_POSx = self.SU_data[:, 0]
        m_mOmx = self.SU_data[:, 1] / self.SU_data[:, 5]
        m_POSy = self.SU_data[:, 2]
        m_mOmy = self.SU_data[:, 3] / self.SU_data[:, 5]
        wts = self.SU_data[:, 6]

        x_2 = ((np.sum(wts*m_POSx*m_POSx))/np.sum(wts))-(np.average(m_POSx, weights=wts))**2.0
        y_2 = ((np.sum(wts*m_POSy*m_POSy))/np.sum(wts))-(np.average(m_POSy, weights=wts))**2.0

        xpx = np.sum(wts*m_POSx*m_mOmx)/np.sum(wts)-np.sum(wts*m_POSx)*np.sum(wts*m_mOmx)/(np.sum(wts))**2
        ypy = np.sum(wts*m_POSy*m_mOmy)/np.sum(wts)-np.sum(wts*m_POSy)*np.sum(wts*m_mOmy)/(np.sum(wts))**2

        xpx = xpx / x_2
        ypy = ypy / y_2

        return xpx, ypy


    def getTwiss(self):

        ex, ey = self.calc_emittance()
        
        sdx = weighted_std(self.SU_data[:, 0], self.SU_data[:, 6])
        sdy = weighted_std(self.SU_data[:, 2], self.SU_data[:, 6])
        bx = getBeta(sdx, ex)
        by = getBeta(sdy, ey)

        xpx, ypy = self.calc_xpxypy()


        axG = getAlpha(xpx, bx)
        ayG = getAlpha(ypy, by)

        twx = [ex, bx, ax]
        twy = [ey, by, ay]
        
        return twx, twy
