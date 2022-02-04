import numpy as np

def weighted_std(values, weights):
    """
    Return the weighted average and standard deviation.

    values, weights -- Numpy ndarrays with the same shape.
    """
    average = np.average(values, weights=weights)
    variance = np.average((values-average)**2, weights=weights)  # Fast and numerically precise
    return np.sqrt(variance)

def getBeta(std_pos, emittance):
    ''' Calculates a simple approximation of the Beta
    function for a slice given standard deviation of position and
    slice emittance'''
    #calc = (np.sqrt(4*np.log(2))*std_pos)
    #calc = np.power(calc , 2)
    calc = np.power(std_pos , 2)
    calc = np.divide(calc, emittance)

    return calc


def getAlpha(xpx_cor, beta):
    ''' Calculates a simple approximation of the alpha
    function for a slice given x-px correlation and beta function'''

    #calc = - (4.*np.log(2.) * xpx_cor) / emittance
    calc = -1. * xpx_cor * beta

#    calc = (np.sqrt(4*np.log(2))*std_pos)
#    calc = np.power(calc , 2)
#    calc = np.divide(calc, emittance)

    return calc
    
def calc_emittance(SU_data):
    '''Returns the global (integrated) emittance'''

    m_POSx = SU_data[:, 0]
    m_mOmx = SU_data[:, 1] / SU_data[:, 5]
    m_POSy = SU_data[:, 2]
    m_mOmy = SU_data[:, 3] / SU_data[:, 5]
    wts = SU_data[:, 6]


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


def calc_xpxypy(SU_data):
    '''returns the global x-xp and y-yp correlations'''

    m_POSx = SU_data[:, 0]
    m_mOmx = SU_data[:, 1] / SU_data[:, 5]
    m_POSy = SU_data[:, 2]
    m_mOmy = SU_data[:, 3] / SU_data[:, 5]
    wts = SU_data[:, 6]

    x_2 = ((np.sum(wts*m_POSx*m_POSx))/np.sum(wts))-(np.average(m_POSx, weights=wts))**2.0
    y_2 = ((np.sum(wts*m_POSy*m_POSy))/np.sum(wts))-(np.average(m_POSy, weights=wts))**2.0

    xpx = np.sum(wts*m_POSx*m_mOmx)/np.sum(wts)-np.sum(wts*m_POSx)*np.sum(wts*m_mOmx)/(np.sum(wts))**2
    ypy = np.sum(wts*m_POSy*m_mOmy)/np.sum(wts)-np.sum(wts*m_POSy)*np.sum(wts*m_mOmy)/(np.sum(wts))**2

    xpx = xpx / x_2
    ypy = ypy / y_2

    return xpx, ypy


def getTwiss(SU_data):
    #    CALCULATE GLOBAL TWISS

    ex, ey = calc_emittance(SU_data)
    sdx = weighted_std(SU_data[:, 0], SU_data[:, 6])
    sdy = weighted_std(SU_data[:, 2], SU_data[:, 6])
    bx = getBeta(sdx, ex)
    by = getBeta(sdy, ey)

    xpx, ypy = calc_xpxypy(SU_data)


    ax = getAlpha(xpx, bx)
    ay = getAlpha(ypy, by)

    twx = [ex, bx, ax]
    twy = [ey, by, ay]
    
    return twx, twy

