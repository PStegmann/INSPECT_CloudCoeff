"""
    file: plot_legendre_MW.py
    
    Description:
    ============
    Reads the CRTM CloudCoeff.bin scalar Legendre expansion
    coefficients from ../build/output/Legendre_terms_MW.txt
    and plots the coefficients and reconstructed phase
    function for the number of streams selected by the user.
    
    Copyright © 2018 Patrick Stegmann
    
    This file is part of INSPECT_CloudCoeff.
    
    INSPECT_CloudCoeff
    you can redistribute it and/or modify it under
    the terms of the Apache License as published by
    the Apache Software Foundation, either version 2.0
    of the License, or (at your option) any later version.
    
    This program is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
    PURPOSE.  See the Apache License for more details.
    
    You should have received a copy of the Apache 2.0
    License along with this program. If not,
    see <https://www.apache.org/licenses/LICENSE-2.0>.


"""

# Load modules
import numpy as np
import matplotlib.pyplot as plt

# Read CRTM MW Legendre expansion coefficients for the
# phase function.
coeff = np.loadtxt("../build/output/Legendre_terms_MW.txt")

# Dictionary mapping of n_streams
switcher = {
   2:0,
   4:0,
   6:5,
   8:12,
   16:21
}

# Query user for desired number of streams:
print "2, 4, 6, 8, or 16 streams are available in the CRTM."
n_streams = raw_input("Select the number of streams: ")
selector = int(switcher.get(int(n_streams), "Invalid number of streams!"))

streams = [2,4,6,8,16]
low = selector
high = low + int(n_streams)
coeff = coeff[low:high]
reso = 1800
p = np.zeros(reso)
x = np.arange(-1,1,2./reso)
for ii in range(0,reso):
        # Reconstruction of the phase function:
	p[ii] = np.polynomial.legendre.legval(x[ii],coeff)
plt.figure(1)
plt.plot(180./np.pi*np.arccos(x),p)
np.savetxt("phasefunction.txt",p,newline='\n')

plt.xlabel('polar angle [$^{\circ}$ deg]', fontsize=22)
plt.ylabel('$P_{11}$', fontsize=22)
plt.grid('on')

plt.figure(2)
plt.semilogy(abs(coeff),'o-')
plt.ylabel('$|C_n|$', fontsize=22)
plt.xlabel('n', fontsize=22)
plt.show()
