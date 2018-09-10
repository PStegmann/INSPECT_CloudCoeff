import numpy as np
import matplotlib.pyplot as plt
c = np.loadtxt("Legendre_terms.txt")
#c = [1, 1]
c = c[4:10]
reso = 1800
p = np.zeros(reso)
x = np.arange(-1,1,2./reso)
for ii in range(0,reso):
	p[ii] = np.polynomial.legendre.legval(x[ii],c)
plt.figure(1)
plt.plot(180./np.pi*np.arccos(x),p)
np.savetxt("phasefunction_273K_0p4_1500_190.txt",p,newline='\n')
#print(p[0])
#print(p[reso-1])
#phafu = np.loadtxt("phaseFu.txt")
plt.figure(1)
xx = np.arange(-1,1,2./194)
#plt.plot(xx,phafu)
plt.xlabel('polar angle [$^{\circ}$ deg]', fontsize=22)
plt.ylabel('$P_{11}$', fontsize=22)
plt.grid('on')
#plt.show()

plt.figure(2)
plt.semilogy(abs(c[0:150]),'o-')
plt.ylabel('$|C_n|$', fontsize=22)
plt.xlabel('n', fontsize=22)
plt.show()
