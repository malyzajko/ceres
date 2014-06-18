#!/usr/bin/env python
##########################################################
# Calcul de la position de la Lune a une date donnee	 #
#							 #
# B.L. - 23/11/2007					 #
# Modif F.B le 02/01/2009 
# Modif A.S le 01/03/09
#Reference: Astronomical Algorithms - Jean Meeus	 #
##########################################################


#import hadmrFITS
#import hadgtVISU
#import hadmrBIAS
#import hadmrDARK
#import hadmrEXTOR
#import hadmrCDB
#import hadmrFLAT
#import hadmrLOCOR
#import hadrgdCONFIG
#import hadmrRV

import sys,string,time,os
import math
#from Numeric import *
#from statis import *
#from fitgaus import *
#from e2dstos1d import *
#from newbervmain import *
import shutil
#import fitsio
import time

#execfile(os.getenv('PYTHONSTARTUP'))
#execfile(os.getenv('PYTHONSTARTUP2'))

# Input de la date d'obs
print 'Date d observation (yyyymmdd): [current date default]'
date=raw_input()

#print 'Berv calculation? (0:no, 1:yes)'
#bervout=input()

bervout=1

pi = math.pi
#pi = 3.141592653589794

# Calcul du bjd
if date=='' :
	y=time.gmtime()[0]
	m=time.gmtime()[1]
	d=time.gmtime()[2]

else :
	y=int(date[0:4])
	m=int(date[4:6])
	d=int(date[6:8])

h=12
mn=0
  
  
print '########################'
print '# Moon radial velocity #'
print '#                      #'
print '# date                 #'
print '# Night %4i-%2i-%2i     #' %(y,m,d)
print '# Time %2ih%2imn         #' %(h,mn)

if m==1 or m==2:
	y=y-1
	m=m+12
#else:
#	y=y
#	m=m
A=int(y/100.)
B=2-A+int(A/4.)
jde=int(365.25*(y+4716))+int(30.6001*(m+1))+d+B-1524.5
jde=jde+h/24.

print '# MJD = %.2f     #' %(jde)
print '########################'


###################################
#    Calcul des parametres de 	  #
# positionnement a la date donnee #
###################################

T = (jde-2451545.)/36525.
print 'T: %s' %(repr(T))

# Longitude moyenne de la Lune:
Ll = 218.3164591+481267.88134236*T-0.0013268*T**2+T**3/538841.-T**4/65194000.
Ll=Ll%360.
Ll=Ll*pi/180.
print 'Ll: %s' %(repr(Ll))

# Elongation moyenne de la Lune:
D = 297.8502042+445267.1115168*T-0.0016300*T**2+T**3/545868.-T**4/113065000.
D=D%360.
D=D*pi/180.
print 'D: %s' %(repr(D))

# Anomalie moyenne du Soleil:
M = 357.5291092+35999.0502909*T-0.0001536*T**2+T**3/24490000.
M=M%360.
M=M*pi/180.
print 'M: %s' %(repr(M))

# Anomalie moyenne de la Lune:
Ml = 134.9634114+477198.8676313*T+0.0089970*T**2+T**3/69699.-T**4/14712000.
Ml=Ml%360.
Ml=Ml*pi/180.
print 'Ml: %s' %(repr(Ml))

# Argument de la latitude de la Lune (distance moyenne de la Lune au noeud ascendant):
F = 93.2720993+483202.0175273*T-0.0034029*T**2-T**3/3526000.+T**4/863310000.
F=F%360.
F=F*pi/180.
print 'F: %s' %(repr(F))

# 3 autres arguments necessaires (en degres):
A1 = 119.75 + 131.849 * T
A1=A1%360.
A1=A1*pi/180.
A2 = 53.09 + 479264.290 * T
A2=A2%360.
A2=A2*pi/180.
A3 = 313.45 + 481266.484 * T
A3=A3%360.
A3=A3*pi/180.
print 'A1: %s' %(repr(A1))
print 'A2: %s' %(repr(A2))
print 'A3: %s' %(repr(A3))

# Lecture du tableau des termes periodiques de longitude (Sigl) 
# et de distance (Sigr) de la Lune
table1=open('Moontable1.rdb','r')
atable1=table1.readlines()

Sigl=0.
Sigr=0.
for i in range(3,len(atable1)):
	ll=string.split(atable1[i])
	coeffD=int(ll[0])
	coeffM=int(ll[1])
	coeffMl=int(ll[2])
	coeffF=int(ll[3])
	coeffSigl=float(ll[4])
	coeffSigr=float(ll[5])
	
	E = 1-0.002516*T-0.0000074*T**2
	if (coeffM==1) or (coeffM==-1):
		Sigl = Sigl + E * coeffSigl * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)
		Sigr = Sigr + E * coeffSigr * math.cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)	      
	elif (coeffM==2) or (coeffM==-2):
		Sigl = Sigl + E**2 * coeffSigl * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)
		Sigr = Sigr + E**2 * coeffSigr * math.cos(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)
	else:
		Sigl = Sigl + coeffSigl * math.sin(D*coeffD+Ml*coeffMl+F*coeffF)
		Sigr = Sigr + coeffSigr * math.cos(D*coeffD+Ml*coeffMl+F*coeffF)

print 'sigl: %s' %(repr(Sigl))
print 'sigr: %s' %(repr(Sigr))

# Lecture du tableau des termes periodiques de latitude (Sigb) de la Lune
table2=open('Moontable2.rdb','r')
atable2=table2.readlines()

Sigb=0.
for i in range(3,len(atable2)):
	ll=string.split(atable2[i])
		
	coeffD=int(ll[0])
	coeffM=int(ll[1])
	coeffMl=int(ll[2])
	coeffF=int(ll[3])
	coeffSigb=float(ll[4])
	
	E = 1.-0.002516*T-0.0000074*T**2
	if (coeffM==1) or (coeffM==-1):
		Sigb = Sigb + E * coeffSigb * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)
	elif (coeffM==2) or (coeffM==-2):
		Sigb = Sigb + E**2 * coeffSigb * math.sin(D*coeffD+M*coeffM+Ml*coeffMl+F*coeffF)
	else:
		Sigb = Sigb + coeffSigb * math.sin(D*coeffD+Ml*coeffMl+F*coeffF)

print 'sigb: %s' %(repr(Sigb))

# Termes additionnels dus a l'action de Venus, Jupiter et l'applanissement de la Terre
Sigl = Sigl + 3958.*math.sin(A1)+1962.*math.sin(Ll-F)+318.*math.sin(A2)
Sigb = Sigb - 2235.*math.sin(Ll)+382.*math.sin(A3)+175.*math.sin(A1-F)+\
  175.*math.sin(A1+F)+127.*math.sin(Ll-Ml)-115.*math.sin(Ll+Ml)

print '\nAfter correction by Jupiter and mates'
print 'sigl: %s' %(repr(Sigl))
print 'sigb: %s' %(repr(Sigb))


# Coordonnees de la Lune (en degres):
lbda = (Ll*180/pi + Sigl/1000000.)
lbda=lbda%360.
beta = (Sigb/1000000.)
beta=beta%360.
# Distance de la Lune au centre de la Terre (en km):
GrdDelta = 385000.56 + Sigr/1000.


## Apparent lambda by adding to lambda the nutation in longitude:
L=280.4665+36000.7698*T
L=L%360.
L=L*pi/180.
# Longitude of the mean ascending node
omega=125.0445550-1934.1361849*T+0.0020762*T**2+T**3/467410.-T**4/60616000.
omega=omega%360.
dphi=-17.20/3600.*math.sin(pi*omega/180.)-1.32/3600.*math.sin(2.*L)-0.23/3600.*math.sin(2.*Ll)+\
  0.21/3600.*math.sin(2.*pi*omega/180.)
dphi=dphi%360.
applbda=lbda+dphi
applbda=applbda%360.

## True obliquity of the ecliptic
eps0=(23.+26./60.+21.448/3600.)-46.8150/3600.*T-0.00059/3600.*T**2+0.001813/3600.*T**3
deps=9.20/3600.*math.cos(pi*omega/180.)+0.57/3600.*math.cos(2.*L)+0.10/3600.*math.cos(2.*Ll)-\
  0.09/3600.*math.cos(2*pi*omega/180.)
eps=eps0+deps
eps=eps%360.

## Ascension droite et declinaison apparente de la Lune

#######################
# Arctan function
def darctan(y,x):
	if (x==0) and (y==0):
		return 0
	elif (y<0.) and (x>0.):
		return (math.atan(y/x)*180./pi)+360.
	elif (x<0.):
		return (math.atan(y/x)*180./pi)+180.
	else:
		return math.atan(y/x)*180./pi
#######################

alpha=darctan((math.sin(applbda*pi/180.)*math.cos(eps*pi/180.)-\
  math.tan(beta*pi/180.)*math.sin(eps*pi/180.)),math.cos(applbda*pi/180.))

delta=math.asin(math.sin(beta*pi/180.)*math.cos(eps*pi/180.)+\
    math.cos(beta*pi/180.)*math.sin(eps*pi/180.)*math.sin(applbda*pi/180.))

delta=delta*180./pi
#alpha=alpha%360.
#delta=delta%360.

halpha=int(alpha/15.)
mnalpha=int((alpha/15.-halpha)*60.)
salpha=((alpha/15.-halpha)*60.-mnalpha)*60.

if delta>90.:
	delta=delta-360.
else:
	delta=1.*delta

print '\nAlpha: %s' %(repr(alpha))
print '\nDelta: %s' %(repr(delta))

ddelta=int(delta)
mndelta=abs(int((delta-ddelta)*60.))
sdelta=abs(((delta-ddelta)*60.-int((delta-ddelta)*60.))*60.)


###################################
# Affichage des 		  #
# elements calcules 		  #
###################################

print 'Moon coordinates :'
#print 'Alpha = ',halpha,'h ',mnalpha,'mn ',salpha,'s'
#print 'Delta = ',ddelta,'d ',mndelta,' ',sdelta
print 'Alpha = %2ih %2im %ss' %(halpha,mnalpha,repr(salpha))
print 'Delta = %2id %2im %s"' %(ddelta,mndelta,repr(sdelta))
