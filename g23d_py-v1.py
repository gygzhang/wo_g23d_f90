#!/usr/bin/env python
"""
Script name: GRAPES23DDAT.py
Author: DA LI (NWPC/NMC/CMA)
Date: 2020.01
Purpose: Generate input file to CALMET from GRAPES Model Pressure Product 
Usage: ./GRAPES23DDAT.py <date>
        <date> - Start date of GRAPES in format YYYYMMDDHH, e.g. 2017120400
Output: File written to <root>/3D.DAT

"""


def writeRec1():
    fout.write('3D.DAT            2.1             GRAPES 3km Model Product\n')

def writeRec2():
    fout.write('0     \n')

def writeRec3():
    IOUTW = 1  
    IOUTQ = 1  
    IOUTC = 1 
    IOUTI = 1 
    IOUTG = 1
    IOSRF = 0 
    fout.write(('{:3d}'*6+'\n').format(IOUTW,IOUTQ,IOUTC,IOUTI,IOUTG,IOSRF))

def writeRec4():
    MAPTXT = 'LLC' 
    RLATC = (lats[iLatMinGRIB]+lats[iLatMaxGRIB])/2.  
    RLONC = (lons[iLonMinGRIB]+lons[iLonMaxGRIB])/2.
    TRUELAT1 = lats[iLatMinGRIB] 
    TRUELAT2 = lats[iLatMinGRIB+1]
    X1DMN = 0.0  
    Y1DMN = 0.0 
    DXY = 0.0
    fout.write(('{:4}{:9.4f}{:10.4f}'+'{:7.2f}'*2+'{:10.3f}'*2+'{:8.3f}'+'{:4d}'*2+'{:3d}\n').
               format(MAPTXT,RLATC,RLONC,TRUELAT1,TRUELAT2,X1DMN,Y1DMN,DXY,NX,NY,NZ))

def writeRec5():
   fout.write(('{:3d}'*23+'\n').format(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

def writeRec6():
    IBYRM = int(date[0:4])  
    IBMOM = int(date[4:6])  
    IBDYM = int(date[6:8]) 
    IBHRM = int(date[8:10])
    NHRSMM5 = nfiles-1
    fout.write(('{:4d}'+'{:02d}'*3+'{:5d}'+'{:4d}'*3+'\n').format(IBYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,NX,NY,NZ))

def writeRec7():
    NX1 = 1  
    NY1 = 1  
    NX2 = NX 
    NY2 = NY  
    NZ1 = 1 
    NZ2 = NZ 
    RXMIN = lons[iLonMinGRIB]  
    RXMAX = lons[iLonMaxGRIB]
    RYMIN = lats[iLatMinGRIB] 
    RYMAX = lats[iLatMaxGRIB] 
    fout.write(('{:4d}'*6+'{:10.4f}'*2+'{:9.4f}'*2+'\n').format(NX1,NY1,NX2,NY2,NZ1,NZ2,RXMIN,RXMAX,RYMIN,RYMAX))
    SIGMA = np.array(levsIncl)/1013.25
    for s in SIGMA:
        fout.write('{:6.3f}\n'.format(s))

def writeRec8():
    IELEVDOT = 0 
    ILAND = -9
    XLATCRS = -999 
    XLONGCRS = -999 
    IELEVCRS = -999
    for j in range(NY):
        JINDEX = j+1 
        XLATDOT = lats[iLatMinGRIB-j]  
        for i in range(NX):
            IINDEX = i+1 
            XLONGDOT = lons[iLonMinGRIB+i]
            fout.write(('{:4d}'*2+'{:9.4f}{:10.4f}{:5d}{:3d} {:9.4f}{:10.4f}{:5d}\n').format(IINDEX,JINDEX,XLATDOT,XLONGDOT,IELEVDOT,ILAND,XLATCRS,XLONGCRS,IELEVCRS))

def writeRec9():
    SC = 0
    SST = 0
    for t in range(nfiles):
        print("Processing file "+filenames[t])
        datetemp = str(date[0:8])
        dateTime = parse(datetemp)+dt.timedelta(hours=t*1)
        MYR = dateTime.year  
        MMO = dateTime.month
        MDAY = dateTime.day
        MHR = dateTime.hour
      
        f = open(filePaths[t], 'r') 
        eccodes.codes_grib_multi_support_on 
        mcount = eccodes.codes_count_in_file(f) 
        [eccodes.codes_grib_new_from_file(f) for i in range(mcount)]
        f.close()  
        PRESgrd = np.zeros(shape=(Nj, Ni))
        RAINgrd = np.zeros(shape=(Nj, Ni))
        RADSWgrd = np.zeros(shape=(Nj, Ni))
        RADLWgrd = np.zeros(shape=(Nj, Ni))
        T2grd = np.zeros(shape=(Nj, Ni))
        Q2grd = np.zeros(shape=(Nj, Ni))
        U10grd = np.zeros(shape=(Nj, Ni))
        V10grd = np.zeros(shape=(Nj, Ni))
        PRESvals = eccodes.codes_get_values(int(gidPRES))
        PRESgrd[:, :] = np.reshape(PRESvals, (Nj, Ni), 'C')      
        RAINvals = eccodes.codes_get_values(int(gidRAIN))   
        RAINgrd[:, :] = np.reshape(RAINvals, (Nj, Ni), 'C')
        RADSWvals = eccodes.codes_get_values(int(gidRADSW))
        RADSWgrd[:, :] = np.reshape(RADSWvals, (Nj, Ni), 'C')
        RADLWvals = eccodes.codes_get_values(int(gidRADLW))
        RADLWgrd[:, :] = np.reshape(RADLWvals, (Nj, Ni), 'C')    
        T2vals = eccodes.codes_get_values(int(gidT2))
        T2grd[:, :] = np.reshape(T2vals, (Nj, Ni), 'C')      
        Q2vals = eccodes.codes_get_values(int(gidQ2))
        Q2grd[:, :] = np.reshape(Q2vals, (Nj, Ni), 'C')       
        U10vals = eccodes.codes_get_values(int(gidU10))
        U10grd[:, :] = np.reshape(U10vals, (Nj, Ni), 'C')      
        V10vals = eccodes.codes_get_values(int(gidV10))
        V10grd[:, :] = np.reshape(V10vals, (Nj, Ni), 'C')       
        WS10grd = np.sqrt(U10grd**2+V10grd**2)
        WD10grd = np.arctan2(V10grd,U10grd) 
        WD10grd *= 180/np.pi
        WD10grd += 180
        WD10grd =- WD10grd 
        WD10grd += 90 
        WD10grd = np.mod(WD10grd, 360)
        HGTgrd = np.zeros(shape=(Nj, Ni, NZ))
        TMPgrd = np.zeros(shape=(Nj, Ni, NZ))
        Ugrd = np.zeros(shape=(Nj, Ni, NZ))
        Vgrd = np.zeros(shape=(Nj, Ni, NZ))
        Wgrd = np.zeros(shape=(Nj, Ni, NZ))
        RHgrd = np.zeros(shape=(Nj, Ni, NZ))
        VAPMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        CLDMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        RAINMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        ICEMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        SNOWMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        GRPMRgrd = np.zeros(shape=(Nj, Ni, NZ))
        for k in range(NZ):
            HGTvals = eccodes.codes_get_values(int(gidHGT[k]))
            HGTgrd[:, :, k] = np.reshape(HGTvals, (Nj, Ni), 'C')          
            TMPvals = eccodes.codes_get_values(int(gidTMP[k]))
            TMPgrd[:, :, k] = np.reshape(TMPvals, (Nj, Ni), 'C')
            Uvals = eccodes.codes_get_values(int(gidU[k]))
            Ugrd[:, :, k] = np.reshape(Uvals, (Nj, Ni), 'C')         
            Vvals = eccodes.codes_get_values(int(gidV[k]))
            Vgrd[:, :, k] = np.reshape(Vvals, (Nj, Ni), 'C')         
            Wvals = eccodes.codes_get_values(int(gidW[k]))
            Wgrd[:, :, k] = np.reshape(Wvals, (Nj, Ni), 'C')          
            RHvals = eccodes.codes_get_values(int(gidRH[k]))
            RHgrd[:, :, k] = np.reshape(RHvals, (Nj, Ni), 'C')           
            VAPMRvals = eccodes.codes_get_values(int(gidVAPMR[k]))
            VAPMRgrd[:, :, k] = np.reshape(VAPMRvals, (Nj, Ni), 'C')           
            CLDMRvals = eccodes.codes_get_values(int(gidCLDMR[k]))
            CLDMRgrd[:, :, k] = np.reshape(CLDMRvals, (Nj, Ni), 'C')           
            RAINMRvals = eccodes.codes_get_values(int(gidRAINMR[k]))
            RAINMRgrd[:, :, k] = np.reshape(RAINMRvals, (Nj, Ni), 'C')           
            ICEMRvals = eccodes.codes_get_values(int(gidICEMR[k]))
            ICEMRgrd[:, :, k]  = np.reshape(ICEMRvals, (Nj, Ni), 'C')       
            SNOWMRvals = eccodes.codes_get_values(int(gidSNOWMR[k]))
            SNOWMRgrd[:, :, k] = np.reshape(SNOWMRvals, (Nj, Ni), 'C')         
            GRPMRvals = eccodes.codes_get_values(int(gidGRPMR[k]))
            GRPMRgrd[:, :, k] = np.reshape(GRPMRvals, (Nj, Ni), 'C')   
            WSgrd = np.sqrt(Ugrd**2+Vgrd**2)
        WDgrd = np.arctan2(Vgrd,Ugrd)
        WDgrd *= 180/np.pi
        WDgrd += 180 
        WDgrd =- WDgrd
        WDgrd += 90
        WDgrd = np.mod(WDgrd, 360)
        for j in range(NY):
            JX = j+1 
            for i in range(NX):
                IX = i+1 
                PRES = PRESgrd[iLatMinGRIB-j,iLonMinGRIB+i] * 0.01 #sea level pressure (hPa)
                RAIN = RAINgrd[iLatMinGRIB-j,iLonMinGRIB+i] * 0.01 #total rainfall
                RADSW = RADSWgrd[iLatMinGRIB-j,iLonMinGRIB+i] #Short wave radiation at the surface (W/m**2)
                RADLW = RADLWgrd[iLatMinGRIB-j,iLonMinGRIB+i] #long wave radiation at the top (W/m**2)
                T2 = T2grd[iLatMinGRIB-j,iLonMinGRIB+i] #Air temperature at 2 m (K)
                Q2 = Q2grd[iLatMinGRIB-j,iLonMinGRIB+i] *1000 #Specific humidity at 2 m (g/kg)
                WD10 = WD10grd[iLatMinGRIB-j,iLonMinGRIB+i] #Wind direction of 10-m wind (m/s)
                WS10 = WS10grd[iLatMinGRIB-j,iLonMinGRIB+i] #Wind speed of 10-m wind (m/s)
                fout.write(('{:4d}'+'{:02d}'*3+'{:3d}'*2+'{:7.1f}{:5.2f}{:2d}'+'{:8.1f}'*3+'{:8.2f}'+'{:8.1f}'*3+'\n').format(MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST))
                for k in range(NZ):
                    PRES2 = levsIncl[k]  # Pressure (mb)
                    Z = int(HGTgrd[iLatMinGRIB-j,iLonMinGRIB+i,k]) #Elevation (m above sea level)
                    TEMPK=int(TMPgrd[iLatMinGRIB-j,iLonMinGRIB+i,k]) #Temperature (Kelvin)
                    WD=int(WDgrd[iLatMinGRIB-j,iLonMinGRIB+i,k]) #Wind direction (degrees)
                    WS=WSgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] #Wind speed (m/s)
                    W=Wgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] #Vertical velocity (m/s)
                    RH=int(RHgrd[iLatMinGRIB-j,iLonMinGRIB+i,k]) #Relative humidity (%)
                    VAPMR=VAPMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] *1000 #Vapor mixing ratio (g/kg)
                    CLDMR=CLDMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] * 1000 #Cloud mixing ratio (g/kg)
                    RAINMR=RAINMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] * 1000 #Rain mixing ratio (g/kg)
                    ICEMR=ICEMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] * 1000#Ice mixing ratio (g/kg)
                    SNOWMR=SNOWMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] *1000 #Snow mixing ratio (g/kg)
                    GRPMR=GRPMRgrd[iLatMinGRIB-j,iLonMinGRIB+i,k] *1000 #Graupel mixing ratio (g/kg)
                    fout.write(('{:4d}{:6d}{:6.1f}{:4d}{:5.1f}{:6.2f}{:3d}{:5.2f}'+'{:6.3f}'*5+'\n').format(PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR))
        for i in range(mcount):
            eccodes.codes_release(i+1)

import sys
import os
sys.path.insert(1, '/g1/app/mathlib/eccodes/2.15.0/intel/lib64/python2.7/site-packages')
import eccodes
import numpy as np
from dateutil.parser import parse
import datetime as dt
import argparse


parser = argparse.ArgumentParser(description="Script to generate input file to CALMET from GRAPES met data",\
   epilog="Example of use: ./GRAPES23DDAT.py 2017120400")
parser.add_argument("date", help="Start date of GRAPES data in format YYYYMMDDHH, e.g. 2017120400",type=str)
args = parser.parse_args()
date = args.date

latMinCP = 35.40  # Min lat
latMaxCP = 42.53  # Max lat
lonMinCP = 112.6  # Min lon
lonMaxCP = 122.1  # Max lon
inDir = '/g3/lida/model/3ddat/ORIG/'  

nfiles = 48
outFile = '/g3/lida/model/3ddat/3d.dat'
levsIncl = [1000,975,950,925,900,850,800,750,700,650,600,550,500,450,400,350,300,200,100] 


filePrefix = 'rmf.hgra.2019032100'
fileSuffix = '.grb2'
filenames = []
filePaths = []
for i in range(nfiles):
    filenames.append(filePrefix+'{:03d}'.format(i)+fileSuffix)
    filePaths.append(os.path.join(inDir, filenames[i]))

f = open(filePaths[0], 'r')
print (filePaths[0])
eccodes.codes_grib_multi_support_on()
mcount = eccodes.codes_count_in_file(f) 
print (mcount)
gids = [eccodes.codes_grib_new_from_file(f) for i in range(mcount)]
f.close()


varNames = []
levels = []
varParCat = []
varParNum = []
prodDefinTemNum = []
for i in range(mcount):
    gid = gids[i]
    varNames.append(eccodes.codes_get(gid, 'shortName'))
    levels.append(eccodes.codes_get(gid, 'level'))
    varParCat.append(eccodes.codes_get(gid, 'parameterCategory'))
    varParNum.append(eccodes.codes_get(gid, 'parameterNumber'))
    prodDefinTemNum.append(eccodes.codes_get(gid, 'productDefinitionTemplateNumber'))

try:
    gidPRMSL = varNames.index("prmsl")+1  
    print "gidprmsl",(gidPRMSL)
except ValueError:
    print('Variables not found')
    print('please check GRAPES grib2 files for corresponting day contain full sized grib files')
    sys.exit()


gidPRES = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 3 and varParNum[i] == 1 and levels[i] == 0)]) #Mean sea level
gidRAIN = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 8 and levels[i] == 0)]) #Total rainfall accumulated on the ground for the past hour
gidRADSW = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 4 and varParNum[i] == 9 and levels[i] == 0)]) #Short wave radiation at the surface
gidRADLW = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 5 and varParNum[i] == 5 and levels[i] == 0)]) #Long wave radiation at the surface
gidT2 = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 0 and varParNum[i] == 0 and levels[i] == 2)]) #Air temperature at 2 m
gidQ2 = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 0 and levels[i] == 2)]) #Specific humidity at 2 m
gidU10 = np.flipud([i+1 for i in range(len(varNames)) if (varNames[i] == '10u' and varParCat[i] == 2 and varParNum[i] == 2 and prodDefinTemNum[i] == 0 and levels[i] == 10)]) #U-component of wind at 10 m
gidV10 = np.flipud([i+1 for i in range(len(varNames)) if (varNames[i] == '10v' and varParCat[i] == 2 and varParNum[i] == 3 and prodDefinTemNum[i] == 0 and levels[i] == 10)]) #V-component of wind at 10 m
gidHGT = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 3 and varParNum[i] == 5 and levels[i] in levsIncl)]) #Height
gidHGT = gidHGT[::-1]
gidTMP = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 0 and varParNum[i] == 0 and levels[i] in levsIncl)]) #Temperature
gidTMP = gidTMP[::-1]
gidU = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 2 and varParNum[i] == 2 and levels[i] in levsIncl)]) #U-component of wind
gidU = gidU[::-1]
gidV = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 2 and varParNum[i] == 3 and levels[i] in levsIncl)]) #V-component of wind
gidV = gidV[::-1]
gidW = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 2 and varParNum[i] == 9 and levels[i] in levsIncl)]) #W-component of wind
gidW = gidW[::-1]
gidRH = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 1 and levels[i] in levsIncl)]) #Relative humidity
gidRH = gidRH[::-1]
gidVAPMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 0 and levels[i] in levsIncl)]) #Vapor mixing ratio
gidVAPMR = gidVAPMR[::-1]
gidCLDMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 22 and levels[i] in levsIncl)]) #Cloud mixing ratio
gidCLDMR = gidCLDMR[::-1]
gidRAINMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 24 and levels[i] in levsIncl)]) #Rain mixing ratio
gidRAINMR = gidRAINMR[::-1]
gidICEMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 23 and levels[i] in levsIncl)]) #Ice mixing ratio
gidICEMR = gidICEMR[::-1]
gidSNOWMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] == 25 and levels[i] in levsIncl)]) #Snow mixing ratio
gidSNOWMR = gidSNOWMR[::-1]
gidGRPMR = np.flipud([i+1 for i in range(len(varNames)) if (varParCat[i] == 1 and varParNum[i] ==32 and levels[i] in levsIncl)]) #Graupel mixing ratio
gidGRPMR = gidGRPMR[::-1]

lats = eccodes.codes_get_array(gidPRMSL,'distinctLatitudes')
lons = eccodes.codes_get_array(gidPRMSL,'distinctLongitudes')
Ni = eccodes.codes_get(gidPRMSL,'Ni')
Nj = eccodes.codes_get(gidPRMSL,'Nj')

for i in range(len(lats)-1,-1,-1):
    if lats[i] >= latMinCP:
        iLatMinGRIB=i+1
        print "i",(i),lats[i],lats[i+1]
        break
for i in range(len(lats)-1,-1,-1):
    if lats[i] > latMaxCP:
        iLatMaxGRIB=i
        break
for i in range(len(lons)-1):
    if lons[i+1] >= lonMinCP:
        iLonMinGRIB=i
        break
for i in range(len(lons)-1):
    if lons[i+1] > lonMaxCP:
        iLonMaxGRIB=i+1
        break

NX=iLonMaxGRIB-iLonMinGRIB+1 
NY=iLatMinGRIB-iLatMaxGRIB+1 
NZ=len(levsIncl)

for i in range(mcount):
    eccodes.codes_release(i+1)

fout=open(outFile,'w')

writeRec1()
writeRec2()
writeRec3()
writeRec4()
writeRec5()
writeRec6()
writeRec7()
writeRec8()
writeRec9()

fout.close()
