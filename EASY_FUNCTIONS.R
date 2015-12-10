#############################################################################################
#This script contains methodologies for quality control for hourly and daily climatic series
#
#This script is free: you can redistribute it and/or modify
#Autor: VICTOR HUGO PATIÑO BRAVO
#v.h.patino@cgiar.org
#Agosto, 2015
#Version V.02.15
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

#PACKAGES
library(chron)
library(stringr)
library(tcltk)
library(sirad)
library(randomForest)
library(rpart)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RMAWGEN)
library(utils)
#Work directory
<<<<<<< HEAD
dirFol    <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/DATOS_PROCESADOS/_clima/IDEAM"
=======
dirFol    <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/CLIMA/SERIES_CLIMA_PROCESADO/AntCorTol/"
>>>>>>> refs/remotes/victor1130/master
setwd(dirFol)

GRAPHICS  <- function(){source(paste0(dirFol,"/GRAPHICS.R"))}
#Years of analisys

#Para Tolima
<<<<<<< HEAD
YStart    <- 2008 #Star Year for analisys
=======
YStart    <- 2011 #Star Year for analisys
>>>>>>> refs/remotes/victor1130/master
YEnd      <- 2015 #End  Year for analisys

                                  ########  ########  ######   #### ##    ## 
                                  ##     ## ##       ##    ##   ##  ###   ## 
                                  ##     ## ##       ##         ##  ####  ## 
                                  ########  ######   ##   ####  ##  ## ## ## 
                                  ##     ## ##       ##    ##   ##  ##  #### 
                                  ##     ## ##       ##    ##   ##  ##   ### 
                                  ########  ########  ######   #### ##    ## 
                        ########  ########   #######   ######  ########  ######   ######  
                        ##     ## ##     ## ##     ## ##    ## ##       ##    ## ##    ## 
                        ##     ## ##     ## ##     ## ##       ##       ##       ##       
                        ########  ########  ##     ## ##       ######    ######   ######  
                        ##        ##   ##   ##     ## ##       ##             ##       ## 
                        ##        ##    ##  ##     ## ##    ## ##       ##    ## ##    ## 
                        ##        ##     ##  #######   ######  ########  ######   ######  



#Create folders
FOLDERS(dirFol)
        ########################################################################
        #WARNING!!!! :You need put files on SERIES_ORIGINAL folder for continue#
        ########################################################################

#Quality Control station hourly
QCHORLY(dirFol)

#Convert
CONVERT(dirFol)###Corregir

#Mix: solo si se tienen datos horarios y diarios de una misma estacion
MIX(dirFol)

#Quality Control station daily
QCDAILY(dirFol)###Corregir


#Inputs
INPUTS(dirFol)

#Graphics after QC
GRAPHICS()

#Summary for variable

SUMMARY(dirFol,"TMAX",YStart,YEnd)
SUMMARY(dirFol,"TMIN",YStart,YEnd)
SUMMARY(dirFol,"RAIN",YStart,YEnd)
SUMMARY(dirFol,"ESOL",YStart,YEnd)
SUMMARY(dirFol,"RHUM",YStart,YEnd)

#Generate Data: Precipitation, Temperature max and min.

      #########################################################################
      #     WARNING!!!! :You need check that the files TMAX_to, TMIN_to and   #
      #          RAIN_to contain the same stations before continue            #
      #########################################################################

#You can use DontUse vector for exclude positions of the stations that you don't will use
<<<<<<< HEAD
#eg: DontUse=c(2,4,9)
YStart <- 2008
YEnd   <- 2015
GENERATOR_T_R(dirFol, YStart, YEnd, DontUse=2)
=======
#eg: 
DontUse=c(6,8,9,11)

GENERATOR_T_R(dirFol,YStart,YEnd)#,DontUse = DontUse)
>>>>>>> refs/remotes/victor1130/master
#length(DontUse)
#Relative humidity and Solar energy
GEN_RHUM(dirFol)
GEN_ESOL(dirFol)

#Text files of ESOL and Final Graphics
END_GRAPS(dirFol)

##############END