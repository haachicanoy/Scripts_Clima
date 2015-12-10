#############################################################################################
# This script contains methodologies for quality control for hourly and daily climatic series
#
# This script is free: you can redistribute it and/or modify
# Autor: VICTOR HUGO PATIÑO BRAVO
# v.h.patino@cgiar.org
# Agosto, 2015
# Version V.02.15
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#############################################################################################

# PACKAGES
if(require(ggplot2)==FALSE){install.packages("ggplot2")} 
if(require(rpart)==FALSE){ install.packages("rpart")}
if(require(RMAWGEN)==FALSE){install.packages("RMAWGEN") } 
if(require(chron)==FALSE){install.packages("chron")}
if(require(randomForest)==FALSE){install.packages("randomForest")}
if(require(utils)==FALSE){install.packages("utils")}
if(require(stringr)==FALSE){install.packages("stringr")}
if(require(tcltk)==FALSE){install.packages("tcltk")}
if(require(sirad)==FALSE){install.packages("sirad")}
if(require(tidyr)==FALSE){install.packages("tidyr")}
if(require(dplyr)==FALSE){install.packages("dplyr")}

# Work directory  :: #dirFol    <- "C:/Users/nameUser/Desktop/workspace/"
dirFol    <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/DATOS_PROCESADOS/_clima/IDEAM"
setwd(dirFol)

GRAPHICS  <- function(){source(paste0(dirFol,"/GRAPHICS.R"))}

# Years of analisys

# Para Magdalena
YStart    <-  2008 # Star Year for analisys
YEnd      <-  2015 # End  Year for analisys

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
CONVERT(dirFol)

#Mix: solo si se tienen datos horarios y diarios de una misma estacion
MIX(dirFol)

#Quality Control station daily
QCDAILY(dirFol)

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

# You can use DontUse vector for exclude positions of the stations that you don't will use
DontUse <- 2# c(3,8,11,4,9,6,12)

GENERATOR_T_R(dirFol, YStart, YEnd, DontUse=DontUse) # Replace internal function: ComprehensivePrecipitationGenerator by:
# //dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/DATOS_PROCESADOS/_clima\IDEAM\PROCESS/RMAWCORREGIDO.txt
# //dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/CLIMA/QC_scripts_R/OldVersion/RMAWCORREGIDO.txt

#Relative humidity and Solar energy
GEN_RHUM(dirFol)
GEN_ESOL(dirFol)

#Text files of ESOL and Final Graphics
END_GRAPS(dirFol)

############## END
