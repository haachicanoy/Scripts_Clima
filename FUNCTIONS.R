## Victor Hugo Patino Bravo2
## Version V.02.15
#Creando carpetas, necesarias para organizar la informacion
FOLDERS <-function(dirFol){
  dir.create(paste0(dirFol,"/SERIES_ORIGINAL"),showWarnings=F)
  dir.create(paste0(dirFol,"/SERIES_ORIGINAL/DAILY"),showWarnings=F)
  dir.create(paste0(dirFol,"/SERIES_ORIGINAL/HOURLY"),showWarnings=F)
  dir.create(paste0(dirFol,"/PROCESS"),showWarnings=F)
  dir.create(paste0(dirFol,"/PROCESS/01_SERIES_HOURLY_QC_OK"),showWarnings=F)
  dir.create(paste0(dirFol,"/PROCESS/02_SERIES_DAILY_to_QC"),showWarnings=F)
  dir.create(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes"),showWarnings=F)
  dir.create(paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK"),showWarnings=F)}

#Contro de calidad horario
QCHORLY <- function(dirFol){
  
  print("Wait a moment please...")
  
  # inicializacion de indicadores reporte
  ProbCohernciaTEMP=FALSE
  ProbUNIDAD=FALSE
  ProbHEADER=FALSE
  ProbDUP=FALSE
  REPORTSINDATA=NA
  
  # Lectura de los datos
  
  Filesroutes=paste0(dirFol,"/SERIES_ORIGINAL/HOURLY/")
  Filesroutesdestino=paste0(dirFol,"/PROCESS/01_SERIES_HOURLY_QC_OK/")
  FilesNames=list.files(Filesroutes, pattern="\\.txt$")
  NumFiles=length(FilesNames)
  nom.files=substring(FilesNames,1,nchar(FilesNames)-4)
  
  Data.all.files=lapply(paste0(Filesroutes,FilesNames),function(x){read.delim(x,sep="\t",header=F,col.names=c("Date", "Hour","Value"))})
  #Data.all.files=lapply(paste0(Filesroutes,FilesNames),function(x){print(x);return(read.table(x,sep="\t",header=T,blank.lines.skip=TRUE))})
  #read.table("//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/CLIMA/SERIES_CLIMA_PROCESADO/Santander//SERIES_ORIGINAL/HOURLY/28025120_WAM2_ESOL.txt",sep="\t",header=T,blank.lines.skip=TRUE)
  names(Data.all.files)=nom.files
  
  # Deteccion de las variables
  VAReasy=grep("TMAX|TMIN|RHUM|WAM2", nom.files)
  VARaccu=grep("RAIN|MJM2|CCM2", nom.files)
  
  #verificacion que se clasificaron todos los archivos
  if (length(VAReasy)+length(VARaccu)==NumFiles){
    Data.easy=Data.all.files[VAReasy]
    Data.accu=Data.all.files[VARaccu]
  }else{
    print("Error: There is a problem with the files names!. ID_VAR.txt or ID_UNIT_VAR.txt")
    stop
  }
  
  # Verificacion de los header:
  for(i in 1:length(Data.all.files)){
    
    if (all(colnames(Data.all.files[[i]])==c("Date","Hour","Value"))){    
    }else{
      ProbHEADER=TRUE
      mens=paste0(("Error: There is a problem with the headers in the file"),i," (Date,Hour or Value).")
      print(mens)
    }
  }
  #########################
  # Crear series de destino
  Data.all.files.OK=Data.all.files
  
  perc=0
  filmin=paste0(YStart,"-01-01")
  filmax=paste0(YEnd,"-12-31")

  pb <- winProgressBar(title = "Progress bar", min = 0,
                      max = length(Data.all.files), width = 300)
  for(i in seq_along(Data.all.files)){
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste( round(i/length(Data.all.files)*100, 0), "% done"))
  
    Data.all.files.OK[[i]][,3]=as.numeric(as.character(Data.all.files.OK[[i]][,3]))  
    ProbDUP=FALSE
    
    dataFech=Data.all.files.OK[[i]]$Date
    typeOrig=length(grep("-|/",dataFech))
    #Ajuste de formato Fecha
    if(typeOrig>0){
      #Si estas vienen con separador yyyy-mm-dd
      Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.files[[i]]$Date))
    }else{
      #Si estas vienen SIN separador yyyymmdd
      Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.files[[i]]$Date), "%Y%m%d")}
    
    Data.all.files.OK2=Data.all.files.OK[[i]]$Date>=as.Date(filmin)&Data.all.files.OK[[i]]$Date<=as.Date(filmax)
    Data.all.files.OK[[i]]=Data.all.files.OK[[i]][Data.all.files.OK2,]
    
    #Conversion de horas
    TEMPhour=Data.all.files[[i]]$Hour
    Data.all.files.OK[[i]]$Hour=gsub("a.m.", "AM", Data.all.files.OK[[i]]$Hour)
    Data.all.files.OK[[i]]$Hour=gsub("p.m.", "PM", Data.all.files.OK[[i]]$Hour)
    Data.all.files.OK[[i]]$Hour=gsub("a,m,", "AM", Data.all.files.OK[[i]]$Hour)
    Data.all.files.OK[[i]]$Hour=gsub("p,m,", "PM", Data.all.files.OK[[i]]$Hour)
    Data.all.files.OK[[i]]$Hour=gsub("a", "AM", Data.all.files.OK[[i]]$Hour)
    Data.all.files.OK[[i]]$Hour=gsub("p", "PM", Data.all.files.OK[[i]]$Hour)
    
    #Condicion para identificar si la hora tiene segundos
    Cond=strftime(strptime(Data.all.files.OK[[i]]$Hour,"%I:%M:%S %p"),"%H:%M:%S")[2]
    #Si Cond==NA no tiene segundos
    if(is.na(Cond)){TEMPhour<- strftime(strptime(Data.all.files.OK[[i]]$Hour,"%I:%M"),"%H:%M:%S")##SI formato original hora es 12:35 am/pm
    }else{TEMPhour<- strftime(strptime(Data.all.files.OK[[i]]$Hour,"%I:%M:%S %p"),"%H:%M:%S")}##SI formato original hora es 12:35:59 am/pm
    
    Data.all.files.OK[[i]]$Hour=times(TEMPhour)
    #Ordeno los datos cronologicamente
    Data.all.files.OK[[i]]=Data.all.files.OK[[i]][order(Data.all.files.OK[[i]]$Date,Data.all.files.OK[[i]]$Hour),]
    
    # Detectar duplicados de un dia con igual hora
    HOURDATE=strptime(paste(Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]]$Hour), "%Y-%m-%d %H:%M:%S")
    DUP=duplicated(HOURDATE)
    
    if(sum(DUP, na.rm=TRUE)>0){
      ProbDUP=TRUE
      print(paste0("Warning: There is a problem with duplicated hours in the same day in: ",nom.files[i]," but this problem was solved"))
      DUPcsv=HOURDATE[which(DUP==TRUE)]
      #write.csv(DUPcsv, paste0(Filesroutesdestino,nom.files[i], "_Duplicated.csv"))
      
      #Si hay horas duplicadas en un dia, se corrige el problema usando la mediana para los datos
      Data.all.files.OK[[i]]=aggregate(Data.all.files.OK[[i]],by=list(Data.all.files.OK[[i]]$Hour,Data.all.files.OK[[i]]$Date),median)[-c(1,2)]
    }

    # Calculo del intervalo de tiempo entre registros
    Data.all.files.OK[[i]]["DIFF"]=NA #Se crea una columna "DIFF" con NA's
    HOURDATE=strptime(paste(Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]]$Hour), "%Y-%m-%d %H:%M") 
    Decal=c(HOURDATE[1],HOURDATE[-length(HOURDATE)])
    Data.all.files.OK[[i]]$DIFF=as.numeric(difftime(HOURDATE,Decal,units="mins"))# Las diferencias son minutos
    
    # Al hacer la diferencia entre el tiempo t_i+1 menos t_i, sabre cuantos minutos pasaron para llegar al registro
    # i+1, pero no hay registro antes de i por lo que no habra intervalo de tiempo para dicho registro
    # En esta parte del codigo lo que se hace es asignar la mediana de tiempo observada en el dia.
    # Reemplazar intervalo del primer dato de cada dia
    NUMDATA=aggregate(Data.all.files.OK[[i]], list(Data.all.files.OK[[i]]$Date), length)#registros por dia
    diasEst=dim(NUMDATA)[1] ###Dias que hay realmente
    PREMJOUR=cumsum(c(1,(NUMDATA$DIFF[-nrow(NUMDATA)])))

    #Mediana de los tiempos entre registros por dia
    MEDIANES=aggregate(Data.all.files.OK[[i]]$DIFF~Data.all.files.OK[[i]]$Date,Data.all.files.OK[[i]],median)
    Data.all.files.OK[[i]]$DIFF[PREMJOUR]=MEDIANES[,2]
    #View(Data.all.files.OK[[i]])

###Para encontrar la cantidad de datos repetidos consecutivos en un dia
###Solo si los registros se hicieron cada 30 minutos o mas    
      #     if(median(Data.all.files.OK[[i]]$DIFF)>=30) {
      #     
      #       Data.all.files.OK2=Data.all.files.OK[[i]]
      #       #head(Data.all.files.OK2,74)
      #       var=substring(nom.files[i],nchar(nom.files[i])-3,nchar(nom.files[i]))
      #       if(var=="RAIN"){
      #         posiZero=(which(Data.all.files.OK2[,3]==0))#buscar valores menores iguales a 0
      #         Data.all.files.OK2[posiZero,3]=NA###reemplazar esos valores temporalmente por NA
      #       }
      #       
      #       sub=Data.all.files.OK2
      #       rez1=sub$Value[-1]
      #       rez1[length(sub$Value)]=sub$Value[length(sub$Value)]
      #       difer=sub$Value-rez1
      #       sub2=cbind(sub,difer)
      #       sub2=subset(sub2,sub2$difer==0)
      #       
      #           if(dim(sub2)[1]!=0){
      #         summValrep=aggregate(sub2,list(sub2$Date),length)[,c(1,4)]
      #         summValrep=subset(summValrep,summValrep$Value>=1)#En el caso de precipitacion esta condicion descarta la NO lluvia o ceros
      #         DateProm=dim(summValrep)[1]
      #         percent=round(DateProm/diasEst*100,2)
      #         
      #         if(DateProm>diasEst*0.2){
      #           write.csv(summValrep,paste0(Filesroutesdestino,"ValRep_",percent,"_",nom.files[i],".csv"))
      #         }
      #       }
      #     }
    
  }
  close(pb)#Cierra la barra de progreso
  # Tabla de valores de referencia
  REF=read.csv("Val_REF_QCHour.csv", header=T, row.names=1)
  
  # Detectar y remplazar los valores fuera de rango
  resul1=list()
  nom.Summary=0
  if(length(VAReasy)!=0){
    for (j in 1:length(VAReasy)){
      
      modif=0
      UNIT=NA
      ProbCohernciaTEMP=FALSE
      IND=VAReasy[j]
      VAR=substring(nom.files[IND],nchar(nom.files[IND])-3, nchar(nom.files[IND]))
      VAR=toupper(VAR)    
      
      Vmax=REF[,VAR][1];Vmin=REF[,VAR][2]
      Error=which(Data.all.files.OK[[IND]]$Value<Vmin|Data.all.files.OK[[IND]]$Value>Vmax)
      error=Data.all.files.OK[[IND]][Error,]    
      
      NUMBER=dim(error)[1]
      Data.all.files.OK[[IND]]$Value[Error]=NA
      
      # Control de coherencia temporal de la serie
      
      if(VAR=="ESOL"){
        UNIT=substring(nom.files[IND],nchar(nom.files[IND])-8, nchar(nom.files[IND])-5)
        UNIT=toupper(UNIT)
        if(UNIT=="WAM2"){
          Hmin=times(strftime(strptime("7:00 PM","%I:%M %p"),"%H:%M:00"))
          Hmax=times(strftime(strptime("4:30 AM","%I:%M %p" ),"%H:%M:00"))
          Noche=Data.all.files.OK[[IND]][(Data.all.files.OK[[IND]]$Hour>Hmin)|(Data.all.files.OK[[IND]]$Hour<Hmax),] 
          Noche2=na.omit(Noche)
#           maxi=function(x){
#             serie=na.omit(x)
#             max=max(serie)
#             return(max)
#           }
          # PROBLEM=aggregate(Noche$Value~Noche$Date, Noche, maxi)
          
#           if(dim(PROBLEM[PROBLEM[,2]>0,])[1]!=0){
#             ProbCohernciaTEMP=TRUE
#             print(paste0("Problem of time inconsistency in the serie: ",nom.files[[IND]]))}
          if(dim(Noche2)[1]!=0){
            ProbCohernciaTEMP=TRUE
            print(paste0("Problem of time inconsistency in the serie: !!",nom.files[[IND]]))}
        }else{
            print(paste0("Unit not covered,",nom.files[[IND]]))
        }
      }
      # Generacion del Reporte
      if(dim(error)[1]!=0){    
        write.csv(error, paste0(Filesroutesdestino,nom.files[IND],"_Datos_erroneos.csv"))
      }
      
      size=dim(Data.all.files[[IND]])[1]
      resul=c(size,NUMBER,round(NUMBER/size*100,2),as.character(ProbCohernciaTEMP), FALSE,as.character(ProbDUP))
      Tabla.fin=as.data.frame(resul,row.names=c("Datos","Erroneos","% Erroneos","Incoherencia temporal", "Problema UND","Duplicados"))
      resul1[[j]]=Tabla.fin
      nom.Summary[j]=nom.files[IND]
      #write.csv(Tabla.fin,paste0(Filesroutesdestino,nom.files[IND],"_Summary.csv"))
      write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,nom.files[IND],"_QC1ready.txt"), sep="\t",row.names = F)
    }
  }
  
  summary=(as.data.frame(do.call(cbind,resul1)));colnames(summary)=nom.Summary
  #
  resul2=list()
  nom.Summary2=0
  if(length(VARaccu)!=0){
    for (j in 1:length(VARaccu)){
      
      UNIT=NA
      ProbCohernciaTEMP=FALSE
      ProbUNIDAD=FALSE
      
      IND=VARaccu[j]
      VAR=substring(nom.files[IND],nchar(nom.files[IND])-3, nchar(nom.files[IND]))
      VAR=toupper(VAR)
      
      # Control RAIN
      if(VAR=="RAIN"){
        #  Funcion que define el umbral de RAIN segun el intervalo de tiempo
        UmbralRAIN=function(x){
          if(is.na(x)){
            y=NA
          }else if(x<=60){y=-0.0207*x^2+34.239*x+15.424
          
          }else{y=2000
          }
          return(y)
        }
        # Detectar valores fuera de rango
        error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>lapply(Data.all.files.OK[[IND]]$DIFF, UmbralRAIN))
        NUMerror=dim(error)[1]
        # remplazar valores fuera de rango
        Indexes=which(Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>lapply(Data.all.files.OK[[IND]]$DIFF, UmbralRAIN))
        Data.all.files.OK[[IND]]$Value[Indexes]=NA
        
        # Control de radiacion solar
      }else if(VAR=="ESOL"){
        #if(VAR=="ESOL"){
        UNIT=substring(nom.files[IND],nchar(nom.files[IND])-8, nchar(nom.files[IND])-5)
        UNIT=toupper(UNIT)
        
        # Control de coherencia temporal de la serie
        Hmin=times(strftime(strptime("7:00 PM","%I:%M %p"),"%H:%M:00"))
        Hmax=times(strftime(strptime("4:30 AM","%I:%M %p"),"%H:%M:00"))
        Noche=Data.all.files.OK[[IND]][(Data.all.files.OK[[IND]]$Hour>Hmin)|(Data.all.files.OK[[IND]]$Hour<Hmax),] 
        Noche2=na.omit(Noche)
#         maxi=function(x){
#           serie=na.omit(x)
#           max=max(serie)
#           return(max)
#         }
#         PROBLEM=aggregate(Noche$Value~Noche$Date, Noche, maxi)
#         if(dim(PROBLEM[PROBLEM[,2]>0,])[1]!=0){
#           ProbCohernciaTEMP=TRUE
#           print(paste0("Warning:  time inconsistency in :",nom.files[[IND]]))}
        if(dim(Noche2)[1]!=0){
          ProbCohernciaTEMP=TRUE
          print(paste0("Problem of time inconsistency in the serie: !!",nom.files[[IND]]))}
        
        # definicion de la funcion de umbral de ESOL
        UmbralESOL=function(x){
          y=-0.0007*x^2+1.9751*x+1.3536
          return(y)
        }
        # Detectar valores fuera de rango
        if(UNIT=="MJM2"){
          # conversion a ccm2
          Data.all.files.OK[[IND]]$Value=Data.all.files[[IND]]$Value*100/4.18
          # Deteccion
          error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
          NUMerror=dim(error)[1]
          # remplazar valores fuera de rango
          Indexes=which(Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
          Data.all.files.OK[[IND]]$Value[Indexes]=NA
          modif=1
          
        }else if(UNIT=="CCM2"){
          # Deteccion
          error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
          NUMerror=dim(error)[1]
          # remplazar valores fuera de rango
          Indexes=which(Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
          Data.all.files.OK[[IND]]$Value[Indexes]=NA
          
        }else{
          ProbUNIDAD=TRUE
          "Unidad no contemplada en el script"
        }
      }
      # Generacion del Reporte
      if(dim(error)[1]!=0){
        write.csv(error, paste0(Filesroutesdestino,nom.files[IND],"_Data_error.csv"))
      }
      
      size=dim(Data.all.files[[IND]])[1]
      resul=c(size,NUMerror,round(NUMerror/size*100,2),as.character(ProbCohernciaTEMP), as.character(ProbUNIDAD), as.character(ProbDUP))
      Tabla.fin=as.data.frame(resul,row.names=c("Data","Error","% Error","Incoherencia temporal", "Problema UND", "Duplicados"))
      resul2[[j]]=Tabla.fin
      nom.Summary2[j]=nom.files[IND]
      write.csv(Tabla.fin,paste0(Filesroutesdestino,nom.files[IND],"_Summary.csv"))
      
      if(modif==1){
        varCCM2=paste0(substring(nom.files[IND],first=1,nchar(nom.files[IND])-9),"CCM2_ESOL")
        write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,varCCM2,"_QC1ready.txt"), sep="\t",row.names = F)
      }else{
        write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,nom.files[IND],"_QC1ready.txt"), sep="\t",row.names = F)
      }
    }
  }
  summary2=(as.data.frame(do.call(cbind,resul2)));colnames(summary2)=nom.Summary2
  summary=t(cbind(summary,summary2))
  
  write.csv(summary,paste0(Filesroutesdestino,"TotalSummary.csv"))
  
  print("Quality control process finalized")
  print("Check 01_SERIES_HOURLY_QC_OK folder")
  ##################### FIN ################
}

#Convertir datos horarios en Diarios
CONVERT <- function(dirFol){
  print("Wait a moment please...")
  #Lectura de los archivos
  #ruta:Ubicacion de los archivos horarios ya controlodas, que se desean convertir a diarios
  ruta=paste0(dirFol,"/PROCESS/01_SERIES_HOURLY_QC_OK/")
  #Destino:ubicacion final de los archivos convertidos
  Destino=paste0(dirFol,"/PROCESS/02_SERIES_DAILY_to_QC/")
  files <- list.files(ruta, pattern="\\.txt$")
  nom.files<-substring(files,1,nchar(files)-13)
  Data.all.files <- lapply(paste0(ruta,files),function(x){read.table(x,header=T,sep="\t")})
  names(Data.all.files)=nom.files
  
  Data.all.filesNAFree=Data.all.files
  Data.all.files.OK=Data.all.files
  Serie.diaria=Data.all.files
  
  summary=list()
  ###Debo corregir el ciclo, cuando no se puede obtener la conversion de un día 
  ###porque no quedaron datos en el archivo horario falla el proceso
  for(i in 1:length(Data.all.files)){
    
    modif=0
    # Crear series de trabajo
    Data.all.filesNAFree[[i]]=Data.all.files[[i]][which(!is.na(Data.all.files[[i]]$Value)),]
    Data.all.files.OK[[i]]=Data.all.filesNAFree[[i]]
    
    # Lectura de fechas y horas
    Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y-%m-%d")
    Data.all.files.OK[[i]]$Hour=times(Data.all.filesNAFree[[i]]$Hour)
    
    # Lectura de la variable
    VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))
    VAR=toupper(VAR)
    
    # Calculo de las medianas de intervalo de tiempo
    medianas=aggregate(Data.all.files.OK[[i]]$DIFF~Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]],median)
    colnames(medianas)=c("Date", "Median")
    
    if(VAR=="TMAX"){
      # Definir fechas usables  
      Hmin=times(strftime(strptime("5:58 AM","%I:%M %p"),"%H:%M:00"))
      Hmax=times(strftime(strptime("6:02 PM","%I:%M %p"),"%H:%M:00"))
      minutos.dia=(hours(Hmax-Hmin))*60
      Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour>Hmin & Data.all.files.OK[[i]]$Hour<Hmax,]
      registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
      colnames(registros)=c("Date", "RegNumber")
      Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
      Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
      
      # Calcular dato diario
      FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
      SerieOP=Data.in.time[FechasOK.POS,]
      Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, max)
      
      # Indicadores del trabajo efectuado
      registros.totales=dim(Data.all.files[[i]])[1]
      NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
      registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
      registros.rescatados=dim(Serie.diaria[[i]])[1]
      
    }else if(VAR=="TMIN"){
      # Definir fechas usables  
      Hmin=times(strftime(strptime("6:02 AM","%I:%M %p"),"%H:%M:00"))
      Hmax=times(strftime(strptime("5:58 PM","%I:%M %p"),"%H:%M:00"))
      minutos.dia=(hours(Hmax-Hmin))*60
      Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour<Hmin | Data.all.files.OK[[i]]$Hour>Hmax,]
      registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
      colnames(registros)=c("Date", "RegNumber")
      Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
      Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
      
      # Calcular dato diario
      FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
      SerieOP=Data.in.time[FechasOK.POS,]
      Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, min)
      
      # Indicadores del trabajo efectuado
      registros.totales=dim(Data.all.files[[i]])[1]
      NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
      registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
      registros.rescatados=dim(Serie.diaria[[i]])[1]
      
    }else if(VAR=="RHUM"){
      # Definir fechas usables  
      minutos.dia=24*60
      Data.in.time=Data.all.files.OK[[i]]
      registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
      colnames(registros)=c("Date", "RegNumber")
      Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
      Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
      
      # Calcular dato diario
      FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
      SerieOP=Data.in.time[FechasOK.POS,]
      Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, mean)
      
      # Indicadores del trabajo efectuado
      registros.totales=dim(Data.all.files[[i]])[1]
      NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
      registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
      registros.rescatados=dim(Serie.diaria[[i]])[1]
      
    }else if(VAR=="RAIN"){
      # Definir fechas usables  
      minutos.dia=24*60
      Data.in.time=Data.all.files.OK[[i]]
      registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
      colnames(registros)=c("Date", "RegNumber")
      Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
      Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
      
      # Calcular dato diario
      FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
      SerieOP=Data.in.time[FechasOK.POS,]
      
      Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, sum)
      
      # Indicadores del trabajo efectuado
      registros.totales=dim(Data.all.files[[i]])[1]
      NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
      registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
      registros.rescatados=dim(Serie.diaria[[i]])[1]
      
    }else if(VAR=="ESOL"){
      
      Hmin=times(strftime(strptime("4:28 AM","%I:%M %p"),"%H:%M:00"))
      Hmax=times(strftime(strptime("7:01 PM","%I:%M %p"),"%H:%M:00"))
      minutos.dia=(hours(Hmax-Hmin))*60
      #subset de datos entre horas coherentes
      Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour>Hmin & Data.all.files.OK[[i]]$Hour<Hmax,]
      #Cuantos datos hay por dia
      registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
      colnames(registros)=c("Date", "RegNumber")
      Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
      Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
      
      ###colocando
      if(dim(Fechas.rescatOK)[1]< 1){
        
        print(paste0("The station ",nom.files[i]," has low or null information during the period of interest, therefore, it cannot be included into the following process"))
        next
      }
      
      # Calcular dato diario
      FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
      SerieOP=Data.in.time[FechasOK.POS,]
      
      UNIT=substring(nom.files[i],nchar(nom.files[i])-8, nchar(nom.files[i])-5)
      UNIT=toupper(UNIT)
      if(UNIT=="WAM2"){
        SerieOPccm2=SerieOP$Value*SerieOP$DIFF*60/4.18/10000
        SerieOP$Value=SerieOPccm2
        modif=1
      }
      Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, sum)
      
      # Indicadores del trabajo efectuado
      registros.totales=dim(Data.all.files[[i]])[1]
      NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
      registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
      registros.rescatados=dim(Serie.diaria[[i]])[1]
      
    }
    colnames(Serie.diaria[[i]])=c("Dates","Value")
    # Generacion del reporte
    resul=c(registros.totales, NAentrada, round(NAentrada/registros.totales*100,2), registros.totales.dias, registros.rescatados, round(registros.rescatados/registros.totales.dias*100,2))
    Tabla.fin=as.data.frame(resul,row.names=c("Datos", "NA's", "% NA's", "Dias con datos", "Dias rescatables", "% rescatables"))
    #write.csv(Tabla.fin,paste0(Destino,nom.files[i], "_ResumenTransfDia.csv"))
    summary[[i]]=Tabla.fin
    
    if(modif==1){
      varCCM2=paste0(substring(nom.files[i],first=1,nchar(nom.files[i])-9),"CCM2_ESOL")
      write.table(Serie.diaria[[i]],paste0(Destino,varCCM2, "_DailyReady.txt"), sep="\t",row.names=F)
    }else {write.table(Serie.diaria[[i]],paste0(Destino,nom.files[i], "_DailyReady.txt"), sep="\t",row.names=F)}
    
  }
  summary2=(as.data.frame(do.call(cbind,summary)))
  colnames(summary2)=nom.files
  summary2=t(summary2)
  
  write.csv(summary2,paste0(Destino,"ResumenTotal.csv"))
  
  print("Convert process finalized")
  print("Check 02_SERIES_DAILY_to_QC folder")
  
}

#Mezclar series provenientes de ambas fuentes (horaria y diaria)
MIX<-function(dirFol){
  
  Filesroutes=paste0(dirFol,"/PROCESS/02_SERIES_DAILY_to_QC/")
  FlsOriHr=list.files(Filesroutes, pattern="\\.txt$")#archivos diarios de origen horario
  All.FlsOriHr=lapply(paste0(Filesroutes,FlsOriHr),function(x){read.table(x,sep="\t",header=T,blank.lines.skip=TRUE)})
  nom1=substring(FlsOriHr,1,nchar(FlsOriHr)-15);names(All.FlsOriHr)=nom1
  
  FilesDlyOr=paste0(dirFol,"/SERIES_ORIGINAL/DAILY/")
  FlsOrDly=list.files(FilesDlyOr, pattern="\\.txt$")  #archivos diarios de origen Diario
  All.FlsOrDly=lapply(paste0(FilesDlyOr,FlsOrDly),function(x){read.table(x,sep="\t",header=T,blank.lines.skip=TRUE)})
  nom2=substring(FlsOrDly,1,nchar(FlsOrDly)-4)
  
  Esolpos=grep("ESOL",nom2)#Elementos de ESOL de origen diario
  modif=nom2[Esolpos]
  if(length(modif)>0){
#Aqui todas las series diarias de ESOL son llevadas a CCM2, para poder efectuar la mezcla  
      for(i in 1:length(modif)){
        UNIT=substring(modif[i],nchar(modif[i])-8,nchar(modif[i])-5); UNIT=toupper(UNIT)
        if(UNIT=="WAM2"){
          Value=All.FlsOrDly[[Esolpos[i]]]$Value*24*60*60/4.18/10000
          All.FlsOrDly[[Esolpos[i]]]$Value=Value
          
        }else if(UNIT=="MJM2"){
          Value=All.FlsOrDly[[Esolpos[i]]]$Value*100/4.18
          All.FlsOrDly[[Esolpos[i]]]$Value=Value
        }

        ###En caso de ESOL, la unidad se convierte de la unidad original hasta CCM2. Aqui se cambia el nombre del archivo final
        ID=substring(modif[i],1,nchar(modif[i])-10)
        nom=paste0(ID,"_CCM2_ESOL")
        if(UNIT=="MJM2"||UNIT=="WAM2"){
          modif[i]=nom
          nom2[Esolpos[i]]=nom
        }
      }
  }
  names(All.FlsOrDly)=nom2
  
  stations=c(nom1,nom2);UniqueNom=unique(stations)#Lista de referencia
  
  All.Files=c(All.FlsOriHr,All.FlsOrDly)
  rep=list()
  
  for(i in 1:length(UniqueNom)){
    
    rep[[i]]=grep(UniqueNom[i],stations)#Aqui identificon si hay repetidos, lo que implica que una 
    #misma serie tiene dos fuentes deistintas
    
    if(length(rep[[i]])>1){
      #Series a combinar
      OrHr=All.Files[[rep[[i]][1]]]#De origen horario; fechas organizadas
      OrDy=All.Files[[rep[[i]][2]]]#De origen diario; fechas sin organizar
      
      #Ajuste de formato Fecha
      OrHr$Dates=as.Date(as.character(OrHr$Dates))
      OrDy$Date=as.Date(as.character(OrDy$Date),"%Y%m%d")
      
      OrHr2=as.data.frame(OrHr);colnames(OrHr2)=c("Date","Value")
      OrDy2=as.data.frame(OrDy)
      
      union=rbind(OrHr2,OrDy2)
      
      OrderDate=order(union$Date);Comb=union[OrderDate,]
      
      mediana=function(x){
        x=na.omit(x);med=median(x)
      }
      
      Comb2=aggregate(Comb[,2],by=list(Comb$Date),mediana)#[2:3]
      colnames(Comb2)=c("Dates","Value")
      
      #Los archivos se guardaran en la misma carpeta de los datos horarios previamente convertidos a diarios
      #De esta manera, si habian series replicadas con informacion de fuentes diferentes, se conservara solo
      #un archivo, es decir el combinado
      write.table(Comb2,paste0(Filesroutes,UniqueNom[i],"_DailyReady.txt"),sep="\t",row.names=F,quote=F)
      print(paste0("The station ",UniqueNom[i]," has been combined"))
      
    }else if(length(rep[[i]])==1 & rep[[i]]>length(All.FlsOriHr)){
      #Si no hay duplicidad de fuente, se debe traer el archivo Diario original a la carpeta de QC diarios
      write.table(All.Files[[rep[[i]][1]]],paste0(Filesroutes,UniqueNom[i],".txt"),sep="\t",row.names=F,quote=F)
      print(paste0("The station ",UniqueNom[i]," was originally preserved"))
    }
    #Faltaria la condicion de que si solo existe archivo de origen horario pero no diario se guarde este archivo; 
    #pero no es necesario porque esos archivos ya se encuentrar en la carpeta deseada
  }
  print("The mix process has finished")
}

#QC daily
QCDAILY <- function(dirFol){
  
  print("Wait a moment please...")
  ###Lectura de los archivos

  if(length(list.files(paste0(dirFol,"/SERIES_ORIGINAL/HOURLY/")))!=0){ #condicion para verificar si hay archivos horarios
    ruta=paste0(dirFol,"/PROCESS/02_SERIES_DAILY_to_QC/") 
  }else{
    ruta=paste0(dirFol,"/SERIES_ORIGINAL/DAILY/") 
  }
  
  Destino=paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/")
  files <- list.files(ruta, pattern="\\.txt$")
  TypeOrig=(grep("DailyReady",files))#Aqui identifico si el archivo origen contiene info diaria desde 
  #la Fuente, o fue convertido desde info horaria a info diaria (DailyReady)
  #si TypeOrig es diferente de cero es porque hubo un proceso Horario previo
  
  #Asignacion de nombres de archivos segun origen
  if(length(TypeOrig)>=1){
    nom.files=rep("",length(files))
    nom.files[TypeOrig]<-substring(files[TypeOrig],1,nchar(files[TypeOrig])-15)
    nom.files[-TypeOrig]<-substring(files[-TypeOrig],1,nchar(files[-TypeOrig])-4)
  }else{
    nom.files=substring(files,1,nchar(files)-4)        
  }
  
  Data.all.files <- lapply(paste0(ruta,files),function(x){read.table(x,header=T,sep="\t")})
  names(Data.all.files)=nom.files
  
  Data.all.filesNAFree=Data.all.files
  Serie.diaria=Data.all.files
  
  # Tabla de valores de referencia
  REF=read.csv("Val_REF_QCDaily.csv", header=T, row.names=1)
  
  summary=list()
  filmin=paste0(YStart,"-01-01")
  filmax=paste0(YEnd,"-12-31")
  #si no hay datos que cumplan con los valores de REF falla el ciclo
  
  for(i in 1:length(Data.all.files)){
    
    ProbDUP=FALSE
    colnames(Data.all.files[[i]])=c("Date", "Value")
    # Quitar NA para trabajar solo con valores
    Data.all.filesNAFree[[i]]=Data.all.files[[i]][which(!is.na(Data.all.files[[i]]$Value)),]
    #summary(Data.all.filesNAFree[[i]])
    # Lectura de fechas
    if(length(grep("TRUE",i==TypeOrig))==1){
      DateOK=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y-%m-%d")  #Formato de fecha CON separadores
    }else{DateOK=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y%m%d")}   #Formato de fecha SIN separadores
    
    Data.all.filesNAFree[[i]]$Date=DateOK
    #filtro year de interes
    Data.all.files.OK=Data.all.filesNAFree[[i]]$Date>=as.Date(filmin) &Data.all.filesNAFree[[i]]$Date<=as.Date(filmax)
    Data.all.filesNAFree[[i]]=Data.all.filesNAFree[[i]][Data.all.files.OK,]
    
    #Filtro para descartar estaciones con muy pocos datos
    if(dim(Data.all.filesNAFree[[i]])[1]<30){   #30 es un valor arbitrario para rechazar una estacion con menos de ese numero de dias de info
      
      print(paste0("The station ",nom.files[i]," has low or null information during the period of interest, therefore, it cannot be included into the following process"))
      next
    }
    # Deteccion de duplicados con valores distintos
    DUP=duplicated(Data.all.filesNAFree[[i]]$Date)
    if(any(DUP)){
      POS=which(DUP==TRUE)
      setDUP=Data.all.filesNAFree[[i]][POS,]
      DUPserious=duplicated(setDUP)
      if(any(!DUPserious)){
        ProbDUP=TRUE
        print("Duplicated dates with different values")
        DUPcsv=Data.all.filesNAFree[[i]][which(DUPserious==TRUE),]
        write.csv(DUPcsv, paste0(Destino,"/",nom.files[i], "_Duplicated.csv"))
      }
    }
    
    # Detectar variable
    VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))
    VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))  
    VAR=toupper(VAR)
    
    # Convertir energia solar en CCM2
    if(VAR=="ESOL"){
      
      UNIT=substring(nom.files[i],nchar(nom.files[i])-8, nchar(nom.files[i])-5)
      UNIT=toupper(UNIT)
      if(UNIT=="WAM2"){
        SerieOPccm2=Data.all.filesNAFree[[i]]$Value*24*60*60/4.18/10000
        Data.all.filesNAFree[[i]]$Value=SerieOPccm2
        #Data.all.filesNAFree[[i]]$Value[which((Data.all.filesNAFree[[i]]$Value>190)==FALSE)]=NA
      }else if(UNIT=="MJM2"){
        SerieOPccm2=Data.all.filesNAFree[[i]]$Value*100/4.18
        Data.all.filesNAFree[[i]]$Value=SerieOPccm2
        #Data.all.filesNAFree[[i]]$Value[which((Data.all.filesNAFree[[i]]$Value>190)==FALSE)]=NA
        
      }#else if(UNIT=="CCM2"){
      #  SerieOPccm2=Data.all.filesNAFree[[i]]$Value>190
      #  Data.all.filesNAFree[[i]]$Value[which(SerieOPccm2==FALSE)]=NA
      #}
      
      ###En caso de ESOL, la unidad se convierte de la unidad original hasta CCM2. Aqui se cambia el nombre del archivo final
      ID=substring(nom.files[i],1,nchar(nom.files[i])-10)
      nom=paste0(ID,"_CCM2_",VAR)
      if(UNIT=="MJM2"||UNIT=="WAM2"){
        nom.files[i]=nom
      }
    }
    # Convertir brillo solar en CCM2
    if(VAR=="SBRI"){
      
      Latitudes=read.table(paste0(dirFol,"/Est_Lat.csv"),dec = ".",header = T,sep=",")
      
      days=as.Date(unlist(Data.all.filesNAFree[[i]][1]))
      sunshine=Data.all.filesNAFree[[i]][2]
      ID=substring(nom.files[i],1,nchar(nom.files[i])-5)
      lat=Latitudes[which(Latitudes$ID==ID),2]#Busca dentro del archivo, la latitud
      A=0.25;B=0.50 #constantes
      
      Rs=ap(days=days, lat=lat, extraT=NULL, A=A, B=B, SSD=sunshine)*(100/4.18)
      ###SIRAD convierte a MJ, por lo que el factor de correccion (100/4.18) en para llevarlo a CCM2
      
      Data.all.filesNAFree[[i]]$Value=Rs
      VAR="ESOL";UNIT="CCM2"
      nom=paste0(ID,"_CCM2_ESOL")
      nom.files[i]=nom
    }
    
    # Detecar valores fuera de rango
    Vmax=REF[,VAR][1];Vmin=REF[,VAR][2]
###########################################################%#$%$%^$%&%^&^&%^&%^&%&%^&%^&    
      
    Error=which(Data.all.filesNAFree[[i]]$Value<Vmin | Data.all.filesNAFree[[i]]$Value>Vmax)
    Bonnes1=Data.all.filesNAFree[[i]][Error,]#Para guardar
    if(dim(Bonnes1)[1]>0){
    Data.all.filesNAFree[[i]][Error,]$Value=NA
    }
    
    if(sum(!is.na(Data.all.filesNAFree[[i]]$Value))<=30){
      print(paste0("The station ",nom.files[i],", do not have data for the period of interest."))
      next
    }
    # Detectar valores fuera de 4 dev, sin tener en cuenta los descartados anteriormente
    if(VAR=="RAIN"){
    Dev=sd(na.omit(Data.all.filesNAFree[[i]]$Value[Data.all.filesNAFree[[i]]$Value>0]))
    PROMEDIO=mean(na.omit(Data.all.filesNAFree[[i]]$Value[Data.all.filesNAFree[[i]]$Value>0]))
    }else{
      Dev=sd(na.omit(unlist(Data.all.filesNAFree[[i]]$Value)))
      PROMEDIO=mean(na.omit(unlist(Data.all.filesNAFree[[i]]$Value)))
      
    }  
    
    Data.all.filesNAFree[[i]]$Out4Desv=NA
    
    #Colocando indicador de valor fuera de 4 desv 
    Data.all.filesNAFree[[i]]$Out4Desv[(Data.all.filesNAFree[[i]]$Value)>(PROMEDIO+4*Dev)|
                                       (Data.all.filesNAFree[[i]]$Value)<(PROMEDIO-4*Dev)]= c("Out4desv")
    
    # Deteccion rango de fechas
    Date.min=min(Data.all.filesNAFree[[i]]$Date);Date.max=max(Data.all.filesNAFree[[i]]$Date)
    # Construir serie resultante
    SEQ=as.data.frame(seq(Date.min,Date.max,1))
    colnames(SEQ)=c("Date")
    
    POSpot=match(SEQ$Date, Data.all.filesNAFree[[i]]$Date)#coincidencia entre fechas creadas y secuencia original procesada
    
    POSlimpio=POSpot[!is.na(POSpot)]#Quitando NA's del vector anterior
    Bonnes3=Data.all.filesNAFree[[i]][POSlimpio,]#datos que hay realmente
    POS=match(Bonnes3$Date,SEQ$Date)#Vector sin NA's indicando la posicion en la q hay datos. Equivalente a usar which
    POS2=as.vector(as.numeric(POS))
    
    SEQ$Value=NA
    SEQ$Out4Desv=NA
    SEQ$Value[POS2]=unlist(Bonnes3$Value)
    SEQ$Out4Desv[POS2]=unlist(Bonnes3$Out4Desv)
    Serie.diaria[[i]]=SEQ
    
    # Generar reporte
    size=dim(Serie.diaria[[i]])[1]
    Good=dim(Bonnes3)[1]
    Duplicados=length(DUP[DUP==TRUE])
    Error=dim(Bonnes1)[1]
    NAentrada=length(which(is.na(SEQ$Value)))  
    POSdecal=POS[-1]
    POSlimpio.small=POS[1:length(POSdecal)]
    MAYORsalto=max(POSdecal-POSlimpio.small)
    Fuera4DEV=sum(!is.na(Bonnes3$Out4Desv))
    
    resul=c(size, Duplicados, round((Duplicados/size)*100,2), Error, round((Error/size)*100,2), Fuera4DEV, NAentrada, round((NAentrada/size)*100,2), ProbDUP, MAYORsalto)
    Tabla.fin=as.data.frame(resul,row.names=c("Datos", "Duplicated", "% duplicated", "Errors", "% errors", "Values out 4 DEV","NA's", "% NA's", "Date duplicated Different values", "Greater Gap"))
    summary[[i]]=Tabla.fin
    #write.csv(Tabla.fin,paste0(Destino,nom.files[i], "_SummaryDaily.csv"))
    write.table(Serie.diaria[[i]],paste0(Destino,nom.files[i], "_ReadyFillGaps.txt"), sep="\t", row.names=FALSE)
    names(Serie.diaria[[i]])
    png(paste0(Destino,nom.files[i], "_Plotserie.png"), width=600, height=400, units="px")
    plot(Serie.diaria[[i]]$Value~Serie.diaria[[i]]$Date, ylab="Value", xlab="Date", type="l")
    dev.off()
    
#     png(paste0(Destino,nom.files[i], "_Boxplotserie.png"), width=600, height=400, units="px")
#     boxplot(Serie.diaria[[i]]$Value)
#     dev.off()
  }
  
  #Identificando elementos NULL de la lista
  #Se dan cuando la serie no tiene informacion en el periodo de interes
  SinDato=which(unlist(lapply(summary, is.null))==TRUE)
  
  if(length(SinDato)>0){
  #Aqui se extraen los NULL
  summary=Filter((Negate(is.null)), summary)
  
  summary2=(as.data.frame(do.call(cbind,summary)))
  #length(summary2)
  print(paste0("The station: ",nom.files[SinDato],", do not have data for the period of interest."))
  colnames(summary2)=nom.files[-SinDato]; summary2=t(summary2)
  
  #Si en este paso se descartan series de TMAX o TMIN, deberan ser descartadas tambien del
  #Procedimiento siguiente que implica una comparacion entre dichas variables, de lo contrario el proceso falla
  #Discard tendra entonces los ID  que se deben quitar
  
  #Discard=nom.files[SinDato][grep("TMAX|TMIN",nom.files[SinDato])]
  #Discard=unique(substring(Discard,1,nchar(Discard)-5))#Extraer solo el ID
  
  }else{
    summary2=(as.data.frame(do.call(cbind,summary)))
    colnames(summary2)=nom.files; summary2=t(summary2)
  }
  
  write.csv(summary2,paste0(Destino,"TotalSummary.csv"))
  
  
  ######################################################################################################
  ## QC para informacion a nivel diario, hallan o no pasado por proceso de conversion a escala diaria.##
  ## Estaciones con informacion desagregada o un archivo por variable                                 ##
  ## TMAX>TMIN                                                                                        ##
  ######################################################################################################
  rutOrigen=Destino;destino=paste0(rutOrigen)
  
  files <- list.files(rutOrigen, pattern="\\.txt$")
  nom.files<-substring(files,1,nchar(files)-4)#
  fil=files[grep("ReadyFillGaps",nom.files)]#
  files=fil[grep("TMAX|TMIN",fil)]#
  nom.files<-substring(files,1,nchar(files)-18)
  
  #La comparacion se debe realizar en parejas. Una misma estacion con ambas variables
  IDs=substring(nom.files,1,nchar(nom.files)-5)
  tab=table(IDs);SinPar=names(tab[tab==1])
  
  if(exists("Discard")){
    Discard=unique(c(SinPar,Discard))}else{Discard=unique(c(SinPar))}
  #Si hubo series descartadas en el paso anterior que involucraran a TMAX o TMIN
  #Se descartan de la comparacion
  if(exists("Discard")){
    
      if(length(Discard)==1){
      DiscarD=grep(Discard,files)
      files=files[-(DiscarD)]
      }else if(length(Discard)>1){
          grep2 <- function(pattern, x){grep(pattern, x)}
          grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')
          
          DiscarD=grep2(Discard,files)
          
          A2=Filter(function(x){length(x)>0},DiscarD)
          A3=((unlist(A2)))
          if(length(A3)!=0){
          files=files[-(A3)]}
        }
    }

  nom.files<-substring(files,1,nchar(files)-18)
  Datos <- lapply(paste(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
  names(Datos)=nom.files
  
  for(i in 1:length(Datos)){
    #si esta condicion se cumple quiere decir que la fecha original viene con separadores
    if(class(Datos[[i]][,1])=="factor"){
      Datos[[i]][,1]=as.Date(Datos[[i]][,1])#,format="%d/%m/%Y")
    }
  }
  
  #Cuales de las estaciones contienen info sobre Tmax y Tmin
  TempMax=Datos[grep("TMAX",nom.files)]
  TempMin=Datos[grep("TMIN",nom.files)]
  #ID's de las estaciones de cada variable
  IDtmax=substring(names(TempMax),1,nchar(names(TempMax))-5)
  IDtmin=substring(names(TempMin),1,nchar(names(TempMin))-5)
  vec=(IDtmax==IDtmin)
  vec1=length(vec[vec==FALSE])
  
  #Para hacer la comparacion correctamente de todas las estaciones debe haber igual cantidad
  #de estaciones, y que sean efectivamente las mismas estaciones entre Tmax y Tmin
  if(length(TempMax)==length(TempMin) &  vec1==0){
    print("The comparation between TMAX and TMIN is possible")
  }else{print("Error, the comparation between TMAX and TMIN is not possible")}
  
  cant.error=0
  percent=0
  for(i in 1:max(length(TempMax),length(TempMin))){

    #Posicion que ocupa la estacion de Tmax entre las estaciones de Tmin  
    pos=grep(IDtmax[i],names(TempMin))
    pos=pos[which.min(nchar(names(TempMin)[pos]))]###Esta condicion surge porque al tener dos estaciones 
    #con el mismo nombre, pero diferente fuente, se estaba generando un error (Estacion IDEAM y NP)
    #Fecha minima y maxima entre las dos bases de datos
    
    Datemin=min(min(TempMax[[i]][,1]),min(TempMin[[pos]][,1]))
    Datemax=max(max(TempMax[[i]][,1]),max(TempMin[[pos]][,1]))
    Dates=seq(Datemin,Datemax,1)
    
    newTmax=0
    for(j in 1:length(Dates)) {
      datasub=TempMax[[i]][TempMax[[i]][,1]==Dates[j],]
      if(dim(datasub)[1]!=0){    
        newTmax[j]=TempMax[[i]][TempMax[[i]][,1]==Dates[j],2]
      }else{
        newTmax[j]=NA
      }
    }
    
    newTmin=0
    for(j in 1:length(Dates)) {
      data_sub=TempMin[[pos]][TempMin[[pos]][,1]==Dates[j],]
      if(dim(data_sub)[1]!=0){    
        newTmin[j]=TempMin[[i]][TempMin[[pos]][,1]==Dates[j], 2]
      }else{
        newTmin[j]=NA
      }
    }
    
    Station.Unit=data.frame(Dates,newTmax,newTmin)
    colnames(Station.Unit)=c("Date","TMAX","TMIN")
    #write.csv(Station.Unit,paste0(destino,IDtmax[i],"_JoinTxTn.csv"))
    Station.Unit.Error=which(Station.Unit$newTmax<=Station.Unit$newTmin)
    originalTMAX=paste0(substring(nom.files[i],1,nchar(nom.files[i])-4),"TMAX")
    originalTMIN=paste0(substring(nom.files[i],1,nchar(nom.files[i])-4),"TMIN")
    
    #Reemplazando los valores de las inconsistencias en las fechas de las estaciones involucradas
    if(length(Station.Unit.Error)>0){
      
      Datos[[originalTMAX]][Station.Unit.Error,2]=NA
      Datos[[originalTMIN]][Station.Unit.Error,2]=NA
      ##Reescribiendo con las correcciones
      write.table(Datos[[originalTMAX]],paste0(destino,originalTMAX,".txt"),sep="\t",row.names=F)
      write.table(Datos[[originalTMIN]],paste0(destino,originalTMIN,".txt"),sep="\t",row.names=F)
    }
    cant.error[i]=length(Station.Unit.Error)
    percent[i]=round((cant.error[i]/dim(Station.Unit)[1])*100,3)
  }
  cant.err=paste(percent,"%");cant.errorf=data.frame(IDtmax,cant.error,cant.err)
  colnames(cant.errorf)=c("ID","ERROR","% ERROR")
  write.csv(cant.errorf,paste0(destino,"Coherence,TMAX_TMIN.csv"),row.names=F)
  print("Quality Control Finalized")
  print("Check 03_SERIES_DAILY_With_Holes folder")
}

#Inputs for generate data
## Rmawgen necesita que los archivos de entrada tengan un formato especial; con este script se logra.
## Adicionalmente se usa para organizar la informacion y graficar las series despues del QC y antes del relleno
INPUTS <- function(dirFol){
  print("Wait a moment please...")
  
  rutOrigen=paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/")
  #rutOrigen=paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/")
  
  files <-list.files(rutOrigen,pattern="\\.txt$")
  #files <-list.files(rutOrigen,pattern="\\_FUS.txt$")
  nom.files<-substring(files,1,nchar(files)-18)
  #nom.files<-substring(files,1,nchar(files)-8)
  Datos <- lapply(paste0(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
  names(Datos)=nom.files
  
  for(i in 1:length(Datos)){
    #si esta condicion se cumple quiere decir que la fecha original viene con separadores
    if(class(Datos[[i]][,1])=="factor"){Datos[[i]][,1]=as.Date(Datos[[i]][,1])} #para reconocer las fechas
    Datos[[i]][,2]=as.numeric(Datos[[i]][,2])                                   #Para garantizar que existan datos numericos
  }
  #Cuales de las estaciones contienen info sobre Tmax, Tmin y Rain
  TempMax =Datos[grep("TMAX",nom.files)]
  TempMin =Datos[grep("TMIN",nom.files)]
  Rain    =Datos[grep("RAIN",nom.files)]
  ESOL    =Datos[grep("ESOL",nom.files)]
  RHUM    =Datos[grep("RHUM",nom.files)]
  
  #ID's de las estaciones involucradas en cada variable
  IDtmax=substring(names(TempMax),1,nchar(names(TempMax))-5)
  IDtmin=substring(names(TempMin),1,nchar(names(TempMin))-5)
  IDrain=substring(names(Rain),1,nchar(names(Rain))-5)
  IDesol=substring(names(ESOL),1,nchar(names(ESOL))-10)
  IDrhum=substring(names(RHUM),1,nchar(names(RHUM))-5)
  
  date.minTmax=as.Date("1980-01-01");date.maxTmax=as.Date("1980-01-01")
  for(i in 1:length(TempMax)){
    date.minTmax[i]=as.Date(min(TempMax[[i]][,1]))
    date.maxTmax[i]=as.Date(max(TempMax[[i]][,1]))
  }
  date.minTmax=as.Date(paste0(substring((min(date.minTmax)),1,4),"-01-01"))
  date.maxTmax=as.Date(paste0(substring((max(date.maxTmax)),1,4),"-12-31"))
  
  DatesTmax=seq(date.minTmax,date.maxTmax,1);tmax_to=matrix(NA,nrow=length(DatesTmax),ncol=length(TempMax),byrow=F)
  
  for(j in 1:length(TempMax)){
    for(i in 1:length(DatesTmax)) {
      data_sub=TempMax[[j]][TempMax[[j]][,1]==DatesTmax[i],]      
      if(dim(data_sub)[1]!=0){    
        tmax_to[i,j]=TempMax[[j]][TempMax[[j]][,1]==DatesTmax[i],2] 
      }else{
        tmax_to[i,j]=NA
      }
    }
  }
  year=substring(DatesTmax,1,4);month=substring(DatesTmax,6,7);day=substring(DatesTmax,9,10)
  
  tmaxtofin=data.frame(month,day,year,tmax_to);colnames(tmaxtofin)=c("month","day","year",IDtmax)
  write.csv(tmaxtofin,paste0(rutOrigen,"/","TMAX_to.csv"),row.names=F)
  
  ####Tmin
  date.minTmin=as.Date("1980-01-01");date.maxTmin=as.Date("1980-01-01")
  for(i in 1:length(TempMin)){
    date.minTmin[i]=as.Date(min(TempMin[[i]][,1]))
    date.maxTmin[i]=as.Date(max(TempMin[[i]][,1]))
  }
  date.minTmin=as.Date(paste0(substring(min(date.minTmin),1,4),"-01-01"))
  date.maxTmin=as.Date(paste0(substring(max(date.maxTmin),1,4),"-12-31"))
  
  DatesTmin=seq(date.minTmin,date.maxTmin,1);tmin_to=matrix(NA,nrow=length(DatesTmin),ncol=length(TempMin),byrow=F)
  
  for(j in 1:length(TempMin)){
    for(i in 1:length(DatesTmin)) {
      data_sub=TempMin[[j]][TempMin[[j]][,1]==DatesTmin[i],]      
      if(dim(data_sub)[1]!=0){    
        tmin_to[i,j]=TempMin[[j]][TempMin[[j]][,1]==DatesTmin[i],2] 
      }else{
        tmin_to[i,j]=NA
      }
    }
  }
  year=substring(DatesTmin,1,4);month=substring(DatesTmin,6,7);day=substring(DatesTmin,9,10)
  
  tmintofin=data.frame(month,day,year,tmin_to);colnames(tmintofin)=c("month","day","year",IDtmin)
  write.csv(tmintofin,paste0(rutOrigen,"/","TMIN_to.csv"),row.names=F)
  
  #Rain
  date.minRain=as.Date("1980-01-01");date.maxRain=as.Date("1980-01-01")
  for(i in 1:length(Rain)){
    date.minRain[i]=as.Date(min(Rain[[i]][,1]))
    date.maxRain[i]=as.Date(max(Rain[[i]][,1]))
  }
  date.minRain=as.Date(paste0(substring(min(date.minRain),1,4),"-01-01"))
  date.maxRain=as.Date(paste0(substring(max(date.maxRain),1,4),"-12-31"))
  
  DatesRain=seq(date.minRain,date.maxRain,1);prec_to=matrix(NA,nrow=length(DatesRain),ncol=length(Rain),byrow=F)
  
  for(j in 1:length(Rain)){
    for(i in 1:length(DatesRain)) {
      data_sub=Rain[[j]][Rain[[j]][,1]==DatesRain[i],]      
      if(dim(data_sub)[1]!=0){    
        prec_to[i,j]=Rain[[j]][Rain[[j]][,1]==DatesRain[i],2] 
      }else{
        prec_to[i,j]=NA
      }
    }
  }
  year=substring(DatesRain,1,4);month=substring(DatesRain,6,7);day=substring(DatesRain,9,10)
  
  prectofin=data.frame(month,day,year,prec_to);colnames(prectofin)=c("month","day","year",IDrain)
  write.csv(prectofin,paste0(rutOrigen,"/","RAIN_to.csv"),row.names=F)
  
  #ESOL
  date.minESOL=as.Date("1980-01-01");date.maxESOL=as.Date("1980-01-01")
  for(i in 1:length(ESOL)){
    date.minESOL[i]=as.Date(min(ESOL[[i]][,1]))
    date.maxESOL[i]=as.Date(max(ESOL[[i]][,1]))
  }
  date.minESOL=as.Date(paste0(substring(min(date.minESOL),1,4),"-01-01"))
  date.maxESOL=as.Date(paste0(substring(max(date.maxESOL),1,4),"-12-31"))
  
  DatesESOL=seq(date.minESOL,date.maxESOL,1);ESOL_to=matrix(NA,nrow=length(DatesESOL),ncol=length(ESOL),byrow=F)
  
  for(j in 1:length(ESOL)){
    for(i in 1:length(DatesESOL)) {
      data_sub=ESOL[[j]][ESOL[[j]][,1]==DatesESOL[i],]      
      if(dim(data_sub)[1]!=0){    
        ESOL_to[i,j]=ESOL[[j]][ESOL[[j]][,1]==DatesESOL[i],2] 
      }else{
        ESOL_to[i,j]=NA
      }
    }
  }
  year=substring(DatesESOL,1,4);month=substring(DatesESOL,6,7);day=substring(DatesESOL,9,10)
  
  ESOLtofin=data.frame(month,day,year,ESOL_to);colnames(ESOLtofin)=c("month","day","year",IDesol)
  write.csv(ESOLtofin,paste0(rutOrigen,"/","ESOL_to.csv"),row.names=F)
  
  ##RHUM
  date.minRHUM=as.Date("1980-01-01");date.maxRHUM=as.Date("1980-01-01")
  for(i in 1:length(RHUM)){
    date.minRHUM[i]=as.Date(min(RHUM[[i]][,1]))
    date.maxRHUM[i]=as.Date(max(RHUM[[i]][,1]))
  }
  date.minRHUM=as.Date(paste0(substring(min(date.minRHUM),1,4),"-01-01"))
  date.maxRHUM=as.Date(paste0(substring(max(date.maxRHUM),1,4),"-12-31"))
  
  DatesRHUM=seq(date.minRHUM,date.maxRHUM,1);RHUM_to=matrix(NA,nrow=length(DatesRHUM),ncol=length(RHUM),byrow=F)
  
  for(j in 1:length(RHUM)){
    for(i in 1:length(DatesRHUM)) {
      data_sub=RHUM[[j]][RHUM[[j]][,1]==DatesRHUM[i],]      
      if(dim(data_sub)[1]!=0){    
        RHUM_to[i,j]=RHUM[[j]][RHUM[[j]][,1]==DatesRHUM[i],2] 
      }else{
        RHUM_to[i,j]=NA
      }
    }
  }
  year=substring(DatesRHUM,1,4)
  month=substring(DatesRHUM,6,7)
  day=substring(DatesRHUM,9,10)
  
  RHUMtofin=data.frame(month,day,year,RHUM_to);colnames(RHUMtofin)=c("month","day","year",IDrhum)
  write.csv(RHUMtofin,paste0(rutOrigen,"/","RHUM_to.csv"),row.names=F)
  
  print("Input files finalized")
  print("Check 03_SERIES_DAILY_With_Holes folder")
}

#DESCRIPTIVOS
descript <- function(object){ #Funcion para descriptivas de las variables
  n=length(object)
  Media=round(mean(object, na.rm=T),3);  Varianza=round(var(object, na.rm=T),3);  Desv.Est=round(sqrt(Varianza),3)
  Mediana=round(as.numeric(quantile(object,probs=0.5, na.rm=T)),3);  Coef.Var=round((sqrt(Varianza)/Media)*100,3)
  Min=round(min(object,na.rm=T),2); Max=round(max(object,na.rm=T),2)
  Datos.NA=round(sum(is.na(object)),2)
  Porcentaje.NA=round((Datos.NA/length(object))*100,3)
  result=cbind(n,Min,Max,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA,Porcentaje.NA)
}
SUMMARY<-function(dirFol,objeto,YStart,YEnd){
  
  dir.create(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/SUMMARY"),showWarnings=F)
  
  archivo=read.csv(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/",objeto,"_to.csv"),header=T)
  archivo=filter(archivo,year %in% c(YStart:YEnd))
  
  d=sapply(archivo[-1:-3],descript)
  row.names(d) <-c("n","Min","Max","Media","Varianza","Desv.Est","Mediana","CV %","NA","NA %")
  d=as.table(d)
  write.csv(d,paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/SUMMARY/",objeto,"_",YStart,"-",YEnd,"_Summary.csv"))
  return(d)
  
}

#################################################Imputacion de tmax tmin y rain
GENERATOR_T_R<-function(dirFol,YStart,YEnd,DontUse=NULL){
  
  print("Wait a moment please...")
  
  rutDestino2=paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/")
  
  year_min <- YStart;year_max <- YEnd
  origin <- paste0(year_min,"-1-1")
  valmin <- 1.0;n_GPCA_iter <- 10
  n_GPCA_iteration_residuals <- 10
  n_GPCA_iter_prec <- 20
  n_GPCA_iteration_residuals_prec <- 20
  
  tmax  <- read.csv(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/TMAX_to.csv"), header = TRUE, sep = ",",  dec=".")
  tmin  <- read.csv(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/TMIN_to.csv"), header = TRUE, sep = ",",  dec=".")
  preci <- read.csv(paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/RAIN_to.csv"), header = TRUE, sep = ",",  dec=".")
  
  tmax_1=subset(tmax,tmax$year>=year_min & tmax$year<=year_max)
  tmin_1=subset(tmin,tmin$year>=year_min & tmin$year<=year_max)
  precipitation=subset(preci,preci$year>=year_min & preci$year<=year_max)
  
  station <- names(tmax_1)[-(1:3)]
  
  #Verificar si las estaciones en los tres archivos son las mismas
  arch1<- names(tmax_1)[-(1:3)];arch2<- names(tmin_1)[-(1:3)];arch3<- names(precipitation)[-(1:3)]
  
  compare=unique(c(unique(arch1==arch2),unique(arch2==arch3),unique(arch1==arch3)))
  if(compare==TRUE){
    print("Start process of filling gaps in Temperature")
  }else {stop("There are different stations inside of files TMAX_to, TMIN_to and RAIN_to. 
              Please check the 03_SERIES_DAILY_With_Holes folder")}
  
  #Eleccion de las estaciones con las que se trabajara  
  if(length(DontUse)>=1){
    stationUSE=station[c(1:length(station))]
    stationUSE=stationUSE[-DontUse]}else{
      stationUSE=station[c(1:length(station))]}
  
  print("Stations to use")
  print(stationUSE)
  
  # generation of temperature max and min 
  generation00_temp <- ComprehensiveTemperatureGenerator(station=stationUSE,
                                                         Tx_all=tmax_1,
                                                         Tn_all=tmin_1,
                                                         year_min=year_min,
                                                         year_max=year_max,
                                                         p=2,n_GPCA_iteration=n_GPCA_iter,
                                                         n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                         sample="monthly")
  
  #Use of measured and observed temperature as exogenous variables
  print("##########################################################################")
  exogen_sim <- cbind(generation00_temp$output$Tx_gen,generation00_temp$output$Tn_gen)
  names(exogen_sim) <- cbind(paste(names(generation00_temp$output$Tx_gen),"_Tx",sep=""),paste(names(generation00_temp$output$Tn_gen),"_Tn",sep=""))
  exogen <- cbind(generation00_temp$input$Tx_mes,generation00_temp$input$Tn_mes)
  names(exogen) <- cbind(paste(names(generation00_temp$input$Tx_mes),"_Tx",sep=""),paste(names(generation00_temp$input$Tn_mes),"_Tn",sep=""))
  
  
  print("Start process of filling gaps in Precipitation") 
  # Precipitation Generator (temperture enters as exogenous variable)
  #fix(ComprehensivePrecipitationGenerator)
  generation00_prec <- ComprehensivePrecipitationGenerator(station=stationUSE,
                                                           prec_all=precipitation,
                                                           year_min=year_min,
                                                           year_max=year_max,
                                                           exogen=exogen,
                                                           exogen_sim=exogen_sim,
                                                           p=2,n_GPCA_iteration=n_GPCA_iter_prec,
                                                           n_GPCA_iteration_residuals=n_GPCA_iteration_residuals_prec,
                                                           sample="monthly",valmin=1,extremes=TRUE,no_spline = T)
  
  #-----------------------Gerar Arquivos--------------------------------------------- 
  prec_mes <- generation00_prec$prec_mes
  prec_gen <- generation00_prec$prec_gen
  
  tmin_mes <-generation00_temp$input$Tn_mes
  tmin_gen <-generation00_temp$out$Tn_gen
  
  tmax_mes <-generation00_temp$input$Tx_mes
  tmax_gen <-generation00_temp$out$Tx_gen
  
  test = names(prec_gen)
  teste_min <- names(tmin_gen)
  teste_max <- names(tmax_gen)
  
  data_genPrec <- extractmonths(data=generation00_prec$prec_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  data_genTmin <- extractmonths(data=generation00_temp$out$Tn_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  data_genTmax <- extractmonths(data=generation00_temp$out$Tx_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  summary(data_genPrec$X24035010)
  #------------------------------Salvando Arquivo------------------------------------
  archPrec=list(0);archTmax=list(0)
  archTmin=list(0);archPrec2=list(0)
  archTmax2=list(0);archTmin2=list(0)
  precipitation_Comp=precipitation[stationUSE]
  tmax_Comp=tmax_1[stationUSE]
  tmin_Comp=tmin_1[stationUSE]
  
  for(i in 1:length(stationUSE)){
    
    Modif=rep("NO",dim(tmax_1)[1])
    Modif1=rep("NO",dim(tmax_1)[1])
    Modif2=rep("NO",dim(tmax_1)[1])
    
    posPrec=which(is.na(precipitation[stationUSE[i]]))
    precipitation_Comp[[i]][posPrec]=data_genPrec[[i]][posPrec]#Fusion
    Modif[posPrec]="SI"
    archPrec[[i]]=cbind(precipitation[,1:3],precipitation_Comp[[i]],Modif)#para llevar las fuciones con indicador a varios txt
    
    postmax=which(is.na(tmax_1[stationUSE[i]]))
    tmax_Comp[[i]][postmax]=data_genTmax[[i]][postmax]
    Modif1[postmax]="SI"
    archTmax[[i]]=cbind(tmax_1[,1:3],tmax_Comp[[i]],Modif1)
    
    postmin=which(is.na(tmin_1[stationUSE[i]]))
    tmin_Comp[[i]][postmin]=data_genTmin[[i]][postmin]
    Modif2[postmin]="SI"
    archTmin[[i]]=cbind(tmin_1[,1:3],tmin_Comp[[i]],Modif2)
    
    datPrec=as.Date(paste0(archPrec[[i]][,1],"/", archPrec[[i]][,2],"/",archPrec[[i]][,3]),format="%m/%d/%Y")
    archPrec2[[i]] =data.frame(datPrec,archPrec[[i]][4:5]); colnames(archPrec2[[i]])=c("Dates","Value","Modif")
    data_genPrec[[i]]=data.frame(datPrec,data_genPrec[[i]]); colnames(data_genPrec[[i]])=c("Dates","Value")
    
    datTmax=as.Date(paste0(archTmax[[i]][,1],"/", archTmax[[i]][,2],"/",archTmax[[i]][,3]),format="%m/%d/%Y")
    archTmax2[[i]] =data.frame(datTmax,archTmax[[i]][4:5]); colnames(archTmax2[[i]])=c("Dates","Value","Modif")
    data_genTmax[[i]]=data.frame(datTmax,data_genTmax[[i]]); colnames(data_genTmax[[i]])=c("Dates","Value")
    
    datTmin=as.Date(paste0(archTmin[[i]][,1],"/", archTmin[[i]][,2],"/",archTmin[[i]][,3]),format="%m/%d/%Y")
    archTmin2[[i]] =data.frame(datTmin,archTmin[[i]][4:5]); colnames(archTmin2[[i]])=c("Dates","Value","Modif")
    data_genTmin[[i]]=data.frame(datTmin,data_genTmin[[i]]); colnames(data_genTmin[[i]])=c("Dates","Value")
    
    if(substring((test[i]),1,1)=="X"){
      test[i]=substring((test[i]),2,nchar(test[i]))
    }
    write.table(archPrec2[[i]],paste0(rutDestino2,test[i],"_RAIN_FUS.txt"), sep = "\t", row.names = F,col.names = TRUE)
    write.table(archTmax2[[i]],paste0(rutDestino2,test[i],"_TMAX_FUS.txt"), sep = "\t", row.names = F,col.names = TRUE)
    write.table(archTmin2[[i]],paste0(rutDestino2,test[i],"_TMIN_FUS.txt"), sep = "\t", row.names = F,col.names = TRUE)
    
  }
  print("Please check 04_SERIES_DAILY_OK folder")
}

##Generator RHUM
GEN_RHUM<-function(dirFol){
  
  #ruta para extraer archivos *ReadyFillGaps de RHUM y ESOL. Estos ya fueron generados en un paso anterior
  rutExtract<-paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/")
  files=list.files(rutExtract,pattern="_ReadyFillGaps.txt",recursive=T,full.names=T)
  filesCopy=grep("RHUM|ESOL",files)
  
  pat     <- paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/")
  
  file.copy(files[filesCopy],pat)
  
  
  
  all.files <- list.files(pat,pattern="txt",full.names = F)
  nom.files<-substring(all.files,1,nchar(all.files)-4)
  Datos<-lapply(paste(pat,all.files,sep=""),function(x){read.table(x,header=T,sep="\t")})
  names(Datos)=nom.files
  
  for(i in 1:length(Datos)){
    #si esta condicion se cumple quiere decir que la fecha original viene con separadores
    if(class(Datos[[i]][,1])=="factor"){
      Datos[[i]][,1]=as.Date(Datos[[i]][,1])#,format="%d/%m/%Y")
    }
  }
  
  all.files.FUS       <-  Datos[grep("FUS",nom.files)]
  all.files.USABRHUM  <-  Datos[grep("RHUM_ReadyFillGaps",nom.files)]
  all.files.USABESOL  <-  Datos[grep("ESOL_ReadyFillGaps",nom.files)]
  
  #ID's de las estaciones involucradas en cada tipo de archivo
  IDFUS=substring(names(all.files.FUS),1,nchar(names(all.files.FUS))-9)
  IDFUS=unique(IDFUS)
  IDUSABRHUM=substring(names(all.files.USABRHUM),1,nchar(names(all.files.USABRHUM))-19)
  IDUSABESOL=substring(names(all.files.USABESOL),1,nchar(names(all.files.USABESOL))-24)
  
  length(IDFUS);length(IDUSABRHUM);length(IDUSABESOL)
  
  ###crear archivos JOINT
  RAINFUS=all.files.FUS[grep("RAIN",names(all.files.FUS))]
  TMAXFUS=all.files.FUS[grep("TMAX",names(all.files.FUS))]
  TMINFUS=all.files.FUS[grep("TMIN",names(all.files.FUS))]
  
  IDRAIN=substring(names(RAINFUS),1,nchar(names(RAINFUS))-9)
  IDTMAX=substring(names(TMAXFUS),1,nchar(names(TMAXFUS))-9)
  IDTMIN=substring(names(TMINFUS),1,nchar(names(TMINFUS))-9)
  
  date.min=min(as.Date(as.character(unlist(RAINFUS[[1]][,1]))))
  date.max=max(as.Date(as.character(unlist(RAINFUS[[1]][,1]))))
  
  Dates=seq(date.min,date.max,1)
  cant.Reg=dim(RAINFUS[[1]])[1]
  cant.Est=length(RAINFUS)
  
  JOINT=list(0)
  Estim=list(0)
  Rhum_ORIG=list(0)
  Esol_ORIG=list(0)
  
  #Para evitar que las estimaciones de RHUM se "salgan" de control
  REF=read.csv(paste0(dirFol,"/Val_REF_QCDaily.csv"), header=T, row.names=1)
  rhmax=REF$RHUM[1];rhmin=REF$RHUM[2]
  
  for(i in 1:length(IDRAIN)){
    
    #Datos originales para la posterior fusion
    names(all.files.USABRHUM)
    Rhum_ORIG=all.files.USABRHUM[grep(IDRAIN[i],IDUSABRHUM)]
    Esol_ORIG=all.files.USABESOL[grep(IDRAIN[i],IDUSABESOL)]
    
    newRHUMOrig=0
    for(j in 1:length(Dates)){
      if(length(Rhum_ORIG)!=0){  
        hayDato=Rhum_ORIG[[1]][Rhum_ORIG[[1]][,1]==Dates[j],]
        if(dim(hayDato)[1]!=0){    
          newRHUMOrig[j]=Rhum_ORIG[[1]][Rhum_ORIG[[1]][,1]==Dates[j],2]
        }else{
          newRHUMOrig[j]=NA
        }
      }
    }
    
    if(length(Rhum_ORIG)!=0){  
      Estim[[i]] =data.frame(Dates,RAINFUS[[i]][2],TMAXFUS[[i]][2],TMINFUS[[i]][2],newRHUMOrig)
      x=as.numeric(unlist(TMAXFUS[[i]][2]))
      y=as.numeric(unlist(TMINFUS[[i]][2]))
      
      mod     <- randomForest(newRHUMOrig ~ x+y+x*y, ntree=800,importance=TRUE,localImp=TRUE,na.action=na.omit) #*
      RHUM_EST<-predict(mod,Estim[[i]][-c(1,2,5)])
      RHUM_EST[RHUM_EST>=rhmax]=NA; RHUM_EST[RHUM_EST<=rhmin]=NA
    }
    
    newESOLOrig=0;RHUM_FUS=0;Modif=0
    
    for(l in 1:length(Dates)){
      if(length(Rhum_ORIG)!=0){
        if(is.na(newRHUMOrig[l])){
          RHUM_FUS[l]= RHUM_EST[l]
          Modif[l]="SI"
        }else{
          RHUM_FUS[l]=newRHUMOrig[l]
          Modif[l]="NO"
        }
      }
      
      if(length(Esol_ORIG)!=0){
        hayDato2=Esol_ORIG[[1]][Esol_ORIG[[1]][,1]==Dates[l],]
        if(dim(hayDato2)[1]!=0){
          newESOLOrig[l]=Esol_ORIG[[1]][Esol_ORIG[[1]][,1]==Dates[l],2]
        }else{
          newESOLOrig[l]=NA
        }
      }
    }
    
    if(length(Esol_ORIG)!=0 & length(Rhum_ORIG)!=0){
      JOINT[[i]]=data.frame(Dates,RAINFUS[[i]][2],TMAXFUS[[i]][2],TMINFUS[[i]][2],newRHUMOrig,RHUM_EST,RHUM_FUS,newESOLOrig); colnames(JOINT[[i]])=c("Date","Rain","TMAX","TMIN","RHUM_ORIG","RHUM_EST","RHUM_FUS","ESOL_ORIG")
    }else if(length(Esol_ORIG)==0 &length(Rhum_ORIG)!=0){
      JOINT[[i]]=data.frame(Dates,RAINFUS[[i]][2],TMAXFUS[[i]][2],TMINFUS[[i]][2],newRHUMOrig,RHUM_EST,RHUM_FUS); colnames(JOINT[[i]])=c("Date","Rain","TMAX","TMIN","RHUM_ORIG","RHUM_EST","RHUM_FUS")
    }else{
      JOINT[[i]]=data.frame(Dates,RAINFUS[[i]][2],TMAXFUS[[i]][2],TMINFUS[[i]][2]); colnames(JOINT[[i]])=c("Date","Rain","TMAX","TMIN")
    }
    
    if(length(Rhum_ORIG)!=0){
      ###Necesito guardar los archivos estimados y los fucionados (De rhum en especial)
      RHUM_ESTwithDate=data.frame(Dates,RHUM_EST)
      RHUMFUS=data.frame(Dates,RHUM_FUS,Modif); colnames(RHUMFUS)=c("Dates","Value","Modif")
      
      #write.table(RHUM_ESTwithDate,paste0(pat,IDRAIN[i],"_RHUM_EST.txt"),,sep="\t",row.names=F)
      write.table(RHUMFUS,paste0(pat,IDRAIN[i],"_RHUM_FUS.txt"),sep="\t",row.names=F)
    }
    
    JOINT[[i]][is.na(JOINT[[i]])]<-""
    write.table(JOINT[[i]],paste0(pat,IDRAIN[i],"_JOINT.xls"),sep="\t",row.names=F)
  }
}

####ESOL Random Forest
GEN_ESOL<-function(dirFol){
  dirFol2 <- paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/")
  setwd(dirFol2)
  
  allFiles <- list.files(dirFol2,pattern="xls")
  joinFiles <- allFiles[grep("JOINT",allFiles)]
  
  statDat <- lapply(joinFiles,function(x){read.table(x,header=T,sep="\t")})
  names(statDat) <- substring(joinFiles,1,nchar(joinFiles)-4)
  
  ###Estaciones con info
  for(i in 1:length(statDat)){
    
    col=dim(statDat[[i]])[2]
    if(col==8){
      
      firstSta <- statDat[[i]][-c(1,5,6,9,11,12)]###Variables no necesarias inicialmente
      longVec=dim(firstSta)[1]
      
      firstSta$rangDiu <- firstSta$TMAX-firstSta$TMIN #COMPUTING Daily RANGE
      NAs= which((is.na(firstSta[,5]=="NA"))==TRUE)#Ubicacion de NA's
      DataNAs=firstSta[NAs,]###BD con los NA's unicamente
      
      modif=rep(times=longVec,"NO")
      modif[NAs]="SI"
      firstSta <- firstSta[complete.cases(firstSta),] #TO QUIT Data missing
      
      attach(firstSta);names(firstSta)
      
      #######################RANDOM FOREST######################
      #####################Relleno Faltantes####################
      
      rf     <- randomForest(ESOL_ORIG ~ ., data=firstSta, ntree=800,importance=TRUE,localImp=TRUE,na.action=na.omit) #*
      valPredic <- predict(rf, newdata=DataNAs[-5]) #*
      
      vec=statDat[[i]][,8]
      vec[NAs]=valPredic
      
      vec2=cbind(vec,modif)
      colnames(vec2)=c("RF_CCM2_ESOL","Modif")
      
      DatTot=cbind(statDat[[i]],vec2)
      
      write.table(DatTot,paste0(dirFol2,names(statDat)[i],".xls"),sep="\t",row.names=F)
    }
  }
  
}

#Final Graphics
END_GRAPS<-function(dirFol){
  pat     <- paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/")
  
  all.files <- list.files(pat,pattern="xls",full.names = F)
  nom.files<-substring(all.files,1,nchar(all.files)-4)
  DATAFUS<-lapply(paste(pat,all.files,sep=""),function(x){read.table(x,header=T,sep="\t")})
  names(DATAFUS)=nom.files
  
  fechas=as.Date(as.character(DATAFUS[[1]][,1]),"%Y-%m-%d")
  
  ##gUARDANDO ARCHIVOS FUS DE ESOL
  for(i in 1:length(nom.files)){
    #
    if(dim(DATAFUS[[i]])[2]==10){
      STATION=substring(nom.files[i],1,nchar(nom.files[i])-6)
      ESOLFUS=data.frame(fechas,DATAFUS[[i]][9],DATAFUS[[i]][10]); colnames(ESOLFUS)=c("Dates","Value","Modif")
      write.table(ESOLFUS,paste0(pat,STATION,"_ESOL_FUS.txt"), sep="\t",row.names=F)
      #ESOLEST=data.frame(fechas,DATAFUS[[i]][9]); colnames(ESOLEST)=c("Dates","Value")
      #write.table(ESOLEST,paste0(pat,STATION,"_ESOL_EST.txt"), sep="\t", row.names=F)
    }
  }
  #####################Graficando las series FUS
  filesFUS <- list.files(path=pat,pattern="\\FUS.txt$")
  nom.files<-substring(filesFUS,1,nchar(filesFUS)-4)
  DatosFUS <- lapply(paste(pat,"/",filesFUS,sep="",dec="."),function(x){read.table(x,header=T,sep="\t")})
  names(DatosFUS)=nom.files
  
  dir.create(paste0(pat,"END_GRAPHICS"),showWarnings=F)
  VAR=0
  for(i in 1:length(nom.files)){
    
    nomStation=substring(nom.files[i],1,nchar(nom.files[i])-9)
    DatosFUS[[i]][,1]=as.Date(as.character(DatosFUS[[i]][,1]),"%Y-%m-%d")
    VAR[i]=substring(nom.files[i],nchar(nom.files[i])-7,nchar(nom.files[i])-4)
    
    if(VAR[i]=="RAIN"){
      VarUnd="Rain (mm)"
    }else if(VAR[i]=="RHUM"){
      VarUnd="Relative Hum (%)"
    }else if(VAR[i]=="TMAX"){
      VarUnd="Temp max (C)"
    }else if(VAR[i]=="TMIN"){
      VarUnd="Temp min (C)"
    }else if(VAR[i]=="ESOL"){
      VarUnd="Solar Energy (ccm2)"
    }  
    
    #para el labDates
    dates=as.Date(DatosFUS[[i]][,1])
    labDates <- seq(min(dates), tail(dates, 1),by = "months")
    png(paste0(pat,"END_GRAPHICS/",nom.files[i],".png",sep=""),width = 1150, height = 500)####Para salvar el grafico
    plot(dates,DatosFUS[[i]][,2], main=nom.files[i], xaxt="n",xlab="",ylab=VarUnd,lwd=1,type="l",col="grey78")
    lines(dates,DatosFUS[[i]][,2],col=ifelse(DatosFUS[[i]][,3]=="SI","blue","red"),type="p",lwd=3)
    axis.Date(side=1,dates,at=labDates,format="%b %y",las=2)
    legend("topright",c("Estimated","Original"),lwd=c(1,1),col=c("blue","red"),box.lty=2)
    dev.off()###para finalmente guargdar el grafico
  }
}