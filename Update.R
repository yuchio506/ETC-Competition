################
#              #
# 每天一點更新 #
#              #
################

################
#              #
#   基礎設定   #
#              #
################

# library
library(forecast)
library(readr)
library(rjson)
library(DMwR)

##########
#        #
#  函數  #
#        #
##########

# 讀檔函數
read_data=function(name,ymd,hms){ # name = M03A, M04A, M05A
  temp=paste("http://tisvcloud.freeway.gov.tw/history/TDCS/",
             name,"/",
             gsub("-",replacement="",ymd),"/",
             substr(hms,start=1,stop=2),"/TDCS_",
             name,"_",
             gsub("-",replacement="",ymd),"_",
             gsub(":",replacement="",hms),".csv",sep="")
  data=read.csv(temp,header=F)
  if(name=="M03A"){
    col_name=c('time','name','SN','type','amount')
    colnames(data)=col_name
  }
  if(name=="M04A"){
    col_name=c('time','from','to','type','travelTime','amount')
    colnames(data)=col_name
  }
  if(name=="M05A"){
    col_name=c('time','from','to','type','speed','amount')
    colnames(data)=col_name
  }
  return(data)
}

# 更新資料函數
update_data=function(yesterday,hms){
  m=length(hms)
  for(i in 1:m){
    data1=read_data("M03A",yesterday,hms[i])
    data2=read_data("M04A",yesterday,hms[i])
    data3=read_data("M05A",yesterday,hms[i])
    for(j in 1:ns){
      # subset
      pmet=subset(data1,data1[,2]==stations[j])
      # 排檔案
      temp=c(as.character(yesterday),
             as.character(hms[i]),
             as.character(pmet[1,3]),
             pmet[1,5],
             pmet[2,5],
             pmet[3,5],
             pmet[4,5],
             pmet[5,5])
      # 寫檔append
      write_csv((data.frame(t(as.character(temp)))),
                paste("./m03/",as.character(stations[j]),".csv",sep=""),
                col_names=F,append=T)
    }
    for(k in 1:np){
      # subset
      temp1=unlist(strsplit(pair[k],split="_"))
      temp2=subset(data2,(data2[,2]==temp1[1])&(data2[,3]==temp1[2]))
      temp3=subset(data3,(data3[,2]==temp1[1])&(data3[,3]==temp1[2]))
      # 排檔案
      temp=c(as.character(yesterday),
             as.character(hms[i]),
             as.character(temp1[1]),
             as.character(temp1[2]),
             sum((temp2$travelTime)*(temp2$amount))/sum(temp2$amount),
             sum((temp3$speed)*(temp3$amount))/sum(temp3$amount))
      # 寫檔append
      write_csv((data.frame(t(as.character(temp)))),
                paste("./m45/",as.character(pair[k]),".csv",sep=""),
                col_names=F,append=T)
    }
  }
}

# 排時間函數
daytoday=function(day1,day2){
  
  hh=c('00','01','02','03','04','05','06','07',
       '08','09','10','11','12','13','14','15',
       '16','17','18','19','20','21','22','23')
  mm=c('00','05','10','15','20','25',
       '30','35','40','45','50','55')
  ss='00'
  
  ddd=seq(from=day1,by="day",to=day2)
  ymd=array(0,length(ddd)*length(hh)*length(mm))
  hms=array(0,length(ddd)*length(hh)*length(mm))
  www=NULL
  for(i in 1:length(ddd)){
    www=c(www,array(weekdays(ddd[i],abbreviate=T),288))
  }
  
  for(i in 1:length(ddd)){
    for(j in 1:length(hh)){
      for(k in 1:length(mm)){
        ymd[ ((((i-1)*length(hh)+j)-1)*length(mm)+k) ]=as.character(ddd[i])
        hms[ ((((i-1)*length(hh)+j)-1)*length(mm)+k) ]=paste(hh[j],mm[k],ss,sep=":") # 超酷
      }
    }
  }
  return(list(ymd=ymd,hms=hms,ddd=ddd,www=www))
}

# 平假日函數
holidays=function(date){
  
  temp=array(0,length(date))
  for(i in 1:length(date)){
    if( (weekdays(date[i],abbreviate=T)=="週六")||(weekdays(date[i],abbreviate=T)=="週日") ){
      temp[i]="holiday"
    } else{
      temp[i]="workday"
    }
  }
  return(temp)
}

# 預測函數(兩天為單位perHour)

flow_pred=function(historydata,historyholiday){
  temp=daytoday(Sys.Date(),(Sys.Date()+1))
  hms=temp$hms
  flow=array(list(),48)
  for(ii in 1:1){
    for(jj in 1:24){
      flow_day1=matrix(0,12,5)
      flow_day2=matrix(0,12,5)
      for(kk in 1:12){
        iii=((((ii-1)*24+jj)-1)*12+kk)
        for(ll in 1:5){
          ts=historydata[intersect(which(historyholiday==as.character(holidays(Sys.Date()))),
                                   which(historydata[,2]==hms[iii])),(ll+3)]
          flow_day1[kk,ll]=round(mean(ts))
          flow_day2[kk,ll]=round(mean(c(ts,flow_day1[kk,ll])))
        }
      }
      flow[[((ii-1)*24+jj)]]=list(typeA=round(colSums(flow_day1)[1]),
                                  typeB=round(colSums(flow_day1)[2]),
                                  typeC=round(colSums(flow_day1)[3]),
                                  typeD=round(colSums(flow_day1)[4]),
                                  typeE=round(colSums(flow_day1)[5]))
      flow[[((ii)*24+jj)]]=list(typeA=round(colSums(flow_day2)[1]),
                                typeB=round(colSums(flow_day2)[2]),
                                typeC=round(colSums(flow_day2)[3]),
                                typeD=round(colSums(flow_day2)[4]),
                                typeE=round(colSums(flow_day2)[5]))
    }
  }
  return(flow)
}

ST_pred=function(historydata,historyholiday){
  temp=daytoday(Sys.Date(),(Sys.Date()+1))
  hms=temp$hms
  ST=array(list(),48)
  for(ii in 1:1){
    for(jj in 1:24){
      ST_day1=matrix(0,12,2)
      ST_day2=matrix(0,12,2)
      for(kk in 1:12){
        iii=((((ii-1)*24+jj)-1)*12+kk)
        for(ll in 1:2){
          ts=historydata[intersect(which(historyholiday==as.character(holidays(Sys.Date()))),
                                   which(historydata[,2]==hms[iii])),(ll+4)]
          ST_day1[kk,ll]=round(mean(ts))
          ST_day2[kk,ll]=round(mean(c(ts,ST_day1[kk,ll])))
        }
      }
      ST[[((ii-1)*24+jj)]]=list(travelTime=round(colMeans(ST_day1)[1]),
                                speed=round(colMeans(ST_day1)[2]))
      ST[[((ii)*24+jj)]]=list(travelTime=round(colMeans(ST_day2)[1]),
                              speed=round(colMeans(ST_day2)[2]))
    }
  }
  return(ST)
}

##########
#        #
#  參數  #
#        #
##########

data=read.csv("detail331.csv",header=T,sep=",")
stations=gsub("-",replacement="",gsub("\\.",replacement="",as.character(data[,3])))
ns=length(stations)
pair=fromJSON(file='pair.json')
np=length(pair)
hour=c('00','01','02','03','04','05','06','07',
       '08','09','10','11','12','13','14','15',
       '16','17','18','19','20','21','22','23')
hour=c(hour,hour)

##########
#        #
#  迴圈  #
#        #
##########

repeat{ #repeat
  if(as.POSIXlt(Sys.time())$hour==1){
    
    temp=read.csv("./m03/01F0005N.csv",header=T,sep=",")
    historyholiday=holidays(as.Date(paste(temp[,1])))
    
    
    
    
    #################################################################
    temp=daytoday((as.Date(temp[dim(temp)[1],1])+1),(Sys.Date()-1)) #
    for(i in 1:length(temp$ddd)){                                   #
      # 更新資料                                                    #
      update_data(temp$ddd[i],temp$hms[1:288])                      #
    }                                                               #
    #################################################################
    
    
    
    ###################################################################
    # 預測, ray=0405, martian=03                                      #
    ray=array(list(),np)                                              #
    martian=array(list(),ns)                                          #
    for(i in 1:np){                                                   #
      historydata=read.csv(paste("./m45/",pair[i],".csv",sep=""),     #
                           header=T,sep=",")                          #
      historydata <- knnImputation(historydata)                       #
      write.table(historydata,                                        #
                  paste("./m45/",pair[i],".csv",sep=""),              #
                  col.names=T,row.names=F,sep=",")                    #
      ray[[i]]=ST_pred(historydata,historyholiday)                    #
    }                                                                 #
    for(i in 1:ns){                                                   #
      historydata=read.csv(paste("./m03/",stations[i],".csv",sep=""), #
                           header=T,sep=",")                          #
      martian[[i]]=flow_pred(historydata,historyholiday)              #
    }                                                                 #
    ###################################################################
    
    
    ###########################################################################################
    # 擺資料                                                                                  #
    allinone=array(list(),ns)                                                                 #
    for(i in 1:ns){                                                                           #
      # 24小時                                                                                #
      ####################################################################################### #
      pred_data=array(list(),48)                                                            # #
      for(j in 1:48){                                                                       # #
        # Prior and Next                                                                    # #
        ################################################################################### # #
        ################################################################################### # #
        speed_temp=NULL                                                                  ## # #
        Next_temp=NULL                                                                   ## # #
        Prior_temp=NULL                                                                  ## # #
        temp=grep(stations[i],pair)                                                      ## # #
        for(k in 1:length(temp)){                                                        ## # #
          speed_temp=c(speed_temp,                                                       ## # #
                       ray[[temp[k]]][[j]]$speed)                                        ## # #
                                                                                         ## # #
          temp1=strsplit(pair[temp[k]],split="_")[[1]]                                   ## # #
          if(grep(stations[i],temp1)==1){                                                ## # #
            if(length(Next_temp)==0){                                                    ## # #
              Next_temp=array(list(),1)                                                  ## # #
              Next_temp[[1]]=list(id=temp1[2],travelTime=ray[[temp[k]]][[j]]$travelTime) ## # #
            } else{                                                                      ## # #
              nnn=length(unlist(Next_temp))/2                                            ## # #
              Next_tttt=array(list(),nnn+1)                                              ## # #
              for(iii in 1:nnn){                                                         ## # #
                Next_tttt[[iii]]=Next_temp[[iii]]                                        ## # #
              }                                                                          ## # #
              Next_tttt[[(nnn+1)]]=list(id=temp1[2],                                     ## # #
                                        travelTime=ray[[temp[k]]][[j]]$travelTime)       ## # #
              Next_temp=Next_tttt                                                        ## # #
            }                                                                            ## # #
          }                                                                              ## # #
          if(grep(stations[i],temp1)==2){                                                ## # #
            if(length(Prior_temp)==0){                                                   ## # #
              Prior_temp=array(list(),1)                                                 ## # #
              Prior_temp[[1]]=list(id=temp1[1],travelTime=ray[[temp[k]]][[j]]$travelTime)## # #
            } else{                                                                      ## # #
              nnn=length(unlist(Prior_temp))/2                                           ## # #
              Prior_tttt=array(list(),nnn+1)                                             ## # #
              for(iii in 1:nnn){                                                         ## # #
                Prior_tttt[[iii]]=Prior_temp[[iii]]                                      ## # #
              }                                                                          ## # #
              Prior_tttt[[(nnn+1)]]=list(id=temp1[1],                                    ## # #
                                         travelTime=ray[[temp[k]]][[j]]$travelTime)      ## # #
              Prior_temp=Prior_tttt                                                      ## # #
            }                                                                            ## # #
          }                                                                              ## # #
        }                                                                                ## # #
        speed_temp=mean(speed_temp)                                                      ## # #
        ################################################################################### # #
        ################################################################################### # #
        if(j %in% (1:24)){                                                                  # #
          pred_data[[j]]=list(date=as.character(Sys.Date()),                                # #
                              weekday=weekdays(Sys.Date(),abbreviate=T),                    # #
                              time=hour[j],                                                 # #
                              flow=martian[[i]][[j]],                                       # #
                              speed=speed_temp,                                             # #
                              Prior=as.list(Prior_temp),                                    # #
                              Next=as.list(Next_temp))                                      # #
        }                                                                                   # #
        if(j %in% (25:48)){                                                                 # #
          pred_data[[j]]=list(date=as.character(Sys.Date()+1),                              # #
                              weekday=weekdays((Sys.Date()+1),abbreviate=T),                # #
                              time=hour[j],                                                 # #
                              flow=martian[[i]][[j]],                                       # #
                              speed=speed_temp,                                             # #
                              Prior=as.list(Prior_temp),                                    # #
                              Next=as.list(Next_temp))                                      # #
        }                                                                                   # #
      }                                                                                     # #
      ####################################################################################### #
      allinone[[i]]=list(id=stations[i],                                                      #
                         predictedData=pred_data)                                             #
    }                                                                                         #
    write.table( toJSON(allinone,"R"),'./output/PredData.json',                               #
                 col.names=F,row.names=F,fileEncoding="utf8",quote=F)                         #
    ###########################################################################################

    
    
    
    Sys.sleep(60*60) # 60分鐘
  }else{
    Sys.sleep(60*10) # 10分鐘
  }
}#repeat










