library(readr)





# 讀檔函數03
read_data_03=function(ymd,hms){
  temp=paste("http://tisvcloud.freeway.gov.tw/history/TDCS/",
             "M03A","/",
             gsub("-",replacement="",ymd),"/",
             substr(hms,start=1,stop=2),"/TDCS_",
             "M03A","_",
             gsub("-",replacement="",ymd),"_",
             gsub(":",replacement="",hms),".csv",sep="")
  data=read.csv(temp,header=F)
  col_name=c('time','name','SN','type','amount')
  colnames(data)=col_name
  return(data)
}

# 讀檔函數45
read_data_45=function(ymd,hms){
  temp1=paste("http://tisvcloud.freeway.gov.tw/history/TDCS/",
              "M04A","/",
              gsub("-",replacement="",ymd),"/",
              substr(hms,start=1,stop=2),"/TDCS_",
              "M04A","_",
              gsub("-",replacement="",ymd),"_",
              gsub(":",replacement="",hms),".csv",sep="")
  temp2=paste("http://tisvcloud.freeway.gov.tw/history/TDCS/",
              "M05A","/",
              gsub("-",replacement="",ymd),"/",
              substr(hms,start=1,stop=2),"/TDCS_",
              "M05A","_",
              gsub("-",replacement="",ymd),"_",
              gsub(":",replacement="",hms),".csv",sep="")
  
  data1=read.csv(temp1,header=F)
  data2=read.csv(temp2,header=F)
  col_name1=c('time','from','to','type','travelTime','amount')
  col_name2=c('time','from','to','type','speed','amount')
  colnames(data1)=col_name1
  colnames(data2)=col_name2
  
  return(list(m04=data1,m05=data2))
}





# 抓日期起迄 做出時間的排程
hh=c('00','01','02','03','04','05','06','07',
     '08','09','10','11','12','13','14','15',
     '16','17','18','19','20','21','22','23')
mm=c('00','05','10','15','20','25',
     '30','35','40','45','50','55')
ss='00'

today=Sys.Date()-2
days=seq(from=as.Date("2017-08-19"),to=today,by="day")

ymd=array(0,length(days)*length(hh)*length(mm))
hms=array(0,length(days)*length(hh)*length(mm))

for(i in 1:length(days)){
  for(j in 1:length(hh)){
    for(k in 1:length(mm)){
      ymd[ ((((i-1)*length(hh)+j)-1)*length(mm)+k) ]=as.character(days[i])
      hms[ ((((i-1)*length(hh)+j)-1)*length(mm)+k) ]=paste(hh[j],mm[k],ss,sep=":")
    }
  }
}





# 偵測站(讀&建)
data=read.csv("detail331.csv",header=T,sep=",")
station_name=gsub("-",replacement="",gsub("\\.",replacement="",as.character(data[,3])))
ns=length(station_name)
for(i in 1:ns){#ns
  write.table(t(c("日期","時間","方向","小型","小貨","大型","大貨","聯結")),
              paste("./m03/",station_name[i],".csv",sep=""),
              col.names=F,row.names=F,sep=',')
}

# 偵測站到偵測站的配對(讀&建)
data=read_data_45(ymd[1],hms[1])
allpairs=unique((data$m04[,2:3]))
allpairs=cbind(as.character(allpairs[,1]),as.character(allpairs[,2]))
allpairs=allpairs[which(substr(allpairs[,1],start=8,stop=8)==substr(allpairs[,2],start=8,stop=8)),]
np=dim(allpairs)[1]
pair=array(0,np)
for(i in 1:np){#np
  pair[i]=paste(allpairs[i,1],"_",allpairs[i,2],sep="")
  write.table(t(c("日期","時間","起站","迄站","平均旅時","平均車速")),
              paste("./m45/",pair[i],".csv",sep=""),
              col.names=F,row.names=F,sep=',')
}




ptm=proc.time()
# 開抓
repeat{#####
  
  # 抓m03檔案
  data=read_data_03(ymd[1],hms[1])
  # 放進m03
  for(i in 1:ns){#ns
    # subset
    pmet=subset(data,data[,2]==station_name[i])
    # 排檔案
    temp=c(ymd[1],hms[1],as.character(pmet[1,3]),pmet[1,5],pmet[2,5],pmet[3,5],pmet[4,5],pmet[5,5])
    # 寫檔
    write_csv((data.frame(t(as.character(temp)))),
              paste("./m03/",as.character(station_name[i]),".csv",sep=""),
              col_names = F,append= T)
  }
  
  # 抓m45檔案
  data=read_data_45(ymd[1],hms[1])
  # 放進m45
  for(i in 1:np){#np
    # subset
    temp1=subset(data$m04,(data$m04[,2]==allpairs[i,1])&(data$m04[,3]==allpairs[i,2]))
    temp2=subset(data$m05,(data$m04[,2]==allpairs[i,1])&(data$m04[,3]==allpairs[i,2]))
    # 排檔案
    temp=c(ymd[1],hms[1],allpairs[i,1],allpairs[i,2],sum((temp1$travelTime)*(temp1$amount))/sum(temp1$amount),sum((temp2$speed)*(temp2$amount))/sum(temp2$amount))
    # 寫檔
    write_csv((data.frame(t(as.character(temp)))),
              paste("./m45/",pair[i],".csv",sep=""),
              col_names=F,append=T)
  }
  
  # 移除已讀的工作
  ymd=ymd[-1]
  hms=hms[-1]
  
  if(length(ymd)==0){
    break()
  }
  
}#####
proc.time()-ptm