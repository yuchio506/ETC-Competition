#install.packages("XML")
library(XML)
library(rjson)



# data
data=read.csv("detail331.csv",header=T,sep=",")
# id
id_temp=gsub("-",replacement="",gsub("\\.",replacement="",as.character(data[,3])))
# distance
distance=as.numeric(substr(id_temp,start=4,stop=7))*0.1
na=which(is.na(distance))
for(i in 1:length(na)){
  distance[na[i]]=(distance[na[i]-1]+distance[na[i]+1])/2
}
# direction
direction=substr(id_temp,start=8,stop=8)
NS=c('N','S','N')
# highway
highway_temp=substr(as.character(data[,1]),start=2,stop=2)
# region
region_temp=iconv(fromJSON(file='region.json'),from="BIG-5",to="UTF-8")
region_temp=gsub("�x",replacement="�O",region_temp)
# wx
wx=read.csv("wx.csv",header=T,sep=",")





repeat{

# 1968 �ɮ׺��}
url <- "http://tisvcloud.freeway.gov.tw/xml/1min_incident_data_1968.xml"
a=xmlToList(url)
n=length(a[[1]])
for(i in 1:n){
  a[[1]][[i]]$incident=iconv(a[[1]][[i]]$incident,from="UTF-8",to="BIG-5")
}
b=as.data.frame(a) #��list��data.frame
b=as.matrix(b) # ��K�B�z
colnames(b)=NULL # ��K�B�z
b=b[,which((b[5,]==1)|(b[5,]==3)|(b[5,]==5))] # �u����D1 3 5
n=length(b[1,])

if(n>0){
  roadInfo=matrix(0,n,2)
  for(i in 1:n){
    
    temp=which( grepl(strsplit(b[8,i],split="-")$inc_location[1],data[,6]) &
                  grepl(strsplit(b[8,i],split="-")$inc_location[2],data[,7]) )
    if(length(temp)==0){
      
      temp1=which(highway_temp==b[5,i])
      temp2=which(direction==NS[as.numeric(b[7,i])+1])
      temp3=intersect(temp1,temp2)
      roadInfo[i,1]=id_temp[temp3[which.min(abs(distance[temp3]-as.numeric(b[9,i])*10^(-3)))]]
      roadInfo[i,2]=b[2,i]
      
    } else{
      roadInfo[i,1]=id_temp[temp]
      roadInfo[i,2]=b[2,i]
    }
    
  }
} else{
  roadInfo=NULL
}




#���j�B����
url="http://opendata.cwb.gov.tw/opendataapi?dataid=W-C0033-003&authorizationkey=CWB-99EB53EE-EE8E-4628-BFDE-5373D13D035A"
rains=xmlToList(url)
rains=unlist(rains)
rains_area=rains[which(rains=="Taiwan_Geocode_103")-1]
rains_city=substr(rains_area,start=1,stop=3)
names(rains_city)=NULL



#�C������
weather1=NULL
#���ݮ�H��-�{�b�Ѯ��[�����i
url="http://opendata.cwb.gov.tw/opendataapi?dataid=O-A0003-001&authorizationkey=CWB-99EB53EE-EE8E-4628-BFDE-5373D13D035A"
temp=xmlToList(url)

for(i in 9:length(temp)){
  temp1=temp[[i]]$lat
  temp2=temp[[i]]$lon
  temp3=temp[[i]][[10]]$elementValue$value #��
  temp4=temp[[i]][[11]]$elementValue$value #��
  temp5=temp[[i]][[25]]$parameterValue #����
  weather1=rbind(weather1,c(temp1,temp2,temp3,temp4,temp5))
}

#�۰ʮ�H��-��H�[�����
url="http://opendata.cwb.gov.tw/opendataapi?dataid=O-A0001-001&authorizationkey=CWB-99EB53EE-EE8E-4628-BFDE-5373D13D035A"
temp=xmlToList(url)

for(i in 9:length(temp)){
  temp1=temp[[i]]$lat
  temp2=temp[[i]]$lon
  temp3=temp[[i]][[10]]$elementValue$value #��
  temp4=temp[[i]][[11]]$elementValue$value #��
  temp5=temp[[i]][[19]]$parameterValue #����
  weather1=rbind(weather1,c(temp1,temp2,temp3,temp4,temp5))
}

colnames(weather1)=c('lat','lon','WDSD','TEMP','city')
weather1=weather1[-union(which(weather1[,3]==-99),which(weather1[,4]==-99)),]



#�C���p��
weather2=NULL
#�@��Ѯ�w��-����36�p�ɤѮ�w��
url="http://opendata.cwb.gov.tw/opendataapi?dataid=F-C0032-001&authorizationkey=CWB-99EB53EE-EE8E-4628-BFDE-5373D13D035A"
temp=xmlToList(url)

for(i in 2:length(temp[[9]])){
  
  temp1=temp[[9]][[i]][[1]] #����
  temp2=temp[[9]][[i]][[2]]$time$parameter$parameterName #�Ѯ𪬪p
  temp3=temp[[9]][[i]][[3]]$time$parameter$parameterName #����
  temp4=temp[[9]][[i]][[4]]$time$parameter$parameterName #�C��
  temp5=temp[[9]][[i]][[5]]$time$parameter$parameterName #�ξA��
  temp6=temp[[9]][[i]][[6]]$time$parameter$parameterName #���B���v
  weather2=rbind(weather2,c(temp1,temp2,temp3,temp4,temp5,temp6))
  
}

colnames(weather2)=c('city','WX','maxT','minT','CI','POP')











otherInfo=array(list(),331)
for(i in 1:331){
  
  temp=which.min(  ((data[i,8]-as.numeric(weather1[,1]))^2)+
                   ((data[i,9]-as.numeric(weather1[,2]))^2)  )
  temp2=which(weather2[,1]==region_temp[i])
  
  otherInfo[[i]]=list(id=id_temp[i],
                      WDSD=as.numeric(weather1[temp,3]),
                      TEMP=as.numeric(weather1[temp,4]),
                      CI=as.character(weather2[temp2,5]),
                      WX=as.character(weather2[temp2,2]),
                      wxIndex=subset(wx[,3],wx[,1]==weather2[temp2,2]),
                      POP=as.numeric(weather2[temp2,6]),
                      rain=as.numeric(region_temp[i]%in%rains_city),
                      event=as.list(unique(subset(roadInfo[,2],roadInfo[,1]==id_temp[i])))
                      )
}
write.table( toJSON(otherInfo,"R"),'./output/otherInfo.json',
             col.names=F,row.names=F,fileEncoding="utf8",quote=F)


Sys.sleep(60*10)
}