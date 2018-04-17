library(rjson)

# �غc region & highway & ...
data=read.csv("detail331.csv",header=T,sep=",")
# ���t�����s��highway
highway_temp=substr(as.character(data[,1]),start=2,stop=2)
# ����region
region_temp=iconv(fromJSON(file='Region.json'),from="BIG-5",to="UTF-8")
# �и�id
id_temp=gsub("-",replacement="",gsub("\\.",replacement="",as.character(data[,3])))
# �W��name
name_temp=iconv(paste(as.character(data[,6]),'��',as.character(data[,7]),'���q'),from="BIG-5",to="UTF-8")
# ��Vdirection
direction_temp=as.character(data[,2])
NS=c("�_�W","�n�U")
# pairs
pairs_temp=fromJSON(file='PairsList.json')



martian=array(list(),331)
for(i in 1:331){
  
  martian[[i]]=list(id=id_temp[i],
                    name=name_temp[i],
                    highway=iconv(paste("��D",highway_temp[i],"��",sep=""),from="BIG-5",to="UTF-8"),
                    direction=iconv(NS[as.numeric(direction_temp[i]=="S")+1],from="BIG-5",to="UTF-8"),
                    location=list(lat=data[i,8],lng=data[i,9]),
                    region=region_temp[i],
                    Prior=as.list(pairs_temp[[i]]$Prio),
                    Next=as.list(pairs_temp[[i]]$Next)
                    )
  
}
write.table( toJSON(martian,"R"),'./output/StationsInfo.json',
             col.names=F,row.names=F,fileEncoding="utf8",quote=F)
# res=fromJSON(file='stationsInfo.json',method="R")