library(rjson)

# 建構 region & highway & ...
data=read.csv("detail331.csv",header=T,sep=",")
# 高速公路編號highway
highway_temp=substr(as.character(data[,1]),start=2,stop=2)
# 縣市region
region_temp=iconv(fromJSON(file='Region.json'),from="BIG-5",to="UTF-8")
# 標號id
id_temp=gsub("-",replacement="",gsub("\\.",replacement="",as.character(data[,3])))
# 名稱name
name_temp=iconv(paste(as.character(data[,6]),'至',as.character(data[,7]),'路段'),from="BIG-5",to="UTF-8")
# 方向direction
direction_temp=as.character(data[,2])
NS=c("北上","南下")
# pairs
pairs_temp=fromJSON(file='PairsList.json')



martian=array(list(),331)
for(i in 1:331){
  
  martian[[i]]=list(id=id_temp[i],
                    name=name_temp[i],
                    highway=iconv(paste("國道",highway_temp[i],"號",sep=""),from="BIG-5",to="UTF-8"),
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