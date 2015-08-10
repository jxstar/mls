library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(RSQLite)

library(xlsx)
library(RMySQL)
library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)

#library(bigmemory)



#==============================================================================================
#1# read the data
#==============================================================================================

previous=as.Date("2015-07-05")
current=as.Date("2015-08-06")


if (F){
  
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  #12 19 26
  category=dbGetQuery(conn,"select * from simple_mls_cat where date>=20150712 and date<=(20150806)")
  haodian=dbGetQuery(conn,"select * from simple_mls_haodian where date>=20150712 and date<=(20150806)")
  sku=dbGetQuery(conn,"select * from simple_mls_sku where date>=20150712 and date<=(20150806)")
  dbDisconnect(conn)
  
  #clean catagory-------------------------------------------------------
  #typelarge typemid typesmall: type1, type2,type3
  names(category)=c("kid","type2","type3","tid2","tid3","type1","date","ext")
  
  catbl=select(category,kid,date,tid1=type1,tid2,tid3,type2,type3)
  catbl=transform(catbl,tid1=factor(tid1),tid2=factor(tid2))
  catbl$date=as.Date(as.character(catbl$date),format="%Y%m%d")
  
  #clean haodian-------------------------------------------------------
  #str(haodian)
  names(haodian)=c("kid","sid","sname","likes","grade","sales","ngoods","date")
  haodian=select(haodian,kid,date,sid,sname,grade,ngoods,sales,likes)
  haodian$date=as.Date(as.character(haodian$date),format="%Y%m%d")
  #table(haodian$date)
  #dim(haodian)
  
  #clean sku-------------------------------------------------------
  #head(sku)
  #str(sku)
  #dim(sku)
  
  names(sku)=c("kid","type2","gid","gname","likes","sales","promotion","price",   "sid"    ,
               "sname",    "date",    "type3",     "tid2",    "tid3"    )
  sku=select(sku,kid,date,gid,gname,sales,likes,price,tid2,tid3,type2,type3,sid,sname)
  sku$date=as.Date(as.character(sku$date),format="%Y%m%d")
  #sku.bk=sku
  
  save(file=paste(current,"mls.Rdata",sep="-"),sku,haodian,catbl)
  
}


if(F){
  load(file="mls20150705-20150712.Rdata")
  table(sku$date)
  glaststatus=filter(sku,date==as.Date("2015-07-05"))
  glaststatus=select(laststatus,date,gid,tid2,tid3,sid,sales,likes,price)
  dim(glaststatus)  
  glaststore=distinct(select(glaststatus,sid),sid)
  glasthd=select(filter(haodian,date==as.Date("2015-07-05")),date,sid,grade,ngoods,sales,likes)
  dim(glaststore)
  dim(glasthd)
  head(glasthd)
  save(file = "glaststatus.Rdata",glaststatus,glaststore,glasthd)
}



#define 2 compared time spot
load(file=paste(current,"mls.Rdata",sep="-"))
load(file="glaststatus.Rdata")
skup=filter(glaststatus,date==previous)
dim(glaststatus)
dim(skup)

gdate=unique(sku$date)
gnperiod=length(gdate)
gnperiod=4
gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf"))
gcol=c("m1","m2","m3","m4")
gcolx=c("m1.x","m2.x","m3.x","m4.x")
gcoly=c("m1.y","m2.y","m3.y","m4.y")






#==============================================================================================
#2# check and clean the data
#==============================================================================================
n_na=function(x){
  return(sum(is.na(x)))
}

#---------------------------------------------------------------------------
# pricelist
#---------------------------------------------------------------------------
pricelist=select(sku,gid,date,price)
pricelist=melt(pricelist,id=c("gid","date"))
pricelist=dcast(pricelist,gid~date)
names(pricelist)=c("gid","m1","m2","m3","m4")
dim(pricelist)
head(pricelist)

# check the NA
pricelist$numna=apply(pricelist[,c("m1","m2","m3","m4")],1,n_na)
table(pricelist$numna)
if (sum(pricelist$numna==gnperiod)>0) cat("Error: price is all NA\n", head(pricelist[pricelist$numna==gnperiod,]))

price_repair=function(x){
  if (!(length(x)==gnperiod)) print("Error:length(x)!=gnperiod")
  if (sum(is.na(x))==0) return(x)
  if (sum(is.na(x))<(gnperiod/2)) {
    x[is.na(x)]=median(x,na.rm=T)
    return(x)
  }
  
  if (sum(is.na(x))>=(gnperiod/2)) {
    x[is.na(x)]=x[!is.na(x)]
    return(x)
  }
  
  if (sum(is.na(x))==gnperiod) print("Error:price is all NA!")
}


price_repair=function(x){
  if (!(length(x)==gnperiod)) print("Error:length(x)!=gnperiod")
  if (sum(is.na(x))==0) return(x)
  
  if (is.na(x[1])) x[1]=first(x[!is.na(x)])
  if (is.na(x[2])) {
    if (any(!is.na(x[c(3,4)]))) {y=x[c(3,4)];x[2]=first(y[!is.na(y)])}
    else {y=x[1];x[2]=last(y[!is.na(y)])}
  }
  
  if (is.na(x[3])) {
    if (any(!is.na(x[4]))) {y=x[4];x[3]=first(y[!is.na(y)])}
    else {y=x[c(1,2)];x[3]=last(y[!is.na(y)])}
  }
  if (is.na(x[4])) x[4]=last(x[!is.na(x)])
  if (any(is.na(x))) print("Error:Still there is NA")
  return(x)
}




#test the function of price_repair
if (F){
  (test=head(pricelist[pricelist$numna==2,gcol],100))
  (as.data.frame(t(apply(test,1, price_repair))))
}

#============================================
#=========repare the missing price===========
if (T){
  pricelist[,gcol]=as.data.frame(t(apply(pricelist[,gcol],1, price_repair)))
  (test=head(pricelist[pricelist$numna==1,gcol]))
  (any(is.na(pricelist[,gcol])))
}






#---------------------------------------------------------------------------
# dealist 
#---------------------------------------------------------------------------
dealist=select(sku,gid,date,sales)
dealist=melt(dealist,id=c("gid","date"))
dealist=dcast(dealist,gid~date)
dim(dealist)
n_distinct(dealist$gid)

#-----------------------------
dealist=left_join(dealist,select(glaststatus,gid,date,sales),by=c("gid"="gid"))
names(dealist)=c("gid","m1","m2","m3","m4","lastdate","m0")
dealist=select(dealist,gid,m0,m1,m2,m3,m4,lastdate)
dealist$maxdate=apply(dealist[,gcol],1, function(x){
  return((gdate[last(which(x==max(x,na.rm=T)))]))
})
dealist$maxdate=as.Date(dealist$maxdate)
head(dealist)

#-----------------------------
# check the NA
dealist$numna=apply(dealist[,c("m1","m2","m3","m4")],1,n_na)
dim(dealist)
table(dealist$numna)
table(apply(dealist[,c("m0","m1","m2","m3","m4")],1,n_na))

#-----------------------------
# print the status 
dealist$status=gstatus[1]
dealist[which(!(dealist$gid %in% glaststatus$gid)),"status"]=gstatus[2]
table(dealist$status)
# the mission samples
#a=head( dealist[(!is.na(dealist[,2]) & !is.na(dealist[,5]) & (is.na(dealist[,3])|is.na(dealist[,4]))),],100)

skup$status=gstatus[1]
skup[which(!(skup$gid %in% dealist$gid)),"status"]=gstatus[3]
#table(skup$status)

#-----------------------------
# join the price
dealist=left_join(dealist, pricelist, by=c("gid"="gid"))
head(dealist[which(!(dealist$numna.x==dealist$numna.y)),])

#-----------------------------
# define the multiple factor
dealist$mfactor=1
dealist$mfactor=apply(dealist[,c("lastdate","maxdate")],1,function(x){
  if (!is.na(x[1])) return(as.integer(as.Date(x[2])-previous)/as.integer(as.Date(x[2])-as.Date(x[1])))
  else return(1)
})
head(dealist[which(!dealist$mfactor==1),])


dealist$ndeals=0
dealist$GMV=0


#=============calculate the Ndeals==============
dealist[,"ndeals"]= apply(dealist[,c("m0",gcolx,"mfactor")],1, 
                          function(x){return((max(x[2:5],na.rm=T)-sum(x[1],na.rm=T))*x[6])})
(test=head(dealist,100))

if (F){
  #1
  sd=dealist[which(dealist$status==gstatus[1] & dealist$lastdate==previous),c("m0",gcolx)]
  dealist[which(dealist$status==gstatus[1] & dealist$lastdate==previous),"ndeals"]=
    apply(sd,1, function(x){return(max(x,na.rm=T)-sum(x[1],na.rm=T))})
  head(dealist[which(dealist$status==gstatus[1] & dealist$lastdate==previous),],100)
  dim(dealist[which(dealist$status==gstatus[1] & dealist$lastdate==previous),])
  
  #2-------problem----------
  sd=filter(dealist,status==gstatus[1] & !lastdate==previous)
  dealist[which(dealist$status==gstatus[1] & !dealist$lastdate==previous),"ndeals"]=
    apply(sd[,gcolx],1,function(x){
      return((max(x,na.rm = T)-min(x,na.rm = T))/(last(which(x==max(x,na.rm = T)))-which.min(x))*gnperiod)
    })  
  head(dealist[which(dealist$status==gstatus[1] & !dealist$lastdate==previous),],100)
  dim(dealist[which(dealist$status==gstatus[1] & !dealist$lastdate==previous),])
  
  
  #3 new multi times
  sd=filter(dealist,!(status==gstatus[1]) & numna.x<(gnperiod-1))
  dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x<(gnperiod-1)),"ndeals"]=
    apply(sd[,gcolx],1,function(x){return(max(x,na.rm = T))})
  head(dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x<(gnperiod-1)),],1000)
  dim(dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x<(gnperiod-1)),])
  
  
  #4 new single time
  sd=filter(dealist,!(status==gstatus[1]) & dealist$numna.x==(gnperiod-1))
  dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x==(gnperiod-1)),"ndeals"]=
    apply(sd[,gcolx],1,function(x){return(max(x,na.rm = T))})
  head(dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x==(gnperiod-1)),],100)
  dim(dealist[which(!(dealist$status==gstatus[1]) & dealist$numna.x==(gnperiod-1)),])
}


#==================repare the deals==============

  dealist[is.na(dealist$m1.x),"m1.x"]=dealist[is.na(dealist$m1.x),"m0"]
  dealist[is.na(dealist$m2.x),"m2.x"]=dealist[is.na(dealist$m2.x),"m1.x"]
  dealist[is.na(dealist$m3.x),"m3.x"]=dealist[is.na(dealist$m3.x),"m2.x"]
  dealist[is.na(dealist$m4.x),"m4.x"]=dealist[is.na(dealist$m4.x),"m3.x"]
  (test=head(dealist[is.na(dealist$m1.x),],10))
  (test=head(dealist[is.na(dealist$m4.x),]))


#=============calculate the GMV==============


#m.x ndeals   m.y=price
dealist$GMV=apply(dealist[,c("m0","m1.x","m2.x","m3.x","m4.x","m1.y","m2.y","m3.y","m4.y")],1,function(x){
  GMV=c()
  x[]
  GMV[1]=(sum(x[2],na.rm=T)-sum(x[1],na.rm=T))*x[6]
  GMV[2]=(sum(x[3],na.rm=T)-sum(x[2],na.rm=T))*x[7]
  GMV[3]=(sum(x[4],na.rm=T)-sum(x[3],na.rm=T))*x[8]
  GMV[4]=(sum(x[5],na.rm=T)-sum(x[4],na.rm=T))*x[9]
  return(sum(GMV,na.rm=T)/100)  
})
dealist$GMV=dealist$GMV*dealist$mfactor

head(dealist,100)

#---------------------------------------------------------------------------
# likelist
#---------------------------------------------------------------------------

likelist=select(sku,gid,date,likes)
likelist=melt(likelist,id=c("gid","date"))
likelist=dcast(likelist,gid~date)
likelist=left_join(likelist,select(glaststatus,gid,date,likes),by=c("gid"="gid"))
names(likelist)=c("gid","m1","m2","m3","m4","lastdate","m0")
likelist=select(likelist,gid,m0,m1,m2,m3,m4,lastdate)

likelist=left_join(likelist,select(dealist,gid,mfactor),by=c("gid"="gid"))
head(likelist[which(!likelist$mfactor==1),])

likelist$nlikes=0
likelist[,"nlikes"]= apply(likelist[,c("m0",gcol,"mfactor")],1, 
                          function(x){return((max(x[2:5],na.rm=T)-sum(x[1],na.rm=T))*x[6])})
(test=head(likelist,100))


# check the NA
likelist$numna=apply(likelist[,c("m1","m2","m3","m4")],1,n_na)

table(dealist$numna.x)
table(pricelist$numna)
table(likelist$numna)
if (sum(likelist$numna==gnperiod)>0) cat("Error: price is all NA\n", head(likelist[likelist$numna==gnperiod,]),"\n")











#==============================================================================================
#3# calculate the ndeals and sku GMV
#==============================================================================================

#---------------------------------------------------------------------------
# skub combine
#---------------------------------------------------------------------------

skub=select(dealist,gid,lastdate,maxdate,status,ndeals,GMV)
skub=left_join(skub,select(pricelist,gid,price=m4),by=c("gid"="gid"))
skub=left_join(skub,select(likelist,gid,nlikes),by=c("gid"="gid"))
skub=left_join(skub,distinct(select(sku,gid,tid2,tid3,sid),gid),by=c("gid"="gid"))

skub$dealdist=cut(skub$ndeals,breaks=c(-1,0,100,500,1000,5000,10000,100000,1000000),labels=c(0,100,500,1000,5000,10000,100000,1000000))
skub$GMVdist=cut(skub$GMV,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000,100000000),labels=c(0,100,1000,10000,100000,1000000,10000000,100000000))
skub$likedist=cut(skub$nlikes,breaks=c(-100000,-10000,-1000,-100,0,100,1000,10000,100000,1000000),
          labels=c(-10000,-1000,-100,0,100,1000,10000,100000,1000000))

table(cut(skub$ndeals,breaks=c(-1,0,100,500,1000,5000,10000,100000,1000000),labels=c(0,100,500,1000,5000,10000,100000,1000000)))
table(cut(skub$GMV,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000,100000000),labels=c(0,100,1000,10000,100000,1000000,10000000,100000000)))
table(cut(skub$nlikes,breaks=c(-100000,-10000,-1000,-100,0,100,1000,10000,100000,1000000),
                      labels=c(-10000,-1000,-100,0,100,1000,10000,100000,1000000)))

head(skub)


#----------------------------------------------------------------------------------------------
#1# overview : gid
#----------------------------------------------------------------------------------------------

skubsumtbl=data.frame(date=last(gdate),ngoods=nrow(skub),
                      ndeals=sum(skub$ndeals),mprice=mean(skub$price/100,na.rm=T),GMV=sum(skub$GMV),nlikes=sum(skub$nlikes),
                      ntype2=length(unique(skub$tid2)),ntype3=length(unique(skub$tid3)),nstore=length(unique(skub$sid)),
                      classical=nrow(filter(skub,status==gstatus[1])), newgoods=nrow(filter(skub,status==gstatus[2])),
                      offshelf=nrow(filter(skup,status==gstatus[3]))
                     )
#skubsumtbl
knitr::kable(as.data.frame(skubsumtbl),caption="SKU Summary Overview Table")


# Deals Top 10

(top10=head(as.data.frame(skub[order(desc(skub$GMV)),]),100))

top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption="SKU Deal Number Top10 Table")

write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=skubsumtbl,sheetName = "overview", append = T, showNA = F)
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "overview top10", append = T, showNA = F)

 



#----------------------------------------------------------------------------------------------
#2# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------

skub=group_by(skub,status)
totnewdeals=sum(skub$ndeals,na.rm=T)
totGMV=sum(skub$GMV,na.rm=T)
statussumtbl=summarise(skub,date=NA,ngoods=n(),goodsratio=n()/nrow(skub),
                       ndeals=sum(ndeals),dealratio=sum(ndeals)/totnewdeals, mprice=mean(price/100,na.rm=T),
                       GMV=sum(GMV,na.rm=T),GMVratio=sum(GMV,na.rm=T)/totGMV,
                       nlikes=sum(nlikes), nstore=length(unique(sid))
                      )
statussumtbl$date=last(gdate)
statussumtbl$status=as.character(statussumtbl$status)


n=nrow(statussumtbl)+1
statussumtbl[n,]=NA
statussumtbl[n,3:ncol(statussumtbl)]=apply(statussumtbl[1:n-1,3:ncol(statussumtbl)],2,sum)
statussumtbl[n,"status"]="Summary"
statussumtbl[n,"date"]=last(gdate)
statussumtbl[n,"mprice"]=mean(skub$price.x/100,na.rm=T)
statussumtbl[n,"nstore"]=length(unique(skub$sid))
statussumtbl[n+1,]=NA
statussumtbl[n+1,"date"]=last(gdate)
statussumtbl[n+1,"status"]="offshelf"
statussumtbl[n+1,"ngoods"]=sum(skup$status==gstatus[3])


knitr::kable(as.data.frame(statussumtbl),caption="SKU Summary By Status Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=statussumtbl,sheetName = "status", append = T, showNA = F)

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[1]),],desc(ndeals))),100)
top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"SKU Deal Number Top10 Table"))
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "classical top10", append = T, showNA = F)

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[2]),],desc(ndeals))),100)
top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"SKU Deal Number Top10 Table"))
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "new top10", append = T, showNA = F)


#----------------------------------------------------------------------------------------------
#3# overview : type2     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
skub=group_by(skub,tid2)
totnewdeals=sum(skub$ndeals)
totGMV=sum(skub$GMV)
typesumtbl=summarise(skub,date=NA,ngoods=n(),goodsratio=n()/nrow(skub),
                      ndeals=sum(ndeals),dealratio=sum(ndeals)/totnewdeals,mprice=mean(price,na.rm=T),
                      GMV=sum(GMV), GMVratio=sum(GMV)/totGMV,
                      nlikes=sum(nlikes), nstore=length(unique(sid))
                    )
typesumtbl$date=last(gdate)

n=nrow(typesumtbl)+1
typesumtbl[n,]=NA
typesumtbl[n,"tid2"]="Summary"
typesumtbl[n,3:ncol(typesumtbl)]=apply(typesumtbl[1:n-1,3:ncol(typesumtbl)],2,sum)
typesumtbl[n,"date"]=last(gdate)
typesumtbl[n,"mprice"]=mean(skub$price,na.rm=T)
typesumtbl[n,"nstore"]=length(unique(skub$sid))



typesumtbl=left_join(typesumtbl,distinct(select(sku,tid2,type2),tid2),by=c("tid2"="tid2"))
knitr::kable(as.data.frame(typesumtbl),caption="SKU Summary By Type Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=typesumtbl,sheetName = "type", append = T, showNA = F)

typelist=distinct(select(sku,tid2,type2,tid3,type3),tid2,tid3)
typelsit=arrange(typelist,tid2,tid3)
#typelist
#knitr::kable(typelist,caption="SKU Type List Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=typelist,sheetName = "typelist", append = T, showNA = F)

#----------------------------------------------------------------------------------------------
#4# overview : store
#4.1 summerize group by
#4.2 mark the status
#----------------------------------------------------------------------------------------------
skub=group_by(skub,sid)
totnewdeals=sum(skub$ndeals)
totGMV=sum(skub$GMV)

if (F){
  storecur=summarise(skub,date=NA,ngoods=n(),goodsratio=n()/nrow(skub),
                     newdeals=sum(ndeals),meannewdeals=mean(ndeals),dealratio=sum(ndeals)/totnewdeals,
                     mprice=mean(price,na.rm=T),GMV=sum(GMV),meanGMV=mean(GMV),GMVratio=sum(GMV)/totGMV,
                     nlikes=sum(nlikes),meannewlikes=mean(nlikes),
                     ntype2=length(unique(tid2)),ntype3=length(unique(tid3))
  )
  
  storecur$date=last(gdate)  
  save(file=paste(previous,max(gdate),"store.Rdata",sep="-"),storecur)
} else {
  load(file=paste(previous,max(gdate),"store.Rdata",sep="-"))
}

skup=group_by(skup,sid)
storepre=distinct(select(skup,sid),sid)
# mark the status
storecur$status=gstatus[1]
storecur[which(!(storecur$sid %in% glaststore$sid)),"status"]=gstatus[2]
#table(storecur$status)
storepre$status=gstatus[1]
storepre[which(!(storepre$sid %in% storecur$sid)),"status"]=gstatus[3]
#table(storepre$status)
#head(as.data.frame(filter(storecur,status==gstatus[2])),100)



storecur=group_by(storecur,status)
totngoods=sum(storecur$ngoods)
totnewdeals=sum(storecur$newdeals)
totGMV=sum(storecur$GMV)
storecursum=summarise(storecur,date=NA,nstore=n(),storeratio=n()/nrow(storecur),
                      ngoods=sum(ngoods),goodsratio=sum(ngoods)/totngoods,
                      newdeals=sum(newdeals),meanndpstore=mean(newdeals),dealratio=sum(newdeals)/totnewdeals,
                      GMV=sum(GMV),meanGMV=mean(GMV),GMVratio=sum(GMV)/totGMV,
                      newlikes=sum(nlikes),meannewlikes=mean(nlikes)
)

storecursum$date=last(gdate)
storecursum$status=as.character(storecursum$status)

n=nrow(storecursum)+1
storecursum[n,]=NA
storecursum[n,3:ncol(storecursum)]=apply(storecursum[1:n-1,3:ncol(storecursum)],2,sum)
storecursum[3,"status"]="Summary"
storecursum[n,"date"]=last(gdate)
storecursum[n,"meanndpstore"]=mean(storecur$newdeals,na.rm=T)
storecursum[n,"meanGMV"]=mean(storecur$GMV,na.rm=T)
storecursum[n,"meannewlikes"]=mean(storecur$nlikes,na.rm=T)

storecursum[n+1,]=NA
storecursum[n+1,"date"]=last(gdate)
storecursum[n+1,"status"]="offshelf"
storecursum[n+1,"nstore"]=sum(storepre$status==gstatus[3])


#storecur
#as.data.frame(storecursum)

knitr::kable(as.data.frame(storecursum),caption="Store Summary By Status Table")

knitr::kable(as.data.frame(table(storecur$status)),caption="New Store Distribution In This Term")
knitr::kable(as.data.frame(table(storepre$status)),caption="Abandoned Store Distribution In Last Term")

write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=storecursum,sheetName = "store", append = T, showNA = F)


(top10=head(as.data.frame(storecur[order(desc(storecur$GMV)),]),100))
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="Store GMV Top10 Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "store GMV top100", append = T, showNA = F)

storecur=ungroup(storecur)
top10=head(as.data.frame(arrange(storecur[which(storecur$status==gstatus[1]),],desc(GMV))),100)
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="Classical Store GMV Top10 Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "classic store GMV top100", append = T, showNA = F)

storecur=ungroup(storecur)
top10=head(as.data.frame(arrange(storecur[which(storecur$status==gstatus[2]),],desc(GMV))),100)
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="New Store GMV Top10 Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "New store GMV top100", append = T, showNA = F)

























#----------------------------------------------------------------------------------------------
#6# overview : haodian
#----------------------------------------------------------------------------------------------
#head(haodian)
#n_distinct(haodian$sid)
#dim(distinct(haodian,sid))
#dim(distinct(haodian,sid,grade,ngoods))
#n_distinct(skuc$sid)

hdeal=select(haodian,sid,date,ndeals=sales)
hdeal=melt(hdeal,id=c("sid","date"))
hdeal=dcast(hdeal,sid~date)
dim(hdeal)
head(hdeal)

#-----------------------------
hdeal=left_join(hdeal,select(glasthd,sid,date,sales),by=c("sid"="sid"))
names(hdeal)=c("sid","m1","m2","m3","m4","lastdate","m0")
hdeal=select(hdeal,sid,m0,m1,m2,m3,m4,lastdate)
hdeal$maxdate=apply(hdeal[,gcol],1, function(x){
  return((gdate[last(which(x==max(x,na.rm=T)))]))
})
hdeal$maxdate=as.Date(hdeal$maxdate)
head(hdeal)

#-----------------------------
# check the NA
hdeal$numna=apply(hdeal[,c("m1","m2","m3","m4")],1,n_na)
table(hdeal$numna)
table(apply(hdeal[,c("m0","m1","m2","m3","m4")],1,n_na))

#-----------------------------
# print the status 
hdeal$status=gstatus[1]
hdeal[which(!(hdeal$sid %in% glasthd$sid)),"status"]=gstatus[2]
table(hdeal$status)
# the mission samples
#a=head( hdeal[(!is.na(hdeal[,2]) & !is.na(hdeal[,5]) & (is.na(hdeal[,3])|is.na(hdeal[,4]))),],100)

hdp=filter(glasthd,date==previous)
hdp$status=gstatus[1]
hdp[which(!(hdp$sid %in% hdeal$sid)),"status"]=gstatus[3]
#table(hdp$status)


#-----------------------------
# define the multiple factor
hdeal$mfactor=1
hdeal$mfactor=apply(hdeal[,c("lastdate","maxdate")],1,function(x){
  if (!is.na(x[1])) return(as.integer(as.Date(x[2])-previous)/as.integer(as.Date(x[2])-as.Date(x[1])))
  else return(1)
})
head(hdeal[which(!hdeal$mfactor==1),])
hdeal$ndeals=0

#=============calculate the Ndeals==============
hdeal[,"ndeals"]= apply(hdeal[,c("m0",gcol,"mfactor")],1, 
                          function(x){return((max(x[2:5],na.rm=T)-sum(x[1],na.rm=T))*x[6])})
(test=head(hdeal,100))


#------------------------------------
#nlikes
hdlike=select(haodian,sid,date,likes)
hdlike=melt(hdlike,id=c("sid","date"))
hdlike=dcast(hdlike,sid~date)
hdlike=left_join(hdlike,select(glasthd,sid,date,likes),by=c("sid"="sid"))
names(hdlike)=c("sid","m1","m2","m3","m4","lastdate","m0")
hdlike=select(hdlike,sid,m0,m1,m2,m3,m4,lastdate)

hdlike=left_join(hdlike,select(hdeal,sid,mfactor),by=c("sid"="sid"))
head(hdlike[which(!hdlike$mfactor==1),])

hdlike$nlikes=0
hdlike[,"nlikes"]= apply(hdlike[,c("m0",gcol,"mfactor")],1, 
                           function(x){return((max(x[2:5],na.rm=T)-sum(x[1],na.rm=T))*x[6])})
(test=head(hdlike,100))

#------------------------------------
#newgoods
hdgoods=select(haodian,sid,date,ngoods)
hdgoods=melt(hdgoods,id=c("sid","date"))
hdgoods=dcast(hdgoods,sid~date)
hdgoods=left_join(hdgoods,select(glasthd,sid,date,ngoods),by=c("sid"="sid"))
names(hdgoods)=c("sid","m1","m2","m3","m4","lastdate","m0")
hdgoods=select(hdgoods,sid,m0,m1,m2,m3,m4,lastdate)
hdgoods$newgoods=0
hdgoods$ngoods=0
hdgoods[,"newgoods"]= apply(hdgoods[,c("m0",gcol)],1, 
                             function(x){return((last(x[which(!is.na(x))]))-sum(x[1],na.rm=T))})
hdgoods[,"ngoods"]= apply(hdgoods[,c("m0",gcol)],1, 
                            function(x){return((last(x[which(!is.na(x))])))})

(test=head(hdgoods,100))








#----------------------------------------------------------------------------
#hdb
#----------------------------------------------------------------------------
head(hdeal)
head(hdlike)

hdb=select(hdeal,sid,lastdate,maxdate,status,ndeals)
hdb=left_join(hdb,select(hdlike,sid,nlikes),by=c("sid"="sid"))
hdb=left_join(hdb,select(hdgoods,sid,ngoods,ngoods,newgoods),by=c("sid"="sid"))
hdb=left_join(hdb,distinct(select(haodian,sid,grade,sname),sid),by=c("sid"="sid"))



#----------------------------------------------------------------------------------------------
#6.2# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
hdb=group_by(hdb,status)
totnewgoods=sum(hdb$newgoods)
totsumgoods=sum(hdb$ngoods)
totndeals=sum(hdb$ndeals)
totnlikes=sum(hdb$nlikes)
hdbstatus=summarise(hdb,date=NA,nstore=n(),storeratio=n()/nrow(hdb),
                    totgoods=sum(ngoods),mgoods=mean(ngoods),totgoodsratio=sum(ngoods)/totsumgoods,
                    newgoods=sum(newgoods),mnewgoods=mean(newgoods),newgoodsratio=sum(newgoods)/totnewgoods,
                    ndeals=sum(ndeals),meandeals=mean(ndeals),dealratio=sum(ndeals)/totndeals,
                    nlikes=sum(nlikes),meanlikes=mean(nlikes),likeratio=sum(nlikes)/totnlikes
)

hdbstatus$status=as.character(hdbstatus$status)

n=nrow(hdbstatus)+1
hdbstatus[n,]=NA
hdbstatus[n,3:ncol(hdbstatus)]=apply(hdbstatus[1:n-1,3:ncol(hdbstatus)],2,sum)
hdbstatus[n,"status"]="Summary"
hdbstatus[n,"mgoods"]=mean(hdb$ngoods,na.rm=T)
hdbstatus[n,"mnewgoods"]=mean(hdb$newgoods,na.rm=T)
hdbstatus[n,"meandeals"]=mean(hdb$ndeals,na.rm=T)
hdbstatus[n,"meanlikes"]=mean(hdb$nlikes,na.rm=T)
hdbstatus[n+1,]=NA
hdbstatus[n+1,"status"]="offshelf"
hdbstatus[n+1,"nstore"]=sum(hdp$status==gstatus[3])
hdbstatus$date=last(gdate)

#as.data.frame(hdbstatus)
knitr::kable(as.data.frame(hdbstatus),caption="Haodian Summary By Status Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=hdbstatus,sheetName = "haodian status", append = T, showNA = F)

top10=head(as.data.frame(arrange(hdb,desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"Haodian Deal Number Top10 Table"))
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "haodian top100", append = T, showNA = F)

top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[1]),],desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"Haodian Old Deal Number Top10 Table"))
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "haodian classic top100", append = T, showNA = F)
top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[2]),],desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"Haodian New Deal Number Top10 Table"))
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "haodian New top100", append = T, showNA = F)

knitr::kable(as.data.frame(table(hdb$status)),caption="Haodian Status New Store Distribution Table")
knitr::kable(as.data.frame(table(hdp$status)),caption="Haodian Status Abandoned Store Distribution Table")



#----------------------------------------------------------------------------------------------
#6.3# overview : grade     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
hdb=group_by(hdb,grade)
totnewgoods=sum(hdb$newgoods)
totsumgoods=sum(hdb$ngoods)
totndeals=sum(hdb$ndeals)
totnlikes=sum(hdb$nlikes)
hdbstatus=summarise(hdb,date=NA,nstore=n(),storeratio=n()/nrow(hdb),
                    totgoods=sum(ngoods),mgoods=mean(ngoods),totgoodsratio=sum(ngoods)/totsumgoods,
                    newgoods=sum(newgoods),mnewgoods=mean(newgoods),newgoodsratio=sum(newgoods)/totnewgoods,
                    ndeals=sum(ndeals),meandeals=mean(ndeals),dealratio=sum(ndeals)/totndeals,
                    nlikes=sum(nlikes),meanlikes=mean(nlikes),likeratio=sum(nlikes)/totnlikes
)

#hdbstatus$grade=as.character(hdbstatus$grade)

n=nrow(hdbstatus)+1
hdbstatus[n,]=NA
hdbstatus[n,3:ncol(hdbstatus)]=apply(hdbstatus[1:n-1,3:ncol(hdbstatus)],2,sum)
hdbstatus[n,"grade"]="Summary"
hdbstatus[n,"mgoods"]=mean(hdb$ngoods,na.rm=T)
hdbstatus[n,"mnewgoods"]=mean(hdb$newgoods,na.rm=T)
hdbstatus[n,"meandeals"]=mean(hdb$ndeals,na.rm=T)
hdbstatus[n,"meanlikes"]=mean(hdb$nlikes,na.rm=T)
hdbstatus$date=last(gdate)

#as.data.frame(hdbstatus)
knitr::kable(as.data.frame(hdbstatus),caption="Haodian Summary By Grade Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=hdbstatus,sheetName = "haodian grade", append = T, showNA = F)

top10=head(as.data.frame(hdb[order(-hdb$grade),]),100)
knitr::kable(as.data.frame(top10),caption="Haodian Grade Top10 Table")
write.xlsx(file=paste(previous,max(gdate),"result.xlsx",sep = "-"),x=top10,sheetName = "haodian grade top100", append = T, showNA = F)



#----------------------------------------------------------------------------------------------
#7# type list table
#----------------------------------------------------------------------------------------------
knitr::kable(typelist,caption="SKU Type List Table")
