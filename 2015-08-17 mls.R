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




#----------------------------------------------
# make sure the time range you wanna analysis
if (T){
  
  #conn <- dbConnect(MySQL(), dbname = "thdata", username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  conn <- dbConnect(MySQL(), dbname = "thdata", username="root", password="123456",host="101.200.189.155",port=3306)
  
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  test=dbGetQuery(conn,"select * from simple_mls_cat")
  (date.range=(as.data.frame=table(test$date)))
  dbDisconnect(conn)
}


#make sure the previous and the current date
#month report: we use the latest 4weeks
#================================================================
(previous=as.Date(as.Date("2015-07-06"):as.Date("2015-08-02")))
(current=as.Date(as.Date("2015-08-03"):as.Date("2015-08-09")))
#================================================================

#----------------------------------------------
# make sure the last updated status

if(T){
#  conn <- dbConnect(MySQL(), dbname = "thdata", username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  conn <- dbConnect(MySQL(), dbname = "thdata", username="root", password="123456",host="101.200.189.155",port=3306)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  
  #===========glastxxx===========================================================================================
  #Read the glastxxx: <=last(previous)
    #SELECT * FROM (SELECT * FROM posts ORDERBY dateline DESC) GROUP BY  tid ORDER BY datelineDESC LIMIT 10
  glastsku=dbGetQuery(conn,"select gid, date, sid, sales, likes, price 
                      from (select * from simple_mls_sku where date<20150802 order by date desc) as sku group by gid order by date desc")
  glasthd=dbGetQuery(conn,"select sid,date, goods, sales, likes
                      from (select * from simple_mls_haodian where date<=20150802 order by date desc) as hd group by sid order by date desc")
  if(F) {
    #check if the SQL equal the R 
    hd=dbGetQuery(conn,"select sid,date,grade, goods, sales, likes from simple_mls_haodian where date<=20150710")
    hd=group_by(hd, sid)
    hd=arrange(hd, desc(date))
    hdsm=summarise(hd, date=first(date), grade=first(grade),  goods=first(goods), sales=first(sales), likes=first(likes))
    table(hdsm$date)
    table(glasthd$date)
    if (!all.equal(hdsm, glasthd)) print("ERROR: SQL is not equal the R Groupby")

    sk=dbGetQuery(conn,"select gid, date, code, nid, sid, sales, likes, price from simple_mls_sku where date<=20150710")
    sk=group_by(sk, gid)
    sk=arrange(sk, desc(date))
    sksm=summarise(sk, date=first(date), code=first(code),  nid=first(nid), sid=first(sid), 
                   sales=first(sales), likes=first(likes), price=first(price))
    table(sksm$date)
    table(glastsku$date)
    all.equal(sksm, glastsku)
  }
  
  names(glastsku)=c("gid", "date","sid", "sales", "likes", "price")
  glastsku=select(glastsku,date,gid,sid,sales,likes,price)
  glastsku$date=as.Date(as.character(glastsku$date),format="%Y%m%d")
  names(glasthd)=c("sid", "date", "ngoods", "sales", "likes")
  glasthd=select(glasthd,date,sid,ngoods, sales,likes)
  glasthd$date=as.Date(as.character(glasthd$date),format="%Y%m%d")
  
  glaststore=distinct(select(arrange(glastsku, desc(date)),sid),sid)
  dim(glaststore)


  #===========sku haodian===================================================================================== 
  #Read SKU: >last(previous)  <=last(current)
  category=dbGetQuery(conn,"select * from simple_mls_cat where date>20150802 and date<=20150809 ")
  haodian=dbGetQuery(conn,"select * from simple_mls_haodian where date>20150802 and date<=20150809 ")
  sku=dbGetQuery(conn,"select * from simple_mls_sku where date>20150802 and date<=20150809 ")

  
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

  names(sku)=c("kid","type2","gid","gname","likes","sales","promotion","price",   "sid"    ,
               "sname",    "date",    "type3",     "tid2",    "tid3"    )
  sku=select(sku,kid,date,gid,gname,sales,likes,price,tid2,tid3,type2,type3,sid,sname)
  sku$date=as.Date(as.character(sku$date),format="%Y%m%d")
  #sku.bk=sku
  
  dbDisconnect(conn)
  save(file=paste(first(current), last(current),"mls.Rdata",sep="-"),sku,haodian,catbl,glastsku,glasthd, glaststore)
  #----------------------------------
  #write the gloabal to the database
}




#define 2 compared time spot
load(file=paste(first(current), last(current),"mls.Rdata",sep="-"))
#----------------------------------------------------------------------------------------------
# Define the Macros Variables
skup=filter(glastsku,date %in% previous)
head(glastsku)
dim(glastsku)
dim(skup)

(gdate=unique(sku$date))
(gnperiod=length(gdate))
gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf"))
gcol=paste("m", 1:gnperiod, sep="")
gcolx=paste("m", 1:gnperiod, ".x", sep="")
gcoly=paste("m", 1:gnperiod, ".y", sep="")
(gfile=paste(last(previous)+1,"to",max(gdate),"MLS Analysis Result By JX @", Sys.Date(),".xlsx",sep = " "))


#==============================================================================================
#2# check and clean the data
#3# calculate the GMV and deal for SKU
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
names(pricelist)=c("gid",gcol)
pricelist$numna=NA
if (gnperiod == 1) pricelist$numna=as.integer(is.na(pricelist[,gcol])) else
                   pricelist$numna=apply(pricelist[,gcol],1,n_na)
head(pricelist)
table(pricelist$numna)
if (sum(pricelist$numna==gnperiod)>0) cat("Error: price is all NA\n", head(pricelist[pricelist$numna==gnperiod,]), "\n")

price_repair=function(x){
  if (!(length(x)==gnperiod)) print("Error:length(x)!=gnperiod")
  if (sum(is.na(x))==0) return(x)
  
  n=length(x)
  if (is.na(x[1])) x[1]=first(x[!is.na(x)])
  if (is.na(x[n])) x[n]=last(x[!is.na(x)])
  if (n<=2) return(x)
  
  for (i in 2:(n-1)){
    pre=x[1:(i-1)]
    post=x[(i+1):(n)]
    if (is.na(x[i])) {
      if (any(!is.na(post))) {x[i]=first(post[!is.na(post)])}
      else {x[i]=last(pre[!is.na(pre)])}
    }
  }
  
  if (any(is.na(x))) print("Error:Still there is NA")
  return(x)
}

#test the function of price_repair
if (F){
  (test=head(pricelist[pricelist$numna==2,gcol],10))
  (as.data.frame(t(apply(test,1, price_repair))))
}

#============================================
#=========repare the missing price===========
if (gnperiod>1)  pricelist[,gcol]=as.data.frame(t(apply(pricelist[,gcol],1, price_repair)))
(test=head(pricelist[pricelist$numna==1,gcol]))
(any(is.na(pricelist[,gcol])))



#---------------------------------------------------------------------------
# dealist 
#---------------------------------------------------------------------------
dealist=select(sku,gid,date,sales)
dealist=melt(dealist,id=c("gid","date"))
dealist=dcast(dealist,gid~date)

#-----------------------------
dealist=left_join(dealist,select(glastsku,gid,date,sales),by=c("gid"="gid"))
names(dealist)=c("gid",gcol,"lastdate","m0")
dealist=select(dealist,gid,m0,starts_with("m"),lastdate)

if (gnperiod==1) dealist$maxdate=gdate else 
  dealist$maxdate=apply(dealist[,gcol],1, function(x){ return((gdate[last(which(x==max(x,na.rm=T)))]))})
dealist$maxdate=as.Date(dealist$maxdate)

if (gnperiod == 1) dealist$numna=as.integer(is.na(dealist[,gcol])) else
                   dealist$numna=apply(dealist[,c(gcol)],1,n_na)
dim(dealist)
table(dealist$numna)
table(apply(dealist[,c("m0",gcol)],1,n_na))


#-----------------------------
# print the status 
dealist$status=gstatus[1]
dealist[which(!(dealist$gid %in% glastsku$gid)),"status"]=gstatus[2]
table(dealist$status)
skup$status=gstatus[1]
skup[which(!(skup$gid %in% dealist$gid)),"status"]=gstatus[3]
table(skup$status)
# the offshelf can be more strict

#-----------------------------
# join the price
dealist=left_join(dealist, pricelist, by=c("gid"="gid"))
head(dealist[which(!(dealist$numna.x==dealist$numna.y)),])

#-----------------------------
# define the multiple factor
dealist$mfactor=1
dealist$lastdate=as.integer(dealist$lastdate)
dealist$maxdate=as.integer(dealist$maxdate)
dealist$mfactor=apply(dealist[,c("lastdate","maxdate")],1,function(x){
  if (!is.na(x[1])) return((x[2]-as.integer(last(previous)))/(x[2]-x[1]))
  else return(1)
})
head(dealist[which(!dealist$mfactor==1),])
table(dealist$mfactor)
dealist$lastdate=as.Date(dealist$lastdate)
dealist$maxdate=as.Date(dealist$maxdate)

dealist$ndeals=0
dealist$GMV=0

#=============calculate the Ndeals=================
dealist[,"ndeals"]= apply(dealist[,c(gcolx,"m0","mfactor")],1, 
                          function(x){return((max(x[1:gnperiod],na.rm=T)-sum(x[gnperiod+1],na.rm=T)))})
dealist$ndeals=dealist$ndeals*dealist$mfactor
(test=head(dealist,10))

#==================repare the deals================
for (i in 1:gnperiod){
  if (i==1) dealist[is.na(dealist$m1.x),"m1.x"]=dealist[is.na(dealist$m1.x),"m0"]
  else dealist[is.na(dealist[,gcolx[i]]),gcolx[i]]=dealist[is.na(dealist[,gcolx[i]]),gcolx[i-1]]
}
(test=head(dealist[is.na(dealist$m1.x),],10))
(test=head(dealist[is.na(dealist[,last(gcolx)]),]))

#=============calculate the GMV====================
#m.x ndeals   m.y=price
dealist$GMV=apply(dealist[,c("m0",gcolx,gcoly)],1,function(x){
  GMV=c()
  for (i in 1:gnperiod){
    GMV[i]=(sum(x[i+1],na.rm=T)-sum(x[i],na.rm=T))*x[i+gnperiod+1]
  }
  return(sum(GMV,na.rm=T)/100)  
})
dealist$GMV=dealist$GMV*dealist$mfactor
head(dealist,10)





#---------------------------------------------------------------------------
# likelist: deltaliks
#---------------------------------------------------------------------------

likelist=select(sku,gid,date,likes)
likelist=melt(likelist,id=c("gid","date"))
likelist=dcast(likelist,gid~date)
likelist=left_join(likelist,select(glastsku,gid,date,likes),by=c("gid"="gid"))
names(likelist)=c("gid",gcol,"lastdate","m0")
likelist=select(likelist,gid,m0,starts_with("m"),lastdate)
likelist=left_join(likelist,select(dealist,gid,mfactor),by=c("gid"="gid"))
head(likelist)

if (gnperiod == 1) likelist$numna=as.integer(is.na(likelist[,gcol])) else
  likelist$numna=apply(likelist[,gcol],1,n_na)
likelist$nlikes=0
likelist[,"nlikes"]= apply(likelist[,c("m0",gcol)],1, 
                           function(x){return((last(x[which(!is.na(x))]))-sum(x[1],na.rm=T))})
(test=head(likelist,10))

table(dealist$numna.x)
table(pricelist$numna)
table(likelist$numna)




#==============================================================================================
#4# calculate the ndeals and sku GMV
#==============================================================================================

#---------------------------------------------------------------------------
# skub combine
#---------------------------------------------------------------------------

skub=select(dealist,gid,lastdate,maxdate,status,ndeals,GMV)
skub=left_join(skub,pricelist[,c("gid",last(gcol))],by=c("gid"="gid"))
names(skub)[ncol(skub)]="price"
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
top10=head(as.data.frame(skub[order(desc(skub$GMV)),]),100)
top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption="SKU Deal Number Top10 Table")

write.xlsx(file=gfile,x=skubsumtbl,sheetName = "overview", append = T, showNA = F)
write.xlsx(file=gfile,x=top10,sheetName = "overview top10", append = T, showNA = F)

 



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
statussumtbl$status=as.character(statussumtbl$status)

n=nrow(statussumtbl)+1
statussumtbl[n,]=NA
statussumtbl[n,3:ncol(statussumtbl)]=apply(statussumtbl[1:n-1,3:ncol(statussumtbl)],2,sum)
statussumtbl[n,"status"]="Summary"
statussumtbl[n,"mprice"]=mean(skub$price/100,na.rm=T)
statussumtbl[n,"nstore"]=length(unique(skub$sid))
statussumtbl[n+1,]=NA
statussumtbl[n+1,"status"]="offshelf"
statussumtbl[n+1,"ngoods"]=sum(skup$status==gstatus[3])
statussumtbl$date=last(gdate)

knitr::kable(as.data.frame(statussumtbl),caption="SKU Summary By Status Table")
write.xlsx(file=gfile,x=statussumtbl,sheetName = "status", append = T, showNA = F)

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[1]),],desc(GMV))),100)
top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"SKU Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "classical top10", append = T, showNA = F)

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[2]),],desc(GMV))),100)
top10=left_join(select(top10,gid,ndeals,price,GMV,nlikes,status),
                distinct(select(sku,gid,gname,type2,type3,sname),gid),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"SKU Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "new top10", append = T, showNA = F)


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
n=nrow(typesumtbl)+1
typesumtbl[n,]=NA
typesumtbl[n,"tid2"]="Summary"
typesumtbl[n,3:ncol(typesumtbl)]=apply(typesumtbl[1:n-1,3:ncol(typesumtbl)],2,sum)
typesumtbl[n,"mprice"]=mean(skub$price,na.rm=T)
typesumtbl[n,"nstore"]=length(unique(skub$sid))
typesumtbl$date=last(gdate)



typesumtbl=left_join(typesumtbl,distinct(select(sku,tid2,type2),tid2),by=c("tid2"="tid2"))
knitr::kable(as.data.frame(typesumtbl),caption="SKU Summary By Type Table")
write.xlsx(file=gfile,x=typesumtbl,sheetName = "type", append = T, showNA = F)

typelist=distinct(select(sku,tid2,type2,tid3,type3),tid2,tid3)
typelsit=arrange(typelist,tid2,tid3)
#knitr::kable(typelist,caption="SKU Type List Table")
write.xlsx(file=gfile,x=typelist,sheetName = "typelist", append = T, showNA = F)

#----------------------------------------------------------------------------------------------
#4# overview : store
#4.1 summerize group by
#4.2 mark the status
#----------------------------------------------------------------------------------------------
skub=group_by(skub,sid)
totnewdeals=sum(skub$ndeals)
totGMV=sum(skub$GMV)

if (T){
  storecur=summarise(skub,date=NA,ngoods=n(),goodsratio=n()/nrow(skub),
                     newdeals=sum(ndeals),meannewdeals=mean(ndeals),dealratio=sum(ndeals)/totnewdeals,
                     mprice=mean(price,na.rm=T),GMV=sum(GMV),meanGMV=mean(GMV),GMVratio=sum(GMV)/totGMV,
                     nlikes=sum(nlikes),meannewlikes=mean(nlikes),
                     ntype2=length(unique(tid2)),ntype3=length(unique(tid3))
  )
  
  storecur$date=last(gdate)  
  save(file=paste(last(previous)+1,max(gdate),"store.Rdata",sep="-"),storecur)
} else {
  load(file=paste(last(previous)+1,max(gdate),"store.Rdata",sep="-"))
}

storepre=distinct(select(skup,sid),sid)
# mark the status
storecur$status=gstatus[1]
storecur[which(!(storecur$sid %in% glaststore$sid)),"status"]=gstatus[2]
table(storecur$status)
storepre$status=gstatus[1]
storepre[which(!(storepre$sid %in% storecur$sid)),"status"]=gstatus[3]
table(storepre$status)
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

storecursum$status=as.character(storecursum$status)
n=nrow(storecursum)+1
storecursum[n,]=NA
storecursum[n,3:ncol(storecursum)]=apply(storecursum[1:n-1,3:ncol(storecursum)],2,sum)
storecursum[3,"status"]="Summary"
storecursum[n,"meanndpstore"]=mean(storecur$newdeals,na.rm=T)
storecursum[n,"meanGMV"]=mean(storecur$GMV,na.rm=T)
storecursum[n,"meannewlikes"]=mean(storecur$nlikes,na.rm=T)
storecursum[n+1,]=NA
storecursum[n+1,"status"]="offshelf"
storecursum[n+1,"nstore"]=sum(storepre$status==gstatus[3])
storecursum$date=last(gdate)

#storecur
as.data.frame(storecursum)
write.xlsx(file=gfile,x=storecursum,sheetName = "store", append = T, showNA = F)
#knitr::kable(as.data.frame(storecursum),caption="Store Summary By Status Table")
#knitr::kable(as.data.frame(table(storecur$status)),caption="New Store Distribution In This Term")
#knitr::kable(as.data.frame(table(storepre$status)),caption="Abandoned Store Distribution In Last Term")

top10=head(as.data.frame(storecur[order(desc(storecur$GMV)),]),100)
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="Store GMV Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "store GMV top100", append = T, showNA = F)

storecur=ungroup(storecur)
top10=head(as.data.frame(arrange(storecur[which(storecur$status==gstatus[1]),],desc(GMV))),100)
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="Classical Store GMV Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "classic store GMV top100", append = T, showNA = F)

storecur=ungroup(storecur)
top10=head(as.data.frame(arrange(storecur[which(storecur$status==gstatus[2]),],desc(GMV))),100)
top10=left_join(top10,distinct(select(sku,sid,sname),sid),by=c("sid"="sid"))
knitr::kable(as.data.frame(top10),caption="New Store GMV Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "New store GMV top100", append = T, showNA = F)






#----------------------------------------------------------------------------------------------
#6# overview : haodian
#----------------------------------------------------------------------------------------------
hdeal=select(haodian,sid,date,ndeals=sales)
hdeal=melt(hdeal,id=c("sid","date"))
hdeal=dcast(hdeal,sid~date)
dim(hdeal)
head(hdeal)

#-----------------------------
hdeal=left_join(hdeal,select(glasthd,sid,date,sales),by=c("sid"="sid"))
names(hdeal)=c("sid",gcol,"lastdate","m0")
hdeal=select(hdeal,sid,m0,starts_with("m"),lastdate)

if (gnperiod==1) hdeal$maxdate=gdate else 
  hdeal$maxdate=apply(hdeal[,gcol],1, function(x){return((gdate[last(which(x==max(x,na.rm=T)))]))})
hdeal$maxdate=as.Date(hdeal$maxdate)
head(hdeal)

if (gnperiod == 1) hdeal$numna=as.integer(is.na(hdeal[,gcol])) else
  hdeal$numna=apply(hdeal[,gcol],1,n_na)
table(hdeal$numna)
table(apply(hdeal[,c("m0",gcol)],1,n_na))

#-----------------------------
# print the status 
hdeal$status=gstatus[1]
hdeal[which(!(hdeal$sid %in% glasthd$sid)),"status"]=gstatus[2]
table(hdeal$status)
hdp=filter(glasthd,date %in% previous)
hdp$status=gstatus[1]
hdp[which(!(hdp$sid %in% hdeal$sid)),"status"]=gstatus[3]
table(hdp$status)

hdeal$mfactor=1
hdeal$lastdate=as.integer(hdeal$lastdate)
hdeal$maxdate=as.integer(hdeal$maxdate)
hdeal$mfactor=apply(hdeal[,c("lastdate","maxdate")],1,function(x){
  if (!is.na(x[1])) return((x[2]-as.integer(last(previous)))/(x[2]-x[1]))
  else return(1)
})
head(hdeal[which(!hdeal$mfactor==1),])
hdeal$lastdate=as.Date(hdeal$lastdate)
hdeal$maxdate=as.Date(hdeal$maxdate)
table(hdeal$mfactor)

#=============calculate the Ndeals==============
hdeal$ndeals=0
hdeal[,"ndeals"]= apply(hdeal[,c(gcol,"m0","mfactor")],1, 
                          function(x){return((max(x[1:gnperiod],na.rm=T)-sum(x[gnperiod+1],na.rm=T)))})
hdeal$ndeals=hdeal$ndeals*hdeal$mfactor
(test=head(hdeal[which(!hdeal$mfactor==1), ],10))


#------------------------------------
#nlikes : deltaliks
hdlike=select(haodian,sid,date,likes)
hdlike=melt(hdlike,id=c("sid","date"))
hdlike=dcast(hdlike,sid~date)
hdlike=left_join(hdlike,select(glasthd,sid,date,likes),by=c("sid"="sid"))
names(hdlike)=c("sid",gcol,"lastdate","m0")
hdlike=select(hdlike,sid,m0,starts_with("m"),lastdate)

hdlike=left_join(hdlike,select(hdeal,sid,mfactor),by=c("sid"="sid"))
head(hdlike[which(!hdlike$mfactor==1),])

hdlike$nlikes=0
hdlike[,"nlikes"]= apply(hdlike[,c("m0",gcol)],1, 
                         function(x){return((last(x[which(!is.na(x))]))-sum(x[1],na.rm=T))})
#hdlike$nlikes=hdlike$nlikes*hdlike$mfactor
(head(hdlike[which(!hdeal$mfactor==1),],10))




#------------------------------------
#newgoods
hdgoods=select(haodian,sid,date,ngoods)
hdgoods=melt(hdgoods,id=c("sid","date"))
hdgoods=dcast(hdgoods,sid~date)
hdgoods=left_join(hdgoods,select(glasthd,sid,date,ngoods),by=c("sid"="sid"))
names(hdgoods)=c("sid",gcol,"lastdate","m0")
hdgoods=select(hdgoods,sid,m0,starts_with("m"),lastdate)
hdgoods$newgoods=0
hdgoods$ngoods=0
hdgoods[,"newgoods"]= apply(hdgoods[,c("m0",gcol)],1, 
                             function(x){return((last(x[which(!is.na(x))]))-sum(x[1],na.rm=T))})
hdgoods[,"ngoods"]= apply(hdgoods[,c("m0",gcol)],1, 
                            function(x){return((last(x[which(!is.na(x))])))})

(test=head(hdgoods,10))








#----------------------------------------------------------------------------
#hdb
#----------------------------------------------------------------------------
head(hdeal)
head(hdlike)

hdb=select(hdeal,sid,lastdate,maxdate,status,ndeals)
hdb=left_join(hdb,select(hdlike,sid,nlikes),by=c("sid"="sid"))
hdb=left_join(hdb,select(hdgoods,sid,ngoods,ngoods,newgoods),by=c("sid"="sid"))
hdb=left_join(hdb,distinct(select(haodian,sid,grade,sname),sid),by=c("sid"="sid"))
head(hdb)


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
write.xlsx(file=gfile,x=hdbstatus,sheetName = "haodian status", append = T, showNA = F)

top10=head(as.data.frame(arrange(hdb,desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"Haodian Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "haodian top100", append = T, showNA = F)

top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[1]),],desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"Haodian Old Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "haodian classic top100", append = T, showNA = F)
top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[2]),],desc(ndeals))),100)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"Haodian New Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "haodian New top100", append = T, showNA = F)

#knitr::kable(as.data.frame(table(hdb$status)),caption="Haodian Status New Store Distribution Table")
#knitr::kable(as.data.frame(table(hdp$status)),caption="Haodian Status Abandoned Store Distribution Table")



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
write.xlsx(file=gfile,x=hdbstatus,sheetName = "haodian grade", append = T, showNA = F)

top10=head(as.data.frame(hdb[order(-hdb$grade),]),100)
knitr::kable(as.data.frame(top10),caption="Haodian Grade Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "haodian grade top100", append = T, showNA = F)

