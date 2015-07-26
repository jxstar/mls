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


##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1),mar=c(4,4,0,0))
mycolor=rainbow(20)


# about the space
#gc()

#----------------------------------------------------------------------------------------------
#1# read and clean the data
#----------------------------------------------------------------------------------------------
previous=as.Date("2015-07-12")
current=as.Date("2015-07-19")


if (F){
  
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="thdata_user", password="gbgj53GD2s2gy64wRT",host="121.43.197.34",port=3306)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  category=dbGetQuery(conn,"select * from simple_mls_cat where date>=20150712 and date<=(20150719+6)")
  haodian=dbGetQuery(conn,"select * from simple_mls_haodian where date>=20150712 and date<=(20150719+6)")
  sku=dbGetQuery(conn,"select * from simple_mls_sku where date>=20150712 and date<=(20150719+6)")
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
  
  save(file=paste(current,"mls.Rdata"),sku,haodian,catbl)
  
}










#----------------------------------------------------------------------------------------------
#2# divide in to previous and current
#   print identifier with:  classical new offshelf
#----------------------------------------------------------------------------------------------
#define 2 compared time spot
load(file=paste(current,"mls.Rdata"))

#head(sku)
skup=filter(sku,date==previous)
skuc=filter(sku,date==current)

#date check--------------
#dim(skuc)
#dim(distinct(skuc,gid))


# print the marker status
gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf"))
#levels(status[1]);as.integer(status[1])

skuc$status=gstatus[1]
skuc[which(!(skuc$gid %in% skup$gid)),"status"]=gstatus[2]
#table(skuc$status)
skup$status=gstatus[1]
skup[which(!(skup$gid %in% skuc$gid)),"status"]=gstatus[3]
#table(skup$status)

#head(as.data.frame(filter(skuc,status==gstatus[2])),100)

#skub= skubind skuc skup
skub=left_join(select(skuc,date,gid,sales,likes,price,tid2,tid3,sid,status),
               select(skup,gid,sales,likes,price),by=c("gid"="gid"))
skub[which(skub$status==gstatus[2]),c("sales.y","likes.y","price.y")]=0

#head(as.data.frame(skub),10)
skub$newsales=skub$sales.x-skub$sales.y
skub$newlikes=skub$likes.x-skub$likes.y




#----------------------------------------------------------------------------------------------
#3# overview : gid
#----------------------------------------------------------------------------------------------
skubsumtbl=data.frame(date=first(skub$date),ngoods=nrow(skub),
                     totdeals=sum(skub$sales.x),newdeals=sum(skub$newsales),
                     mprice=mean(skub$price.x/100,na.rm=T),GMV=sum((skub$newsales)*skub$price.x/100),
                     totlikes=sum(skub$likes.x),newlikes=sum(skub$newlikes),
                     nmtype=length(unique(skub$tid2)),nstype=length(unique(skub$tid3)),nstore=length(unique(skub$sid)),
                     classical=nrow(filter(skub,status==gstatus[1])),newgoods=nrow(filter(skub,status==gstatus[2])),offshelf=nrow(filter(skup,status==gstatus[3]))
                     )
#skubsumtbl
knitr::kable(as.data.frame(skubsumtbl),caption="SKU Summary Overview Table")


# Deals Top 10 

top10=head(as.data.frame(skub[order(desc(skub$newsales)),]),10)
top10=left_join(select(top10,date,gid,newsales,price.x,newlikes,status),
                select(skuc,gid,gname,type2,type3,sname),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption="SKU Deal Number Top10 Table")
          






#----------------------------------------------------------------------------------------------
#4# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------

skub=group_by(skub,status)
totnewdeals=sum(skub$newsales)
totGMV=sum((skub$newsales)*skub$price.x/100)
statussumtbl=summarise(skub,date=first(date),ngoods=n(),goodsratio=n()/nrow(skub),
                       totdeals=sum(sales.x),newdeals=sum(newsales),dealratio=sum(newsales)/totnewdeals,
                       mprice=mean(price.x/100,na.rm=T),GMV=sum((newsales)*price.x/100),
                       GMVratio=sum((newsales)*price.x/100)/totGMV,
                       totlikes=sum(likes.x),newlikes=sum(likes.x-likes.y), nstore=length(unique(sid))
                       #nmtype=length(unique(tid2)),nstype=length(unique(tid3)))
)


statussumtbl[c(3),]=NA
statussumtbl[3,3:ncol(statussumtbl)]=(statussumtbl[1,3:ncol(statussumtbl)])+statussumtbl[2,3:ncol(statussumtbl)]
#statussumtbl[3,"status"]=factor("tot")
statussumtbl[3,"date"]=first(skub$date)
statussumtbl[3,"mprice"]=mean(skub$price.x/100,na.rm=T)
statussumtbl[3,"nstore"]=length(unique(skub$sid))
#statussumtbl[4,]=data.frame(gstatus[3],date=statussumtbl[1,"date"],ngoods=nrow(filter(skup,status==gstatus[3])),
#                            #ngratio=nrow(filter(skup,status==gstatus[3]))/nrow(skup),
#                            t(rep(NA,(ncol(statussumtbl)-3)))
#                            )

#statussumtbl

knitr::kable(as.data.frame(statussumtbl),caption="SKU Summary By Status Table")

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[1]),],desc(newsales))),10)
top10=left_join(select(top10,date,gid,newsales,price.x,newlikes,status),
                select(skuc,gid,gname,type2,type3,sname),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"SKU Deal Number Top10 Table"))

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[2]),],desc(newsales))),10)
top10=left_join(select(top10,date,gid,newsales,price.x,newlikes,status),
                select(skuc,gid,gname,type2,type3,sname),by=c("gid"="gid"))
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"SKU Deal Number Top10 Table"))



#----------------------------------------------------------------------------------------------
#5# overview : type2     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
skub=group_by(skub,tid2)
totnewdeals=sum(skub$newsales)
totGMV=sum((skub$newsales)*skub$price.x/100)
typesumtbl=summarise(skub,date=first(date),ngoods=n(),goodsratio=n()/nrow(skub),
                       totdeals=sum(sales.x),newdeals=sum(newsales),dealratio=sum(newsales)/totnewdeals,
                       mprice=mean(price.x/100,na.rm=T),GMV=sum((newsales)*price.x/100),
                       GMVratio=sum((newsales)*price.x/100)/totGMV,
                       totlikes=sum(likes.x),newlikes=sum(likes.x-likes.y), nstore=length(unique(sid))
                       #nmtype=length(unique(tid2)),nstype=length(unique(tid3)))
)

n=nrow(typesumtbl)+1
typesumtbl[n,]=NA
typesumtbl[n,3:ncol(typesumtbl)]=apply(typesumtbl[1:n-1,3:ncol(typesumtbl)],2,sum)
#typesumtbl[3,"status"]=factor("tot")
typesumtbl[n,"date"]=first(skub$date)
typesumtbl[n,"mprice"]=mean(skub$price.x/100,na.rm=T)
typesumtbl[n,"nstore"]=length(unique(skub$sid))
#typesumtbl[4,]=data.frame(gstatus[3],date=typesumtbl[1,"date"],ngoods=nrow(filter(skup,status==gstatus[3])),
#                            #ngratio=nrow(filter(skup,status==gstatus[3]))/nrow(skup),
#                            t(rep(NA,(ncol(typesumtbl)-3)))
#                            )
typesumtbl=left_join(typesumtbl,distinct(select(skuc,tid2,type2),tid2,type2),by=c("tid2"="tid2"))
#as.data.frame(typesumtbl)
knitr::kable(as.data.frame(typesumtbl),caption="SKU Summary By Type Table")


typelist=distinct(select(skuc,tid2,type2,tid3,type3),tid2,tid3)
typelsit=arrange(typelist,tid2,tid3)
#typelist
#knitr::kable(typelist,caption="SKU Type List Table")




#----------------------------------------------------------------------------------------------
#5# overview : store
#4.1 summerize group by
#4.2 mark the status
#----------------------------------------------------------------------------------------------
skub=group_by(skub,sid)
totnewdeals=sum(skub$newsales)
totGMV=sum((skub$newsales)*skub$price.x/100)

if (T){
storecur=summarise(skub,date=first(date),ngoods=n(),goodsratio=n()/nrow(skub),
                       totdeals=sum(sales.x),newdeals=sum(newsales),meannewdeals=sum(newsales)/n(),dealratio=sum(newsales)/totnewdeals,
                       mprice=mean(price.x/100,na.rm=T),GMV=sum((newsales)*price.x/100),meanGMV=sum((newsales)*price.x/100)/n(),
                       GMVratio=sum((newsales)*price.x/100)/totGMV,
                       totlikes=sum(likes.x),newlikes=sum(likes.x-likes.y),meannewlikes=sum(likes.x-likes.y)/n()
                       #nstore=length(unique(sid))
                       #nmtype=length(unique(tid2)),nstype=length(unique(tid3)))
)

save(file=paste(current,"store.Rdata"),storecur)
} else {
  load(file=paste(current,"store.Rdata"))
}


skup=group_by(skup,sid)
#storepre=summarise(skub,date=first(date))
storepre=distinct(select(skup,sid),sid)
  

# mark the status
storecur$status=gstatus[1]
storecur[which(!(storecur$sid %in% storepre$sid)),"status"]=gstatus[2]
#table(storecur$status)
storepre$status=gstatus[1]
storepre[which(!(storepre$sid %in% storecur$sid)),"status"]=gstatus[3]
#table(storepre$status)
#head(as.data.frame(filter(storecur,status==gstatus[2])),100)

storecur=group_by(storecur,status)
totngoods=sum(storecur$ngoods)
totnewdeals=sum(storecur$newdeals)
totGMV=sum(storecur$GMV)
storecursum=summarise(storecur,date=first(date),nstore=n(),storeratio=n()/nrow(storecur),
                      ngoods=sum(ngoods),goodsratio=sum(ngoods)/totngoods,
                      totdeals=sum(totdeals),newdeals=sum(newdeals),meanndpstore=sum(newdeals)/n(),dealratio=sum(newdeals)/totnewdeals,
                      GMV=sum(GMV),meanGMV=sum(GMV)/n(),GMVratio=sum(GMV)/totGMV,
                      totlikes=sum(totlikes),newlikes=sum(newlikes),meannewlikes=sum(newlikes)/n()
)

n=nrow(storecursum)+1
storecursum[n,]=NA
storecursum[n,3:ncol(storecursum)]=apply(storecursum[1:n-1,3:ncol(storecursum)],2,sum)
#storecursum[3,"status"]=factor("tot")
storecursum[n,"date"]=first(storecur$date)
storecursum[n,"meanndpstore"]=mean(storecur$newdeals,na.rm=T)
storecursum[n,"meanGMV"]=mean(storecur$GMV,na.rm=T)
storecursum[n,"meannewlikes"]=mean(storecur$newlikes,na.rm=T)

#storecur
#as.data.frame(storecursum)
knitr::kable(as.data.frame(storecursum),caption="Store Summary By Status Table")

knitr::kable(as.data.frame(table(storecur$status)),caption="New Store Distribution In This Term")
knitr::kable(as.data.frame(table(storepre$status)),caption="Abandoned Store Distribution In Last Term")









#----------------------------------------------------------------------------------------------
#6# overview : haodian
#----------------------------------------------------------------------------------------------
#head(haodian)
#n_distinct(haodian$sid)
#n_distinct(skuc$sid)

#head(sku)
hdp=filter(haodian,date==previous)
hdc=filter(haodian,date==current)

#date check--------------
#dim(hdc)
#dim(distinct(hdc,sid))

hdc$status=gstatus[1]
hdc[which(!(hdc$sid %in% hdp$sid)),"status"]=gstatus[2]
#table(hdc$status)
hdp$status=gstatus[1]
hdp[which(!(hdp$sid %in% hdc$sid)),"status"]=gstatus[3]
#table(hdp$status)

#head(as.data.frame(filter(hdc,status==gstatus[2])),100)

#hdb=  haodian bind hdc hdp
hdb=left_join(hdc,select(hdp,sid,ngoods,sales,likes),by=c("sid"="sid"))
hdb[which(hdb$status==gstatus[2]),c("ngoods.y","sales.y","likes.y")]=0

#head(as.data.frame(hdb),10)
hdb$newgoods=hdb$ngoods.x-hdb$ngoods.y
hdb$newsales=hdb$sales.x-hdb$sales.y
hdb$newlikes=hdb$likes.x-hdb$likes.y



#----------------------------------------------------------------------------------------------
#6.2# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
hdb=group_by(hdb,status)
totnewgoods=sum(hdb$newgoods)
totsumgoods=sum(hdb$ngoods.x)
totnewdeals=sum(hdb$newsales)
totnewlikes=sum(hdb$newlikes)
hdbstatus=summarise(hdb,date=first(date),nstore=n(),goodsratio=n()/nrow(hdb),
                    totgoods=sum(ngoods.x),mtotgoods=sum(ngoods.x)/n(),totgoodsratio=sum(ngoods.x)/totsumgoods,
                    newgoods=sum(newgoods),mnewgoods=sum(newgoods)/n(),goodsratio=sum(newgoods)/totnewgoods,
                    totdeals=sum(sales.x),newdeals=sum(newsales),meandeals=sum(newsales)/n(),dealratio=sum(newsales)/totnewdeals,
                    totlikes=sum(likes.x),newlikes=sum(newlikes),meanlikes=sum(newlikes)/n(),likeratio=sum(newlikes)/totnewlikes
)


n=nrow(hdbstatus)+1
hdbstatus[n,]=NA
hdbstatus[n,3:ncol(hdbstatus)]=apply(hdbstatus[1:n-1,3:ncol(hdbstatus)],2,sum)
#hdbstatus[3,"status"]=factor("tot")
hdbstatus[n,"date"]=first(hdb$date)
hdbstatus[n,"mtotgoods"]=mean(hdb$ngoods.x,na.rm=T)
hdbstatus[n,"mnewgoods"]=mean(hdb$newgoods,na.rm=T)
hdbstatus[n,"meandeals"]=mean(hdb$newsales,na.rm=T)
hdbstatus[n,"meanlikes"]=mean(hdb$newlikes,na.rm=T)

#as.data.frame(hdbstatus)
knitr::kable(as.data.frame(hdbstatus),caption="Haodian Summary By Status Table")


top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[1]),],desc(newsales))),10)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"Haodian Old Deal Number Top10 Table"))
top10=head(as.data.frame(arrange(hdb[which(hdb$status==gstatus[2]),],desc(newsales))),10)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"Haodian New Deal Number Top10 Table"))

knitr::kable(as.data.frame(table(hdb$status)),caption="Haodian Status New Store Distribution Table")
knitr::kable(as.data.frame(table(hdp$status)),caption="Haodian Status Abandoned Store Distribution Table")


#range(hdb$grade)
#knitr::kable(as.data.frame(table(cut(hdb$grade,breaks=c(-1,1,400,450,460,470,480,490,500)))),caption=" Investor tot investment distribution Table")
#table(hdb$grade)

#----------------------------------------------------------------------------------------------
#6.3# overview : grade     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
hdb=group_by(hdb,grade)
totnewgoods=sum(hdb$newgoods)
totsumgoods=sum(hdb$ngoods.x)
totnewdeals=sum(hdb$newsales)
totnewlikes=sum(hdb$newlikes)
hdbgrade=summarise(hdb,date=first(date),nstore=n(),nstoreratio=n()/nrow(hdb),
                   totgoods=sum(ngoods.x),mtotgoods=sum(ngoods.x)/n(),totgoodsratio=sum(ngoods.x)/totsumgoods,
                   newgoods=sum(newgoods),mnewgoods=sum(newgoods)/n(),newgoodsratio=sum(newgoods)/totnewgoods,
                   totdeals=sum(sales.x),newdeals=sum(newsales),meandeals=sum(newsales)/n(),dealratio=sum(newsales)/totnewdeals,
                   totlikes=sum(likes.x),newlikes=sum(newlikes),meanlikes=sum(newlikes)/n(),likeratio=sum(newlikes)/totnewlikes
)


n=nrow(hdbgrade)+1
hdbgrade[n,]=NA
hdbgrade[n,3:ncol(hdbgrade)]=apply(hdbgrade[1:n-1,3:ncol(hdbgrade)],2,sum)
#hdbgrade[3,"status"]=factor("tot")
hdbgrade[n,"date"]=first(hdb$date)
hdbgrade[n,"mtotgoods"]=mean(hdb$ngoods.x,na.rm=T)
hdbgrade[n,"mnewgoods"]=mean(hdb$newgoods,na.rm=T)
hdbgrade[n,"meandeals"]=mean(hdb$newsales,na.rm=T)
hdbgrade[n,"meanlikes"]=mean(hdb$newlikes,na.rm=T)

#as.data.frame(hdbgrade)
knitr::kable(as.data.frame(hdbgrade),caption="Haodian Summary By Grade Table")


top10=head(as.data.frame(hdb[order(-hdb$grade),]),10)
knitr::kable(as.data.frame(top10),caption="Haodian Grade Top10 Table")






#----------------------------------------------------------------------------------------------
#7# type list table
#----------------------------------------------------------------------------------------------
knitr::kable(typelist,caption="SKU Type List Table")