## 12/1/2018 KHE
setwd("C:\\Users\\KE\\Desktop\\2018-1201-PARK")

# turn grid into per-student data in an ugly way.
x<-read.csv("2018-1201-SY1819-grid.csv",row.names=1)
a<-apply(x,2,function(col)rep(rownames(x)[!is.na(col)],times=col[!is.na(col)]))
b<-rep(c("K",1:12),times=apply(x,2,sum,na.rm=TRUE))
c<-apply(x,2,function(col)rep((1:20)[!is.na(col)],times=col[!is.na(col)]))



data<-data.frame(level=unlist(a),grade=factor(b,c("K",1:12)),nlevel=unlist(c))
rownames(data)<-NULL
levels(data$level)<-gsub(",","",gsub("\\$","",levels(data$level)))
data$bracketavg<-sapply(levels(data$level),function(x)mean(as.numeric(strsplit(x,split=" - ")[[1]])))[as.numeric(data$level)]
data$bracketavg[is.na(data$bracketavg)]<-6000

# hard coded.
data$actualavg<-c(10582,11518,8351,14170,9777,12626,11271,9660,10823,10887,10649,12511,12039,12019)[as.numeric(data$grade)]


save(data,file="2018-1201-parkgrid18.rdata")

as.matrix(table(data$level)[c(20,19,15,16,17,18,1:14)])

table(data$level,data$grade)[c(20,19,15,16,17,18,1:14),]


## plots to look at.
plot(as.numeric(data$grade),jitter(data$nlevel,factor=2),cex=2,
 axes=FALSE,ann=FALSE,bg=c("#00000055","#0000FF55","#FF000055")[1+(data$nlevel==20)+2*(data$nlevel==1)],pch=21,col=NA)
box()
axis(1,at=c(0:12),labels=c("K",1:12))
axis(2)
title(xlab="Grade Level",ylab="Tuition Level")
tmax<-c(12,14,14,14,14,15,15,16,17,18,19,19,19)
segments(x0=(1:13)-.5,x1=(1:13)+.5,y0=tmax,y1=tmax,lwd=3,col="blue")


plot(as.numeric(data$grade),jitter(data$nlevel,factor=2),cex=2,
 axes=FALSE,ann=FALSE,bg=c("#00000055","#0000FF55","#FF000055")[1+(data$nlevel==20)+2*(data$nlevel==1)],pch=21,col=NA)
box()
axis(1,at=c(0:12),labels=c("K",1:12))
axis(2)
title(xlab="Grade Level",ylab="Tuition Level")
tmax<-c(12,14,14,14,14,15,15,16,17,18,19,19,19)
segments(x0=(1:13)-.5,x1=(1:13)+.5,y0=tmax,y1=tmax,lwd=3,col="blue")
abline(h=median(data$nlevel))
abline(h=mean(data$nlevel))

## Thinking about solutions.

data2<-data.frame(grade=data$grade,nlevel=mean(data$nlevel),bracketavg=mean(data[,4]))
plot(as.numeric(data2$grade),jitter(data2$nlevel,factor=2),cex=2,ylim=c(1,20),
 axes=FALSE,ann=FALSE,bg="#00000055",pch=21,col=NA)
axis(1,at=c(0:12),labels=c("K",1:12))
axis(2)
box()
title(xlab="Grade Level",ylab="Tuition Level",main="Average Strategy")


## barbell
target<-tapply(data[,4],data$grade,sum)
dom<-data[data$nlevel!=20,]
d3<-data[data$nlevel==20,]
d3<-tapply(d3[,4],d3$grade,sum)
d3[is.na(d3)]<-0
intl<-d3
cap<-c(15640,rep(17100,4),c(18170,18580,19890,20440,21770,22870,22870,22870))

out<-
cbind(target,intl,domestic=target-intl,TuitionCap=cap,AllSeats=table(data$grade),
DomSeats=table(dom$grade),
 IntlSeat=table(data$grade)-table(dom$grade),
 FullPayNeeded=ceiling((target-intl)/cap),
 ZeroPay = table(dom$grade)- ceiling((target-intl)/cap))
round(out)

sum(out[,7:9])




data3<-data.frame(grade=factor(rep(rownames(out),times=out[,5]),c("K",1:12)),
   price=unlist(apply(out,1,function(x) rep(c(x[4],0,31500),times=c(x[8],x[9],x[7]) ))),
   nlevel=unlist(apply(cbind(tmax,out[,-1]),1,function(x) rep(c(x[1],6,20),times=c(x[8],x[9],x[7]) )))
  )

plot(as.numeric(data3$grade),jitter(data3$nlevel,factor=2),cex=2,
 axes=FALSE,ann=FALSE,bg=c("#00000055","#0000FF55","#FF000055")[1+(data3$nlevel==20)+2*(data3$nlevel==1)],
  pch=21,col=NA)
box()
axis(1,at=c(1:13),labels=c("K",1:12))
axis(2)
title(xlab="Grade Level",ylab="Tuition Level",main="Barbell Strategy")
tmax<-c(12,14,14,14,14,15,15,16,17,18,19,19,19)
segments(x0=(1:13)-.5,x1=(1:13)+.5,y0=tmax,y1=tmax,lwd=3,col="blue")
text(x=1:13,y=6,out[,9],col=2,cex=2)



plot(data$grade,jitter(data$actualavg))
## verifies it works.

cens<-read.csv("Census QuickFacts Dec-01-2018.csv",row.names=1,nrow=9)#,colClasses="numeric")
colnames(cens)<-gsub("\\.","",colnames(cens))
colnames(cens)<-gsub("CountyNewYork","",colnames(cens))
colnames(cens)<-gsub("townErie","",colnames(cens))
colnames(cens)<-gsub("cityNewYork","",colnames(cens))
colnames(cens)<-gsub("townNiagara","",colnames(cens))
cens<-as.matrix(cens)
class(cens)<-"numeric"
cens<-t(cens)

colnames(cens)

library(Rtsne)
library(circlize)
set.seed(808)
fit<-Rtsne(scale(cens)[,-4],perplex=1)
Y<-fit$Y
rownames(Y)<-rownames(cens)
KM<-kmeans(fit$Y,center=6)$cluster
tapply(rownames(Y),KM,c)
plot(fit$Y,axes=FALSE,ann=FALSE,col=KM)
KM[KM==6]<-4
KM[KM==3]<-1

grad<-colorRamp2(c(-2,0,2),c("purple","grey","orange"))
Z<-scale(cens)
colnames(Z)[8]<-"Language other than English spoken at home"
k<-3
plot(fit$Y,type="n",axes=FALSE,ann=FALSE)
text(fit$Y,label=rownames(Y),xpd=NA,col=grad(Z[,k]))
title(main=colnames(Z)[k])
legend("bottomright",fill=c("orange","purple"),legend=c("High","Low"))

k<-8
plot(fit$Y,type="n",axes=FALSE,ann=FALSE)
text(fit$Y,label=rownames(Y),xpd=NA,col=grad(Z[,k]))
title(main=colnames(Z)[k])
legend("bottomright",fill=c("orange","purple"),legend=c("High","Low"))

k<-9
plot(fit$Y,type="n",axes=FALSE,ann=FALSE)
text(fit$Y,label=rownames(Y),xpd=NA,col=grad(Z[,k]))
title(main=colnames(Z)[k])
legend("bottomright",fill=c("orange","purple"),legend=c("High","Low"))

k<-2
plot(fit$Y,type="n",axes=FALSE,ann=FALSE)
text(fit$Y,label=rownames(Y),xpd=NA,col=grad(Z[,k]))
title(main=colnames(Z)[k])
legend("bottomright",fill=c("orange","purple"),legend=c("High","Low"))


cx<-t(data.frame(cluster=KM,cens))[,order(KM)]
cx<-t(apply(cx[-1,],1,function(x)tapply(x,cx[1,],median)))
rownames(cx)<-colnames(Z)
#colnames(cx)<-nam
cx


