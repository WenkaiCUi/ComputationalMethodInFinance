setwd("C:/Users/cui_w/Desktop/796/HW/HW3")
data<-read.csv("data.csv",head=T)
head(data)
library(stringr)
library(splines)
library(ggplot2)
library(reshape2)
#a
strike<-function(delta,T,sigma,type){
  d1<-rep(0,7)
  for (i in 1:7){
    if (type[i]=="P" | type[i]=="D"){
      d1[i]<-qnorm(-delta[i]+1)
    }
    if (type[i]=="C"){
      d1[i]<- qnorm(delta[i])
    }
  }
  a<-exp(d1*sigma*sqrt(T)-sigma^2/2*T)
  100/a
}

delta<- as.numeric(substr(data[,1],1,2))/100
type<- str_sub(data[,1],-1,-1)

tab1<-strike(delta,1/12,data[,2],type)
tab1<-cbind(tab1,strike(delta,3/12,data[,3],type))
Ktable<-as.data.frame(tab1)
colnames(Ktable)<-c("1M","3M")
#b
inter1M <- approxfun(tab1[,1],data[,2],rule=2 )
inter3M  <- approxfun(tab1[,1],data[,3],rule=2 )

#c
predictsigma<-function(K,T){
  if (T==1){
    p<-inter1M(K) 
  }
  if (T==3){
    p<-inter3M(K)
  }
  p
}

Pcall<-function(T_month,K,sigma="changing"){
  if (sigma=="changing"){
    sigma<-predictsigma(K,T_month)
  }
  T<- T_month/12
  d1<-  1/(sigma*sqrt(T))*(log(100/K)+(sigma)^2/2*T)
  d2<- d1-sigma*sqrt(T)
  nd1<- pnorm(d1)
  nd2<- pnorm(d2)
  nd1*100-nd2*K
}
Pput<-function(T_month,K,sigma="changing"){
  if (sigma=="changing"){
    sigma<-predictsigma(K,T_month)
  }
  T<- T_month/12
  d1<- 1/(sigma*sqrt(T))*(log(100/K)+(sigma)^2/2*T)
  d2<- d1-sigma*sqrt(T)
  nd1<- pnorm(-d1)
  nd2<- pnorm(-d2)
  nd2*K-nd1*100
}
rnp<-function(K,T=1,h=0.01,sigma="changing"){
  
  p<- (Pcall(T,K-h,sigma)-2*Pcall(T,K,sigma)+Pcall(T,K+h,sigma))/h^2

  # if (K<100){
  #   p <- (Pput(T,K-h)-2*Pput(T,K)+Pput(T,K+h))/h^2
  # }
  p
}
s<-seq(50,150,by=0.01)
pdf1M<-as.data.frame(cbind(s,pdf1M=rnp(s,T=1)))
pdf3M<-as.data.frame(cbind(s,pdf3M=rnp(s,T=3)))

#clean data
#pdf1M<-pdf1M[pdf1M[,2]>0 & pdf1M[,2]<0.2,]
#pdf3M<-pdf3M[pdf3M[,2]>0& pdf3M[,2]<0.2,]
for (i in 2:dim(pdf1M)[1]){
  pdf1M[1,2]<- 0
  if (pdf1M[i,2]>0.2){
    pdf1M[i,2]<-pdf1M[i-1,2]
  }
  if (pdf1M[i,2]<0 & pdf1M[i-1,2]>0){
    pdf1M[i,2]<-pdf1M[i-1,2]
  }
  if(pdf1M[i,2]<0){
    pdf1M[i,2]<- 0
  }
}
for (i in 1:dim(pdf3M)[1]){
  
  # if(pdf3M[i,2]<0){
  #   pdf3M[i,2]<- 0
  # }
  if (pdf3M[i,2]>0.2|pdf3M[i,2]<0){
    pdf3M[i,2]<-pdf3M[i-1,2]
  }
}
# change scale
pdf1M[,2]<-pdf1M[,2]*100/sum(pdf1M[,2])
pdf3M[,2]<-pdf3M[,2]*100/sum(pdf3M[,2])

#create dataframe
pdf<-cbind(pdf1M,pdf3M["pdf3M"])
pdf<-melt(pdf,id.vars=c("s"),variable.name = "type",value.name = "pdf")
ggplot(pdf,aes(x=s,y=pdf,col=factor(type)))+
  geom_line()+labs(title="Density with Changing Sigma")

#d
s2<-seq(70,130,by=0.1)
pdf1M.con<- as.data.frame(cbind(s=s2,pdf1M=rnp(s2,T=1,sigma=0.1824)))
pdf3M.con<- as.data.frame(cbind(s=s2,pdf3M=rnp(s2,T=3,sigma=0.1645)))
pdf.con<-cbind(pdf1M.con,pdf3M.con["pdf3M"])
pdf.con<-melt(pdf.con,id.vars=c("s"),variable.name = "type",value.name = "pdf")
ggplot(pdf.con,aes(x=s,y=pdf,col=factor(type)))+
  geom_line()+labs(title="Density with Constant Sigma")

#e  digital price


quad_digput1M<- function(K){
  b<- K
  qua<-0
  for (i in 1:dim(pdf1M)[1]){
    if (pdf1M[i,1]<=K){
      qua<- qua + pdf1M[i,2]*0.01
    }
  }
  qua
}
quad_digcall3M<-function(K){
  a<- K
  qua<-0
  for (i in 1:dim(pdf3M)[1]){
    if (pdf3M[i,1]>=K){
      qua<- qua+pdf3M[i,2]*0.01
    }
  }
  qua
}
quad_eurocall<-function(K){
  a<- K
  qua1M<- 0
  qua3M<- 0
  for (i in 1:dim(pdf1M)[1]){
    if (pdf1M[i,1]>=K){
      qua1M<- qua1M+ (pdf1M[i,1]-K)*pdf1M[i,2]*0.01
    }
  }
  for (i in 1:dim(pdf3M)[1]){
    if (pdf3M[i,1]>=K){
      qua3M<- qua3M+ (pdf3M[i,1]-K)*pdf3M[i,2]*0.01
    }
  }
  (qua1M+qua3M)/2
}
quad_digput1M(110)
quad_digcall3M(105)
quad_eurocall(100)
