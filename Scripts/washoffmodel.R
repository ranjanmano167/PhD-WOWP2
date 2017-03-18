library(minpack.lm)
library(ggplot2)

setwd('..')

washoff_fun=function(k,c,i,t){
  washoff_frac=c*20*k*(1-exp(-k*i*t/60))
  return(washoff_frac)
}

#method 2 - using nlsLM

int_ser=c(33,47,75,110,155)
washoff_coe_all=NULL

for (j in int_ser) {
  plot(x=NULL,y=NULL,xlim=c(0,60),ylim=c(0,1), main=paste0(j,"mm/hr"),
       xlab = "Duration (min)", ylab = "Fraction washoff (%)")
  data_exp= read.csv(paste0("./1_Data/data_",j,"_200.csv"),header=T)
  washoff_coe=NULL
  washoff_dev=NULL
  
  for (i in 2:ncol(data_exp)){
    df=data.frame(x=data_exp[,1],y = data_exp[,i])
    washoff_fit=nlsLM(y~washoff_fun(k,c,j,x),data=df,start=list(k=0.16,c=0.5),
                      lower = c(0,1), upper = c(1,1))
    points(df)
    curve(washoff_fun(k=coef(washoff_fit)[[1]],c=coef(washoff_fit)[[2]],j,x),from=1, to=60, add=TRUE)
    washoff_coe=rbind(washoff_coe,coef(washoff_fit))
    washoff_dev=rbind(washoff_dev,deviance(washoff_fit))
  }
  #plot(c(16,8,4,2),washoff_coe[,1])
  washoff_coe_all=rbind(washoff_coe_all,washoff_coe[,1])
}

washoff_coe_all=as.data.frame(c(t(washoff_coe_all)), optional = T)
colnames(washoff_coe_all)="k"

#sub setting by slope
washoff_coe_all$Intensity=rep(c(33,47,75,110,155), each = 4)
washoff_coe_all$Slope=rep(c("16%","8%","4%","2%"),times=5)
#k_fit=nlsLM(y~(1-a*exp(b*x)),data=df,start=list(a=0.5,b=0.5),
#            lower = c(0,1), upper = c(1,1))
ggplot(washoff_coe_all, aes(x = Intensity, y = k, linetype=Slope, shape=Slope))+
  geom_point()+
  scale_y_continuous(limits = c(0, 0.1))
  #geom_smooth(formula=y~exp(x), fill=NA, linetype="solid", colour ="black")


#sub setting by intensity
washoff_coe_all$Intensity=rep(c("33 mm/hr","47 mm/hr","75 mm/hr","110 mm/hr","155 mm/hr"), each = 4)
washoff_coe_all$Slope=rep(c(16,8,4,2),times=5)
ggplot(washoff_coe_all, aes(x = Slope, y = k, linetype=Intensity, shape=Intensity))+
  scale_y_continuous(limits = c(0, 0.1))+
  geom_point()+
  geom_smooth(method='lm',formula=y~(a-exp(x)),fill=NA, linetype="solid", colour ="black")

for (i in 1:ncol(data_exp)){
  df=data.frame(x=data_exp[,1],y = data_exp[,i])
  #df=data.frame(x=c(0,5,10,17,24,31,38,45,52,59),y = data_exp[,i]) # 47mm/hr
  washoff_fit=nlsLM(y~washoff_fun(k,c,100,x),data=df,start=list(k=0.16,c=0.5),
                    lower = c(0,1), upper = c(1,1))
  #washoff_fit=nls(y~washoff_fun(k,c,100,x),data=df,start=list(k=0.16,c=1))
  points(df)
  curve(washoff_fun(k=coef(washoff_fit)[[1]],c=coef(washoff_fit)[[2]],100,x),from=1, to=60, add=TRUE)
  washoff_coe=rbind(washoff_coe,coef(washoff_fit))
  washoff_dev=rbind(washoff_dev,deviance(washoff_fit))
}


#fitting a function for k

k_fun=function(a,b,c,i){
  k_func=a+(b*exp(c*i))
  return(k_func)
}

k_coe=NULL
k_dev=NULL
plot(x=NULL,y=NULL,xlim=c(0,160),ylim=c(0,0.1))
for (m in 1:ncol(washoff_coe_all)) {
  df2=data.frame(x=int_ser,y=washoff_coe_all[,m])
  k_fit=nlsLM(y~k_fun(a,b,c,x),data=df2,start=list(a=0.01,b=0.05,c=0.02))
  points(df2)
  #k_fun_vec=Vectorize(k_fun)
  curve(k_fun(coef(k_fit)[[1]],coef(k_fit)[[2]],coef(k_fit)[[3]],x),from=0, to=155, add=TRUE)
  k_coe=rbind(k_coe,coef(k_fit))
  k_dev=rbind(k_dev,deviance(k_fit))
}






# bin
# #method 1 - using optim
# plot(x=NULL,y=NULL,xlim=c(0,60),ylim=c(0,1))
# data_exp= read.csv("data_75_200.csv",header=F)
# washoff_coe=NULL
# 
# for (i in 1:ncol(data_exp)){
#   df=data.frame(x=c(0,2,5,8,13,19,25,31,38,45,52,59),
#                 y = data_exp[,i])
#   washoff_fit=function(param){
#     x=c(0,2,5,8,13,19,25,31,38,45,52,59)
#     y=data_exp[,i]
#     k=param[1]
#     c=param[2]
#     fit=sum((y-c*(1-exp(-k*75*x/60)))^2)
#     return(fit)
#   }
#   coeff=optim(par=c(0.2,0.5),fn=washoff_fit)$par
#   points(df)
#   curve(washoff_fun(k=coeff[1],c=coeff[[2]],100,x),from=1, to=60, add=TRUE)
#   washoff_coe=rbind(washoff_coe, coeff)
# }
