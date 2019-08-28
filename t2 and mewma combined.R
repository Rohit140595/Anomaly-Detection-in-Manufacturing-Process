Project_dataset<- read_excel("C:/Users/Admin/Desktop/ISEN614/Project_dataset.xlsx")
str(Project_dataset)
summary(Project_dataset)
pr.out<-prcomp(Project_dataset,scale. = F)
pr.out
names(pr.out)
pr.out$sdev^2 #eigen values
pr.out$center 
pr.out$scale
pr.out$x[,1:4]
plot(pr.out) #scree plot

#not a necessity~lines 14~17
pr.out$rotation
pr.out$rotation <- -pr.out$rotation
pr.out$rotation
plot(pr.out, type = "l") 

(VE <- pr.out$sdev^2)
pve <- VE*100 / sum(VE)
pve <- round(pve, 2) #proportion contribution of each eigen corresponding to PCs
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,100),type='b')

PC1<-as.data.frame(pr.out$x[,1])
PC2<-as.data.frame(pr.out$x[,2])
PC3<-as.data.frame(pr.out$x[,3])
PC4<-as.data.frame(pr.out$x[,4])


#Plotting t2 chart

library(MSQC)
PC_old<-cbind(PC1,PC2,PC3,PC4)


PC_new <- PC_old

for (i in seq(1, 12)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_new,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  mw<-mult.chart(type = "mewma", PC_new, lambda=0.5, phase=1)
  critical_val = mewma.crit(l=0.5,L0=200,p=4)
  outliers = (tsquare$t2 > tsquare$ucl | mw$t2 > critical_val)
  print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}
#46, 31, 22, 12, 8, 3, 3, 1, 1, 1, 1



