mcusum3 <-mult.chart(type = "mcusum", PC,k=2,h=6, phase=1)
B4 = sum(mcusum3$t2 > mcusum3$ucl)

PC_old<-cbind(PC1,PC2,PC3,PC4)

PC_new <- PC_old

for (i in seq(1, 10)) {
 print("----------------")
 print(paste("****Results for ", i, " Iteration****"))
 
 mcusum3 <-mult.chart(type = "mcusum", PC_new,k=1.5, h=6, phase=1)
 
 outliers = mcusum3$t2 > mcusum3$ucl
 print(sum(outliers))
 
 inctrl_index = which(outliers==FALSE)
 
 #removing ooc from data 1st it
 
 PC_1<-PC_new[inctrl_index,]
 
 PC_new <- PC_1
 nrow(PC_new)
}
