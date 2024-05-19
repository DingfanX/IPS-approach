##Interpenetrating subsamples for SRS
###Interpenetrating samples for USGS Pilot study pairs of 2 interpreters

dataset <- read.csv("E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Calculation/Water/2000_Water_Binary.csv")
data <- data.frame(p1=c(dataset$Int109_2000), p2=c(dataset$Int110_2000))

##Setting up variables
n= 300 ##number of pixels
#s=10   ##number of trials, used 10, 100, nd 1000 trials in study
#s=100
s=1000
#s=5000
a<-2
m<-150

##Setting up empty Vectors
variances = rep(NA,s)
SE = rep(NA,s)
V12 = rep(NA,s)
SE_V12=rep(NA,s) #we will take square root of absolute V12
MSb = rep(NA,s)
MSw = rep(NA,s)
p = rep(NA,s)
#SE_V12 =rep(NA,s) #we cannot calculate SE of V12 as V12 can be negative

##Starting loop for calculations
##First loop repeats whole things for nessesary trials
for(j in 1:s){
  #j=1
  ##Setting up repeating variables within loop
  list = sample(1:300, 300, replace=F)
  x = rep(NA,300)
  data1=data2= rep(NA,m)
  data.total = rep(NA,300)
  c1=c2=c.total=1
  
  ##After sample data is drawn, assign it to first or second group
  for(i in 1:n){
    if(list[i] <=m){
      x[i] = 1}
    if((list[i] >m) & (list[i] <=300)){
      x[i] = 2}
  }
  
  for(i in 1:n){
    if(x[i] == 1){
      data1[c1] = data$p1[i]#first column
      data.total[i] = data$p1[i]
     # c.total = c.total+1
      c1=c1+1}
    if(x[i] == 2){
      data2[c2] = data$p2[i]
      data.total[i] = data$p2[i] #second column
      #c.total = c.total+1
      c2=c2+1}
  }
  
#dataRandom1 <- data.frame(data1=c(data1), data2=c(data2))
#write.csv(dataRandom1,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/SRS/CheckRandomWater_Int2_3.csv")
##V12 Calculations
  ybar.s1 = mean(data1)
  ybar.s2 = mean(data2)
  ybar.total = mean(data.total)
  
  data.1.msw = (data1 - ybar.s1)^2
  data.2.msw = (data2 - ybar.s2)^2
  
 # MSb[j] = (1/(m*(a-1)))*(((ybar.s1-ybar.total)^2)+((ybar.s2-ybar.total)^2))
  MSb[j] = (m/(a-1))*(((ybar.s1-ybar.total)^2)+((ybar.s2-ybar.total)^2))
  MSw[j] = (1/(a*(m-1)))*(sum(data.1.msw) + sum(data.2.msw))
  p[j] = ((MSb[j]-MSw[j])/m)/(((MSb[j]-MSw[j])/m)+MSw[j])
  
  V12[j] = ((m-1)/m) * ((MSb[j] - MSw[j])/300)
  SE_V12[j] = sqrt(abs(V12[j])) #we can not take square root of V12 as V12 can be negative in some cases.
  
  
  ##Calculating Interpenetrating variances
  ys1_ys <- ybar.s1 - ybar.total
  ys2_ys <- ybar.s2 - ybar.total
  variances[j] <- (1/(a*(a-1)))*(ys1_ys^2+ys2_ys^2)
  SE[j] <- sqrt(variances[j])

}

##Calculating final variance
var_mean <- mean(variances)
SE_mean <- mean(SE)
V12_mean <- mean(V12)
SE_V12_mean <- mean(SE_V12)
p_mean <- mean(p)

StDev_variances <- sqrt(var(variances))
StDev_SEs <- sqrt(var(SE))
StDev_V12s <- sqrt(var(V12))
StDev_SEs_V12 <- sqrt(var(SE_V12))
StDev_p <- sqrt(var(p))

SE2 <- sqrt(var_mean)


output = c(var_mean,SE_mean,V12_mean,SE_V12_mean,p_mean,StDev_variances,StDev_SEs,StDev_V12s,StDev_SEs_V12,StDev_p,SE2)
results <- data.frame(variances=c(variances), SE=c(SE),V12=c(V12),SE_V12<-c(SE_V12), MSb=c(MSb),MSw=c(MSw),pint=c(p))
write.csv(results,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/SRS/NewMSb_ResultsWater_2pairs_PilotInt9_10_s1000.csv")
write.table(output, 'clipboard', sep='\t', row.names=FALSE, col.names=FALSE)

