##Interpenetrating subsamples for STR
###Interpenetrating samples for USGS Pilot study pairs of 2 interpreters

setwd("E:/Ph.D in ESF/Research Work/Interpenetrating subsampling")
dataset <- read.csv( "Data/PugetSound_2000_Map&7int_4Strata_modified for a=2.csv")
#Define strata, #6forest, #2Developed.#8Agriculture, #7Grass/Shrub
for (i in 1:ncol(dataset)){
  dataset[,i][dataset[,i]==0] <- 1# Others
  dataset[,i][dataset[,i]==2] <- 3# Developed
  dataset[,i][dataset[,i]==6] <- 2# forest
  dataset[,i][dataset[,i]==8] <- 4# Agriculture
}

data <- data.frame(strat=c(dataset$Map2000), p1=c(dataset$Int102_2000), p2=c(dataset$Int103_2000))
n<- 300
m=150
n0 <- length(which(dataset$Map2000==1)) #Check if this number is correct 2000forest,Grass is 155,9, 2010forest is 153
n_forest <- length(which(dataset$Map2000==2)) # Forest
n_Dev <- length(which(dataset$Map2000==3)) # Develoepd
n_Agri <- length(which(dataset$Map2000==4)) # Agriculture
n0; n_forest; n_Dev; n_Agri
##Setting up variables
s=1000   
a<-2
m0<-13; m_forest <- 77; m_Dev <- 39; m_Agri <- 21 # number of pixels in each stratum in subgroup
#w1 <- 0.0670; w2<- 0.5618; w3<-0.2234; w4<-0.1478 ##this numbers are from Bruce Pengra, if we use it it will like poststratified random sampling
w1 <- m0/150; w2<- m_forest/150; w3<-m_Dev/150; w4<-m_Agri/150# Weights for each stratum
#This bumber would be same as we use n0/300, n_forest/300, n_Dev/300 and n_Agri/300

##Setting up empty Vectors/dataframe
variances = data.frame(var_0=c(rep(NA,s)), var_forest=c(rep(NA,s)),var_dev=c(rep(NA,s)),var_agr=c(rep(NA,s)))
SE = data.frame(SE_0=c(rep(NA,s)), SE_forest=c(rep(NA,s)),SE_dev=c(rep(NA,s)),SE_agr=c(rep(NA,s)))
V12 = data.frame(V12_0=c(rep(NA,s)), V12_forest=c(rep(NA,s)),V12_dev=c(rep(NA,s)),V12_agr=c(rep(NA,s)))
SE_V12=data.frame(SEV12_0=c(rep(NA,s)), SEV12_forest=c(rep(NA,s)),SEV12_dev=c(rep(NA,s)),SEV12_agr=c(rep(NA,s))) #we will take square root of absolute V12
MSb = data.frame(MSb_0=c(rep(NA,s)), MSb_forest=c(rep(NA,s)),MSb_dev=c(rep(NA,s)),MSb_agr=c(rep(NA,s)))
MSw = data.frame(MSw_0=c(rep(NA,s)), MSw_forest=c(rep(NA,s)),MSw_dev=c(rep(NA,s)),MSw_agr=c(rep(NA,s)))
p = data.frame(p_0=c(rep(NA,s)), p_forest=c(rep(NA,s)),p_dev=c(rep(NA,s)),p_agr=c(rep(NA,s)))
names1<- c("data1_1", "data1_2", "data1_3","data1_4") # represent the data for Others, developed, forest and angriculture
names2<- c("data2_1", "data2_2", "data2_3","data2_4")
# I make it 77 is because 77 is the largest number for 
#subgourp stratum size, we can also let it >77. this number shoul be least 77
data1 = data.frame(matrix(nrow=77, ncol = length(names1))) 
colnames(data1) = names1
data2 = data.frame(matrix(nrow=77, ncol = length(names2)))
colnames(data2) = names2
##setting up n and m dataframe
mn_Info <- data.frame(id=c(1:4), n= c(n0,n_forest,n_Dev,n_Agri), m=c(m0,m_forest,m_Dev,m_Agri), w=c(w1,w2,w3,w4)) 
mat_1 <- matrix(nrow=4,ncol=4) # error matrix for first int
mat_2 <- matrix(nrow=4,ncol=4) # error matrix for second int
##Starting loop for calculations 
for(r in 1:s)
  ## loop for each repretation s=1000
{
  ##loop for each stratum, total 4 stratum
  for (t in 1:4)
    {
    data_Str <- data[which(data$strat==t),]
    ##Setting up repeating variables within loop
    list = sample(1:mn_Info$n[t], mn_Info$n[t], replace=F) #return the random numbers from 1-size n[t]
    #data1=data2= rep(NA,mn_Info$m[t])
    data.total = rep(NA,mn_Info$n[t])
    c1=c2=c.total=1
    ##After sample data is drawn, assign it to first or second group
    x = rep(NA,mn_Info$n[t])
    for(i in 1:mn_Info$n[t]){
      if(list[i] <=mn_Info$m[t]){
        x[i] = 1}
      if((list[i] >mn_Info$m[t]) & (list[i] <=2*mn_Info$m[t])){
        x[i] = 2}
    }
      for(i in 1:mn_Info$n[t]){
        if(x[i] == 1){
          data1[c1,t] = data_Str$p1[i]#first column
          data.total[i] = data_Str$p1[i]
          # c.total = c.total+1
          c1=c1+1}
        if(x[i] == 2){
          data2[c2,t] = data_Str$p2[i]
          data.total[i] = data_Str$p2[i] #second column
          #c.total = c.total+1
          c2=c2+1}
      }
    #create a 4*4 error matrix
    #get the row proprortions in error matrix for first subgroup
      for (j in 1:4){ # 1:4 columns
        mat_1[t,j] <- length(which(data1[,t]==j))/mn_Info$m[a]*mn_Info$w[a]#Count the specific value in a given vector, then divided by total m
      }
    #get the row proprortions in error matrix for second subgroup
    for (g in 1:4){ # 1:4 columns
      mat_2[t,g] <- length(which(data2[,t]==g))/mn_Info$m[a]*mn_Info$w[a]#Count the specific value in a given vector, then divided by total m
    }
  #  dataRandom1 <- data.frame(data1=c(data1$data1_4), data2=c(data2$data2_4))
  #  write.csv(dataRandom1,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/CheckRandomForest_Int2_3_S4.csv")
    ###make data1 and data2 as 0/1 values so that we can calculate (y̅k1-y̅s1)^2 and (y̅k2-y̅s2)^2
    for (i in 1:length(data1[!(is.na(data1[,t])), t]))
    {
      if(data1[i,t] == t){
        data1[i,t] = 1}
      else if(data1[i,t] != t){
        data1[i,t] = 0}
    }
    for (i in 1:length(data2[!(is.na(data2[,t])), t]))
    {
      if(data2[i,t] == t){
        data2[i,t] = 1}
      else if(data2[i,t] != t){
        data2[i,t] = 0}
    }
    # we have to save all data 1 and data2 for all 4 strata, so the loop should be ended here.
  }
  
  #create dataframe to save all four strata information 
  names1<- c("data1msw_1", "data1msw_2", "data1msw_3","data1msw_4")
  names2<- c("data2msw_1", "data2msw_2", "data2msw_3","data2msw_4")
  data.1.msw = data.frame(matrix(nrow=77, ncol = length(names1)))
  colnames(data.1.msw) = names1
  data.2.msw = data.frame(matrix(nrow=77, ncol = length(names2)))
  colnames(data.2.msw) = names2###
  #starting loop for estiamtion variance and v12, p, msw, msb
  for (l in 1:4){
     ybar.s1 = sum(mat_1[,l])
     ybar.s2 = sum(mat_2[,l])
     ybar.total = (ybar.s1+ybar.s2)/2
     data.1.msw[,l] = (data1[,l] - ybar.s1)^2
     data.2.msw[,l] = (data2[,l] - ybar.s2)^2
     MSb[r,l] = (m/(a-1))*(((ybar.s1-ybar.total)^2)+((ybar.s2-ybar.total)^2))
    # MSb[r,l] = (mn_Info$m[l]/(a-1))*(((ybar.s1-ybar.total)^2)+((ybar.s2-ybar.total)^2))
     MSw[r,l] = (1/(a*(m-1)))*(sum(data.1.msw[!is.na(data.1.msw[,l]),l]) + sum(data.2.msw[!is.na(data.2.msw[,l]),l]))
    # MSw[r,l] = (1/(a*(mn_Info$m[l]-1)))*(sum(data.1.msw[!is.na(data.1.msw[,l]),l]) + sum(data.2.msw[!is.na(data.2.msw[,l]),l]))
     p[r,l] = ((MSb[r,l]-MSw[r,l])/m)/(((MSb[r,l]-MSw[r,l])/m)+MSw[r,l])
    # p[r,l] = ((MSb[r,l]-MSw[r,l])/mn_Info$m[l])/(((MSb[r,l]-MSw[r,l])/mn_Info$m[l])+MSw[r,l])
     V12[r,l] = ((m-1)/m) * ((MSb[r,l] - MSw[r,l])/n)
    # V12[r,l] = ((mn_Info$m[l]-1)/mn_Info$m[l]) * ((MSb[r,l] - MSw[r,l])/mn_Info$n[l])
     SE_V12[r,l] = sqrt(abs(V12[r,l])) #we can not take square root of V12 as V12 can be negative in some cases.take absolute values of V12
     ys1_ys <- ybar.s1 - ybar.total
     ys2_ys <- ybar.s2 - ybar.total
     variances[r,l] <- (1/(a*(a-1)))*(ys1_ys^2+ys2_ys^2)
     SE[r,l] <- sqrt(variances[r,l])
     }
  }
##Calculating final variance
var_mean<-colMeans(variances[sapply(variances, is.numeric)])
SE_mean <- colMeans(SE[sapply(SE, is.numeric)])
V12_mean <- colMeans(V12[sapply(V12, is.numeric)])
SE_V12_mean <- colMeans(SE_V12[sapply(SE_V12, is.numeric)])
p_mean <- colMeans(p[sapply(p, is.numeric)])

StDev_var <- sqrt(sapply(variances, var))
StDev_SEs <- sqrt(sapply(SE, var))
StDev_V12s <- sqrt(sapply(V12, var))
StDev_SEs_V12 <- sqrt(sapply(SE_V12, var))
StDev_p <- sqrt(sapply(p, var))

output <- data.frame(var_mean=c(var_mean),SE_mean=c(SE_mean),V12_mean=c(V12_mean),SE_V12_mean=c(SE_V12_mean),p_mean=c(p_mean),
                     StDev_var=c(StDev_var),StDev_SEs=c(StDev_SEs),StDev_V12s=c(StDev_V12s),StDev_SEs_V12=c(StDev_SEs_V12),
                     StDev_p=c(StDev_p))
write.table(output, 'clipboard', sep='\t', row.names=FALSE, col.names=FALSE)
write.csv(variances,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/New_ResultsForest_2pairs_Int2_3_s1000_Variance.csv")
write.csv(SE,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/New_ResultsForest_2pairs_Int2_3_s1000_SE.csv")
write.csv(V12,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/New_ResultsForest_2pairs_Int2_3_s1000_V12.csv")
write.csv(SE_V12,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/New_ResultsForest_2pairs_Int2_3_s1000_SE_V12.csv")
write.csv(p,"E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Results/STR/New_ResultsForest_2pairs_Int2_3_s1000_p.csv")




