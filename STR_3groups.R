##Interpenetrating subsamples for Stratified random sampling 
###Interpenetrating samples for 3 groups under Stratified random sampling 
dataset <- read.csv( "~/Group_3_for STR.csv")
#Define strata, #6forest, #2Developed.#8Agriculture, #7Grass/Shrub
for (i in 1:ncol(dataset)){
  dataset[,i][dataset[,i]==0] <- 1# Others
  dataset[,i][dataset[,i]==2] <- 3# Developed
  dataset[,i][dataset[,i]==6] <- 2# forest
  dataset[,i][dataset[,i]==8] <- 4# Agriculture
}
data <- data.frame(strat1=c(dataset$map1), strat2=c(dataset$map2), strat3=c(dataset$map3), p1=c(dataset$Int1), p2=c(dataset$Int2),p3=c(dataset$Int3))
n <- 300
m <- 100
strata <- c(dataset$map1,dataset$map2,dataset$map3)
n0 <- length(which(strata==1)) # number of Other pixels in map
n_forest <- length(which(strata==2)) # number of Forest pixels 
n_Dev <- length(which(strata==3))#  number of Developed pixels 
n_Agri <- length(which(strata==4)) # number of Agriculture pixels 
n0; n_forest; n_Dev; n_Agri

##Define variables
a<-3 ##number of subgroup
m0 <- n0/a; m_forest <- n_forest/a; m_Dev <- n_Dev/a; m_Agri <- n_Agri/a # number of pixels in each stratum in subgroup
w1 <- m0/100; w2<- m_forest/100; w3<-m_Dev/100; w4<-m_Agri/100# Weights for each stratum

names1<- c("data1_1", "data1_2", "data1_3","data1_4") # represent the data for Others, developed, forest and angriculture
names2<- c("data2_1", "data2_2", "data2_3","data2_4") #Three subgroups
names3<- c("data3_1", "data3_2", "data3_3","data3_4")
# I make it 51 is because 51 is the largest number for 
#subgourp stratum size, we can also let it >51. this number shoul be least 51
data1 = data.frame(matrix(nrow=51, ncol = length(names1))) 
colnames(data1) = names1
data2 = data.frame(matrix(nrow=51, ncol = length(names2)))
colnames(data2) = names2
data3 = data.frame(matrix(nrow=51, ncol = length(names3)))
colnames(data3) = names3

data1$data1_1 <- c(dataset[which(dataset$map1==1),"Int1"],rep(NA, nrow(data1)-length(dataset[which(dataset$map1==1),"Int1"])))
data1$data1_2 <- c(dataset[which(dataset$map1==2),"Int1"],rep(NA, nrow(data1)-length(dataset[which(dataset$map1==2),"Int1"])))
data1$data1_3 <- c(dataset[which(dataset$map1==3),"Int1"],rep(NA, nrow(data1)-length(dataset[which(dataset$map1==3),"Int1"])))
data1$data1_4 <- c(dataset[which(dataset$map1==4),"Int1"],rep(NA, nrow(data1)-length(dataset[which(dataset$map1==4),"Int1"])))

data2$data2_1 <- c(dataset[which(dataset$map2==1),"Int2"],rep(NA, nrow(data2)-length(dataset[which(dataset$map2==1),"Int2"])))
data2$data2_2 <- c(dataset[which(dataset$map2==2),"Int2"],rep(NA, nrow(data2)-length(dataset[which(dataset$map2==2),"Int2"])))
data2$data2_3 <- c(dataset[which(dataset$map2==3),"Int2"],rep(NA, nrow(data2)-length(dataset[which(dataset$map2==3),"Int2"])))
data2$data2_4 <- c(dataset[which(dataset$map2==4),"Int2"],rep(NA, nrow(data2)-length(dataset[which(dataset$map2==4),"Int2"])))

data3$data3_1 <- c(dataset[which(dataset$map3==1),"Int3"],rep(NA, nrow(data3)-length(dataset[which(dataset$map3==1),"Int3"])))
data3$data3_2 <- c(dataset[which(dataset$map3==2),"Int3"],rep(NA, nrow(data3)-length(dataset[which(dataset$map3==2),"Int3"])))
data3$data3_3 <- c(dataset[which(dataset$map3==3),"Int3"],rep(NA, nrow(data3)-length(dataset[which(dataset$map3==3),"Int3"])))
data3$data3_4 <- c(dataset[which(dataset$map3==4),"Int3"],rep(NA, nrow(data3)-length(dataset[which(dataset$map3==4),"Int3"])))

##setting up n and m dataframe
mn_Info <- data.frame(id=c(1:4), n= c(n0,n_forest,n_Dev,n_Agri), m=c(m0,m_forest,m_Dev,m_Agri), w=c(w1,w2,w3,w4)) 
mat_1 <- matrix(nrow=4,ncol=4) # error matrix for first group (int1)
mat_2 <- matrix(nrow=4,ncol=4) # error matrix for second group (int2)
mat_3 <- matrix(nrow=4,ncol=4) # error matrix for third group (int3)

for (t in 1:4){ #1:4 rows
  for (j in 1:4){ # 1:4 columns
    mat_1[t,j] <- length(which(data1[,t]==j))/mn_Info$m[t]*mn_Info$w[t]#Count the specific value in a given vector, then divided by total m
  }
  #get the row proprortions in error matrix for second subgroup
  for (g in 1:4){ # 1:4 columns
    mat_2[t,g] <- length(which(data2[,t]==g))/mn_Info$m[t]*mn_Info$w[t]#Count the specific value in a given vector, then divided by total m
  }
  #get the row proprortions in error matrix for third subgroup
  for (k in 1:4){ # 1:4 columns
    mat_3[t,k] <- length(which(data3[,t]==k))/mn_Info$m[t]*mn_Info$w[t]#Count the specific value in a given vector, then divided by total m
  }
  
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
  for (i in 1:length(data3[!(is.na(data3[,t])), t]))
  {
    if(data3[i,t] == t){
      data3[i,t] = 1}
    else if(data3[i,t] != t){
      data3[i,t] = 0}
  }
}
#create dataframe to save all four strata information 
names1<- c("data1msw_1", "data1msw_2", "data1msw_3","data1msw_4")
names2<- c("data2msw_1", "data2msw_2", "data2msw_3","data2msw_4")
names3<- c("data3msw_1", "data3msw_2", "data3msw_3","data3msw_4")
data.1.msw = data.frame(matrix(nrow=51, ncol = length(names1)))
colnames(data.1.msw) = names1
data.2.msw = data.frame(matrix(nrow=51, ncol = length(names2)))
colnames(data.2.msw) = names2###
data.3.msw = data.frame(matrix(nrow=51, ncol = length(names3)))
colnames(data.3.msw) = names3###
MSb <- vector()
MSw <- vector()
pho <- vector()
V12 <- vector()
SE_V12 <- vector()
variances <- vector()
SE <- vector()
  
for (l in 1:4){
  ybar.s1 = sum(mat_1[,l])
  ybar.s2 = sum(mat_2[,l])
  ybar.s3 = sum(mat_3[,l])
  ybar.total = (ybar.s1+ybar.s2+ybar.s3)/3
  data.1.msw[,l] = (data1[,l] - ybar.s1)^2
  data.2.msw[,l] = (data2[,l] - ybar.s2)^2
  data.3.msw[,l] = (data3[,l] - ybar.s3)^2
  MSb[l] = (m/(a-1))*(((ybar.s1-ybar.total)^2)+((ybar.s2-ybar.total)^2)+((ybar.s3-ybar.total)^2))
  MSw[l] = (1/(a*(m-1)))*(sum(data.1.msw[!is.na(data.1.msw[,l]),l]) + sum(data.2.msw[!is.na(data.2.msw[,l]),l]) + sum(data.3.msw[!is.na(data.3.msw[,l]),l]))
  pho[l] = ((MSb[l]-MSw[l])/m)/(((MSb[l]-MSw[l])/m)+MSw[l])
  V12[l] = ((m-1)/m) * ((MSb[l] - MSw[l])/n)
  SE_V12[l] = sqrt(abs(V12[l])) #we can not take square root of V12 as V12 can be negative in some cases.take absolute values of V12
  ys1_ys <- ybar.s1 - ybar.total
  ys2_ys <- ybar.s2 - ybar.total
  ys3_ys <- ybar.s3 - ybar.total
  variances[l] <- (1/(a*(a-1)))*(ys1_ys^2+ys2_ys^2+ys3_ys^2)
  SE[l] <- sqrt(variances[l])
}
