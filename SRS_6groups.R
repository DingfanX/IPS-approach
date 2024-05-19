
##Interpenetrating subsamples for SRS
###Interpenetrating samples for 6 groups under simple random sampling

dataset <- read.csv("E:/Ph.D in ESF/Research Work/Interpenetrating subsampling/Manuscript/Submission/Code Example/Groups_6_Binary.csv")
data <- data.frame(p1=c(dataset$Int1), p2=c(dataset$Int2), p3=c(dataset$Int3),p4=c(dataset$Int4),p5=c(dataset$Int5),p6=c(dataset$Int6))
##Setting up variables
n= 300 ##number of total pixels
a<-6  ##number of interpreters
m<-50 ##number of pixels for each group

ybar.s1 = mean(data$p1) 
ybar.s2 = mean(data$p2)
ybar.s3 = mean(data$p3)
ybar.s4 = mean(data$p4)
ybar.s5 = mean(data$p5)
ybar.s6 = mean(data$p6)
ybar.total = mean(as.matrix(data))

data.1.msw = (data$p1 - ybar.s1)^2
data.2.msw = (data$p2 - ybar.s2)^2
data.3.msw = (data$p3 - ybar.s3)^2
data.4.msw = (data$p4 - ybar.s4)^2
data.5.msw = (data$p5 - ybar.s5)^2
data.6.msw = (data$p6 - ybar.s6)^2

ys1_ys <- ybar.s1 - ybar.total
ys2_ys <- ybar.s2 - ybar.total
ys3_ys <- ybar.s3 - ybar.total
ys4_ys <- ybar.s4 - ybar.total
ys5_ys <- ybar.s5 - ybar.total
ys6_ys <- ybar.s6 - ybar.total
sum_ys_ys <- ys1_ys^2 + ys2_ys^2 + ys3_ys^2 + ys4_ys^2 + ys5_ys^2 + ys6_ys^2

#Estimating MSb, MSw
MSb = (m/(a-1))*(sum_ys_ys)
MSw = (1/(a*(m-1)))*(sum(data.1.msw) + sum(data.2.msw) + sum(data.3.msw) + sum(data.4.msw) + sum(data.5.msw) + sum(data.6.msw))
pho = ((MSb-MSw)/m)/(((MSb-MSw)/m)+MSw)

##Estimating V12 
V12 = ((m-1)/m) * ((MSb - MSw)/n) 
SE_V12 = sqrt(abs(V12)) 

##Estimating Interpenetrating subsampling total variances ans SE
variances <- (1/(a*(a-1)))*sum_ys_ys
SE <- sqrt(variances)
