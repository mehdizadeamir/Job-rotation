
#Required libraries
library(rAMPL)
library(ggplot2)
library(dplyr)
library(reshape2)

#Deading data form LiFFT and DUET datasets
d_l <- read.csv("LiFFT.CSV", header = T)
d_d <- read.csv("DUET.CSV", header = T)
dat_l <- data.frame(d_l$Job, d_l$CD, d_l$P, d_l$Injury)
dat_d <- data.frame(d_d$Job, d_d$CD, d_d$P, d_d$Injury)
colnames(dat_l) <- c("Job", "Damage", "Probability", "Injury")
colnames(dat_d) <- c("Job", "Damage", "Probability", "Injury")

#Calling the optimization model from AMPL
ampl <- new(AMPL)
ampl$read("Optimization-model.mod")
ampl$readData("Optimization-model.dat")
ampl$solve()

#Specefiying the number of simulations
numsamples <- 100

#Defining data structures
output_minmax <- array (0, dim = c(numsamples, 10, 5))
output_norot <- array (0, dim = c(numsamples, 10, 5))
desc <- array (0, c(numsamples, 1, 5))
colnames(output_minmax) <- paste("WM", 1:10)
colnames(output_norot) <- paste("W", 1:10)
colnames(desc) <- c("MaxProb")
data.m <- matrix(nrow =20, ncol =1)


#Defining quantile
dat_l$quantile <- with(dat_l, cut(Probability, 
                                breaks=quantile(Probability, probs=seq(0,1, by=0.20), na.rm=TRUE), 
                                include.lowest=TRUE, labels=c("1","2","3","4", "5")))
dat_l$quantile <- as.numeric(dat_l$quantile)

dat_d$quantile <- with(dat_d, cut(Probability, 
                                  breaks=quantile(Probability, probs=seq(0,1, by=0.20), na.rm=TRUE), 
                                  include.lowest=TRUE, labels=c("1","2","3","4", "5")))
dat_d$quantile <- as.numeric(dat_d$quantile)


# Visualization of data distribution on each quntile 
qplot(as.factor(dat_l$quantile), dat_l$Probability) + geom_boxplot() +
  ylab("Injury Probability") + xlab("Class")
qplot(as.factor(dat_d$quantile), dat_d$Probability) + geom_boxplot() +
  ylab("Injury Probability") + xlab("Class")
table(dat_d$quantile)



q <- c(0.40, 0.65, 1)
w <- seq(0.25, 1, by = 0.25)

set.seed(1)
c <- 0
for (m in 1:5) {
  c <- c + 1
  for (i in 1:(numsamples)) { 
    #Filtering jobs for each quntile
    mdata.l <- filter(dat_l, quantile <= m)
    mdata.d <- filter(dat_d, quantile <= m)
    #Picking 10 jobs randomly
    data.l <- sample_n(mdata.l,10)
    data.d <- sample_n(mdata.d,10)
    for (k in 1:10) {
      output_norot[i, k, c] <- ((data.l$Probability[k] + data.d$Probability[k]) -
                                  data.l$Probability[k] * data.d$Probability[k])
    }
    #Selecting the job with maximum injury prob
    m.l <- max(data.l$Probability)
    m.d <-max(data.d$Probability)
    desc[i, 1, c] <- max(m.l, m.d)
    data_j<- data.frame(cbind(data.l[1:10, 2], data.d[1:10, 2]))
    colnames(data_j) <- c("Damage_L", "Damage_D")
    z = 0
    for (x in 1:20) {
      z = z + 1
      if (x %% 2 != 0) {
        data.m[z,1] <- data_j[(z + 1)/2,1]
      }
      if (x %% 2 == 0) {
        data.m[z,1] <- data_j[z/2,2]
      }
    }
    data <- data.frame(data.m)
    colnames(data) <- c("Damage")
    dam_cycle <- ampl$getParameter("dpc")
    dam_cycle_val <- dam_cycle$getValues()
    dam_cycle$setValues(data$Damage)
    ampl$solve()
    prob <- ampl$getVariable("Tprob")
    probval <- prob$getValues()
    for (j in 1: 10){
      output_minmax[i, j, c] <- probval[j, 2]
    }
  }
} 

#Converting the results into a more convenient data structure
om <- data.frame(output_minmax[,,1:5])
on <- data.frame(output_norot[,,1:5])
ds <- data.frame(desc[,,1:5])

results <- data.frame(om, on)
r1 <- data.frame(results[,1:10] - results[,51:60], 
                 results[,11:20] - results[,61:70],
                 results[,21:30] - results[,71:80], 
                 results[,31:40] - results[,81:90],
                 results[,41:50] - results[,91:100])

r2 <- data.frame(sum1 = rowSums(r1[1:10]), 
                 sum2 = rowSums(r1[11:20]), sum3 = rowSums(r1[21:30]),
                 sum4 = rowSums(r1[31:40]), sum5 = rowSums(r1[41:50]))


#calculating the AIPW and ER
sumneg <- function(x) sum(x[x<0]) 
sumpos <- function(y) sum(y[y>0]) 
ra <- data.frame(apply(r1[,1:10],1,sumneg), apply(r1[,1:10],1,sumpos),
                 apply(r1[,11:20],1,sumneg), apply(r1[,11:20],1,sumpos),
                 apply(r1[,21:30],1,sumneg), apply(r1[,21:30],1,sumpos),
                 apply(r1[,31:40],1,sumneg), apply(r1[,31:40],1,sumpos),
                 apply(r1[,41:50],1,sumneg), apply(r1[,41:50],1,sumpos))
colnames(ra) <- paste("R", 1:10)

ratio <- data.frame(abs((ra$`R 1`/ ra$`R 2`)), abs((ra$`R 3`/ ra$`R 4`)),
                    abs((ra$`R 5`/ ra$`R 6`)), abs((ra$`R 7`/ ra$`R 8`)),
                    abs((ra$`R 9`/ ra$`R 10`)))
colnames(ratio) <- paste("Ratio", 1:5)

#visualization
vis <- data.frame(r2)
vis1<- data.frame(ds)
m_ratio <- melt(ratio)
mvis <- melt(vis/10)
mvis1<- melt(vis1)
vismerg_l <- data.frame(mvis, mvis1)
vismerg_ratio_l <- data.frame(m_ratio, mvis1)

#ER
qplot(vismerg_ratio_l$value.1, vismerg_ratio_l$value, data = vismerg_ratio_l) + geom_smooth() +
  labs(y = "ER") +
  labs(x = "Maximum Probability") + 

#AIPW
qplot(vismerg_l$value.1, vismerg_l$value, data = vismerg_l) + 
  geom_smooth() +
  #ggtitle("Efficiency of Job rotation") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(y = "AIPW") +
  labs(x = "Maximum Probability") + 
  theme(legend.text=element_text(size=8), legend.title=element_text(size=9)) 
