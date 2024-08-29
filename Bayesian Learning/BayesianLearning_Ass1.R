#Q1
the.fulldata <-read.csv("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A1/MelbAirportSolarData.csv",header = TRUE, sep = ",")
my.data <- the.fulldata[sample(1: 8928, 5000), c(1:5)]

# Save the selected data to a txt file
write.table(my.data, "E:/2-学习/1-Deakin/24-T1/SIT743/Task/A1/LIWAN-S223718804-MelbAirSolarMyData.txt", sep = "\t", row.names = FALSE)

#1.2)Draw a histogram and plotbox for the windspeed variable, provide a five number summary
hist(my.data[,3], xlab="ws", main="wind seed",col = "lightblue")
boxplot(my.data[,3], xlab="ws", main="wind speed",col = "lightblue")
summary(my.data[,3])

#1.3)Draw a scatterplot and write the linear regression equation and analysis.
plot(my.data[,2],my.data[,4], xlab="Temperature", ylab="Humidity", main="scatter: Temp vs Humid",col = "lightblue")
cor(my.data[,2],my.data[,4])
#the value is -0.645174 and it means it is moderate negative correlation
coeOfDet = cor(my.data[,2],my.data[,4])^2*100  # in percentage
coeOfDet
# 41.62495% coeff of determination

#linear regression line
lm(my.data[,4]~my.data[,2])
# Add regression line
abline(lm(my.data[,4]~my.data[,2]),col="red")

#1.4) Create 3 new variables as defined 
#write function for bucketing the windspeed
#High if ws > 25 low otherwise
compute.WSB_bucketing <- function(x){
  a = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 25){
      a[i] <- 'WSB_High'
    }
    else {
      a[i] <- 'WSB_Low'
    }
  }
  a
}

if ('WSB' %in% colnames(my.data)) {
  my.data <- my.data[, -which(colnames(my.data) == 'WSB')]
}

my.data <- cbind(my.data, WSB = compute.WSB_bucketing(my.data[,3]))
head(my.data)

#write function for bucketing the Temperature
#high if temp > 30, [20-30] moderate, low otherwise
compute.TB_bucketing <- function(x){
  b = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 30){
      b[i] <- 'TB_High'
    }
    else if (x[i] >= 20 && x[i] <= 30) {
      b[i] <- 'TB_Moderate'
    }
    else{
      b[i] <- 'TB_Low'
    }
  }
  b
}
if ('TB' %in% colnames(my.data)) {
  my.data <- my.data[, -which(colnames(my.data) == 'TB')]
}

my.data <- cbind(my.data, TB = compute.temp_bucketing(my.data[,2]))
head(my.data)

#write function for bucketing the irradiance
#High if ws > 800 low otherwise
compute.IrrB_bucketing <- function(x){
  a = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 800){
      a[i] <- 'IrrB_High'
    }
    else {
      a[i] <- 'IrrB_Low'
    }
  }
  a
}

if ('IrrB' %in% colnames(my.data)) {
  my.data <- my.data[, -which(colnames(my.data) == 'IrrB')]
}

my.data <- cbind(my.data, IrrB = compute.IrrB_bucketing(my.data[,1]))
head(my.data)

#create cross table
ct <- table(my.data$WSB, my.data$TB, my.data$IrrB)
ct


#Q3.3.2.C)
# Load necessary library
library(Bolstad)
library(invgamma)
install.packages("invgamma")
# Data
data <- c(10, 12, 15, 20, 15, 20)
a <- 0.5  # Shape parameter for the prior
b <- 10   # Scale parameter for the prior

# Define likelihood function (Gamma distribution)
likelihood <- function(x, lambda) {
  dgamma(x, shape = 1, rate = 1/lambda)
}

# Define prior function (Inverse-Gamma distribution)
prior <- function(lambda, a, b) {
  dinvgamma(lambda, shape = a, scale = b)
}

# Compute likelihood, prior, and posterior
lambda_values <- seq(0.1, 50, length.out = 100)
likelihood_values <- likelihood(data, lambda_values)
prior_values <- prior(lambda_values, a, b)
posterior_values <- likelihood_values * prior_values

# Plot
plot(lambda_values, likelihood_values, type = "l", col = "blue",
     xlab = "Lambda", ylab = "Density", main = "Likelihood, Prior, and Posterior Distributions")
lines(lambda_values, prior_values, col = "red", lty = 2)
lines(lambda_values, posterior_values, col = "green", lty = 3)
legend("topright", legend = c("Likelihood", "Prior", "Posterior"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3))


#正确答案
#data
dtimes<-c(10, 12, 15, 20, 15, 20)
S<-sum(dtimes);
N<-length(dtimes)
#invgamma hyper param (prior)
a<-0.5
b<-10
#theta values (range)
lambda = seq(from=0.1, to=50, length=10000)
lambda
### prior distribution
# invgamma prior 
plambda = dinvgamma(lambda, shape=a, rate=b)
plambda
hist(plambda)
# Normalize so that values sum to 1
plambda<-plambda/sum(plambda) 
plambda
hist(plambda)
#liklihood
#
likl<- function (lambdav,s,n)
{
  pT<-(1/lambdav^(n)) * exp(-s/lambdav)
}
pDataGivenlambda<-likl(lambda,S,N)
pDataGivenlambda
# Normalize so that values sum to 1
pDataGivenlambda<-pDataGivenlambda/sum(pDataGivenlambda)
#posterior
pData <- sum(pDataGivenlambda*plambda) # marginal probability of the data
pData
plambdaGivenData <- pDataGivenlambda*plambda / pData # Bayes theorem
plambdaGivenData
#plot
colors <- c("black", "blue", "red")
labels <- c("prior", "liklihood", "posterior")
#prior
plot(lambda,plambda, main="Bayesian estimation", axes=TRUE, ylim = c(0,0.0004),lwd=2, lty=c(1, 1, 1, 1, 2), 
     col=colors[1] )
#liklihood
lines(lambda, pDataGivenlambda,lwd=2, col=colors[2])
legend("topright", inset=.005, labels, lwd=2, col=colors)
#posterior
lines(lambda, plambdaGivenData,lwd=2, col=colors[3])
legend("topright", inset=.005, labels, lwd=2, col=colors)


#Q4.c)
## observations
y = (160)
#mu values
mu = seq(50, 250, by = 0.1)
#define the trapezoidal prior
mu.prior = rep(0, length(mu))
mu.prior[mu <= 150] = -1 / 900 + mu[mu <= 150] /45000
#mu.prior[mu>150 & mu<=200] = 7 / 1800 - mu[mu <= 200] /90000#这个条件也错了！
mu.prior[mu>150 & mu<=200] = 7 / 1800 - mu[(mu>150)&(mu <= 200)] /90000

#mu.prior[mu>200] = 1 / 120 - mu[mu > 200] /30000#这个条件错了！
mu.prior[mu>200 & mu<=250]=  1/120-mu[(mu>200)&(mu <= 250)]/30000

#find posterior
results = normgcp(y,10, density = "user", mu = mu, mu.prior = mu.prior)
#plot prior, likelihood and posterior on a single plot
plot(results, overlay = TRUE, which = 1:3)
#plot the above results (prior, likelihood. posterior) in different axes
decomp(results)
#Finding the posterior mean and standard deviation for the above.
## find the posterior mean and std. deviation for the above
mean(results)
var(results)
sd(results)


#Q5
zz<-read.table("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A1/lettersdata.txt")
zz<-as.matrix(zz)

# a)Draw a scatterplot
plot(zz[,1], zz[,2], pch = 16, col = "blue", xlab = "Feature 1", ylab = "Feature 2", main = "Scatterplot of Data")


# b) Perform k-means
## random starts do help here with too many clusters
cl <- kmeans(zz, 4, nstart = 25)
plot(zz, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)# cluster 4 

#c) run for several k value
totwss = array(,c(20,1))
for (i in 2:20)
{
  
  print(i)
  totwss[i,1]=(kmeans(zz,centers=i))$tot.withinss
  print(totwss[i])
} 
plot(totwss, main="total within sum of squres (totWSS) with diiferent K value")

#5.2)
#install.packages("kernlab") create a spiral dataset.
library(kernlab)

#compute similarity matrix
dXX<-as.matrix(dist(zz)) # compute eucleadean distance between datapoints
cParam =1 # parameter of similarity function
S<-exp(-dXX/cParam) #compute similarity matrix
S

#c) Compute affinity matrix using the following code.
# k-nearest neighbor graph.
#modified from: nng() method (https://rdrr.io/cran/cccd/src/R/nng.R
AffMat<-function(S,k) #S-distance matrix and k-no of neighbours
{
  AM <- matrix(0,nrow=nrow(S),ncol=ncol(S))
  for(i in 1:nrow(S)){
    d <- sort(S[i,],decreasing=TRUE)
    for (t in 1:ncol(S))
    {
      if (S[i,t] < d[k])
      {
        AM[i,t]<-0
        AM[t,i]<-0
      }
      else
      {
        AM[i,t] <- S[i,t]
        AM[t,i] <- AM[i,t]
      }
    }
  }
  AM
}
kVal=18# can modify
A<-AffMat(S,kVal)
A

#d) Compute degree of Affinity matrix
D <- diag(apply(A, 1, sum)) # sum rows
D

#e) #compute graph laplasian matrix (un-normalised)
L <- D - A
L

#f) #find eigenvalues and eigenvectors
eigL<-eigen(L)
eigL
eigL$values
plot(eigL$values)

#g) #smallest eigenvalues of L
#smallest eigenvalues of L
k<-4
Z<- eigL$vectors[,(ncol(eigL$vectors)-k+1):ncol(eigL$vectors)]
#plot data using the two eigenvectors
plot(Z)# 

#h) Perform k means clustering
#run kMeans clustering
library(stats)
km <- kmeans(Z, centers=k, nstart=5)
km
plot(Z, col=km$cluster)
plot(zz, col=km$cluster)

#Q6
usadata <-read.csv("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A1/usa.csv",header = TRUE, sep = ",")
head(usadata)

# Select Total Cases and Deaths values when USA State is "Florida"
FL_total_cases <- usadata$Total.Cases[usadata$USA.State == "Florida"]

FL_total_cases

FL_total_deaths <- usadata$Total.Deaths[usadata$USA.State == "Florida"]

FL_total_deaths


# Select Total Cases and Deaths values when USA State is "Arizona"
AZ_total_cases <- usadata$Total.Cases[usadata$USA.State == "Arizona"]

AZ_total_cases

AZ_total_deaths <- usadata$Total.Deaths[usadata$USA.State == "Arizona"]

AZ_total_deaths

################################################
theta= seq(from=0.001, to=1, length=10000)

pTheta1 = dbeta(theta,6000,500000)#Florida
pTheta2 = dbeta(theta,2700,200000)#Arizona

#Florida Number of deaths m = 95,206
#Florida Total number of covide N=8,048,191

#Arizona Number of deaths m = 34,402
#Arizona Total number of covide N=2,607,545

colors <- c("blue", "green")
labels <- c("FL_Beta(6000,500000)", "AZ_Beta(2700,200000)" )

plot(theta,pTheta1, main="Priors", axes=TRUE, ylim = c(0,10),col=colors[1])
#lines(theta, pTheta1, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors[1])
lines(theta, pTheta2,lwd=2, col=colors[2])
legend("topright", inset=.005,
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

###############################################################
#95% Florida credible interval
a=6000+95206
b=500000+8048191-95206
x<-rbeta(1000000, a, b) #randomly draw 1 million
q1<-quantile(x, c(0.025, 0.975))
#95% Arizona credible interval
a=2700+34402
b=200000+2607545-34402
x<-rbeta(10000000, a, b) #randomly draw 1 million
q2<-quantile(x, c(0.025, 0.975))



####################################################################
#Use 'Bolstad' package in R i.e., use "library(Bolstad)" to draw the prior, likelihood and posterior distributions for the above.
library(Bolstad)
#https://cran.r-project.org/web/packages/Bolstad/Bolstad.pdf
#Florida
ap=6000 #prior a
bp=500000 #prior b
NoOfsuccess=95206
Totaltrial=8048191
results<-binobp(NoOfsuccess,Totaltrial,ap,bp); 

# Create a grid of x-values to plot the distributions
x_grid <- seq(0, 1, length.out = 100)#adjust the graph

# Evaluate the prior and posterior distributions
prior_density <- dbeta(x_grid, ap, bp)
posterior_density <- dbeta(x_grid, ap + NoOfsuccess, bp + Totaltrial - NoOfsuccess)

# Plot the prior and posterior distributions on the same plot
par(mfrow = c(1, 1))
plot(x_grid, prior_density, type = "l", col = "blue", xlab = "Infection Fatality Rate", ylab = "Probability Density", main = "Florida Prior and Posterior Distributions")
lines(x_grid, posterior_density, col = "red")
legend("topright", legend = c("Prior", "Posterior"), col = c("blue", "red"), lty = 1)

################
#Arizona
ap=2700 #prior a
bp=200000 #prior b
NoOfsuccess=34402
Totaltrial=2607545
results<-binobp(NoOfsuccess,Totaltrial,ap1,bp); 
# Create a grid of x-values to plot the distributions
x_grid <- seq(0, 1, length.out = 100)

# Evaluate the prior and posterior distributions
prior_density <- dbeta(x_grid, ap, bp)
posterior_density <- dbeta(x_grid, ap + NoOfsuccess, bp + Totaltrial - NoOfsuccess)

# Plot the prior and posterior distributions on the same plot
par(mfrow = c(1, 1))
plot(x_grid, prior_density, type = "l", col = "blue", xlab = "Infection Fatality Rate", ylab = "Probability Density", main = "Arizona Prior and Posterior Distributions")
lines(x_grid, posterior_density, col = "red")
legend("topright", legend = c("Prior", "Posterior"), col = c("blue", "red"), lty = 1)

#plot prior, liklihood and posterior on a single plot
#plot(results, overlay = TRUE, which = 1:3)
#plot the above results (prior, liklihood. posterior) in different axes
#decomp(results)
# Load necessary libraries
library(dplyr)

# Create the summary table
summary_table <- data.frame(
  "Study Regions" = c("Florida", "Arizona"),
  "ASX (m)" = c(95206, 34402),
  "Infection (N)" = c(8048191, 2607545),
  "Prior Beta(a, b)" = c("Beta(6000, 500000)", "Beta(2700, 200000)"),
  "Posterior Beta(a+m, b+N-m)" = c(paste0("Beta(", 6000 + 95206, ", ", 500000 + 8048191 - 95206, ")"),
                                   paste0("Beta(", 2700 + 34402, ", ", 200000 + 2607545 - 34402, ")")),
  "Posterior median (95% credible interval), %" = c(
    paste0(round(median(rbeta(1000000, 6000 + 95206, 500000 + 8048191 - 95206)) * 100, 2),
           " (", round(q1[1] * 100, 2), ", ", round(q1[2] * 100, 2), ")"),
    paste0(round(median(rbeta(1000000, 2700 + 34402, 200000 + 2607545 - 34402)) * 100, 2),
           " (", round(q2[1] * 100, 2), ", ", round(q2[2] * 100, 2), ")")
  )
)

# Display the summary table
summary_table
