#Q1
#1.1)
TempData <-read.csv("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A2/AIMSWaterTempData.csv",header = FALSE, sep = ",")
head(TempData)

#Analyse underwater temperature data
#remove NA
TempData<-TempData[!is.na(TempData)]
TempData

summary(TempData)
hist(TempData)# 3 normal distribution combined  from the histogram

#1.2)Single Gaussian

# Fit a single Gaussian model to the data
singlemdl <- list()
singlemdl$mean <- mean(TempData)
singlemdl$mean
singlemdl$sd <- sd(TempData)
singlemdl$sd
# Generate x values for the density curve
x <- seq(min(TempData), max(TempData), length.out = 100)
# Calculate the density values using dnorm
y <- dnorm(x, mean = singlemdl$mean, sd = singlemdl$sd)

# Plot histogram
hist(TempData, probability = TRUE, main = "Histogram with Single Gaussian Model")
# Add density curve
lines(x, y, col = "red", lwd = 2)



#install.packages("mixtools")
library(mixtools)
#Gaussian mixture
mixmdl = normalmixEM(TempData,k=3) # k components  
mixmdl
summary(mixmdl)
plot(mixmdl,which=2)
#plot(mixmdl, density = TRUE, w = 1.1)#
mixmdl$lambda # 3 lamda
mixmdl$mu# 3 mean
mixmdl$sigma
mixmdl$loglik# based on 3

#plotting the combined curve
x1 <- seq(min(TempData),max(TempData),length=10000)

y = array(0,c(10000,length(mixmdl$lambda)))
for (i in (1:length(mixmdl$lambda)))
{
  y[,i] <- dnorm(x1,mean=mixmdl$mu[i], sd=mixmdl$sigma[i])
}
ycomb=array(0,c(10000,1))
for (j in 1:length(mixmdl$lambda))
{
  ycomb[,1]<-ycomb[,1] + mixmdl$lambda[j]*y[,j]
}
lines(x1,ycomb, col="black", lwd=2, type="l", lty=2) 

######
#plot log liklihood
plot(mixmdl$all.loglik)
plot(mixmdl,which=1)


#Q2
#https://cran.r-project.org/web/packages/ggm/ggm.pdf
#https://www.rdocumentation.org/packages/ggm/versions/2.5/topics/plotGraph

#2.7)install.packages("igraph")
library(igraph)
#install.packages("ggm")
library(ggm)

dag <- DAG( K ~ T+L, A ~ L+H, V ~ W, S ~ K + A + V)
plotGraph(dag, nodesize=20, tcltk=FALSE, vc="white")

#2.5)
#dSep(dag, first="T", second="V", cond=c())
#dSep(dag, first="H", second="W", cond=c("K","S","A"))

#2.6)
dSep(dag, first="A", second="W", cond=c("L","H","S","K","V"))

#2.7)
dSep(dag, first="W", second="K", cond="S")
dSep(dag, first="H", second="V", cond=c("K","A"))


######################################

#2.8)you can use model string to draw bayesian network as well
install.packages(bnlearn)
library(bnlearn)
models = "[T][L][H][W][K|T:L][A|L:H][V|W][S|K:A:V]"
res2 = model2network(models)
res2
plot(res2)

#find the Markov blanket of A 
mb(res2, "A")

#plot the markov blanket of G in the network
graphviz.plot(res2, highlight = list(nodes = mb(res2, "A"), col = "blue", fill = "orange"))

###########################################
#Markov blanket
mb(res2, "K")

#plot the markov blanket of K in the network
graphviz.plot(res2, highlight = list(nodes = mb(res2, "K"), col = "blue", fill = "orange"))


#Q3
#3.5)
#https://www.bioconductor.org/install/
#BiocManager::install(c("gRain", "RBGL", "gRbase"))
#BiocManager::install(c("Rgraphviz"))
library("Rgraphviz")
library(RBGL)
library(gRbase)
library(gRain)
#𝜶=0.3 , 𝜷=0.6,𝝀=0.5,𝜸=0.2,𝝈=0.3.
yn <- c("0","1")
yn1 <- c("0","1","2")
a <- cptable(~A, values=c(0.3,0.7),levels=yn)
b <- cptable(~B, values=c(0.2,0.8),levels=yn)
c.a.b <- cptable(~C|A:B, values=c(0.1,0.4,0.5,0.2,0.5,0.3,0.5,0.2,0.3,0.1,0.1,0.8),levels=yn1)
c.d <- cptable(~D|C, values=c(0.7,0.3,0.3,0.7,0.4,0.6), levels=yn)
c.e <- cptable(~E|C, values=c(0.4,0.6,0.6,0.4,0.8,0.2), levels=yn)


#Compile list of conditional probability tables and create the network:
plist <- compileCPT(list(a, b,c.a.b, c.d, c.e))
plist
plist$A
plist$B
plist$C
plist$D
plist$E


net1 <- grain(plist)
summary(net1)
#plot DAG
plot(net1$dag) 

#gives marginal probability of all nodes
#querygrain(net1)

#The network can be queried to give marginal probabilities of selected nodes:
querygrain(net1, nodes=c("C"), type="marginal")


#Likewise, a joint distribution can be obtained:
querygrain(net1,nodes=c("A","B","C"), type="joint")


#Evidence can be entered as shown below:
net1WithEvidence <- setEvidence(net1,nodes=c("A","D","E"), states=c("0", "1","0"))

#The network can be queried again:
querygrain(net1WithEvidence, nodes=c("C"))


#Q4
#library (bnlearn)
library (bnlearn)
# load the data.
data(alarm)
summary(alarm)
#Sample size
data_subset <- head(alarm, 500)
data_subset1 <- head(alarm, 5000)
data_subset2 <- head(alarm, 10000)

# create and plot the network structure.
modelstring = paste0("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF][LVF]",
                     "[STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR][ANES]",
                     "[APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2][PVS|FIO2:VALV]",
                     "[SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC]",
                     "[MVS][VMCH|MVS][VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG]",
                     "[ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]")
dag = model2network(modelstring)
par(mfrow = c(1,1))
#BiocManager::install(c("Rgraphviz"))
graphviz.plot(dag, shape = "circle")

#4.1) BIC and BDe score
# 500 samples
bnet1 = hc(data_subset, score = "bic")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)

bic_bnet1 <- BIC(bnet1, data_subset)
bic_bnet1 


bnet1 = hc(data_subset, score = "bde")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)

bde_bnet1 <- score(bnet1, data_subset, type = "bde")
bde_bnet1


# 5000 samples
bnet1 = hc(data_subset1, score = "bic")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)

bic_bnet1 <- BIC(bnet1, data_subset1)
bic_bnet1 

bnet1 = hc(data_subset1, score = "bde")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)


bde_bnet1 <- score(bnet1, data_subset1, type = "bde")
bde_bnet1


# 10000 samples
bnet1 = hc(data_subset2, score = "bic")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)

bic_bnet1 <- BIC(bnet1, data_subset2)
bic_bnet1 


bnet1 = hc(data_subset2, score = "bde")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)

bde_bnet1 <- score(bnet1, data_subset2, type = "bde")
bde_bnet1



#4.3)
# full dataset
bnet1 = hc(alarm, score = "bic")#We can change data_subset and score type
bnet1
graphviz.plot(bnet1)
bic_bnet1 <- BIC(bnet1, alarm)
bic_bnet1 



bnet2 = hc(alarm, score = "bde")#We can change data_subset and score type
bnet2
graphviz.plot(bnet2)

bde_bnet1 <- score(bnet2, alarm, type = "bde")
bde_bnet1


#blacklist
#bl = matrix(c("P", "C", "C", "P", "A", "H", "H", "A", "H", "S" ), ncol = 2, byrow = TRUE)
#whitelist
#bnet2 = hc(df, score = "bic", blacklist =bl, whitelist = c("S", "H"))
#bnet2# this is the correct one 
#par(mar = c(5, 5, 2, 2))
#graphviz.plot(bnet2)


#the true positive (tp) arcs, which appear both in target and in current;
#the false positive (fp) arcs, which appear in current but not in target;
#the false negative (fn) arcs, which appear in target but not in current.

#Compare the to the true one. 
comparison1=compare(dag, bnet1,arcs = TRUE)#the first is the target network
comparison1

comparison2=compare(dag, bnet2,arcs = TRUE)#the first is the target network
comparison2

########################
fittedParams = bn.fit(bnet1, alarm,method = "mle")
fittedParams
# Display the fitted parameters for the variable "HR"
fittedParams$HR

########################
cpquery(fittedParams, event=(HR=="HIGH"), evidence= ((BP=="LOW") & (PRSS=="NORMAL")))# every time use different sample.

#Q6
# Read data
CrashData <- read.csv("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A2/Crash_Reporting_-_Drivers_Data.csv", header = TRUE, sep = ",")
head(CrashData) # View the first few rows of the data
sample_count <- nrow(CrashData)
print(sample_count)

# Filter data for the years 2022 and 2023
CrashData_2022_2023 <- CrashData[grep("2022|2023", CrashData$Crash.Date.Time), ]
sample_count <- nrow(CrashData_2022_2023)
print(sample_count) # Print the sample count
head(CrashData_2022_2023, 10) # View the first few rows of the filtered data

# Handle missing values and select variables
na_counts <- colSums(is.na(CrashData_2022_2023))
print(na_counts) # Print the number of missing values per column

# Select required columns
selected_columns <- c("Collision.Type", "Surface.Condition", "Light", "Injury.Severity", "Driver.Distracted.By", "Speed.Limit", "Vehicle.Movement", "Crash.Date.Time", "Driver.Substance.Abuse", "Vehicle.Year", "Route.Type")
CrashData_selected <- CrashData_2022_2023[selected_columns]

# Read weather data
weather <- read.csv("E:/2-学习/1-Deakin/24-T1/SIT743/Task/A2/Montgomery_2022_2023.csv", header = TRUE, sep = ",")
head(weather) # View the first few rows of the weather data
sample_count <- nrow(weather)
print(sample_count) # Print the sample count of the weather data

# Select required columns, including the datetime column
selected_columns_weather <- c("datetime", "temp", "humidity", "windspeed", "visibility", "conditions")
weather_selected <- weather[selected_columns_weather]

# Ensure date formats are consistent and extract date parts
CrashData_selected$Crash.Date <- as.Date(substr(CrashData_selected$Crash.Date.Time, 1, 10), format = "%m/%d/%Y")
weather_selected$Date <- as.Date(weather_selected$datetime, format = "%Y-%m-%d")

# Merge datasets
sample <- merge(CrashData_selected, weather_selected, by.x = "Crash.Date", by.y = "Date", all.x = TRUE)

# View the merged dataset
head(sample) # View the first few rows of the merged data
nrow(sample) # View the number of rows in the merged data

# Convert Speed.Limit into discrete variable
sample$Speed.Limit.Discrete <- cut(sample$Speed.Limit, breaks = c(0, 25, 50, 75), labels = c("Low", "Medium", "High"))

# Convert temp into discrete variable
sample$temp.Discrete <- cut(sample$temp, breaks = c(-Inf, 0, 10, 20, 30, 40, Inf), labels = c("Very Cold", "Cold", "Cool", "Moderate", "Warm", "Hot"))

# Convert humidity into discrete variable
sample$humidity.Discrete <- cut(sample$humidity, breaks = c(0, 40, 60, 80, 100), labels = c("Low", "Moderate", "High", "Very High"))

# Convert windspeed into discrete variable
sample$windspeed.Discrete <- cut(sample$windspeed, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Calm", "Light Breeze", "Moderate Breeze", "Strong Breeze", "High Wind"))

# Convert visibility into discrete variable
sample$visibility.Discrete <- cut(sample$visibility, breaks = c(0, 5, 10, 15, 20), labels = c("Very Low", "Low", "Moderate", "High"))

# Convert Vehicle.Year into a discrete variable
sample$Vehicle.Year.Discrete <- cut(sample$Vehicle.Year, breaks <- c(0, 1950, 1970, 1990, 2010, 2024, 9999), labels <- c("N/A", "1950-1970", "1970-1990", "1990-2010", "2010-2024", "N/A"))


# Select the specified columns
selected_columns <- c("Collision.Type", "Surface.Condition", "Light", "Injury.Severity", "Driver.Distracted.By", "Speed.Limit.Discrete", "Vehicle.Movement", "Driver.Substance.Abuse", "Vehicle.Year.Discrete", "Route.Type", "temp.Discrete", "humidity.Discrete", "windspeed.Discrete", "visibility.Discrete", "conditions")
sample1 <- sample[selected_columns]


# Count the number of rows with at least one N/A or empty value
#na_rows_count <- sum(apply(sample1, 1, function(row) any(is.na(row) | row == "")))

# Display the count
#print(na_rows_count)

# Identify rows with at least one N/A or empty value
sample1[sample1 == "N/A" | sample1 == "" | sample1 == "NULL"] <- NA
na_rows <- apply(sample1, 1, function(row) any(is.na(row) | row == ""))
na_rows_count<-sum(na_rows)
na_rows_count

# Find the na data by variable
na_columns <- apply(sample1, 2, function(col) any(is.na(col)))
names_with_na <- names(sample1)[na_columns]
print(names_with_na)
na_counts <- colSums(is.na(sample1))
print(na_counts)

# Remove rows with at least one N/A or empty value
sample1<- sample1[!na_rows, ]

# Display the cleaned dataframe
head(sample1)
nrow(sample1) 



# Exploratory analysis on the variables
# Load necessary libraries
library(ggplot2)
# Generate summary statistics for all selected variables
summary_stats <- summary(sample1)
print(summary_stats)
# Define a function to create bar plots for categorical variables
create_bar_plot <- function(data, variable, title) {
  ggplot(data, aes_string(x = variable)) + 
    geom_bar() + 
    ggtitle(title) + 
    xlab(variable) + 
    ylab("Count") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# List of categorical variables to plot
categorical_vars <- c("Collision.Type", "Surface.Condition", "Light", "Injury.Severity", "Driver.Distracted.By", "Vehicle.Movement", "Driver.Substance.Abuse", "Vehicle.Year.Discrete", "Route.Type", "conditions")

# Create bar plots for all categorical variables
for (var in categorical_vars) {
  print(create_bar_plot(sample1, var, paste("Bar Plot of", var)))
}
# Define a function to create histogram plots for discrete numeric variables
create_histogram_plot <- function(data, variable, title) {
  ggplot(data, aes_string(x = variable)) + 
    geom_bar() + 
    ggtitle(title) + 
    xlab(variable) + 
    ylab("Frequency")
}

# List of discrete numeric variables to plot
discrete_numeric_vars <- c("Speed.Limit.Discrete", "temp.Discrete", "humidity.Discrete", "windspeed.Discrete", "visibility.Discrete")

# Create histogram plots for all discrete numeric variables
for (var in discrete_numeric_vars) {
  print(create_histogram_plot(sample1, var, paste("Histogram of", var)))
}



library(bnlearn)
# Convert 'Collision.Type' to a factor
sample1$Collision.Type <- as.factor(sample1$Collision.Type)
# Convert 'Surface.Condition' to a factor
sample1$Surface.Condition <- as.factor(sample1$Surface.Condition)
# Convert 'Light' to a factor
sample1$Light <- as.factor(sample1$Light)

# Convert 'Injury.Severity' to a factor
sample1$Injury.Severity <- as.factor(sample1$Injury.Severity)
# Convert 'Driver.Distracted.By' to a factor
sample1$Driver.Distracted.By <- as.factor(sample1$Driver.Distracted.By)
# Convert 'Vehicle.Movement' to a factor
sample1$Vehicle.Movement <- as.factor(sample1$Vehicle.Movement)
# Convert 'Driver.Substance.Abuse' to a factor
sample1$Driver.Substance.Abuse <- as.factor(sample1$Driver.Substance.Abuse)
# Convert 'Route.Type' to a factor
sample1$Route.Type <- as.factor(sample1$Route.Type)
# Convert 'conditions' to a factor
sample1$conditions <- as.factor(sample1$conditions)

# Perform structure learning using the Hill-Climbing algorithm
hc_net <- hc(sample1)
graphviz.plot(hc_net)

# Perform structure learning using the Tabu Search algorithm
tabu_net <- tabu(sample1)
graphviz.plot(tabu_net)

# Perform structure learning using the Max-Min Hill-Climbing algorithm
mmhc_net <- mmhc(sample1)
graphviz.plot(mmhc_net)


# Load the necessary library
library(bnlearn)
#library(Rgraphviz)
#install.packages("pcalg")
#library(pcalg)

# Perform parameter learning for the three networks
hc_fitted <- bn.fit(hc_net, data = sample1,)
tabu_fitted <- bn.fit(tabu_net, data = sample1)
mmhc_fitted <- bn.fit(mmhc_net, data = sample1)
hc_fitted
tabu_fitted
mmhc_fitted

# Compare the models using BIC
bic_hc <- BIC(hc_fitted, sample1)
bic_tabu <- BIC(tabu_fitted, sample1)
bic_mmhc <- BIC(mmhc_fitted, sample1)

bic_comparison <- data.frame(Model = c("HC", "Tabu", "MMHC"), BIC = c(bic_hc, bic_tabu, bic_mmhc))
print(bic_comparison)


#Compute the probability
########################
fittedParams = bn.fit(hc_net, sample1,method = "mle")
fittedParams


########################
cpquery(fittedParams, event=(Injury.Severity=="POSSIBLE INJURY"), evidence= ((Surface.Condition=="WET") ))# every time use different sample.

#dSep(hc_net, first="Injury.Severity", second="Speed.Limit.Discrete", cond=c())
# Test d-separation between "Injury.Severity" and "Speed.Limit.Discrete", given no other nodes
#independence <- dsep(hc_net, x = c("Injury.Severity"), y = c("Speed.Limit.Discrete"), z = character(0))

# Print the result
print(independence)

#find the Markov blanket of Driver Distracted By 
mb(hc_net, "Driver.Distracted.By")
#plot the markov blanket of G in the network
graphviz.plot(hc_net, highlight = list(nodes = mb(mmhc_net, "Driver.Distracted.By"), col = "blue", fill = "orange"))


