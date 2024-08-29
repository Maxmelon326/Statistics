Assessment Task 2

T1. Understand the data

X1: Temperature in living room area (Celsius degrees)
X2: Humidity in living room area (percentage)
X3: Temperature in office room (Celsius degrees)
X4: Humidity in office room (percentage)
X5: Pressure (millimeter of mercury)
Y: Appliances energy consumption (Wh)
```{r}
# Read data
the.data <- as.matrix(read.table("E:/2-学习/1-Deakin/23-T3/SIT718/Tutorial/ENB_2023.txt"))

# Set the number of samples and columns
num_samples <- nrow(the.data)
num_col <- ncol(the.data)

# Set the size of the subset to be generated
num_row <- 400

# Generate a random subset of samples
sample_rows <- sample(1:num_samples, num_row, replace = FALSE)
my.data <- the.data[sample_rows,2:7]# Select all columns


# Set column names
colnames(my.data) <- c("X1", "X2", "X3", "X4", "X5", "Y")

# Display data
my.data

```

```{r}
# Scatter plots
par(mfrow = c(2, 3))  # Set up a 2x3 layout for scatter plots

# Define deep green color in RGB (you can adjust the values as needed)
deep_green <- rgb(34, 130, 50, maxColorValue = 255)

# Scatter plot for X1 and Y
plot(my.data[, 1], my.data[, 6], main = "Scatter Plot: X1 vs Y", xlab = "X1", ylab = "Y", col=deep_green)

# Scatter plot for X2 and Y
plot(my.data[, 2], my.data[, 6], main = "Scatter Plot: X2 vs Y", xlab = "X2", ylab = "Y", col=deep_green)

# Scatter plot for X3 and Y
plot(my.data[, 3], my.data[, 6], main = "Scatter Plot: X3 vs Y", xlab = "X3", ylab = "Y", col=deep_green)

# Scatter plot for X4 and Y
plot(my.data[, 4], my.data[, 6], main = "Scatter Plot: X4 vs Y", xlab = "X4", ylab = "Y", col=deep_green)

# Scatter plot for X5 and Y
plot(my.data[, 5], my.data[, 6], main = "Scatter Plot: X5 vs Y", xlab = "X5", ylab = "Y", col=deep_green)

# Reset the plotting layout
par(mfrow = c(1, 1))


X1 <- my.data[, 1]
X2 <- my.data[, 2]
X3 <- my.data[, 3]
X4 <- my.data[, 4]
X5 <- my.data[, 5]
Y <- my.data[, 6]

#Calculate correlation between X and Y
cor_X1_Y <- cor(X1, Y)
cor_X2_Y <- cor(X2, Y)
cor_X3_Y <- cor(X3, Y)
cor_X4_Y <- cor(X4, Y)
cor_X5_Y <- cor(X5, Y)

# 打印相关系数
print(paste("Correlation between X1 and Y:", cor_X1_Y))
print(paste("Correlation between X2 and Y:", cor_X2_Y))
print(paste("Correlation between X3 and Y:", cor_X3_Y))
print(paste("Correlation between X4 and Y:", cor_X4_Y))
print(paste("Correlation between X5 and Y:", cor_X5_Y))

```

We also want to know the distribution of variables in order to find proper way to transform data.

```{r}
# Histograms with mean, median, and sd labels
par(mfrow = c(2, 3))  # Set up a 2x3 layout for histograms

# Function to add mean, median, and sd labels to the histogram
add_stats_labels <- function(x, variable_name) {
  hist(x, main = paste("Histogram: ", variable_name), xlab = variable_name,ylab="",col=deep_green)
  
  # Calculate mean, median, and sd
  mean_val <- mean(x)
  median_val <- median(x)
  sd_val <- sd(x)
  
  # Add labels to the histogram
  text(par("usr")[2] - 0.2 * diff(par("usr")[1:2]), par("usr")[4] - 0.1 * diff(par("usr")[3:4]),
       paste("Mean: ", round(mean_val, 2)), cex = 0.7)
  text(par("usr")[2] - 0.2 * diff(par("usr")[1:2]), par("usr")[4] - 0.2 * diff(par("usr")[3:4]),
       paste("Median: ", round(median_val, 2)), cex = 0.7)
  text(par("usr")[2] - 0.2 * diff(par("usr")[1:2]), par("usr")[4] - 0.3 * diff(par("usr")[3:4]),
       paste("SD: ", round(sd_val, 2)), cex = 0.7)
}

# Apply the function to each variable
add_stats_labels(my.data[, 1], "X1")
add_stats_labels(my.data[, 2], "X2")
add_stats_labels(my.data[, 3], "X3")
add_stats_labels(my.data[, 4], "X4")
add_stats_labels(my.data[, 5], "X5")
add_stats_labels(my.data[, 6], "Y")

# Reset the plotting layout
par(mfrow = c(1, 1))



```

We want to see the use K-S test to see whether all the varibles are close to normal distributions. 
```{r}
# Perform K-S test for each variable
ks_test_results <- lapply(list(X1, X2, X3, X4, X5, Y), function(variable) {
  ks_result <- ks.test(variable, "pnorm", mean = mean(variable), sd = sd(variable))
  return(ks_result)
})

# Display the K-S test results
for (i in 1:length(ks_test_results)) {
  variable_name <- c("X1", "X2", "X3", "X4", "X5", "Y")[i]
  cat(sprintf("K-S Test Results for %s:\n", variable_name))
  cat(sprintf("   Statistic: %.4f\n", ks_test_results[[i]]$statistic))
  cat(sprintf("   P-value: %.4f\n", ks_test_results[[i]]$p.value))
  cat(sprintf("   Method: %s\n", ks_test_results[[i]]$method))
  cat("\n")
}



```
```{r}
# Calculate skewness before transformation
selected_columns <- my.data[, c('X1', 'X2', 'X3', 'X4', 'X5', 'Y')]

skewness <- describe(selected_columns)$skew

print(skewness)


```

According to the p-value results of the K-S test, we can see that X2,X3,X4,X5 are more in line with the normal distribution, while X1 and y do not show the characteristics of normal distribution. 

Through scatter plots we can observe if there is a correlation between different X variables and Y, but no obvious linear relationship is found. Considering correlation of the data, we chose x1,x2,x3,x5 to model Y for prediction. In order to reduce the skewness and make attributes in the similar range, we need to do data transformation. We will use log for X1 and Y, Z-score for X2,X3,X5 and min-max for all.

T2. Transform the data
```{r}
# Choose the variables X1, X2, X3, X5,Y
transformed_data <- my.data[, c("X1", "X2", "X3", "X5","Y")]

#Since X1,X2,X3,X5 are more consistent with the normal distribution, we first do the z-score for them. 
#Transformation for X1, X2, X3, X5 (Standardization)
standardization <- function(t) {
 return((t - mean(t)) / sd(t))
  }

#transformed_data[, "X1"] <- standardization(my.data[, "X1"])
transformed_data[, "X2"] <- standardization(my.data[, "X2"])
transformed_data[, "X3"] <- standardization(my.data[, "X3"])
transformed_data[, "X5"] <- standardization(my.data[, "X5"])

# Transformation for Y (log transformation)
transformed_data[, "X1"] <- log(my.data[, "X1"])
transformed_data[, "Y"] <- log(my.data[, "Y"])


# Display data
transformed_data


```

```{r}

#Use Min-Max normalization to make all variables be on the same scale [0,1]
transformed2_data <- transformed_data
# Min-Max normalization function
min_max_normalize <- function(x, negate = FALSE) {
  normalized_value <- (x - min(x)) / (max(x) - min(x))
  if (negate) {
    return(1 - normalized_value)
  } else {
    return(normalized_value)
  }
}

# Apply Min-Max normalization to selected variables
transformed2_data[, "X1"] <- min_max_normalize(transformed_data[, "X1"])
transformed2_data[, "X2"] <- min_max_normalize(transformed_data[, "X2"],negate=TRUE)
transformed2_data[, "X3"] <- min_max_normalize(transformed_data[, "X3"])
transformed2_data[, "X5"] <- min_max_normalize(transformed_data[, "X5"],negate=TRUE)
transformed2_data[, "Y"] <- min_max_normalize(transformed_data[, "Y"])

# Plot normalized variables
plot(transformed2_data[,"X1"])
plot(transformed2_data[,"X2"])
plot(transformed2_data[,"X3"])
plot(transformed2_data[,"X5"])
plot(transformed2_data[,"Y"])


# Save the transformed data to a txt file
write.table(transformed2_data, "E:/2-学习/1-Deakin/23-T3/SIT718/Tutorial/A2-sample-transformed.txt", sep = "\t", row.names = FALSE)

# Display data
print(transformed2_data)



```


```{r} 
# Calculate skewness after transformation
selected_columns <- transformed2_data[, c('X1', 'X2', 'X3', 'X5', 'Y')]

skewness <- describe(selected_columns)$skew

print(skewness)


```
```{r}
# Scatter plots
par(mfrow = c(2, 3))  # Set up a 2x3 layout for scatter plots

# Scatter plot for X1 and Y
plot(transformed2_data[, 1], transformed2_data[, 5], main = "Scatter Plot: X1 vs Y", xlab = "X1", ylab = "Y",col=deep_green)

# Scatter plot for X2 and Y
plot(transformed2_data[, 2], transformed2_data[, 5], main = "Scatter Plot: X2 vs Y", xlab = "X2", ylab = "Y",col=deep_green)

# Scatter plot for X3 and Y
plot(transformed2_data[, 3], transformed2_data[,5], main = "Scatter Plot: X3 vs Y", xlab = "X3", ylab = "Y",col=deep_green)

# Scatter plot for X5 and Y
plot(transformed2_data[, 4], transformed2_data[, 5], main = "Scatter Plot: X5 vs Y", xlab = "X5", ylab = "Y",col=deep_green)

# Reset the plotting layout
par(mfrow = c(1, 1))

X1 <- transformed2_data[, 1]
X2 <- transformed2_data[, 2]
X3 <- transformed2_data[, 3]
X5 <- transformed2_data[, 4]
Y <- transformed2_data[, 5]

#Calculate correlation between X and Y
cor_X1_Y <- cor(X1, Y)
cor_X2_Y <- cor(X2, Y)
cor_X3_Y <- cor(X3, Y)
cor_X5_Y <- cor(X5, Y)

# Print correlation
print(paste("Correlation between X1 and Y:", cor_X1_Y))
print(paste("Correlation between X2 and Y:", cor_X2_Y))
print(paste("Correlation between X3 and Y:", cor_X3_Y))
print(paste("Correlation between X5 and Y:", cor_X5_Y))


```
T3. Build models and investigate the importance of each variable
```{r}
source("E:/2-学习/1-Deakin/23-T3/SIT718/Tutorial/AggWaFit718.R")

```

```{r}
#A weighted arithmetic mean:
fit.QAM(transformed2_data[,c(1:5)],"WAMoutput.txt", "WAMstats.txt")

#Weighted power means (WPM) with p = 0.5
fit.QAM(transformed2_data[,c(1:5)],"PMoutput.txt", "PMstats.txt", g=PM05, g.inv = invPM05)

#Quadratic Mean (Power Mean with p = 2):
fit.QAM(transformed2_data[,c(1:5)],"QMoutput.txt", "QMstats.txt", g=QM, g.inv = invQM)

#Ordered Weighted Average:
fit.OWA(transformed2_data[,c(1:5)],"OWAoutput.txt", "OWAstats.txt")


```

T4. Prediction Model

```{r}
# Given values
given_x1 <- 19.1
given_x2 <- 43.29
given_x3 <- 19.7
given_x4 <- 43.4
given_x5 <- 743.6

# Reversing transformations
# Z-score Normalization
standardization_x2 <- (given_x2 - mean(my.data[, "X2"])) / sd(my.data[, "X2"])
standardization_x3 <- (given_x3 - mean(my.data[, "X3"])) / sd(my.data[, "X3"])
standardization_x5 <- (given_x5 - mean(my.data[, "X5"])) / sd(my.data[, "X5"])

standardization_x1 <- log(given_x1)

# Min-Max Scaling
reversed_x1 <- (standardization_x1 - min(transformed_data[,"X1"])) / (max(transformed_data[,"X1"]) - min(transformed_data[,"X1"]))
reversed_x2 <- 1-((standardization_x2 - min(transformed_data[,"X2"])) / (max(transformed_data[,"X2"]) - min(transformed_data[,"X2"])))
reversed_x3 <- (standardization_x3 - min(transformed_data[,"X3"])) / (max(transformed_data[,"X3"]) - min(transformed_data[,"X3"]))
reversed_x5 <- 1-((standardization_x5 - min(transformed_data[,"X5"])) / (max(transformed_data[,"X5"]) - min(transformed_data[,"X5"])))

#WAM
predicted_y1=0.137551130796078*reversed_x1+0.368556143950835*reversed_x2+0.215330428536255*reversed_x3+0.278562296716829*reversed_x5
predicted_y1


#PM
predicted_y2=0.120117376691837*reversed_x1+0.328744483539326*reversed_x2+0.306106528587957*reversed_x3+0.245031611180879*reversed_x5
predicted_y2

#QM
predicted_y3=0.268154055815913*reversed_x1+0.402115127878086*reversed_x2+0.0640627285620987*reversed_x3+0.265668087743903*reversed_x5
predicted_y3



#OWA
predicted_y4=0.32221612157142*reversed_x1+0.0875625255880658*reversed_x2+0.362965437457791*reversed_x3+0.227255915382721*reversed_x5
predicted_y4



# Reverse Min-Max Scaling
min_orig_y <- min(transformed_data[, 5])
max_orig_y <- max(transformed_data[, 5])


scaled_predicted_y1<- predicted_y1 *(max_orig_y - min_orig_y)+min_orig_y
scaled_predicted_y2<- predicted_y2 *(max_orig_y - min_orig_y)+min_orig_y
scaled_predicted_y3<- predicted_y3 *(max_orig_y - min_orig_y)+min_orig_y
scaled_predicted_y4<- predicted_y4 *(max_orig_y - min_orig_y)+min_orig_y



# Reverse Logarithmic Transformation
original_y1 <- 10^scaled_predicted_y1
original_y2 <- 10^scaled_predicted_y2
original_y3 <- 10^scaled_predicted_y3
original_y4 <- 10^scaled_predicted_y4
original_y1
original_y2
original_y3
original_y4

GAP1=(original_y1-60)/60
GAP2=(original_y2-60)/60
GAP3=(original_y3-60)/60
GAP4=(original_y4-60)/60
formatted_GAP1 <- sprintf("%.2f%%", GAP1 * 100)
formatted_GAP2 <- sprintf("%.2f%%", GAP2 * 100)
formatted_GAP3 <- sprintf("%.2f%%", GAP3 * 100)
formatted_GAP4 <- sprintf("%.2f%%", GAP4 * 100)

cat("GAP1 =", formatted_GAP1, "\n")
cat("GAP2 =", formatted_GAP2, "\n")
cat("GAP3 =", formatted_GAP3, "\n")
cat("GAP4 =", formatted_GAP4, "\n")


```

