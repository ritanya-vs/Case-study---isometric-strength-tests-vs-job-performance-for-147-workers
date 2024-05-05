library(palmerpenguins)
library("scatterplot3d")
library(rgl)
library("plot3D")
library(moments)                 
library(statip)
attach(data)
graphics.off() 



#number of data collected in each test

GRIP_N <- length(data$GRIP)
ARM_N <- length(data$ARM)
RATINGS_N <- length(data$RATINGS)
SIMS_N <- length(data$SIMS)
cat("Number of data collected in GRIP",GRIP_N)
cat("Number of data collected in ARM",ARM_N)
cat("Number of data collected in RATINGS",RATINGS_N)
cat("Number of data collected in SIMS",SIMS_N)

#mean (average) of each survey

GRIP_MEAN <- mean(data$GRIP)
ARM_MEAN <- mean(data$ARM)
RATINGS_MEAN <- mean(data$RATINGS)
SIMS_MEAN <- mean(data$SIMS)
cat("Mean of data collected in GRIP",GRIP_MEAN)
cat("Mean of data collected in ARM",ARM_MEAN)
cat("Mean of data collected in RATINGS",RATINGS_MEAN)
cat("Mean of data collected in SIMS",SIMS_MEAN)

#median of each survey
# Half of the data have values less than the median.

GRIP_MEDIAN <- median(data$GRIP)
ARM_MEDIAN <- median(data$ARM)
RATINGS_MEDIAN <- median(data$RATINGS)
SIMS_MEDIAN <- median(data$SIMS)
cat("Median of data collected in GRIP",GRIP_MEDIAN)
cat("Median of data collected in ARM",ARM_MEDIAN)
cat("Median of data collected in RATINGS",RATINGS_MEDIAN)
cat("Median of data collected in SIMS",SIMS_MEDIAN)

#trimean of each survey(The trimean function calculates the trimean,
#which is the average of the median (middle value) and
#the two quartiles (the 25th and 75th percentiles).
#It provides a measure of central tendency that is
#less sensitive to extreme values than the mean.)

GRIP_TRIMEAN <- trimean(data$GRIP)
ARM_TRIMEAN <- trimean(data$ARM)
RATINGS_TRIMEAN <- trimean(data$RATINGS)
SIMS_TRIMEAN <- trimean(data$SIMS)
cat("Tri-Mean of data collected in GRIP",GRIP_TRIMEAN)
cat("Tri-Mean of data collected in ARM",ARM_TRIMEAN)
cat("Tri-Mean of data collected in RATINGS",RATINGS_TRIMEAN)
cat("Tri-Mean of data collected in SIMS",SIMS_TRIMEAN)

#Minimum of each survey

GRIP_MIN <- min(data$GRIP)
ARM_MIN <- min(data$ARM)
RATINGS_MIN <- min(data$RATINGS)
SIMS_MIN <- min(data$SIMS)
cat("Minimum of data collected in GRIP",GRIP_MIN)
cat("Minimum of data collected in ARM",ARM_MIN)
cat("Minimum of data collected in RATINGS",RATINGS_MIN)
cat("Minimum of data collected in SIMS",SIMS_MIN)

#Maximum of each survey

GRIP_MAX <- max(data$GRIP)
ARM_MAX <- max(data$ARM)
RATINGS_MAX <- max(data$RATINGS)
SIMS_MAX <- max(data$SIMS)
cat("Maximum of data collected in GRIP",GRIP_MAX)
cat("Maximum of data collected in ARM",ARM_MAX)
cat("Maximum of data collected in RATINGS",RATINGS_MAX)
cat("Maximum of data collected in SIMS",SIMS_MAX)

#QUANTILE OF 25TH PERCENTILE OF EACH SURVEY
# 25% of the data have values less than the first quartile 

GRIP_QUANTILE25 <- quantile(data$GRIP,0.25)
ARM_QUANTILE25 <- quantile(data$ARM,0.25)
RATINGS_QUANTILE25 <- quantile(data$RATINGS,0.25)
SIMS_QUANTILE25 <- quantile(data$SIMS,0.25)
cat("quantile(25) of data collected in GRIP",GRIP_QUANTILE25)
cat("quantile(25) of data collected in ARM",ARM_QUANTILE25)
cat("quantile(25) of data collected in RATINGS",RATINGS_QUANTILE25)
cat("quantile(25) of data collected in SIMS",SIMS_QUANTILE25)

#QUANTILE OF 75TH PERCENTILE OF EACH SURVEY
#75% of the data have values less than the third quartile. 

GRIP_QUANTILE75 <- quantile(data$GRIP,0.75)
ARM_QUANTILE75 <- quantile(data$ARM,0.75)
RATINGS_QUANTILE75 <- quantile(data$RATINGS,0.75)
SIMS_QUANTILE75 <- quantile(data$SIMS,0.75)
cat("quantile(75) of data collected in GRIP",GRIP_QUANTILE75)
cat("quantile(75) of data collected in ARM",ARM_QUANTILE75)
cat("quantile(75) of data collected in RATINGS",RATINGS_QUANTILE75)
cat("quantile(75) of data collected in SIMS",SIMS_QUANTILE75)

#Standard deviation of each survey

GRIP_SD <- sd(data$GRIP)
ARM_SD <- sd(data$ARM)
RATINGS_SD <- sd(data$RATINGS)
SIMS_SD <- sd(data$SIMS)
cat("Standard deviation of data collected in GRIP",GRIP_SD)
cat("Standard deviation of data collected in ARM",ARM_SD)
cat("Standard deviation of data collected in RATINGS",RATINGS_SD)
cat("Standard deviation of data collected in SIMS",SIMS_SD)

#Skewness of each survey

GRIP_SKEW <- skewness(data$GRIP)
ARM_SKEW <- skewness(data$ARM)
RATINGS_SKEW <- skewness(data$RATINGS)
SIMS_SKEW <- skewness(data$SIMS)
cat("SKEWNESS of data collected in GRIP",GRIP_SKEW)
cat("SKEWNESS of data collected in ARM",ARM_SKEW)
cat("SKEWNESS of data collected in RATINGS",RATINGS_SKEW)
cat("SKEWNESS of data collected in SIMS",SIMS_SKEW)


#Kurtosis of each survey

GRIP_KURTOSIS <- kurtosis(data$GRIP)
ARM_KURTOSIS <- kurtosis(data$ARM)
RATINGS_KURTOSIS <- kurtosis(data$RATINGS)
SIMS_KURTOSIS <- kurtosis(data$SIMS)
cat("KURTOSIS of data collected in GRIP",GRIP_KURTOSIS)
cat("KURTOSIS of data collected in ARM",ARM_KURTOSIS)
cat("KURTOSIS of data collected in RATINGS",RATINGS_KURTOSIS)
cat("KURTOSIS of data collected in SIMS",SIMS_KURTOSIS)

#function to get value between ranges
createDataFrameInRange <- function(dat, column, min_value, max_value) {
  # Extract the specified column from the data frame
  column_data <- data[[column]]
  
  # Filter the data frame to include only values within the range
  filtered_data <- data[column_data >= min_value & column_data <= max_value, ]
  
  return(filtered_data)
}

filtered_data1 <- createDataFrameInRange(data, "RATINGS", 20, 30)
filtered_data2 <- createDataFrameInRange(data, "RATINGS", 30, 40)
filtered_data3 <- createDataFrameInRange(data, "RATINGS", 40, 50)
filtered_data4 <- createDataFrameInRange(data, "RATINGS", 50, 60)

RATINGS_RANGE <- c(length(filtered_data1$RATINGS),length(filtered_data2$RATINGS),length(filtered_data3$RATINGS),length(filtered_data4$RATINGS))
ranges1 <- c("20 to 30","30 to 40","40 to 50","50 to 60")
colors <- c("black","red","yellow","blue","purple")
barplot(RATINGS_RANGE,names.arg =  ranges1, xlab = "RANGES",ylab ="NUMBER OF OBSERVATIONS", main = "RATINGS",col = colors)

filtered_data5 <- createDataFrameInRange(data, "SIMS", -4, -2)
filtered_data6 <- createDataFrameInRange(data, "SIMS", -2, 0)
filtered_data7 <- createDataFrameInRange(data, "SIMS", 0, 2)
filtered_data8 <- createDataFrameInRange(data, "SIMS", 2, 4)
filtered_data9 <- createDataFrameInRange(data, "SIMS", 4, 6)
ranges2 <- c("-4 to -2","-2 to 0","0 to 2","2 to 4","4 to 6")
SIMS_RANGE <- c(length(filtered_data5$SIMS),length(filtered_data6$SIMS),length(filtered_data7$SIMS),length(filtered_data8$SIMS),length(filtered_data9$SIMS))

barplot(SIMS_RANGE,names.arg =  ranges2, xlab = "RANGES",ylab ="NUMBER OF OBSERVATIONS", main = "SIMS",col = colors)



#covariance and correlation
arms=ARM
sims=SIMS
ratings=RATINGS
grips=GRIP
print(paste("Covariance between arms and sims:  ",cov(arms,sims)))
print(paste("Covariance between arms and ratings:  ",cov(arms,ratings)))
print(paste("Covariance between grips and sims:  ",cov(grips,sims)))
print(paste("Covariance between grips and ratings:  ",cov(grips,ratings)))
print(paste("Correlation between arms and sims:  ",cor(arms,sims)))
print(paste("Correlation between arms and ratings:  ",cor(arms,ratings)))
print(paste("Correlation between grips and sims:  ",cor(grips,sims)))
print(paste("Correlation between grips and ratings:  ",cor(grips,ratings)))

#covariance in matrix
data_subset <- data[, c("ARM", "GRIP", "RATINGS", "SIMS")]
covariance_matrix <- cov(data_subset)
print(covariance_matrix)

#correlation in matrix
data_subset <- data[, c("ARM", "GRIP", "RATINGS", "SIMS")]
correlation_matrix <- cor(data_subset)
print(correlation_matrix)

print(paste("The strongest correlation observed was between arm strength and work simulations (r = .686)"))
print(paste("The weakest relationship was between ratings and work simulations (r = .1681)."))


#spearman correlation
data_subset <- data[, c("ARM", "GRIP", "RATINGS", "SIMS")]
spearman_matrix <- cor(data_subset,method ="spearman")
print(spearman_matrix)

#SHAPIRO - WILK NORMALITY TEST - To test if the variable is normally distributed or not
#Null Hypothesis: The data is normally distributed
#Alternating Hypothesis: The data is not normally distributed
#Assumption: level of significance : 0.05

# Shapiro-Wilk test for Grip
shapiro_grip <- shapiro.test(data$GRIP)
print(shapiro_grip)
print(paste("P-value > 0.05 => GRIP variable is likely normally distributed"))

#Shapiro-Wilk test for Arm
shapiro_arm <- shapiro.test(data$ARM)
print(shapiro_arm)
print(paste("P-value > 0.05 => ARM variable is likely normally distributed."))

#Shapiro-Wilk test for Ratings
shapiro_ratings <- shapiro.test(data$RATINGS)
print(shapiro_ratings)
print(paste("P-value > 0.05 =>RATINGS variable is approximately normally distributed."))

#Shapiro-Wilk test for Sims
shapiro_sims <- shapiro.test(data$SIMS)
print(shapiro_sims)
print(paste("P-value < 0.05 =>SIMS variable is not normally distributed."))



#LINEAR REGRESSION

par(mfcol = c(2,1))
plot(x=ARM,y=SIMS,xlab="arm ",ylab="sims",ylim = c(-12, 12),
     xlim = c(0, 150),       
     main = "arm Vs sims",pch=8,col="red")
abline(lm(SIMS~ARM),col="blue",lwd=1)
plot(x=ARM,y=RATINGS,xlab="arm ",ylab="ratings",ylim = c(0, 60),
     xlim = c(0, 150),       
     main = "arm Vs ratings",pch=8,col="blue")
abline(lm(RATINGS~ARM),col="red",lwd=1)
plot(x=GRIP,y=SIMS,xlab="grip  ",ylab="sims",ylim = c(-8, 8),
     xlim = c(0, 200),       
    main = "grip Vs sims",pch=8,col="red")
abline(lm(SIMS~GRIP),col="blue",lwd=1)
plot(x=GRIP,y=RATINGS,xlab="grip  ",ylab="ratings",ylim = c(0, 60),
     xlim = c(0, 200),       
     main = "grip Vs ratings",pch=8,col="blue")
abline(lm(RATINGS~GRIP),col="red",lwd=1)
par(mfrow = c(1, 1))

print(paste("SIMS is related to ARMS by relation:  y= " ,lm(SIMS~ARM)$coefficients[1]," +  ",lm(SIMS~ARM)$coefficients[2]," * x"))
print(paste("SIMS is related to GRIP by relation:  y= " ,lm(SIMS~GRIP)$coefficients[1]," +  ",lm(SIMS~GRIP)$coefficients[2]," * x"))
print(paste("RATINGS is related to ARMS by relation:  y= " ,lm(RATINGS~ARM)$coefficients[1]," +  ",lm(RATINGS~ARM)$coefficients[2]," * x"))
print(paste("RATINGS is related to GRIP by relation:  y= " ,lm(RATINGS~GRIP)$coefficients[1]," +  ",lm(RATINGS~GRIP)$coefficients[2]," * x"))



# MULTIPLE REGRESSION

par(mfcol = c(1,2))

x=ARM
y=GRIP
z=RATINGS
fit <- lm(z ~ x + y)
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)
scatter3D(x, y, z, pch = 19, cex = 1,colvar = NULL, col="red", 
          theta = 20, phi = 10, bty="b",
          xlab = "ARM", ylab = "GRIP", zlab = "RATINGS",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE, fit = fitpoints, col=ramp.col (col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.9), border="black"), main = "RATINGS vs ARM vs GRIP")
c=fit$coefficients
print(paste("Ratings is related to arm and grip by the relation   z= ",c[1]," + ",c[2]," * x + ",c[3]," * y"))

x=ARM
y=GRIP
z=SIMS
fit <- lm(z ~ x + y)
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)
scatter3D(x, y, z, pch = 19, cex = 1,colvar = NULL, col="red", 
          theta = 20, phi = 10, bty="b",
          xlab = "ARM", ylab = "GRIP", zlab = "SIMS",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE, fit = fitpoints, col=ramp.col (col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.9), border="black"), main = "SIMS vs ARM vs GRIP")
par(mfrow = c(1, 1))
c=fit$coefficients
print(paste("Sims is related to arm and grip by the relation   z= ",c[1]," + ",c[2]," * x + ",c[3]," * y"))


#ANOVA

anova_ratings_as_grouping <- aov(SIMS ~ RATINGS, data=data)
summary_anova=summary(anova_ratings_as_grouping)
p_value=summary_anova[[1]]$`Pr(>F)`[1]
if(p_value<0.05){
  print("Population means of ratings and sims are not equal")
}else{
  print("Population means of ratings and sims are equal")
}

#ANOVA - JOB TYPE
length(GRIP)
job_types <- c("Electrician", "Auto Mechanic", "Lineman")

repeats <- 147 / length(job_types)

# Create the "Job_Type" variable by repeating the job types
Job_Type <- rep(job_types, each = repeats)
length(Job_Type)

# One-way ANOVA for Grip Strength
anova_grip <- aov(GRIP ~ Job_Type, data=data)

# One-way ANOVA for Arm Strength
anova_arm <- aov(ARM ~ Job_Type, data=data)

# One-way ANOVA for Simulation Scores (Sims)
anova_sims <- aov(SIMS ~ Job_Type, data=data)

# One-way ANOVA for Supervisor Ratings (Rating)
anova_rating <- aov(RATINGS ~ Job_Type, data=data)

summary(anova_grip)
summary(anova_arm)
summary(anova_sims)
summary(anova_rating)
