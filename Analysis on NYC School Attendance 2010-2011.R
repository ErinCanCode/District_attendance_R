#Set working directory and load data
setwd("C:/Users/Erin/Documents/Masters Program/R Directories")
school_data <- read.csv("2010_-_2011_School_Attendance_and_Enrollment_Statistics_by_District.csv")
#Review/Preview data in R
head(school_data)
tail(school_data)
#Boxplot to identify potential outliers
boxplot(school_data$YTD...Attendance..Avg.)
boxplot(school_data$YTD.Enrollment.Avg.)
#Identify location of outliers
OutVals <- boxplot(school_data$YTD...Attendance..Avg.)$out
which(school_data$YTD...Attendance..Avg. %in% OutVals)
OutVals <- boxplot(school_data$YTD.Enrollment.Avg.)$out
which(school_data$YTD.Enrollment.Avg. %in% OutVals)
#Subset data to exclude outliers
district_data <- school_data[-c(33:35), c(1:3)]
tail(district_data)
nrow(district_data)
#Test for outliers in new data set
OutVals <- boxplot(district_data$YTD...Attendance..Avg.)$out
which(district_data$YTD...Attendance..Avg. %in% OutVals)
OutVals <- boxplot(district_data$YTD.Enrollment.Avg.)$out
which(district_data$YTD.Enrollment.Avg. %in% OutVals)
#Calculate summary statistics
mean(district_data$YTD...Attendance..Avg.)
mean(district_data$YTD.Enrollment.Avg.)
median(district_data$YTD...Attendance..Avg.)
median(district_data$YTD.Enrollment.Avg.)
summary(district_data$YTD...Attendance..Avg.)
summary(district_data$YTD.Enrollment.Avg.)
sd(district_data$YTD...Attendance..Avg.)
sd(district_data$YTD.Enrollment.Avg.)
#Subset data into small district and large district attendance records based on median for two-sample t test
small_dist <- subset(district_data, district_data$YTD.Enrollment.Avg <= 27009, select = c("District","YTD...Attendance..Avg.","YTD.Enrollment.Avg."))
large_dist <- subset(district_data, district_data$YTD.Enrollment.Avg >= 27009, select = c("District","YTD...Attendance..Avg.","YTD.Enrollment.Avg."))
#Create specific variables for small and large district attendance percentages for ease of use
small_dist_attend <- small_dist$YTD...Attendance..Avg.
large_dist_attend <- large_dist$YTD...Attendance..Avg.
#Summary statistics and standard deviation for small and large district attendance to determine if using pooled or unpooled t.test
summary(small_dist_attend)
summary(large_dist_attend)
sda_sd <- sd(small_dist_attend)
lda_sd <- sd(large_dist_attend)
sda_sd/lda_sd
#t.test to test hypothesis
t.test(x=small_dist_attend, y=large_dist_attend, alternative="two.sided", conf.level = 0.95, var.equal=TRUE)
#Plot and regression line on district_data to measure impact of district size on attendance
plot(district_data$YTD.Enrollment.Avg., district_data$YTD...Attendance..Avg., pch = 16, cex = 1.3, col = "blue", main = "Attendance Plotted Against Enrollment", xlab = "Enrollment", ylab = "Attendance %")
abline(lm(district_data$YTD...Attendance..Avg.~district_data$YTD.Enrollment.Avg.))
#Covariance and correlation tests (Pearson and Spearman)
cov(district_data$YTD.Enrollment.Avg.,district_data$YTD...Attendance..Avg.)
cor(district_data$YTD.Enrollment.Avg.,district_data$YTD...Attendance..Avg.)
cor(district_data$YTD.Enrollment.Avg.,district_data$YTD...Attendance..Avg., method = c("spearman"))
#Further subset, plot, covariance and correlation to measure when schools above 50k are excluded
dig_deeper <- subset(district_data, district_data$YTD.Enrollment.Avg <= 50000, select = c("District","YTD...Attendance..Avg.","YTD.Enrollment.Avg."))
plot(dig_deeper$YTD.Enrollment.Avg., dig_deeper$YTD...Attendance..Avg., pch = 16, cex = 1.3, col = "orange", main = "Attendance Plotted Against Enrollment - below 50K", xlab = "Enrollment", ylab = "Attendance %")
abline(lm(dig_deeper$YTD...Attendance..Avg.~dig_deeper$YTD.Enrollment.Avg.))
cov(dig_deeper$YTD.Enrollment.Avg.,dig_deeper$YTD...Attendance..Avg.)
cor(dig_deeper$YTD.Enrollment.Avg.,dig_deeper$YTD...Attendance..Avg., method = c("spearman"))
