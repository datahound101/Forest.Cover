# clear environment 
rm (list = ls())

# set working directory
setwd("~/R programming/Final Study/Forest Cover")

# load libraries
library(clusterSim)
library(corrplot)
library(C50)
library(RWeka)
library(inTrees)

# import data set 
 dat <- read.table("covtype.data",sep = ",")
 
 # add names of attributes for better understanding
 names(dat) <- c("Elevation", "Aspect", "Slope", "horz.Dist.Hydrology",
                "Vert.Dist.Hydrology", "horz.Dist.Rpadways", "HillShade.9am",
                "HillShade.Noon", "HillShade.3pm", "Horz.Dist.FirePoints",
                "Rawah.Wild", "Neota.Wild", "Comanche.Wild", "Cache.la.Poudre.Wild", paste("Soil.Type",1:40),
                "cover.Type")


# understand the data
str(dat)
summary(dat[,1:10])

# missing data analysis
na_df <- data.frame(apply(is.na(dat), 2, sum))  # create data frame of missing data

# outlier analysis
# use boxplot to show all outliers
boxplot(dat[, 1:10], col = "SkyBlue")

outlier <- function(x, n = 1.5){
#  outlier function converts outliers to NA
#  Input : 
#      x = vector/list for which outlier has to be found
#      n = 1.5 for all outliers i.e. mild and extreme & 
#          3   for only extreme outliers
#  Output :
#     x = converts all outliers to NA
       
    a <- quantile(x, 0.25)  # calculate 1st quantile 
    b <- quantile(x, 0.75)  # calculate 3rd quantile
    IQR <- n * (b - a)  # calculate IQR
    lower.limit <- a - IQR  # calculate lower vaule 
    upper.limit <- b + IQR  # calculate upper value
    x <- ifelse(x < lower.limit, NA,
                ifelse(x > upper.limit, NA, x))  # converts outliers to NA
    return(x)
}

# use function outlier to convert all outliers to NAs
dat[, 1:10] <- apply(dat[, 1:10], 2, outlier)
dat <- na.omit(dat)  # remove observations with NA

dat$cover.Type <- as.factor(dat$cover.Type) #  convert target variable into a factor

# normalizing the data
dat[, 1:10] <- data.Normalization(dat[, 1:10], type = "n4")


# buliding a correlation plot
corrplot(cor(dat[,1:10]), method = "number", title = "correlation matrix",
         order = "hclust")

# Remove highly correlated data
dat$HillShade.3pm <- NULL 


dat <- dat[order(runif(nrow(dat))), ]  # randamize order of data 
train <- dat[1:ceiling(nrow(dat) * 0.8), ]  # split 80% of data into train
test <- dat[(nrow(train)+ 1) : nrow(dat), ]  # split remaining data into test

c50Grid <- expand.grid(.model = c("tree","rules"), .winnow = c(TRUE, FALSE))
model1 <- C5.0(cover.Type ~., data = train, rules = T,
               tuneGrid = c50Grid, trControl = ctrl)  # build a C5.0 model for classification
pred1 <- predict(model1, test[, -55])  # predict cover types of forest in test data
table(pred1, test$cover.Type)  # build confusion matrix

# write all rules in text for evaluation
write(capture.output(summary(model1)), "forestcover.txt") 

