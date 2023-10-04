# ACTIVITY 1 _________________________________________________________________________________
#Create Table
df <- data.frame(Product = c('A', 'B', 'C', 'D', 'E'), Price = c(612, 447, NA, 374, 831))
#Replace with 0
df$Price[is.na(df$Price)] <- 0
#Replace with mean
df$Price[is.na(df$Price)] <- mean(df$Price,na.rm = TRUE)
#Replace with median
df$Price[is.na(df$Price)] <- median(df$Price,na.rm = TRUE)

#Titanic library
library(titanic)
summary(titanic_train)
titanic_train$Age

#Histogram
library(ggplot2)
library(dplyr)
library(cowplot)

ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Value imputation
value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age, is.na(titanic_train$Age), 0),
  imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age), mean(titanic_train$Age, na.rm = TRUE)),
  imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, na.rm = TRUE))
)
value_imputed

#Histogram after imputation
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", 
                 color = "#000000", 
                 position = "identity") + 
  ggtitle("Original distribution") + theme_classic()

h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Zero-imputed distribution") + theme_classic()

h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Mean-imputed distribution") + theme_classic()

h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Median-imputed distribution") + theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#ACTIVITY 2 ___________________________________________________
#MICE
library(mice)
titanic_numeric <- titanic_train %>% select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

#MICE imputation
mice_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
  imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
  imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age)
mice_imputed

#Histogram after value imputation - MICE
m1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", 
                 color = "#000000", 
                 position = "identity") + 
  ggtitle("Original distribution") + theme_classic()

m2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Zero-imputed distribution") + theme_classic()

m3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Mean-imputed distribution") + theme_classic()

m4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", 
                 color = "#000000", 
                 position = "identity") +
  ggtitle("Median-imputed distribution") + theme_classic()

plot_grid(m1, m2, m3, m4, nrow = 2, ncol = 2)

#ACTIVITY 3 _____________________________________________________
install.packages("missForest")
library(missForest)

missForest_imputed <- data.frame(
  original = titanic_numeric$Age,
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)

missForest_imputed

#ACTIVITY 4 _____________________________________________________
log_scale = log(as.data.frame(titanic$Fare))

library(caret)
process <- preProcess(as.data.frame(titanic$Fare), method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))

scale_data <- as.data.frame(scale(titanic$Fare))

#ACTIVITY 5 _______________________________________________________
# 1 Label Encoding
gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)

# 2 One-Hot Encoding
new_dat = 
  data.frame(titanic_train$Fare,titanic_train$Sex,titanic_train$Embarked)
summary(new_dat)

library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)

# 3 Encoding Continuous (or Numeric) Variables
summary(new_dat$titanic_train.Fare)
bins <- c(-Inf, 7.91, 31.00, Inf)
bin_names <- c("Low", "Mid50", "High")
new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks = bins, labels = bin_names)
summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)
