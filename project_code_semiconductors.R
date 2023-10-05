getwd()
setwd('C:/Users/Sriram/Downloads')

library(dplyr)


my_data = read.csv('train.csv')
my_data

str(my_data)

colnames(my_data)


my_data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
                       'wtd_std_atomic_mass',
                       'wtd_entropy_fie',
                       'wtd_std_fie',
                       'wtd_entropy_atomic_radius',
                       'wtd_std_atomic_radius',
                       'wtd_entropy_Density',
                       'wtd_std_Density',
                       'wtd_entropy_ElectronAffinity',
                       'wtd_std_ElectronAffinity',
                       'wtd_entropy_FusionHeat',
                       'wtd_std_FusionHeat',
                       'wtd_entropy_ThermalConductivity',
                       'wtd_std_ThermalConductivity',
                       'wtd_entropy_Valence',
                       'wtd_std_Valence','critical_temp')]
#YOUR CODE HERE
library(leaps)
library(MASS)

n = dim(my_data)[1]; 
reg1 = regsubsets(critical_temp ~ ., data = my_data,really.big=T)
rs = summary(reg1,really.big=T)
rs$which

AIC = 2*(1:81) + n*log(rs$rss/n)
plot(AIC ~ I(1:81), xlab = "number of predictors", ylab = "AIC")

#YOUR CODE HERE
BIC = log(n)*(1:81) + n*log(rs$rss/n) 
plot(BIC ~ I(1:81), xlab = "number of predictors", ylab = "BIC")

plot(1:81, rs$adjr2, xlab = "number of predictors", ylab = "adjusted R-squared")


my_data <- na.omit(my_data)


dup_rows <- duplicated(my_data)


my_data <- my_data[!dup_rows,]


my_data<- sample_n(my_data, 1000, fac = "number_of_elements")


library(corrplot)

cor_mat = cor(my_data)
# Create the correlation plot
corrplot(cor_mat, order = "hclust", tl.col = "transparent", type = "lower", tl.srt = 45, width = 15, height = 15)

# Calculate the correlation matrix between each column and critical_temp
corr_matrix <- cor(my_data)

# Sort the correlation coefficients in descending order based on absolute value
corr_sorted <- sort(abs(corr_matrix[,"critical_temp"]), decreasing = TRUE)

# Get the names of the sorted columns
colnames_sorted <- names(corr_sorted)

# Print the sorted columns in descending order of correlation with critical_temp
print(colnames_sorted)

# Load the ggplot2 package
library(ggplot2)


# Create a histogram and density plot of critical_temp
ggplot(data = my_data, aes(x = critical_temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "grey", color = "black") +
  geom_density(alpha = 0.5, color = "red",size =1.5) +
  xlab("Critical Temperature") +
  ylab("Density")


# create a vector of variable names
var_names <- c("wtd_std_ThermalConductivity", "wtd_entropy_atomic_mass", "wtd_entropy_atomic_radius", 
               "wtd_entropy_Valence", "wtd_std_fie")

# create an empty list to store plots
plots_list <- list()

# loop through the variables and create density plots
for (var in var_names) {
  plot <- ggplot(data = my_data, aes(x = !!sym(var))) +
    geom_histogram(aes(y = ..density..),fill = "grey", color = "black") +
    geom_density(alpha = 0.5, color = "red") +
    xlab(var) +
    ylab("Density")
  plots_list[[var]] <- plot
}

# print the plots
plots_list



ggplot(my_data) + geom_point(aes(x=(wtd_std_ThermalConductivity), y=critical_temp))+ geom_smooth(aes(x=(wtd_std_ThermalConductivity), y=critical_temp)) 
ggplot(my_data) + geom_point(aes(x=exp(wtd_entropy_atomic_mass), y=critical_temp)) + geom_smooth(aes(x=exp(wtd_entropy_atomic_mass), y=critical_temp))
ggplot(my_data) + geom_point(aes(x=wtd_entropy_atomic_radius, y=critical_temp)) + geom_smooth(aes(x=wtd_entropy_atomic_radius, y=critical_temp))
ggplot(my_data) + geom_point(aes(x=wtd_entropy_Valence, y=critical_temp)) + geom_smooth(aes(x=wtd_entropy_Valence, y=critical_temp))
ggplot(my_data) + geom_point(aes(x=wtd_std_fie, y=critical_temp)) + geom_smooth(aes(x=wtd_std_fie, y=critical_temp))

colnames(my_data)

top_5_cols <- c("wtd_entropy_atomic_mass", "wtd_entropy_atomic_radius", 
                "wtd_entropy_Valence")

# Create a subset of the data with only the top 5 columns
subset_data <- my_data[, top_5_cols]

# Reshape the data into a long format
library(tidyr)
subset_data_long <- gather(subset_data, key = "variable", value = "value")

# Create the boxplot
library(ggplot2)
ggplot(subset_data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  ggtitle("Boxplots of Top 5 Columns") +
  theme(plot.title = element_text(hjust = 0.5))

library(forcats)
library(RColorBrewer)
ggplot(my_data, aes(x = fct_reorder(factor(number_of_elements), critical_temp, .desc =TRUE), y = critical_temp)) + 
  geom_boxplot(aes(fill = fct_reorder(factor(number_of_elements), critical_temp, .desc =TRUE))) + 
  scale_fill_manual(values = brewer.pal(9, "Spectral"), guide = guide_legend(title = "Species")) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw(base_size = 14) +
  xlab("Elements") +
  ylab("Critical Temp") 



#######################################


my_data = read.csv('train.csv')
my_data

str(my_data)

colnames(my_data)


my_data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
                       'wtd_std_atomic_mass',
                       'wtd_entropy_fie',
                       'wtd_std_fie',
                       'wtd_entropy_atomic_radius',
                       'wtd_std_atomic_radius',
                       'wtd_entropy_Density',
                       'wtd_std_Density',
                       'wtd_entropy_ElectronAffinity',
                       'wtd_std_ElectronAffinity',
                       'wtd_entropy_FusionHeat',
                       'wtd_std_FusionHeat',
                       'wtd_entropy_ThermalConductivity',
                       'wtd_std_ThermalConductivity',
                       'wtd_entropy_Valence',
                       'wtd_std_Valence','critical_temp')]


#splitting Test & Train

dim(my_data)
library(caret)
train_index <- createDataPartition(my_data$critical_temp, p = 0.8, list = FALSE)
train_data <- my_data[train_index, ]
test_data <- my_data[-train_index, ]




regressor = lm(formula = critical_temp ~.,
               data = train_data)
summary(regressor)

plot(regressor)





#DT R
model <- rpart(critical_temp ~ ., data = train_data, method = "anova")

# Make predictions on the testing data
predictions <- predict(model, newdata = test_data)

# Evaluate the performance of the model
mse <- mean((test_data$critical_temp - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$critical_temp - predictions))

residuals <- residuals(model, newdata = test_data)

plot(x = train_data$critical_temp, y = residuals, main = "Residual vs Fitted Plot", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

summary(model)


#Gam


gam_model <- gam(critical_temp ~ s(number_of_elements, k = 9) + s(wtd_entropy_atomic_mass, k = 9) + 
                   s(wtd_std_atomic_mass, k = 9) + s(wtd_entropy_fie, k = 9) + s(wtd_std_fie, k = 9) + 
                   s(wtd_entropy_atomic_radius, k = 9) + s(wtd_std_atomic_radius, k = 9) + 
                   s(wtd_entropy_Density, k = 9) + s(wtd_std_Density, k = 9) + 
                   s(wtd_entropy_ElectronAffinity, k = 9) + s(wtd_std_ElectronAffinity, k = 9) + 
                   s(wtd_entropy_FusionHeat, k = 9) + s(wtd_std_FusionHeat, k = 9) + 
                   s(wtd_entropy_ThermalConductivity, k = 9) + s(wtd_std_ThermalConductivity, k = 9) + 
                   s(wtd_entropy_Valence, k = 9) + s(wtd_std_Valence, k = 9),
                 data = train_data)
summary(gam_model)

pred <- predict(gam_model, newdata = test_data)

# calculate RMSE (root mean squared error) of the prediction
rmse <- sqrt(mean((test_data$critical_temp - pred)^2))/10
rmse




#GAM Full Data
gam_model2 <- gam(critical_temp ~ s(number_of_elements) + s(mean_atomic_mass) + s(wtd_mean_atomic_mass, bs = "cr", k = 20) + s(gmean_atomic_mass) +
                   s(wtd_gmean_atomic_mass) + s(entropy_atomic_mass) + s(wtd_entropy_atomic_mass) + s(range_atomic_mass) +
                   s(wtd_range_atomic_mass) + s(std_atomic_mass) + s(wtd_std_atomic_mass) + s(mean_fie) + s(wtd_mean_fie) +
                   s(gmean_fie) + s(wtd_gmean_fie) + s(entropy_fie) + s(wtd_entropy_fie) + s(range_fie) + s(wtd_range_fie) +
                   s(std_fie) + s(wtd_std_fie) + s(mean_atomic_radius) + s(wtd_mean_atomic_radius) + s(gmean_atomic_radius) +
                   s(wtd_gmean_atomic_radius) + s(entropy_atomic_radius) + s(wtd_entropy_atomic_radius) + s(range_atomic_radius) +
                   s(wtd_range_atomic_radius) + s(std_atomic_radius) + s(wtd_std_atomic_radius) + s(mean_Density) + s(wtd_mean_Density) +
                   s(gmean_Density) + s(wtd_gmean_Density) + s(entropy_Density) + s(wtd_entropy_Density) + s(range_Density) +
                   s(wtd_range_Density) + s(std_Density) + s(wtd_std_Density) + s(mean_ElectronAffinity) + s(wtd_mean_ElectronAffinity) +
                   s(gmean_ElectronAffinity) + s(wtd_gmean_ElectronAffinity) + s(entropy_ElectronAffinity) + s(wtd_entropy_ElectronAffinity) +
                   s(range_ElectronAffinity) + s(wtd_range_ElectronAffinity) + s(std_ElectronAffinity) + s(wtd_std_ElectronAffinity) +
                   s(mean_FusionHeat) + s(wtd_mean_FusionHeat) + s(gmean_FusionHeat) + s(wtd_gmean_FusionHeat) +
                   s(entropy_FusionHeat) + s(wtd_entropy_FusionHeat) + s(range_FusionHeat) + s(wtd_range_FusionHeat) +
                   s(std_FusionHeat) + s(wtd_std_FusionHeat) + s(mean_ThermalConductivity) + s(wtd_mean_ThermalConductivity) +
                   s(gmean_ThermalConductivity) + s(wtd_gmean_ThermalConductivity) + s(entropy_ThermalConductivity) +
                   s(wtd_entropy_ThermalConductivity) + s(range_ThermalConductivity) + s(wtd_range_ThermalConductivity) +
                   s(std_ThermalConductivity) + s(wtd_std_ThermalConductivity) + s(mean_Valence) + s(wtd_mean_Valence) +
                   s(gmean_Valence) + s(wtd_gmean_Valence) + s(entropy_Valence) + s(wtd_entropy_Valence) + s(range_Valence) +
                   s(wtd_range_Valence) + s(std_Valence) + s(wtd_std_Valence), data = my_data)

summary(gam_model2)


plot(gam_model)






##########################

# fit a polynomial regression model of degree 2
poly_model <- lm(critical_temp ~ poly(number_of_elements, 9) + poly(wtd_entropy_atomic_mass, 10) + poly(wtd_std_atomic_mass, 17) + poly(wtd_entropy_fie, 3) + poly(wtd_std_fie, 10) + poly(wtd_entropy_atomic_radius, 6) + poly(wtd_std_atomic_radius, 8) + poly(wtd_entropy_Density, 15) + poly(wtd_std_Density, 2) + poly(wtd_entropy_ElectronAffinity, 5) + poly(wtd_std_ElectronAffinity, 11) + poly(wtd_entropy_FusionHeat, 10) + poly(wtd_std_FusionHeat, 5) + poly(wtd_entropy_ThermalConductivity, 7) + poly(wtd_std_ThermalConductivity, 5) + poly(wtd_entropy_Valence, 5) + poly(wtd_std_Valence, 11), data = train_data)
poly_model2 <- lm(critical_temp ~ poly(range_fie, 2)+poly(wtd_mean_ThermalConductivity,2) + poly(wtd_mean_ElectronAffinity,2)+poly(std_atomic_mass,2), data = train_data)
# make predictions on the testing set
poly_predict <- predict(poly_model2, newdata = test_data)


# evaluate model performance
poly_rmse <- sqrt(mean((poly_predict - test_data$critical_temp)^2))
poly_rmse

########################################



my_data = read.csv('train.csv')
my_data

str(my_data)

colnames(my_data)


my_data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
                       'wtd_std_atomic_mass',
                       'wtd_entropy_fie',
                       'wtd_std_fie',
                       'wtd_entropy_atomic_radius',
                       'wtd_std_atomic_radius',
                       'wtd_entropy_Density',
                       'wtd_std_Density',
                       'wtd_entropy_ElectronAffinity',
                       'wtd_std_ElectronAffinity',
                       'wtd_entropy_FusionHeat',
                       'wtd_std_FusionHeat',
                       'wtd_entropy_ThermalConductivity',
                       'wtd_std_ThermalConductivity',
                       'wtd_entropy_Valence',
                       'wtd_std_Valence','critical_temp')]

my_data<-na.omit(my_data)


my_lm_model <- lm(critical_temp ~ number_of_elements + exp(wtd_entropy_atomic_mass) + wtd_std_atomic_mass +
                    exp(wtd_entropy_fie) + wtd_std_fie  +
                    wtd_std_atomic_radius + wtd_entropy_Density + wtd_std_Density +
                    wtd_entropy_ElectronAffinity + 
                    (wtd_entropy_FusionHeat)^2 + wtd_std_FusionHeat +
                    wtd_entropy_ThermalConductivity + wtd_std_ThermalConductivity +
                    wtd_entropy_Valence + wtd_std_Valence, data = my_data)


summary(my_lm_model)




ggplot(my_data, aes(x = (wtd_entropy_FusionHeat)^2 , y = critical_temp)) +
  geom_point(alpha = 0.2)




library(caret)

# Define control parameters for cross-validation
ctrl <- trainControl(method = "cv",   # K-fold cross-validation method
                     number = 5)      # Number of folds

# Train the lm model using cross-validation
lm_cv <- train(critical_temp ~ number_of_elements + wtd_entropy_atomic_mass + wtd_std_atomic_mass +
                 wtd_entropy_fie + wtd_std_fie  +
                 wtd_std_atomic_radius + wtd_entropy_Density + wtd_std_Density +
                 wtd_entropy_ElectronAffinity + 
                 (wtd_entropy_FusionHeat)^2 + wtd_std_FusionHeat +
                 wtd_entropy_ThermalConductivity + wtd_std_ThermalConductivity +
                 wtd_entropy_Valence + wtd_std_Valence, 
               data = my_data, 
               method = "gam",   # Specify the model to use
               trControl = ctrl)

# Print the cross-validation results
print(lm_cv)




# Calculate correlation matrix
cor_matrix <- cor(my_data)

cor_matrix_rm <- cor_matrix                  # Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
data_new<- my_data[,!apply(cor_matrix_rm,2,function(x) any(x>0.7))]

#Correlation Reduced GAM
my_gam_model <- gam(critical_temp ~ 
                      s(gmean_fie, k=5) +
                      s(wtd_range_fie, k=5) +
                      s(gmean_atomic_radius, k=5) +
                      s(wtd_range_Density, k=5) +
                      s(wtd_std_Density, k=5) +
                      s(wtd_gmean_ElectronAffinity, k=5) +
                      s(wtd_range_ElectronAffinity, k=5) +
                      s(wtd_std_ElectronAffinity, k=5) +
                      s(wtd_std_FusionHeat, k=5) +
                      s(mean_ThermalConductivity, k=5) +
                      s(wtd_gmean_ThermalConductivity, k=5) +
                      s(wtd_entropy_ThermalConductivity, k=5) +
                      s(wtd_range_ThermalConductivity, k=5) +
                      s(wtd_gmean_Valence, k=5) +
                      s(wtd_entropy_Valence, k=5) +
                      s(wtd_range_Valence, k=5) +
                      s(wtd_std_Valence, k=5), data=data_new)


summary(my_gam_model)
head(data_new) 




#Correlation Reduced LM

final1 <- lm(critical_temp~.,data =data_new )
summary(final1)









######################## Models Comparision ###############################################
library(caret)

train_index <- createDataPartition(my_data$critical_temp, p = 0.8, list = FALSE)
train_data <- my_data[train_index, ]
test_data <- my_data[-train_index, ]



train_index <- createDataPartition(data_new$critical_temp, p = 0.8, list = FALSE)
train_data_corr <- data_new[train_index, ]
test_data_corr <- data_new[-train_index, ]


Corr_Red_GAM <- gam(critical_temp ~ 
                      s(gmean_fie, k=5) +
                      s(wtd_range_fie, k=5) +
                      s(gmean_atomic_radius, k=5) +
                      s(wtd_range_Density, k=5) +
                      s(wtd_std_Density, k=5) +
                      s(wtd_gmean_ElectronAffinity, k=5) +
                      s(wtd_range_ElectronAffinity, k=5) +
                      s(wtd_std_ElectronAffinity, k=5) +
                      s(wtd_std_FusionHeat, k=5) +
                      s(mean_ThermalConductivity, k=5) +
                      s(wtd_gmean_ThermalConductivity, k=5) +
                      s(wtd_entropy_ThermalConductivity, k=5) +
                      s(wtd_range_ThermalConductivity, k=5) +
                      s(wtd_gmean_Valence, k=5) +
                      s(wtd_entropy_Valence, k=5) +
                      s(wtd_range_Valence, k=5) +
                      s(wtd_std_Valence, k=5), data=train_data_corr)


summary(Corr_Red_GAM)
pred_Corr_Red_GAM <- predict(Corr_Red_GAM, newdata = test_data_corr)

# calculate RMSE (root mean squared error) of the prediction
rmse_Corr_Red_GAM <- sqrt(mean((test_data_corr$critical_temp - pred)^2))
rmse_Corr_Red_GAM

library(mgcv)

Self_Red_GAM <- gam(critical_temp ~ s(number_of_elements, k = 9) + s(wtd_entropy_atomic_mass, k = 9) + 
                   s(wtd_std_atomic_mass, k = 9) + s(wtd_entropy_fie, k = 9) + s(wtd_std_fie, k = 9) + 
                   s(wtd_entropy_atomic_radius, k = 9) + s(wtd_std_atomic_radius, k = 9) + 
                   s(wtd_entropy_Density, k = 9) + s(wtd_std_Density, k = 9) + 
                   s(wtd_entropy_ElectronAffinity, k = 9) + s(wtd_std_ElectronAffinity, k = 9) + 
                   s(wtd_entropy_FusionHeat, k = 9) + s(wtd_std_FusionHeat, k = 9) + 
                   s(wtd_entropy_ThermalConductivity, k = 9) + s(wtd_std_ThermalConductivity, k = 9) + 
                   s(wtd_entropy_Valence, k = 9) + s(wtd_std_Valence, k = 9),
                 data = train_data_Self)

pred <- predict(Self_Red_GAM, newdata = test_data_Self)

summary(Self_Red_GAM)
# calculate RMSE (root mean squared error) of the prediction
rmse_Self_Red_GAM <- sqrt(mean((test_data_Self$critical_temp - pred)^2))
rmse_Self_Red_GAM


anova(Corr_Red_GAM,Self_Red_GAM)
#Second Model is Best

Self_LM_Data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
                                       'wtd_std_atomic_mass',
                                       'wtd_entropy_fie',
                                       'wtd_std_fie',
                                       'wtd_entropy_atomic_radius',
                                       'wtd_std_atomic_radius',
                                       'wtd_entropy_Density',
                                       'wtd_std_Density',
                                       'wtd_entropy_ElectronAffinity',
                                       'wtd_std_ElectronAffinity',
                                       'wtd_entropy_FusionHeat',
                                       'wtd_std_FusionHeat',
                                       'wtd_entropy_ThermalConductivity',
                                       'wtd_std_ThermalConductivity',
                                       'wtd_entropy_Valence',
                                       'wtd_std_Valence','critical_temp')]

train_index <- createDataPartition(Self_LM_Data$critical_temp, p = 0.8, list = FALSE)
train_data_Self <- Self_LM_Data[train_index, ]
test_data_Self <- Self_LM_Data[-train_index, ]

Self_LM_Data

Self_Red_LM <- lm(critical_temp ~., data = train_data_Self)
summary(Self_Red_LM)
pred_Self_Red_LM <- predict(Self_Red_LM, newdata = test_data_Self)

# calculate RMSE (root mean squared error) of the prediction
rmse_Self_Self_Red_LM <- sqrt(mean((test_data_Self$critical_temp - pred_Self_Red_LM)^2))
rmse_Self_Self_Red_LM



dim(data_new)

Corr_Red_LM <- lm(critical_temp ~., data = train_data_corr)
summary(Corr_Red_LM)
pred <- predict(Corr_Red_LM, newdata = test_data_corr)

# calculate RMSE (root mean squared error) of the prediction
rmse_Corr_Red_LM <- sqrt(mean((test_data_corr$critical_temp - pred)^2))
rmse_Corr_Red_LM





Full_Data_LM <- lm(critical_temp ~., data = train_data)
pred <- predict(Full_Data_LM, newdata = test_data)

# calculate RMSE (root mean squared error) of the prediction
rmse_Corr_Red_LM <- sqrt(mean((test_data$critical_temp - pred)^2))
rmse_Corr_Red_LM

#summary(Full_Data_LM)

anova(Corr_Red_LM,Self_Red_LM,Full_Data_LM)
# Second Model is Better


# fit a polynomial regression model of degree 2
poly_model <- lm(critical_temp ~ poly(number_of_elements, 2) + poly(wtd_entropy_atomic_mass, 2) + poly(wtd_std_atomic_mass, 2) + poly(wtd_entropy_fie, 3) + poly(wtd_std_fie, 3) + poly(wtd_entropy_atomic_radius, 2) + poly(wtd_std_atomic_radius, 8) + poly(wtd_entropy_Density, 15) + poly(wtd_std_Density, 2) + poly(wtd_entropy_ElectronAffinity, 3) + poly(wtd_std_ElectronAffinity, 1) + poly(wtd_entropy_FusionHeat, 3) + poly(wtd_std_FusionHeat, 2) + poly(wtd_entropy_ThermalConductivity, 3) + poly(wtd_std_ThermalConductivity, 3) + poly(wtd_entropy_Valence, 3) + poly(wtd_std_Valence, 2), data = train_data)
# make predictions on the testing set
poly_predict <- predict(poly_model, newdata = test_data)


# evaluate model performance
poly_rmse <- sqrt(mean((poly_predict - test_data$critical_temp)^2))
poly_rmse



#All time Best MODEL
summary(Self_Red_GAM)




library(glmnet)


xtrain_self_red <-  as.matrix(train_data_corr[, !(names(train_data_corr) %in% "critical_temp")])
ytrain_self_red <- train_data_corr$critical_temp

xtest_self_red <-  as.matrix(test_data_corr[, !(names(test_data_corr) %in% "critical_temp")])
ytest_self_red <- test_data_corr$critical_temp

ridge_model <- glmnet(xtrain_self_red, ytrain_self_red, alpha = 0, lambda = seq(0, 1, 0.05))

# Select the best lambda value using cross-validation
cv_fit <- cv.glmnet(xtrain_self_red, ytrain_self_red, alpha = 0)
best_lambda <- cv_fit$lambda.min

ridge_model <- glmnet(xtrain_self_red, ytrain_self_red, alpha = 0, lambda = best_lambda)

# Make predictions on the test data
y_pred <- predict(ridge_model, newx = xtest_self_red)


# Calculate RMSE
rmse <- sqrt(mean((ytest_self_red - y_pred)^2))
rmse







ggplot(my_data, aes(x = wtd_mean_atomic_mass, y = mean_atomic_mass)) +
  geom_point() +
  labs(x = "Weighted Mean Atomic Mass",
       y = " Mean Atomic Mass",
       title = "Scatter plot of Weighted Mean Atomic Mass and Mean Atomic Mass")


# Calculate correlation matrix
cor_matrix <- cor(my_data)

cor_matrix_rm <- cor_matrix                  # Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
data_new1<- my_data[,!apply(cor_matrix_rm,2,function(x) any(x>0.9))]
colnames(data_new1)




cor_mat = cor(Self_LM_Data)
# Create the correlation plot
corrplot(cor_mat, order = "hclust", tl.col = "black", type = "lower", tl.srt = 45, width = 15, height = 15)









require(reshape2)

pima_melt <- melt(Self_LM_Data)
ggplot(pima_melt, aes(x=factor(variable), y=value)) + geom_boxplot() + facet_grid(~variable, scale="free")





#########################################################


AIC_Data <- my_data[, c('critical_temp', 'wtd_entropy_atomic_mass','wtd_std_atomic_mass', 'wtd_entropy_fie', 'wtd_entropy_Density', 'wtd_std_Density', 'wtd_entropy_ElectronAffinity', 'wtd_std_ThermalConductivity', 'wtd_std_Valence')]
train_index <- createDataPartition(AIC_Data$critical_temp, p = 0.8, list = FALSE)
train_data_aic <- AIC_Data[train_index, ]
test_data_aic <- AIC_Data[-train_index, ]

AIC_Data_LM <- lm(critical_temp ~., data = train_data_aic)

summary(AIC_Data_LM)
pred <- predict(AIC_Data_LM, newdata = test_data_aic)

# calculate RMSE (root mean squared error) of the prediction
rmse_Corr_Red_LM <- sqrt(mean((test_data_aic$critical_temp - pred)^2))
rmse_Corr_Red_LM

library(mgcv)

AIC_Data_gam <- gam(critical_temp ~ s(wtd_entropy_atomic_mass, k=7) + s(wtd_std_atomic_mass, k=7) + s(wtd_entropy_fie, k=7) + s(wtd_entropy_Density, k=7) + s(wtd_std_Density, k=7) + s(wtd_entropy_ElectronAffinity, k=7) + s(wtd_std_ThermalConductivity, k=7) + s(wtd_std_Valence, k=7), data=train_data_aic)


# summary of model
summary(AIC_Data_gam)

pred <- predict(AIC_Data_gam, newdata = test_data_aic)

# calculate RMSE (root mean squared error) of the prediction
rmse_AIC_Data_gam<- sqrt(mean((test_data_aic$critical_temp - pred)^2))
rmse_AIC_Data_gam

#######################################################################
dim(my_data)

library(ggplot2)

# Create a subset of the data with the five main columns
subset_data <- my_data[, c("critical_temp", "wtd_entropy_atomic_mass", "wtd_std_atomic_mass", 
                           "wtd_entropy_fie", "wtd_entropy_Density")]
m1 <- melt(as.data.frame(subset_data))
library(ggplot2)
ggplot(m1,aes(x = variable,y = value)) + facet_wrap(~variable) + geom_boxplot()

boxplot(data=m1, value~variable)






cor_mat = cor(data_new)
# Create the correlation plot
corrplot(cor_mat, order = "hclust", tl.col = "black", type = "lower", width = 15, height = 15)




# Calculate F-statistic and degrees of freedom
# Fit model 1 and model 2
model1 <- lm(critical_temp ~ wtd_entropy_atomic_mass + wtd_std_atomic_mass + wtd_entropy_fie + wtd_entropy_Density + wtd_std_Density + wtd_entropy_ElectronAffinity + wtd_std_ThermalConductivity + wtd_std_Valence, data = train_data_aic)
model2 <- lm(critical_temp ~ gmean_fie + wtd_range_fie + gmean_atomic_radius + wtd_range_Density + wtd_std_Density + wtd_gmean_ElectronAffinity + wtd_range_ElectronAffinity + wtd_std_ElectronAffinity + wtd_std_FusionHeat + mean_ThermalConductivity + wtd_gmean_ThermalConductivity + wtd_entropy_ThermalConductivity + wtd_range_ThermalConductivity + wtd_gmean_Valence + wtd_entropy_Valence + wtd_range_Valence + wtd_std_Valence, data = train_data_corr)

# Calculate F-statistic
RSS1 <- sum(resid(model1)^2)
RSS2 <- sum(resid(model2)^2)
n1 <- nrow(train_data_aic)
n2 <- nrow(train_data_corr)
p1 <- length(coef(model1))
p2 <- length(coef(model2))

F_stat <- ((RSS1 - RSS2) / (p2 - p1)) / (RSS2 / (n2 - p2))

# Calculate degrees of freedom and p-value
df1 <- p2 - p1
df2 <- n2 - p2

p_value <- 1 - pf(F_stat, df1, df2)
p_value
RSS1
RSS2


ggplot(test_data_Self) + geom_point(aes(x=(critical_temp), y=pred_Self_Red_LM)) + geom_smooth(aes(x = (critical_temp), y=pred_Self_Red_LM)) + ggtitle('Property Model- (Y Vs Y_hat)')

par(mfrow= c(2,2))

theme_classic()
grid()
plot(Self_Red_LM)



ggplot(test_data_Self) + geom_point(aes(x=(critical_temp), y=pred)) + geom_smooth(aes(x = (critical_temp), y=pred)) + ggtitle('GAM Property Model- (Y Vs Y_hat)')
par(mfrow= c(2,2))

library(splines)

Self_Red_GAM2 <- gam(critical_temp ~ bs(number_of_elements, degree=3) + 
                       bs(wtd_entropy_atomic_mass, degree=3) + 
                       bs(wtd_std_atomic_mass, degree=3) + 
                       bs(wtd_entropy_fie, degree=3) + 
                       bs(wtd_std_fie, degree=3) + 
                       bs(wtd_entropy_atomic_radius, degree=3) + 
                       bs(wtd_std_atomic_radius, degree=3) + 
                       bs(wtd_entropy_Density, degree=3) + 
                       bs(wtd_std_Density, degree=3) + 
                       bs(wtd_entropy_ElectronAffinity, degree=3) + 
                       bs(wtd_std_ElectronAffinity, degree=3) + 
                       bs(wtd_entropy_FusionHeat, degree=3) + 
                       bs(wtd_std_FusionHeat, degree=3) + 
                       bs(wtd_entropy_ThermalConductivity, degree=3) + 
                       bs(wtd_std_ThermalConductivity, degree=3) + 
                       bs(wtd_entropy_Valence, degree=3) + 
                       bs(wtd_std_Valence, degree=3),
                     data = train_data_Self)
#plot(test_data_Self$critical_temp, test_data_Self$number_of_elements)
plot(Self_Red_GAM, rug =TRUE, pch = 1, cex = 0.5, shade = TRUE, shade.col = "green")

summary(Self_Red_GAM)

pred <- predict(Self_Red_GAM2, newdata = test_data_Self)

# calculate RMSE (root mean squared error) of the prediction
rmse_Self_Red_GAM <- sqrt(mean((test_data_Self$critical_temp - pred)^2))
rmse_Self_Red_GAM



library(mgcv)



Self_Red_GAM <- gam(critical_temp ~ s(number_of_elements) + s(wtd_entropy_atomic_mass, k = 9) + 
                      s(wtd_std_atomic_mass, k = 9) + s(wtd_entropy_fie, k = 9) + s(wtd_std_fie, k = 9) + 
                      s(wtd_entropy_atomic_radius, k = 9) + s(wtd_std_atomic_radius, k = 9) + 
                      s(wtd_entropy_Density, k = 9) + s(wtd_std_Density, k = 9) + 
                      s(wtd_entropy_ElectronAffinity, k = 9) + s(wtd_std_ElectronAffinity, k = 9) + 
                      s(wtd_entropy_FusionHeat, k = 9) + s(wtd_std_FusionHeat, k = 9) + 
                      s(wtd_entropy_ThermalConductivity, k = 9) + s(wtd_std_ThermalConductivity, k = 9) + 
                      s(wtd_entropy_Valence, k = 9) + s(wtd_std_Valence, k = 9),
                    data = my_data)

summary(Self_Red_GAM)
gam.check(Self_Red_GAM)



str(my_data)
dim(my_dta)
my_data<-na.omit(my_data)




estimates <- coef(Self_Red_LM)
print(estimates)

plot_data<- data.frame(estimates)
plot_data_2 <- plot_data %>% rownames_to_column(var = "Features")
# create the bar plot
ggplot(plot_data_2, aes(x=Features , y = estimates)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance of LM") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
