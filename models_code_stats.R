setwd('C:/Users/Sriram/Downloads')

library(dplyr)
library(caret)

my_data = read.csv('train.csv')
my_data

str(my_data)

colnames(my_data)


self_data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
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

train_index_self <- createDataPartition(self_data$critical_temp, p = 0.8, list = FALSE)
train_data_self <- self_data[train_index_self, ]
test_data_self <- self_data[-train_index_self, ]




# Calculate correlation matrix
cor_matrix <- cor(my_data)

cor_matrix_rm <- cor_matrix                  # Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
corr_data<- my_data[,!apply(cor_matrix_rm,2,function(x) any(x>0.7))]
corr_data
str(corr_data)
library(corrplot)
cor_mat = cor(AIC_Data)
# Create the correlation plot
corrplot(cor_mat, order = "hclust", tl.col = "black", type = "lower", tl.srt = 45, width = 15, height = 15)



train_index_corr <- createDataPartition(corr_data$critical_temp, p = 0.8, list = FALSE)
train_data_corr <- corr_data[train_index_corr, ]
test_data_corr <- corr_data[-train_index_corr, ]


AIC_Data <- my_data[, c('critical_temp', 'wtd_entropy_atomic_mass','wtd_std_atomic_mass', 'wtd_entropy_fie', 'wtd_entropy_Density', 'wtd_std_Density', 'wtd_entropy_ElectronAffinity', 'wtd_std_ThermalConductivity', 'wtd_std_Valence')]
AIC_Data
train_index_aic <- createDataPartition(AIC_Data$critical_temp, p = 0.8, list = FALSE)
train_data_aic <- AIC_Data[train_index_aic, ]
test_data_aic <- AIC_Data[-train_index_aic, ]



##############################################################################
library(randomForest)
rdm_self <- randomForest(critical_temp ~ ., data = train_data_self, ntree = 500)
rdm_corr <- randomForest(critical_temp ~ ., data = train_data_corr, ntree = 500)
rdm_aic <- randomForest(critical_temp ~ ., data = train_data_aic, ntree = 500)

pred_rdm_self <- predict(rdm_self, newdata = test_data_self)
rmse_rdm_self <- sqrt(mean((test_data_self$critical_temp - pred_rdm_self)^2))
rmse_rdm_self
R2_rdm_self <- R2(pred_rdm_self, test_data_self$critical_temp)
R2_rdm_self


# Calculate the adjusted R-squared value
n_train_data_self <- nrow(train_data_self)
p_train_data_self <- ncol(train_data_self) - 1
adjR2_rdm_self <- 1 - (1 - R2_rdm_self) * (n_train_data_self - 1) / (n_train_data_self - p_train_data_self - 1)

# Print the results
cat("R-squared: ", R2, "\n")
cat("Adjusted R-squared: ", adjR2_rdm_self, "\n")


pred_rdm_corr<- predict(rdm_corr, newdata = test_data_corr)
rmse_rdm_corr <- sqrt(mean((test_data_corr$critical_temp - pred_rdm_corr)^2))
rmse_rdm_corr
R2_rdm_corr <- R2(pred_rdm_corr, test_data_corr$critical_temp)
R2_rdm_corr

n_train_data_corr <- nrow(train_data_corr)
p_train_data_corr <- ncol(train_data_corr) - 1
adjR2_rdm_corr <- 1 - (1 - R2_rdm_corr) * (n_train_data_corr - 1) / (n_train_data_corr - p_train_data_corr - 1)

# Print the results
cat("R-squared: ", R2_rdm_corr, "\n")
cat("Adjusted R-squared: ", adjR2_rdm_corr, "\n")





pred_rdm_aic<- predict(rdm_aic, newdata = test_data_aic)
rmse_rdm_aic <- sqrt(mean((test_data_aic$critical_temp - pred_rdm_aic)^2))
rmse_rdm_aic
R2_rdm_aic <- R2(pred_rdm_aic, test_data_aic$critical_temp)
R2_rdm_aic
n_train_data_aic <- nrow(train_data_aic)
p_train_data_aic <- ncol(train_data_aic) - 1
adjR2_rdm_aic <- 1 - (1 - R2_rdm_aic) * (n_train_data_aic - 1) / (n_train_data_aic - p_train_data_aic - 1)

# Print the results
cat("R-squared: ", R2_rdm_aic, "\n")
cat("Adjusted R-squared: ", adjR2_rdm_aic, "\n")

###########################################################################

y_yhat <- data.frame(y_actual = test_data_self$critical_temp, y_predicted = pred_rdm_self)

# plot y vs y_hat
ggplot(y_yhat, aes(x = y_actual, y = y_predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  labs(title = "Actual vs Predicted - Random Forest",
       x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggplot(y_yhat) + geom_point(aes(x=y_actual, y=y_predicted)) + geom_smooth(aes(x=y_actual, y=y_predicted)) + ggtitle('Actual Vs Predicted - Random Forest')

p1 <- ggplot(data = y_yhat,
             mapping = aes(x = y_actual, y = y_predicted)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Actual", y = "Predicted") +
  theme_bw() +
  ggtitle('Actual vs Predicted - Random Forest')

# Replace "histogram" with "boxplot" or "density" for other types
ggMarginal(p1, type="histogram")


plot(rdm_self)



##############################################################################
#Support vector machines 

library(e1071)
svm_self <- svm(critical_temp ~ ., data = train_data_self, kernel = "radial")
svm_corr <- svm(critical_temp ~ ., data = train_data_corr, kernel = "radial")
svm_aic <- svm(critical_temp ~ ., data = train_data_aic, kernel = "radial")


pred_svm_self <- predict(svm_self, newdata = test_data_self)
rmse_svm_self <- sqrt(mean((test_data_self$critical_temp - pred_svm_self)^2))
rmse_svm_self

R2_svm_self <- R2(pred_svm_self, test_data_self$critical_temp)
R2_svm_self
n_train_data_self <- nrow(train_data_self)
p_train_data_self <- ncol(train_data_self) - 1
adjR2_svm_self <- 1 - (1 - R2_svm_self) * (n_train_data_self - 1) / (n_train_data_self - p_train_data_self - 1)

# Print the results
cat("R-squared: ", R2_svm_self, "\n")
cat("Adjusted R-squared: ", adjR2_svm_self, "\n")




pred_svm_corr<- predict(svm_corr, newdata = test_data_corr)
rmse_svm_corr <- sqrt(mean((test_data_corr$critical_temp - pred_svm_corr)^2))
rmse_svm_corr

R2_svm_corr <- R2(pred_svm_corr, test_data_corr$critical_temp)
R2_svm_corr

n_train_data_corr <- nrow(train_data_corr)
p_train_data_corr <- ncol(train_data_corr) - 1
adjR2_svm_corr <- 1 - (1 - R2_svm_corr) * (n_train_data_corr - 1) / (n_train_data_corr - p_train_data_corr - 1)

# Print the results
cat("R-squared: ", R2_svm_corr, "\n")
cat("Adjusted R-squared: ", adjR2_svm_corr, "\n")



pred_svm_aic<- predict(svm_aic, newdata = test_data_aic)
rmse_svm_aic <- sqrt(mean((test_data_aic$critical_temp - pred_svm_aic)^2))
rmse_svm_aic

R2_svm_aic <- R2(pred_svm_aic, test_data_aic$critical_temp)
R2_svm_aic
n_train_data_aic <- nrow(train_data_aic)
p_train_data_aic <- ncol(train_data_aic) - 1
adjR2_svm_aic <- 1 - (1 - R2_svm_aic) * (n_train_data_aic - 1) / (n_train_data_aic - p_train_data_aic - 1)

# Print the results
cat("R-squared: ", R2_svm_aic, "\n")
cat("Adjusted R-squared: ", adjR2_svm_aic, "\n")
#########################################################################

y_yhat <- data.frame(y_actual = test_data_self$critical_temp, y_predicted = pred_svm_self)

# plot y vs y_hat
ggplot(y_yhat, aes(x = y_actual, y = y_predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  labs(title = "Actual vs Predicted - SVM",
       x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggplot(y_yhat) + geom_point(aes(x=y_actual, y=y_predicted)) + geom_smooth(aes(x=y_actual, y=y_predicted)) + ggtitle('Actual Vs Predicted - SVM')



p1 <- ggplot(data = y_yhat,
             mapping = aes(x = y_actual, y = y_predicted)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Actual", y = "Predicted") +
  theme_bw() +
  ggtitle('Actual vs Predicted - SVM')

# Replace "histogram" with "boxplot" or "density" for other types
ggMarginal(p1, type="histogram")






########################################################################

svm_self_l <- svm(critical_temp ~ ., data = train_data_self, kernel = "linear")
svm_self_r <- svm(critical_temp ~ ., data = train_data_self, kernel = "radial")
svm_self_p <- svm(critical_temp ~ ., data = train_data_self, kernel = "polynomial")

pred_svm_self_l <- predict(svm_self_l, newdata = test_data_self)
rmse_svm_self_l <- sqrt(mean((test_data_self$critical_temp - pred_svm_self_l)^2))
rmse_svm_self_l

pred_svm_self_r <- predict(svm_self_r, newdata = test_data_self)
rmse_svm_self_r<- sqrt(mean((test_data_self$critical_temp - pred_svm_self_r)^2))
rmse_svm_self_r

pred_svm_self_p <- predict(svm_self_p, newdata = test_data_self)
rmse_svm_self_p <- sqrt(mean((test_data_self$critical_temp - pred_svm_self_p)^2))
rmse_svm_self_p



model_names <- c("SVM (linear)", "SVM (radial)", "SVM (polynomial)")
rmse <- c(rmse_svm_self_l, rmse_svm_self_r, rmse_svm_self_p)
data <- data.frame(model_names, rmse)

# plot the RMSE values for different SVM models
ggplot(data, aes(x = model_names, y = rmse)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  labs(title = "Comparison of SVM Models",
       x = "Model",
       y = "RMSE") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))



##########################################################################

# Decisoion Tress

library(rpart)
tree_self <- rpart(critical_temp ~ ., data = train_data_self, control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 10, minbucket = 5, maxcompete = 2))
tree_corr <- rpart(critical_temp ~ ., data = train_data_corr, control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 10, minbucket = 5, maxcompete = 2))
tree_aic <- rpart(critical_temp ~ ., data = train_data_aic, control = rpart.control(cp = 0.005, maxdepth = 6, minsplit = 10, minbucket = 5, maxcompete = 2))

pred_tree_self <- predict(tree_self, newdata = test_data_self)
rmse_tree_self <- sqrt(mean((test_data_self$critical_temp - pred_tree_self)^2))
rmse_tree_self

R2_tree_self <- R2(pred_tree_self, test_data_self$critical_temp)
R2_tree_self
n_train_data_self <- nrow(train_data_self)
p_train_data_self <- ncol(train_data_self) - 1
adjR2_tree_self <- 1 - (1 - R2_tree_self) * (n_train_data_self - 1) / (n_train_data_self - p_train_data_self - 1)

# Print the results
cat("R-squared: ", R2_tree_self, "\n")
cat("Adjusted R-squared: ", adjR2_tree_self, "\n")




pred_tree_corr<- predict(tree_corr, newdata = test_data_corr)
rmse_tree_corr <- sqrt(mean((test_data_corr$critical_temp - pred_svm_corr)^2))
rmse_tree_corr

R2_tree_corr <- R2(pred_tree_corr, test_data_corr$critical_temp)
R2_tree_corr

n_train_data_corr <- nrow(train_data_corr)
p_train_data_corr <- ncol(train_data_corr) - 1
adjR2_tree_corr <- 1 - (1 - R2_tree_corr) * (n_train_data_corr - 1) / (n_train_data_corr - p_train_data_corr - 1)

# Print the results
cat("R-squared: ", R2_tree_corr, "\n")
cat("Adjusted R-squared: ", adjR2_tree_corr, "\n")




pred_tree_aic<- predict(tree_aic, newdata = test_data_aic)
rmse_tree_aic <- sqrt(mean((test_data_aic$critical_temp - pred_svm_aic)^2))
rmse_tree_aic

R2_tree_aic <- R2(pred_tree_aic, test_data_aic$critical_temp)
R2_tree_aic
n_train_data_aic <- nrow(train_data_aic)
p_train_data_aic <- ncol(train_data_aic) - 1
adjR2_tree_aic <- 1 - (1 - R2_tree_aic) * (n_train_data_aic - 1) / (n_train_data_aic - p_train_data_aic - 1)

# Print the results
cat("R-squared: ", R2_tree_aic, "\n")
cat("Adjusted R-squared: ", adjR2_tree_aic, "\n")



####################################################################################

rpart.plot(tree_self, main = "Decision Tree for Superconductor Dataset", 
           extra = 101, box.palette = "RdBu", cex = 0.5)



####################################################################################

#mODEL sUMMARIES

summary(rdm_self)
summary(rdm_corr)
summary(rdm_aic)


summary(tree_self)
summary(tree_corr)
summary(tree_aic)


summary(svm_self)
summary(svm_self)
summary(svm_self)

######################################################################################

################### RD\F ###################################
feature_importance <- importance(rdm_self)
sorted_importance <- sort(feature_importance, decreasing = TRUE)
plot_data <- data.frame(feature_importance)
plot_data_2 <- plot_data %>% rownames_to_column(var = "Features")
# create the bar plot
ggplot(plot_data_2, aes(x=Features , y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance of RF") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


################### Decision Tree ###################################

importance <- varImp(tree_self)


plot_data <- data.frame(importance)
plot_data_2 <- plot_data %>% rownames_to_column(var = "Features")

# create the bar plot
ggplot(plot_data_2, aes(x = Features, y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance of DT") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


################### Support Vector Machine ###################################

library(rminer)
#svm_self <- svm(critical_temp ~ ., data = train_data_self, kernel = "linear")

M <- fit(critical_temp~., data=train_data_self, model="svm", kpar=list(sigma=0.10), C=2)
svm.imp <- Importance(M, data=train_data_self)

features <- c('Const','number_of_elements','wtd_entropy_atomic_mass',
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
              'wtd_std_Valence')
plot_data<- data.frame(svm.imp$imp, features)


#plot_data_2 <- plot_data %>% rownames_to_column(var = "Features")

# create the bar plot
ggplot(plot_data, aes(y = svm.imp.imp, x = features)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance SVM") +
  ggtitle("Feature Importance of SVM") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))




library(mgcv)



Self_Red_GAM <- gam(critical_temp ~ s(wtd_std_Valence, k = 3),data = my_data)

Self_Red_GAM <- gam(critical_temp ~ s(number_of_elements) + s(wtd_entropy_atomic_mass, bs = "cr", k = 3) + s(wtd_std_atomic_mass, bs = "cr", k = 3) + s(wtd_entropy_fie, bs = "cr", k = 3) + s(wtd_std_fie, bs = "cr", k = 3) + s(wtd_entropy_atomic_radius, bs = "cr", k = 3) + s(wtd_std_atomic_radius, bs = "cr", k = 3) + s(wtd_entropy_Density, bs = "cr", k = 3) + s(wtd_std_Density, bs = "cr", k = 3) + s(wtd_entropy_ElectronAffinity, bs = "cr", k = 3) + s(wtd_std_ElectronAffinity, bs = "cr", k = 3) + s(wtd_entropy_FusionHeat, bs = "cr", k = 3) + s(wtd_std_FusionHeat, bs = "cr", k = 3) + s(wtd_entropy_ThermalConductivity, bs = "cr", k = 3) + s(wtd_std_ThermalConductivity, bs = "cr", k = 3) + s(wtd_entropy_Valence, bs = "cr", k = 3) + s(wtd_std_Valence, bs = "cr", k = 3),data = my_data)
