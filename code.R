#### LOAD LIBRARIES ----
library(glmnet)
library(pROC)
library(caret)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
#install.packages("factoextra")
library(ggfortify)
library(factoextra)

#### Problem 1: COVID ----

# Load the dataset
df <- readxl::read_excel("Immunologic profiles of patients with COVID-19.xlsx")

# Check and encode Categorical Variables
table(df$SEX) #only two severities --> set as categorical with 1=severe
df$SEX <- ifelse(df$SEX == "M", 1, 0) # Male: 1, Female: 0
table(df$Severirty) #only two severities --> set as categorical with 1=severe
df$Severirty <- as.numeric(df$Severirty == "Severe")
#NOTE: we see some data imbalance in both variables here

# Handle Missing Values (no missing values found) --> no need for imputation
table(is.na(df))

# Remove 'Patient Number' column if it's not needed for the model
df$Patient.Num <- NULL

# Split the Data
set.seed(1717) # For reproducibility
trainIndex <- createDataPartition(df$Severirty, p = .75, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Normalize the Data
preProcValues <- preProcess(trainData[, -which(names(trainData) == "Severirty")], method = c("center", "scale"))
trainData_norm <- predict(preProcValues, trainData[, -which(names(trainData) == "Severirty")])
testData_norm <- predict(preProcValues, testData[, -which(names(testData) == "Severirty")])

# Convert to matrix form as required by glmnet
x_train <- as.matrix(trainData_norm)
y_train <- trainData$Severirty
x_test <- as.matrix(testData_norm)
y_test <- testData$Severirty

# Explore a range of alpha values and select the best model
alpha_values <- seq(0, 1, 0.01) 
auc_values <- numeric(length(alpha_values))

#fit to get foldID
set.seed(1717)
cvx <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", nfolds = 10, keep=TRUE)
#keep randomly generated fold IDs
foldid <- cvx$foldid 
foldid

#grid search 
for (i in 1:length(alpha_values)) {
  cvfit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = alpha_values[i], type.measure = "auc", foldid=foldid)
  auc_values[i] <- max(cvfit$cvm)
}

# Identify the best alpha based on highest AUC
best_alpha_index <- which.max(auc_values)
best_alpha_index
best_alpha <- alpha_values[best_alpha_index]
best_alpha
best_auc <- auc_values[best_alpha_index]
best_auc

#CHECK ALPHAS (TRAINING PLOT)
plot(alpha_values,auc_values,pch=16,main="Cross-Validated AUC for Various Alpha Values",ylab="AUC") #no change in y value too much
abline(v=0.88)

# Fit a glmnet model with the best alpha
bestModel <- glmnet(x_train, y_train, family = "binomial", alpha = best_alpha)

# Visualize the coefficient path
plot(bestModel, xvar = "lambda", label = TRUE)
title(paste("Coefficient Path for Alpha =", best_alpha), line = 2.5)

# Train the model with the best alpha value using cross-validation
cv_fit_best_alpha <- cv.glmnet(x_train, y_train, family = "binomial", alpha = best_alpha, type.measure = "auc", foldid=foldid)

#PLOT LAMBDAS 
plot(cv_fit_best_alpha, main = "AUC (10-fold) for Alpha = 0.88 at Various Lambda Values\n")
cv_fit_best_alpha$lambda.min

# Predict and evaluate using the chosen model for TRAINING DATA
predictions_train_10 <- predict(cv_fit_best_alpha, s = "lambda.min", newx = x_train, type = "response")
roc_result_train_10 <- roc(response = y_train, predictor = predictions_train_10)
#plot(roc_result_train_10, main = paste("Training data (10-fold) ROC Curve for Alpha =", best_alpha))
auc_value_best_alpha_10_train <- auc(roc_result_train_10)
print(paste("Training data (10-fold) AUC for Alpha =", best_alpha, ":", auc_value_best_alpha_10_train))
snsp.train_10 <- cbind(roc_result_train_10$sensitivities,roc_result_train_10$specificities)
indx_10_train <- which.max(apply(snsp.train_10,1,min))  ### min-max approach! 
#best sensitivity/specificity
snsp.train_10[indx_10_train,]
## save best theshhold
cutoff_train_10 <- roc_result_train_10$thresholds[indx_10_train]
cutoff_train_10

# ROC Curve plot for Training data with threshold
plot(roc_result_train_10, main = paste("Training data (10-fold) ROC Curve for Alpha =", best_alpha))
abline(h = snsp.train_10[indx_10_train, 1], v = snsp.train_10[indx_10_train, 2], col = 'blue', lty = 2)
print(paste("Training data (10-fold) AUC for Alpha =", best_alpha, ":", auc_value_best_alpha_10_train))
print(paste("Training data threshold:", cutoff_train_10))

# Predict and evaluate for the chosen model for TESTING DATA
predictions_test_10 <- predict(cv_fit_best_alpha, s = "lambda.min", newx = x_test, type = "response")
roc_result_test_10 <- roc(response = y_test, predictor = predictions_test_10)
#plot(roc_result_test_10, main = paste("Testing data (10-fold) ROC Curve for Alpha =", best_alpha))
auc_value_best_alpha_10_test <- auc(roc_result_test_10)
print(paste("Testing data (10-fold) AUC for Alpha =", best_alpha, ":", auc_value_best_alpha_10_test))
snsp.test_10 <- cbind(roc_result_test_10$sensitivities,roc_result_test_10$specificities)
indx_10_test <- which.max(apply(snsp.test_10,1,min))  ### min-max approach! 
#best sensitivity/specificity
snsp.test_10[indx_10_test,]
## save best theshhold
cutoff_test_10 <- roc_result_test_10$thresholds[indx_10_test]
cutoff_test_10

# ROC Curve plot for Testing data with threshold
plot(roc_result_test_10, main = paste("Testing data (10-fold) ROC Curve for Alpha =", best_alpha))
abline(h = snsp.test_10[indx_10_test, 1], v = snsp.test_10[indx_10_test, 2], col = 'blue', lty = 2)
print(paste("Testing data (10-fold) AUC for Alpha =", best_alpha, ":", auc_value_best_alpha_10_test))
print(paste("Testing data threshold:", cutoff_test_10))

#Check AUC for each alpha on testing data and plot
# Assuming modelList already contains the fitted models for each alpha
# Initialize a numeric vector to store AUC values corresponding to each alpha for the test set
modelList <- list()
for(i in seq_along(alpha_values)) {
  cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = alpha_values[i], type.measure = "auc", foldid = foldid)
  modelList[[i]] <- cv_fit
}
ENpredictedAUC_test <- numeric(length(alpha_values))
# Predict and calculate AUC for each model using the test set
for (i in seq_along(alpha_values)) {
  # Predict probabilities using the best lambda for the current model
  predicted_probabilities <- predict(modelList[[i]], newx = x_test, s = "lambda.min", type = "response")
  
  # Calculate AUC using the roc function from pROC package
  roc_result <- roc(response = y_test, predictor = as.vector(predicted_probabilities[, 1]))
  ENpredictedAUC_test[i] <- auc(roc_result)
}
# Combine alpha values and their corresponding AUC values into a data frame for plotting
ENpredictionAUC_test <- data.frame(Alpha = alpha_values, AUC = ENpredictedAUC_test)
# Plot AUC vs. Alpha using base R plot for test data
plot(ENpredictionAUC_test$Alpha, ENpredictionAUC_test$AUC, type = 'b', main = "AUC vs. Alpha on Test Data", xlab = "Alpha", ylab = "AUC", pch = 19)
# Add a horizontal line at the maximum AUC value
abline(h = max(ENpredictionAUC_test$AUC), col = "red", lty = 2)
best_alpha_index_test <- which.max(ENpredictionAUC_test$AUC)
best_alpha_test <- alpha_values[best_alpha_index_test]
best_auc_test <- ENpredictionAUC_test$AUC[best_alpha_index_test]
print(paste("Best Alpha on Test Data:", best_alpha_test, "with AUC:", best_auc_test))


### AUC is 0.87 here with best alpha 0.22 for test set. While before from best alpha(0.88) AUC on testing set was 0.866 and for training 0.849. AUC is slightly better here.Since the auc of 0.877 is about the same as the one with alpha of 0.88, there is no need to train model with new alpha.
#___________________________________________________________________________________________________
#20 - FOLD-----------------------------------------------------------
#fit to get foldID
set.seed(1717)
cvx_20 <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", nfolds = 20, keep=TRUE)
#keep randomly generated fold IDs
foldid_20 <- cvx_20$foldid 
foldid_20

#grid search 
for (i in 1:length(alpha_values)) {
  cvfit_20 <- cv.glmnet(x_train, y_train, family = "binomial", alpha = alpha_values[i], type.measure = "auc", foldid=foldid_20)
  auc_values[i] <- max(cvfit_20$cvm)
}

# Identify the best alpha based on highest AUC
best_alpha_index_20 <- which.max(auc_values)
best_alpha_index_20
best_alpha_20 <- alpha_values[best_alpha_index_20]
best_alpha_20
best_auc_20 <- auc_values[best_alpha_index_20]
best_auc_20

#CHECK ALPHAS (TRAINING PLOT)
plot(alpha_values,auc_values,pch=16,main="Cross-Validated AUC for Various Alpha Values",ylab="AUC") #no change in y value too much
abline(v=0.95)

# Fit a glmnet model with the best alpha
bestModel_20 <- glmnet(x_train, y_train, family = "binomial", alpha = best_alpha_20)

# Visualize the coefficient path
plot(bestModel_20, xvar = "lambda", label = TRUE)
title(paste("Coefficient Path for Alpha =", best_alpha_20), line = 2.5)

# Train the model with the best alpha value using cross-validation
cv_fit_best_alpha_20 <- cv.glmnet(x_train, y_train, family = "binomial", alpha = best_alpha_20, type.measure = "auc", foldid=foldid_20)

#PLOT LAMBDAS (REVIEW THIS, WHAT IS IT TAKING THE MIN OF? AND WHY? )
plot(cv_fit_best_alpha_20, main = "AUC (20-fold) for Alpha = 0.95 at Various Lambda Values\n")
cv_fit_best_alpha_20$lambda.min

# Predict and evaluate using the chosen model for TRAINING DATA
predictions_train_20 <- predict(cv_fit_best_alpha_20, s = "lambda.min", newx = x_train, type = "response")
roc_result_train_20 <- roc(response = y_train, predictor = predictions_train_20)
auc_value_best_alpha_20_train <- auc(roc_result_train_20)
print(paste("Training data (20-fold) AUC for Alpha =", best_alpha, ":", auc_value_best_alpha_20_train))
snsp.train_20 <- cbind(roc_result_train_20$sensitivities,roc_result_train_20$specificities)
indx_20_train <- which.max(apply(snsp.train_20,1,min))  ### min-max approach! 
#best sensitivity/specificity
snsp.train_20[indx_20_train,]
## save best theshhold
cutoff_train_20 <- roc_result_train_20$thresholds[indx_20_train]
cutoff_train_20

# ROC Curve plot for Training data with threshold
plot(roc_result_train_20, main = paste("Training data (20-fold) ROC Curve for Alpha =", best_alpha_20))
abline(h = snsp.train_20[indx_20_train, 1], v = snsp.train_20[indx_20_train, 2], col = 'blue', lty = 2)
print(paste("Training data (20-fold) AUC for Alpha =", best_alpha_20, ":", auc_value_best_alpha_20_train))
print(paste("Training data threshold:", cutoff_train_20))

# Predict and evaluate for the chosen model for TESTING DATA
predictions_test_20 <- predict(cv_fit_best_alpha_20, s = "lambda.min", newx = x_test, type = "response")
roc_result_test_20 <- roc(response = y_test, predictor = predictions_test_20)
auc_value_best_alpha_20_test <- auc(roc_result_test_20)
print(paste("Testing data (20-fold) AUC for Alpha =", best_alpha_20, ":", auc_value_best_alpha_20_test))
snsp.test_20 <- cbind(roc_result_test_20$sensitivities,roc_result_test_20$specificities)
indx_20_test <- which.max(apply(snsp.test_20,1,min))  ### min-max approach! 
#best sensitivity/specificity
snsp.test_20[indx_20_test,]
## save best theshhold
cutoff_test_20 <- roc_result_test_20$thresholds[indx_20_test]
cutoff_test_20

# ROC Curve plot for Testing data with threshold
plot(roc_result_test_20, main = paste("Testing data (20-fold) ROC Curve for Alpha =", best_alpha_20))
abline(h = snsp.test_20[indx_20_test, 1], v = snsp.test_20[indx_20_test, 2], col = 'blue', lty = 2)
print(paste("Testing data (20-fold) AUC for Alpha =", best_alpha_20, ":", auc_value_best_alpha_20_test))
print(paste("Testing data threshold:", cutoff_test_20))

#Check AUC for each alpha on testing data and plot
# Assuming modelList already contains the fitted models for each alpha
# Initialize a numeric vector to store AUC values corresponding to each alpha for the test set
modelList_1 <- list()
for(i in seq_along(alpha_values)) {
  cv_fit_20 <- cv.glmnet(x_train, y_train, family = "binomial", alpha = alpha_values[i], type.measure = "auc", foldid = foldid_20)
  modelList_1[[i]] <- cv_fit_20
}
ENpredictedAUC_test_20 <- numeric(length(alpha_values))
# Predict and calculate AUC for each model using the test set
for (i in seq_along(alpha_values)) {
  # Predict probabilities using the best lambda for the current model
  predicted_probabilities_20 <- predict(modelList_1[[i]], newx = x_test, s = "lambda.min", type = "response")
  
  # Calculate AUC using the roc function from pROC package
  roc_result_20 <- roc(response = y_test, predictor = as.vector(predicted_probabilities_20[, 1]))
  ENpredictedAUC_test_20[i] <- auc(roc_result_20)
}
# Combine alpha values and their corresponding AUC values into a data frame for plotting
ENpredictionAUC_test_20 <- data.frame(Alpha = alpha_values, AUC = ENpredictedAUC_test_20)
# Plot AUC vs. Alpha using base R plot for test data
plot(ENpredictionAUC_test_20$Alpha, ENpredictionAUC_test_20$AUC, type = 'b', main = "AUC vs. Alpha on Test Data", xlab = "Alpha", ylab = "AUC", pch = 19)
# Add a horizontal line at the maximum AUC value
abline(h = max(ENpredictionAUC_test_20$AUC), col = "red", lty = 2)
best_alpha_index_test_20 <- which.max(ENpredictionAUC_test_20$AUC)
best_alpha_test_20 <- alpha_values[best_alpha_index_test_20]
best_auc_test_20 <- ENpredictionAUC_test_20$AUC[best_alpha_index_test_20]
print(paste("Best Alpha on Test Data:", best_alpha_test_20, "with AUC:", best_auc_test_20))
#_______________________________________________________________________________________________

#Which cytokines are most effective at predicting the COVID-19 disease progression in patients?
# Extract and examine non-zero coefficients to identify influential features excluding the intercept

coef_matrix <- coef(cv_fit_best_alpha, s = "lambda.min")
coef_df <- as.data.frame(as.matrix(coef_matrix), stringsAsFactors = FALSE)
names(coef_df) <- c("Coefficient")
coef_df$Feature <- rownames(coef_matrix)
# Exclude the intercept from the significant features
significant_features <- coef_df[coef_df$Coefficient != 0 & rownames(coef_matrix) != "(Intercept)", ]
print(significant_features)

# Initialize an empty data frame to store results
results <- data.frame(Cytokine = character(), Coefficient = numeric(), PValue = numeric(), stringsAsFactors = FALSE)
# Logistic regression for each cytokine against severity
for(cytokine in colnames(df)[!colnames(df) %in% c("Severirty")]) {
  formula_str <- paste("Severirty ~", "`", cytokine, "`", sep="")
  formula <- as.formula(formula_str)
  model <- glm(formula, data = df, family = "binomial")
  # Extract coefficient and p-value for the cytokine
  if(length(coef(summary(model))) > 1){ # Check if the model has coefficients (in case of perfect separation etc.)
    coef_val <- coef(summary(model))[2,1]  # Second row corresponds to cytokine coefficient
    p_val <- coef(summary(model))[2,4]     # P-value of the cytokine coefficient
    # Append to the results data frame
    results <- rbind(results, data.frame(Cytokine = cytokine, Coefficient = coef_val, PValue = p_val))
  }
}
ordered_results <- results[order(results$PValue), ]
print(ordered_results)

# Model evaluation
# Evaluate accuracy, sensitivity, specificity, precision
# 10-fold
# Generate threshold for classification
threshold_test_10 <- roc_result_test_10$thresholds[which.max(roc_result_test_10$sensitivities + roc_result_test_10$specificities - 1)]
predicted_classes_test_10 <- ifelse(predictions_test_10 > threshold_test_10, 1, 0)
# Create confusion matrix
confusion_matrix_test_10 <- table(Predicted = predicted_classes_test_10, Actual = y_test)
# Calculate metrics
accuracy_test_10 <- sum(diag(confusion_matrix_test_10)) / sum(confusion_matrix_test_10)
sensitivity_test_10 <- confusion_matrix_test_10[2,2] / sum(confusion_matrix_test_10[2,])
specificity_test_10 <- confusion_matrix_test_10[1,1] / sum(confusion_matrix_test_10[1,])
precision_test_10 <- confusion_matrix_test_10[2,2] / sum(confusion_matrix_test_10[,2])
# Print metrics
cat("10-Fold Cross-validation Metrics:\n")
cat(sprintf("Accuracy: %f\n", accuracy_test_10))
cat(sprintf("Sensitivity: %f\n", sensitivity_test_10))
cat(sprintf("Specificity: %f\n", specificity_test_10))
cat(sprintf("Precision: %f\n", precision_test_10))

# 20-fold
threshold_test_20 <- roc_result_test_20$thresholds[which.max(roc_result_test_20$sensitivities + roc_result_test_20$specificities - 1)]
predicted_classes_test_20 <- ifelse(predictions_test_20 > threshold_test_20, 1, 0)
# Create confusion matrix
confusion_matrix_test_20 <- table(Predicted = predicted_classes_test_20, Actual = y_test)
# Calculate metrics
accuracy_test_20 <- sum(diag(confusion_matrix_test_20)) / sum(confusion_matrix_test_20)
sensitivity_test_20 <- confusion_matrix_test_20[2,2] / sum(confusion_matrix_test_20[2,])
specificity_test_20 <- confusion_matrix_test_20[1,1] / sum(confusion_matrix_test_20[1,])
precision_test_20 <- confusion_matrix_test_20[2,2] / sum(confusion_matrix_test_20[,2])
# Print metrics
cat("20-Fold Cross-validation Metrics:\n")
cat(sprintf("Accuracy: %f\n", accuracy_test_10))
cat(sprintf("Sensitivity: %f\n", sensitivity_test_10))
cat(sprintf("Specificity: %f\n", specificity_test_10))
cat(sprintf("Precision: %f\n", precision_test_10))

#### PROBLEM 2: T-Cell Types ----



#### LOAD & FORMAT FILE ----

#load file
load(file="geneexpression2.rda")
View(dat)

#format metadata in new columns & clean data
df<- dat %>% tibble::rownames_to_column() %>% rename("ID"=rowname) %>%  #move the row names into a column
  separate_wider_delim(ID, "_", names = c("Status", "CellType", "Rep")) %>% #sperate meta data into new cols
  mutate(Status = str_replace (Status, "\\d+", "")) #clean numbers from HEA/MEL identification

#### Look at file ----

#isolate all the variables from the metadata
dfVariables <- df[, c(-1, -2, -3)]
dim(dfVariables) #we have 30 observations of 156 variables

#### PERFORM PCA ----

#check distribution of mean and standard deviation (range of both are high; we should normalize)
apply(dfVariables,2, mean) %>% sort() #around -4 to 4, difference in mean. 
apply(group_by(df, CellType),2, sd) %>% sort() #large difference on STDV

#compute PCs (uncorrelated linear combination explaining as much variance as possible)
pc <- prcomp(dfVariables, center = TRUE, scale. = TRUE)
#NOTE: we center because the we are are not interested in the relative mean of each variable. That is, their mean expression levels are not interesting to us, we only wish to know how these expression levels differ between the different groups. We also scale, because we are interested in fold-differences and not the absolute difference in expression levels, as that naturally vary between biological replicates. 


#these are the loading vectors (eg. the eigen vectors, explains contribution to the PC) 
pc$rotation
#check that they are normalized (yes they are)
apply(pc$rotation, 2, function(i) sqrt(sum(i^2)))


#PLOT

#Scree Plot (proportion of variance for each PC)
p <- fviz_eig(pc, xlab = "Principal component", addlabels=TRUE, hjust = -0.1, main = "Figure B1. Scree Plot") +
  ylim(0, 70)
print(p)

#Cumulative sum of PC variance plot
plot( 1:30 , summary(pc)$importance[3,], ylim = c(0, 1), 
      main = "Figure B2. Cumulative sum of variance over PC", 
      ylab = "Cumulative variance explained",
      xlab="Principal component", type = "b", xaxt="n")
grid(nx = 30)
axis(1, at = seq(1, 30, by = 1), las=2)


#Examine Loading Vectors

#take all loading vectors in PC1, sort by magnitude, take names of top 10
top10pc1 <- data.frame(pc$rotation[, 1]) %>% 
  mutate(magnitude = abs(pc.rotation...1.)) %>% 
  arrange(desc(magnitude)) %>% head(10) %>% row.names()


#take all loading vectors in PC2, sort by magnitude, take names of top 10
top10pc2 <- data.frame(pc$rotation[, 2]) %>% 
  mutate(magnitude = abs(pc.rotation...2.)) %>% 
  arrange(desc(magnitude)) %>% head(10) %>% row.names()

#No overlap between the top 10 loading vectors of the two PCs
intersect(top10pc1, top10pc2)


#PCA plots

#PCA biplot by cell type with top 10 explainer genes PC1
fviz_pca_biplot(pc, repel = TRUE,
                select.var = list(name=c(top10pc1)),
                habillage=df$CellType,
                label = "var",
                geom = "point", 
                pointsize = 3, 
                title = "Figure B3. PCA biplot by cell type with top 10 explainer genes PC1",
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#PCA biplot by cell type with top 10 explainer genes PC2
fviz_pca_biplot(pc, repel = TRUE,
                select.var = list(name=c(top10pc2)),
                habillage=df$CellType,
                label = "var",
                geom = "point", 
                pointsize = 3, 
                title = "Figure B4. PCA biplot by cell type with top 10 explainer genes of PC2",
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#PCA by cell status
fviz_pca_biplot(pc, repel = TRUE,
                select.var = list(contrib=15),
                habillage=df$Status,
                label = "var",
                geom = "point", 
                pointsize = 3, 
                title = "Figure B5. PCA biplot by cell status with top 15 contributing genes",
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

