# Predicting COVID-19 Disease Progression Using Elastic-Net Regularized Logistic Regression

## Overview

This project focuses on developing a predictive model to assess COVID-19 disease progression based on cytokine profiles and patient characteristics using Elastic-Net regularized logistic regression. The Elastic-Net approach was chosen due to its ability to handle multicollinearity among predictors and select relevant features by combining the properties of both Ridge and Lasso regression techniques.

## Objectives

- Predict the progression of COVID-19 using cytokine levels and patient characteristics.
- Apply Elastic-Net regularization to handle multicollinearity and perform feature selection.
- Compare model performance using different cross-validation techniques (10-fold and 20-fold).

## Key Technologies

- **R**: For data analysis and model development.
- **glmnet**: An R package used for fitting generalized linear models with elastic-net regularization.
- **pROC**: Used for ROC curve analysis and calculating the area under the curve (AUC).
- **caret**: For data preparation, splitting datasets, and model evaluation.
- **ggplot2**: For data visualization.

## Data Description

The dataset, titled "Immunologic profiles of patients with COVID-19.xlsx," includes measurements of various cytokines, chemokines, and acute phase proteins from plasma samples of 76 COVID-19 positive patients. The dataset contains two key categorical variables: SEX and Severity, which have been encoded for analysis.

## Analysis Workflow

1. **Data Preparation**:
   - Loaded the necessary libraries and imported the dataset.
   - Encoded categorical variables (SEX and Severity) and removed unnecessary columns.
   - Split the dataset into training (75%) and test (25%) sets.
   - Normalized the data to improve model convergence and performance.

2. **Model Development**:
   - Applied Elastic-Net regularization to handle multicollinearity and perform feature selection.
   - Evaluated a range of alpha values from 0 to 1 using 10-fold cross-validation to identify the optimal model.
   - Compared model performance across different alpha values using the AUC metric.
   - Repeated the analysis using 20-fold cross-validation for further validation.

3. **Results**:
   - Identified an optimal alpha value of 0.88 for 10-fold cross-validation and 0.95 for 20-fold cross-validation.
   - Achieved an AUC of approximately 0.85 on training data and 0.87 on test data, indicating good model generalization.
   - Identified significant cytokines, including PTX3 and IL-6, as key predictors of severe COVID-19 outcomes.

4. **Interpretation**:
   - Analyzed the influence of cytokines and other predictors on COVID-19 disease progression.
   - Evaluated the consistency of model performance across different cross-validation techniques.

## Future Directions

- Explore additional predictive models and machine learning techniques to improve performance.
- Investigate the impact of demographic variables and comorbidities on disease progression.
- Apply the model to larger datasets to validate its generalizability.

## References

- Blanco-Melo, D., et al. (2020). Imbalanced host response to SARS-CoV-2 drives development of COVID-19. *Cell*, 181(5), 1036-1045.
- Coomes, E. A., & Haghbayan, H. (2020). Interleukin-6 in COVID-19: a systematic review and meta-analysis. *Reviews in Medical Virology*, 30(6), 1-9.
- Holmes, S., et al. (2005). Memory T cells have gene expression patterns intermediate between naïve and effector. *Proceedings of the National Academy of Sciences*, 102(15), 5519–5523.
- Lucas, C., et al. (2020). Longitudinal analyses reveal immunological misfiring in severe COVID-19. *Nature*, 584(7821), 463-469.

