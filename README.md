# Early-Prediction-of-Academic-Dropout
Identification of factors associated with academic dropout using machine learning techniques

## Abstract
This project analyses academic dropout in higher education with the aim of identifying relevant risk factors and developing predictive models that support early detection of students at risk. The study is based on a dataset of university students containing academic, demographic and socioeconomic variables. The methodology combines exploratory data analysis, supervised classification models and unsupervised techniques, and is fully implemented in R.

## Dataset
The original dataset used in this project is publicly available and was obtained from the UCI Machine Learning Repository.

As part of the work, the data were cleaned, filtered and preprocessed.  
The processed dataset used for the analysis is included in this repository to facilitate reproducibility.

Original dataset source:
- UCI Machine Learning Repository (link to be added)

Processed data:
- `data/processed/` contains the cleaned dataset used in the models.

## Repository structure
data/
 └── processed/
     └── data_v2.csv        # cleaned dataset used in the analysis

code/
 ├── 01_eda/                # exploratory data analysis
 ├── 02_logistic_regression/# logistic regression models
 ├── 03_decision_tree/      # decision tree models
 ├── 04_random_forest/      # random forest models
 ├── 05_gower_distance/     # computation of Gower distance
 ├── 06_pam/                # PAM clustering
 └── 07_compare_groups/     # descriptive comparison of groups

results/
 ├── figures/               # plots and visualizations
 └── tables/                # result tables

## Requirements
- R (version >= 4.0)
- RStudio 

Main R packages used:
- tidyverse
- caret
- randomForest
- rpart
- cluster
- compareGroups

## Reproducibility
The analysis is fully reproducible.  
Once the dataset is available, the scripts can be executed in the order provided in the `code/` folder to reproduce the results reported in the thesis.

## Author
Bachelor’s Thesis  
Universitat Politècnica de Catalunya (UPC)
