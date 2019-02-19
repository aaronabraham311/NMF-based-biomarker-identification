# NMF-based machine learning for the identification of Alzheimer's disease biomarkers

## Introduction
With more than half a million Canadians living with Alzheimer's disease combined with a fast-growing senior population, the Canadian healthcare system is in dire need of a way to better diagnose Alzheimer's disease patients and treat them earlier. 

Current-day diagnostic treatments rely on clinical diagnosis that are unable to fully diffrentiate the different types of dementias that have molecular differences rather than clinical differences. Inaccurate diagnoses is problematic, as it often leads to treatments that have, at best, no effect on the progression of Alzheimer's. Due to this unique problem, researchers are trying to use big data methodologies to identify chemicals in our cerebrospinal fluid (CSF) and blood to act as accurate biomarkers. 

These current-day methodologies involved 3 steps:
1. Sampling: patients with that have different conditions are sampled
2. Metabolomics: high-throughput machines like LC-MS are used to find small chemicals and measure concentrations
3. Statistical analysis: principal component analysis or factor analysis is used to find significant biomarkers.

Recently, non-negative matrix factorization (NMF) has been seen as a viable alternative to principal component analysis. NMF is able to create collections of k meta-metabolites that are able to significantly diffrentiate between different diagnoses. Unlike PCA, meta-metabolites are intrepretable and robust ([Metagene projection](https://www.pnas.org/content/101/12/4164))

## Objective
The objective of this project is to determine the efficacy of NMF in identifying key biomarkers fo Alzheimer's disease. This will be tested through the use of machine learning algorithms: a higher test accuracy indicates that a particular dimensionality reduction technique can successfully diffrentiate between different diagnoses. 

## Methodology
All data is pulled from the open-source Alzheimer's disease database, ADNI. Specifically,  566 patients and 148 proteins were included in the database (AD, healthy and MCI patients). The following procedures were conducted

### Data handling
1. All non-negative data was replaced with 0
2. Any rows with missing data were removed
3. Ceilings were used for extremely high outliers
4. Divide data into 70% training and 30% testing

### Projection
1. NMF:
    1. Created H and W matrices from training set with k = 5
    2. Convert test set into same hyperspace as training set using pseudo-inverses
2. PCA:
    1. Construct components with variance maximized for training data
    2. Scale test set onto same components
3. tSNE: Convert whole data into tSNE set and then split into training and testing set (data leakage! - not advised)

### Statistics and Visualizations
1. Rank metabolites in each NMF component and extract top 20 unique metabolites
2. Use ANOVA to find metabolites that are significantly different among the three groups
3. Use Tukey analysis for metabolites that are deemed statistically different under ANOVA to determine where they are significantly different
4. Visualize significantly different metabolites using violin plots, boxplots and correlation heatmaps

### Machine learning
1. Trained random forest, extreme gradient boosting, K-nearest neighbors and support vector machine and evaluate on test set
    1. Ensured random grid search with up-sampling of under-represented classes
2. Collect predictions of all models on training and test set. Use predictions to train an extreme gradient boosting classifier as an ensemble model.

### Explainability
1. Create permutation independence plots to determine the most significant features
2. Create partial dependence plots for a single important feature to determine how feature affects model
3. Use Shapley game theory values to determine how model predicts on a single example

## Results
TBA
