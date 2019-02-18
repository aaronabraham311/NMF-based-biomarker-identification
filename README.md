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
All data is pulled from the open-source Alzheimer's disease database, ADNI. Specifically, 
