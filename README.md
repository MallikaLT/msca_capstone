# msca_capstone
Code for MSCA capstone

This repo contains code for both problems:

1) Provider Location (Zip Code) problem
2) Percent MAR problem

PROBLEM 1:

For the provider location problem, the first file you must run is final_data_and_features.R.  This code takes in MDC data, cleans it (removes services we don't want to analyze, like NDCs and such), and engineers features. It outputs a CSV with original data + features for every row for which we want to predict services.

Both PAM clustering and Logistic Regression use this as input.  

PAM Clustering has two steps (though they could be combined in one file if you want to do a which.max() on the silhouette width):
1. final_pam_clustering_optimalK.R: Find optimal K
2. final_pam_cluserting.R: Run clustering algorithm on optimal K

Residual Analysis also has two parts of code:
1. final_log_reg.R: Train a model and predict probability of being outside the claimant base for every row
2. final_resid_analysis.R: Summarize the results (I mostly used this code to output data to make charts locally)

All of this code was submitted to our server as batch jobs, so I have several print statements to monitor my jobs' progress, which you can remove if you want.  

Note: We tried Local Outlier Factor on the continuous variables (scaled) only, Robust Principal Component Analysis on the scaled, continuous variables only, and Association Rules Mining on early variables (not the entire, final list).  LOF and RPCA results did not pass a "sanity check." RPCA can't be tuned without some labelled data.  No good assocation rules were produced. 

For the residual analysis, we compared Naive Bayes, Random Forest, and Logistic Regression. Logistic Regression predicted well and was not computationally expensive, which is why we included that code only.

PROBLEM 2:

The Percent MAR (PercentMAR.R) code cleans the data, computes percent MAR, and presents functions to analyze (and visualize) a univariate distribution with the Tukey and Adjusted Tukey Methods.   
