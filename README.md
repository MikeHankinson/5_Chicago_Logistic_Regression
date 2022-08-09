# 5_Chicago_Logistic_Regression
 
## Project: Logistic Regression

### Skills and Tools
* Incorporate categorical predictors
* One-way ANOVA
* F test 
* Extension of one-way ANOVA to two-way ANOVA
* Random Effects

### Context
Examine how logistic regression is a suitable alternative to linear regression for classification problems. 

### Problem Statement
Use student GRE scores and GPA to determine if a student was admitted (binary) to a particular university.  binary column indicating whether they  were admitted to a certain university. 

### Pre-processing Algorithm Methodology
1. Load in the data. 
2. GRE and GPA are measured on significantly different scales. To allow us to interpret these variables on the same range, scale both variables using standardization. This means each variable will have a mean of 0 and a standard deviation of 1. 
3. Set the dependent variable "admit" as a factor variable and perform logistic regression with two predictors: GRE and GPA. 

### Results
All code to complete this work is found in the attached [notebook](Module7_Homework.R) or with diagrams in the [Word File](Module7_Homework.docx).



