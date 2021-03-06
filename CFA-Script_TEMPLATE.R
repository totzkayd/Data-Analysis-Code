
### -----------------------------------------------------------------------------------------------###

## Last saved: 6-21-17
## Saved by: Dan Totzkay
## Description: Using R for Confirmatory Factor Analysis

### ---------------------------------------------------------------------------------------------- ###

######################################################################################################
##################################                                 ###################################
##################################       Confirmatory Factor       ###################################
##################################             Analysis            ###################################
##################################                                 ###################################
##################################   per Hunter & Gerbing (1982)   ###################################
##################################                                 ###################################
######################################################################################################

### ----------------------------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------------------------- ###

##  The purpose of this R script is to facilitate the use of confirmatory factor analysis using the 
##   centroid solution (see Hunter & Gerbing, 1982). Sections of this script that must be edited by 
##   the user are noted in comments, as are sections that can be left the way they are. For the most 
##   part, so long as you know some very basic aspects of your data (e.g., variable names, sample 
##   size), there should be little difficulty utlizing this code, even if you have no experience with 
##   syntax. If you made changes, remember to keep letter case consistent, close all quotations and 
##   parentheses, and define any variable/object you intend to manipulate prior to doing so. For best
##   outcomes, use RStudio to implement this code and, if you run into errors or have questions about
##   a specific action, you can always enter the function into a new line with a question mark before
##   it (e.g., ?View) and a help file will open in the lower right pane of RStudio. 

### ----------------------------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------------------------- ###

##  First, define your working directory within the quotation marks. This should be where your 
##    data are and where you would like to save your files. Find this by opening the information 
##    of the folder you are wanting to set and type that information in here. Separate each 
##    folder with a slash (/). An example of this on a Mac (Windows may be different) would be
##    setwd("/Users/Dan/Desktop/CFA")

setwd("")

##  Make sure the working directory is correct.

getwd()

### ----------------------------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------------------------- ###

##  If you do not have the lessR, stats, or strinr packages installed, do so with these lines.

install.packages("lessR")
install.packages("stats")
install.packages("stringr")

## Now, load lessR, stats, and stringr. Even if you just installed them, you still need to load them.

library(lessR)
library(stats)
library(stringr)

### ----------------------------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------------------------- ###

##  Load your dataset into R; to do this, make sure a .csv file of your data is saved within the
##    folder you set as the working directory above. Type the name of the file within the 
##    quotation marks on the line below in the read.csv() function.

mydata <- data.frame(read.csv(".csv"))

View(mydata)

######################################################################################################
########################################                     #########################################
########################################   Specify Factors   #########################################
########################################                     #########################################
######################################################################################################

##  Specify which variables in your dataset will be used for each factor. On the right of each  
##   arrow, leave a space and then type "mydata$" followed immediately (no space) with the name 
##   of the variable in your data set (which is assigned to "mydata").

### ----------------------------------------------------------------------------------------------- ###

## Factor 1 ##

F1_1 <- mydata$
F1_2 <-
F1_3 <-
F1_4 <-
F1_5 <-

### ----------------------------------------------------------------------------------------------- ###

## Factor 2 ##

F2_1 <- 
F2_2 <-
F2_3 <-
F2_4 <-
F2_5 <-

### ----------------------------------------------------------------------------------------------- ###

## Factor 3 ##

F3_1 <- 
F3_2 <-
F3_3 <-
F3_4 <-
F3_5 <-

### ----------------------------------------------------------------------------------------------- ###

## Create a new data object that is only the variables you are using in your CFA. If you have assigned
##  your variables to names other than those provided in the template, enter them into the parentheses
##  in the order you have listed them above, separated by a comma. A hard return is seen in this 
##  template (following "F3_2") simply to create a cleaner and slimmer file. You do not need to retain 
##  this in your use.

variables_CFA <- data.frame(F1_1, F1_2, F1_3, F1_4, F1_5, F2_1, F2_2, F2_3, F2_4, F2_5, F3_1, F3_2, 
                            F3_3, F3_4, F3_5)
View(variables_CFA)

### ------------------------------------------------------------------------------------------------###
### ------------------------------------------------------------------------------------------------###

##  Now, calculate your correlation matrix with listwise deletion. You do not need to change
##   anything on this line. No need to modify this.

corr_matrix <- cor(variables_CFA, use = "complete.obs") 
View(corr_matrix)

## Export your correlation matrix to a text file for your review. Put the name you would like the 
##    file to be in the quotation marks

write.csv(corr_matrix, file = "Correlation Matrix for CFA")

######################################################################################################
########################################                     #########################################
########################################     Conduct CFA     #########################################
########################################                     #########################################
######################################################################################################

##  Conduct the CFA. For each factor (e.g., "F1"), type the first item included in the factors, a  and 
##   colon, then the last item included in that factor into c(#:#). These items are in order per your 
##   correlation matrix rows.

myCFA <- corCFA(x = corr_matrix, F1=c(1:5), F2=c(6:10), F3=c(11:15), sort=FALSE, resid=TRUE)

##  Write your CFA output to a txt file in your working directory. Highlight and run the following 
##   three lines all at once. You can also simply run the "myCFA" line to have these results populate
##   your R console. No need to modify this.

sink("CFA Output")
myCFA
sink()

######################################################################################################
########################################                     #########################################
########################################     Fit Indices     #########################################
########################################                     #########################################
######################################################################################################

##  Find the "Total sum of squares for all items" number in the CFA output - type the number directly  
##  to the right of the arrow with a single space between it and the number

SS_total <- 

##  How many items did you include in your CFA? Again, type this directly to the right of the arrow
##   with one space.

number_items <- 

##  What is your sample size? Again, type this directly to the right of the arrow with one space.

N <- 

##  Calculate the  number of correlations in your matrix (lower left triangle). No need to modify this.

number_corr <- ((number_items^2) - number_items)/2

##  Calculate average correlation coefficient. No need to modify this.

avg_corr <- ((sum(corr_matrix) - number_items)/2)/number_corr
avg_corr

##  Calculate the expected error. This is used to calculate 95% confidnece intervals around the 
##   average correlation, as well as to evaluate fit vs. RMSE and  vs. residuals in the residual
##   matrix of the CFA output. No need to modify this.

expected_error <- 1.96*((1 - avg_corr^2)/(sqrt(N - 1)))
expected_error

### ------------------------------------------------------------------------------------------------###
### ------------------------------------------------------------------------------------------------###

##  Calculate RMSE. No need to modify this.

RMSE = sqrt(SS_total/number_corr)
RMSE

##  Is your RMSE within expected error? TRUE if yes, FALSE if no. 

RMSE <= expected_error

### ----------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------###

##  Compute a 95% confidence interval (it will print to your console). No need to modify this. However, 
##   again, a hard return in included in this template, which you do not need to retain for your use.

CI_output <- str_c("P(",(avg_corr - expected_error)," <= r_avg <= ",(avg_corr + expected_error),
                   ") = 0.95", sep = "")
CI_output

### ----------------------------------------------------------------------------------------------- ###
### ------------------------------------------------------------------------------------------------###

### Calculate CFI ###

##### This is still needed. Once Dan learns enough, this section of code should be able to calculate
#####   the fit of the measurement model, assign the CFI to an object named "CFI," and then be used
####    in the code following this to give a text output of fit indices in a manuscript.

CFI <- 

## Fit indices to report in your manuscript

Fit_Indices <- str_c("(RMSE = ", RMSE, ", CFI =", CFI, ").", sep = "")

Fit_Indices

######################################################################################################
########################################                     #########################################
########################################  Example Write-up   #########################################
########################################                     #########################################
######################################################################################################

##  Highlight the following section of code and run as a whole. This will provide you with a text file
##   that you may use to report your CFA results in a manuscript.

sink("CFA Write-Up for Results Section")
Results <- str_c("A confirmatory factor analysis was employed to assess the validity of the presently
                 employed measures, using the centroid solution (Hunter & Gerbing, 1982) in R with 
                 the corCFA function. Tests for internal consistency and parallelism were performed 
                 for each of the  factors to test the fit of correlations between items designated 
                 as alternate indicators of the same factor and the fit of the correlations between 
                 items indicated different factors, respectively. Results indicated adequate factor 
                 loadings and an acceptable level of error among indicators, demonstrating that 
                 the 3-factor model fit the present data", Fit_indices, sep = "")
sink()


######################################################################################################
########################################                     #########################################
########################################     References      #########################################
########################################                     #########################################
######################################################################################################

## Hunter, J. E., & Gerbing, D. W. (1982). Unidimensional measurement, second order factor 
##    analysis, and factor models. Research in Organizational Behavior, 4, 267-320. 

