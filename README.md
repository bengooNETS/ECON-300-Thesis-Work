# ECON-300-Thesis-Work
Public Repository of data collected and R code created as part of an ECON 300 Senior Thesis at UPenn.

Academic Year 2018-2019

## File Directory Description
### This is the thesis PDF:

### Final thesis presentation:
A final presentation of this thesis was given on May 1, 2019. This Presentation is available as a .pdf file and is titled "".

### Raw data files can be found here:

#### Qualtrics Data-

#### MTurk Data-

#### Calculated Demographic Data

### Qualtrics Survey Information:
The relevant folder is the "Survey structure" folder. It contains a .pdf file which was generated by exporting the Qualtrics survey used as a Word document, and then saving that document as a .pdf. There is also a .qsf file. Any Qualtrics user can upload this file to exactly replicate the survey deployed as part of the conducted experiment.

### R-Code can be found here:
Both .R files used to conduct this analysis are found within the "R Code" subdirectory.

#### Random number generation for survey-
"Random Number.R", this file was used to generate the random numbers presented to subjects within the ambiguous-risk treatments.

#### Data Cleaning and Analysis, Regression, Bonus Payment Calculation-
qualtrics_analysis_script.R, this file was used to merge Mechanical Turk data with Qualtrics survey responses, clean the data into a usable form with only viable respondents included, conduct EDA, calculate the bootstrapped statistics, perform the gender-based regression analysis, and lastly allocate approporiatley bonus payments to be deployed later in a python script.

### Bonus payment distribution:

paymentDeployment.py, this file uses Mechanical Turk's API to automate the distribution of bonus payments calculated within the main R file.
