# ECON-300-Thesis-Work
Public Repository of the data collected, survey created and analysis conducted as part of an ECON 300 Senior Thesis at UPenn.

Academic Year 2018-2019

## File Directory Description
### Final Thesis Paper:

### Final Thesis Presentation:
A final presentation of this thesis was given on May 1, 2019. This Presentation is available in the main directory as a .pdf file and is titled "final_seminar_presentation".

### Data Files:
All data files collected or created can be found within the "Data" subdirectory.
#### Qualtrics Data-
Within the "Qualtrics_data" subdirectory, there are two raw data files. Survey_Results_final.csv was collected during full deployment of the final survey. It was cleaned and used for the full analysis.

trial_survey_results.csv was collected while piloting the experimental design with Qualtrics and Mechanical Turk. It was decided before deployment of the trial that all data collected would not be used for analysis.

#### MTurk Data-
Within the "MTurk_batches" subdirectory, there are four raw data files. Three of the .csv files were collected from Mechanical Turk across three different batches. All batches used the same survey, and they were merged and cleaned for the full analysis. Using different batches allowed for faster data collection, as too big of a batch deployed for too long ends up getting a very low rate of responses per hour.

The last file, data_trial_50.csv, was data collected while piloting the experimental design with Qualtrics and Mechanical Turk. It was decided before deployment of the trial that all data collected would not be used for analysis.

#### Calculated Demographic Data-
Within the "demographics" subdirectory, there are several processed data files. These data files are titled in correspondence with the demographic data question the file deals with. Each file shows the distribution of responses to the demographic question, broken down by experimental treatment.

### Qualtrics Survey Information:
The relevant folder is the "Survey structure" subdirectory. It contains a .pdf file which was generated by exporting the Qualtrics survey used as a Word document, and then saving that document as a .pdf. There is also a .qsf file. Any Qualtrics user can upload this file to exactly replicate the survey deployed as part of the conducted experiment.

### Code Used for Analysis (R):
Both .R files used to conduct this analysis are found within the "R Code" subdirectory.

#### Random Number Generation for Survey-
"Random Number.R", this file was used to generate the random numbers presented to subjects within the ambiguous-risk treatments.

#### Data Cleaning and Analysis, Regression, Bonus Payment Calculation-
qualtrics_analysis_script.R, this file was used to merge Mechanical Turk data with Qualtrics survey responses, clean the data into a usable form with only viable respondents included, conduct EDA, calculate the bootstrapped statistics, perform the gender-based regression analysis, and lastly allocate bonus payments to be later deployed in a python script.

### Bonus payment distribution:

paymentDeployment.py, found within the "Python Code" subdirectory, uses Mechanical Turk's API to automate the distribution of bonus payments calculated within the main R file.
