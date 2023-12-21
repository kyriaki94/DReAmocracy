# DReAmocracy
DReAmocracy exploits information such as proposed lists of drugs by previous studies and clinical trials and creates a dynamic reference matrix with enhanced resolution. Through a weight-modulated majority voting of the modes of action, initial indications and targeted pathways of the drugs in a well-known repository, Drug Repurposing Hub, it generates  a disease suitability score for each drug from the selected library. As a testbed, we applied this method to a group of neurodegenerative diseases (Alzheimer’s, Parkinson’s, Huntington’s and Multiple Sclerosis). A super-reference table with drug suitability scores is created for all four neurodegenerative diseases.

## in case the user wants to use our approach while selecting different weights, a simple R shiny web application is available. The user can choose a drug of interest for one of the four diseases available, the weights of MoAs, Inds and Paths and also the type of studies (CDRS and/or CTS)
  
## input files:
* input files can be found in the data folder and should be downloaded as it is in order to run the app.

* the user should only change the working directory


## weights:

* default weights used in the paper: indication=0.2,moa=0.4,pathway=0.4

* default weights used in the paper: trials=0.5,repurposed=0.5

* note: the above weights can be adjusted by the user (sum of weights =1)



