# Dremocracy
DReAmocracy exploits information such as proposed lists of drugs by previous studies and clinical trials and creates a dynamic reference matrix with enhanced resolution. Through a weight-modulated majority voting of the modes of action, initial indications and targeted pathways of the drugs in a well-known repository, Drug Repurposing Hub, it generates  a disease suitability score for each drug from the selected library. As a testbed, we applied this method to a group of neurodegenerative diseases (Alzheimer’s, Parkinson’s, Huntington’s and Multiple Sclerosis). A super-reference table with drug suitability scores is created for all four neurodegenerative diseases.

#input files
#_input_refDis: for ref diseases inputs

#_input_refData: for database input files

#output file:
reference tables with final composite scores for each disease in a .csv format

#weights can be adjusted by the user (sum of weights =1)

#default weights used: trials=0.5,repurposed=0.5

#default weights used: indication=0.2,moa=0.4,pathway=0.4

