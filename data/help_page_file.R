  #https://github.com/PavlopoulosLab/Flame
#https://bib.fleming.gr:8084/app/norma
#https://github.com/PavlopoulosLab/NORMA/blob/master/views/welcome.R


helpPage <- tabsetPanel(
  tabPanel( "How to use",
            br(),
            strong("DReAmocracy:"),
            br(), br(),
            strong("Background:"),
            helpText( "In the area of drug research, several computational drug repurposing studies have highlighted candidate repurposed drugs, as well as clinical trial studies have tested/are testing drugs in different phases. Drug repurposing or repositioning, which is the identification of novel uses for existing drugs, has attracted considerable attention during the past few decades as it offers a cost-efficient and time-effective alternative avenue to therapeutics, compared to de novo drug discovery. To our knowledge, the aggregation of the proposed lists of drugs by previous studies has not been extensively exploited towards the generation of a dynamic reference matrix with enhanced resolution. To fill this knowledge gap, we performed a weight-modulated majority voting of the modes of action (MoAs), initial indications (Inds) and targeted pathways (Paths) of the drugs in a well-known repository, namely the Drug Repurposing Hub. As a testbed, we applied this method to a group of neurodegenerative diseases (Alzheimer’s Disease, Parkinson’s Disease, Huntington’s Disease and Multiple Sclerosis)." ),
            strong("Description of the tool:"),
            helpText( "The DReAmocracy tool will give the opportunity to the user to query a drug of interest that is included in the reference database used, to choose the weights they want to give for Paths, MoAs and Inds, and also the weights of the method of collection (drug repurposing lists or clinical trial lists). This query will give as an output a data table that will include a Composite score for both the repurposing lists and clinical trial lists, as well as a final Composite score of the two for the drug of interest."),
            strong("Weights for Paths, MoAs and Inds:"),
            helpText( "It is obligatory that the sum of weights should always equal to 1."),
            strong("Weights for collection method:"),
            helpText( "It is obligatory that the sum of weights should always equal to 1."),
            br(), br(),
  ) #Tabpanel How to use
  
) #tabsetPanel
