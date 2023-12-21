#https://github.com/PavlopoulosLab/Flame
#https://bib.fleming.gr:8084/app/norma
#https://github.com/PavlopoulosLab/NORMA/blob/master/views/welcome.R


welcomePage <- div(id = "welcome_div",
                   h1("Welcome to DReAmocracy"),
                   strong("a tool that scores repurposed drugs and not only, in a Democratic way."),
                   br(),
                   br(),
                   helpText(
                     "In the area of drug research, several computational drug repurposing studies have highlighted candidate repurposed drugs,
                     as well as clinical trial studies have tested/are testing drugs in different phases. Drug repurposing or repositioning, which is the
                     identification of novel uses for existing drugs, has attracted considerable attention during the past few decades as it offers a cost-efficient
                     and time-effective alternative avenue to therapeutics, compared to de novo drug discovery. To our knowledge, the aggregation of the proposed lists of drugs
                     by previous studies has not been extensively exploited towards the generation of a dynamic reference matrix with enhanced resolution. To fill this knowledge gap,
                     we performed a weight-modulated majority voting of the modes of action (MoAs), initial indications (Inds) and targeted pathways (Paths) of the drugs in a well-known repository,
                     namely the Drug Repurposing Hub. As a testbed, we applied this method to a group of neurodegenerative diseases (Alzheimer’s Disease, Parkinson’s Disease, Huntington’s Disease
                     and Multiple Sclerosis). In order for DReAmocracy to run, two simple steps are required:"
                   ),
                   tags$ul(
                     tags$li("1. Please choose your disease of interest and the weights of Pathways (Paths), initial Indications (Inds) and Mechanisms of action (MoAs), 
                             as well as the weights of the collection methods (computational drug repurposing lists and clinical trial lists)"),
                     tags$li("2. Please upload your own files of the disease of interest and choose the weights as in step 1)")
                   ),
                   tags$ul(
                     "File instructions as well as downloadable examples can be found in the HELP page."
                   ),
                   br(),
                   br(),
                   p("Available paper: ",
                     a("https://doi.org/10.1101/2023.01.12.523717", 
                     href = "https://doi.org/10.1101/2023.01.12.523717"))
)


