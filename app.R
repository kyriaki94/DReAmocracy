library(shinysky)
library(shinythemes)
library(shiny)
library(dplyr)
library(stringr)
library(data.table)
library(compare)
library(shinydashboard)
library(shinyjs)

#####functions#############

#this function can r-bind the specific drug name and NAs and 0s, if there is not in the specific list
findD<-function(ch1,uniqueD,data1,data2){
  
  if (length(ch1)!=0){
    for (i in 1:length(ch1)){
      if (!(ch1[i] %in%  uniqueD$drug.name)){
        data1<-bind_rows(data1,data.frame(ch1[i],NA,rep(0,8)))
        #data1<-rbind(data1,c(ch1[i],NA,rep(0,8)))
        
      }else{
        data2<-bind_rows(data2,data.frame(ch1[i],NA,rep(0,8)))
        #data2<-rbind(data2,c(ch1[i],NA,rep(0,8)))
      }
    }
  }
  return (list(data1,data2))
}

#this function calculates the dremocracy methodology 

calcDrug<-function(allTog,nameDrug,x1,value,score){ 
  
  modality<-data.frame(unique(allTog[which(allTog$drug.name==nameDrug),c(x1,value,score)])) #step2: unique per modality(moa, indi, path)
  
  columns <-c(value,score)
  modality[, columns] <- lapply(columns, function(x) as.numeric(modality[[x]]))
  
  #which modalities p<0.05 (keep), zero to p>0.05
  modaPvalue<-modality[which(modality[value]<0.05),]
  zero_count_moas<-modality[which(modality[value]>0.05),]
  
  #median of scores per modality if more than one present
  if (nrow(modaPvalue)!=0){
    moda_median<-median(unlist(modaPvalue[score]))
    
    return (moda_median)
  }else
    
    return (0)
  
  
}

#reading txt files
#_input_refDis: for ref diseases inputs
#_input_refData: for database input files
#check colnames to trim drug.name
#listnames

readFiles<-function(pathFiles,filelist){
  
  a<-lapply(filelist, function(k){
    texts<-read.delim(paste0(pathFiles, k), header=TRUE)
    
    
    if("drug.name" %in% colnames(texts)){
      texts$drug.name<-str_trim(texts$drug.name, side = c("both"))
      
    }
    texts
  }) 
  return(a)
}

joinTables<- function(modality, resReadFiles,resReadFilesInp,whole_database, diseaseName,inUserTrialRep){
  list_allTOG<-list()
  dataNames<-data.frame()
  
  for(i in 1:length(modality)){
    in1<-names(resReadFilesInp)[which(str_detect(names(resReadFilesInp),diseaseName)==TRUE)]
    in2<-in1[which(str_detect(in1, modality[i])==TRUE)]
    
    finalTM<-referenceDatabase(modality[i],resReadFiles,whole_database)
    
    finalTM_SCORE<-resReadFilesInp[[in2]][which(resReadFilesInp[[in2]][,c(modality[i])] %in% finalTM[,c(modality[i])]),]
    list_allTOG[[i]]<-finalTM %>% inner_join(finalTM_SCORE,by=c(modality[i]))
    dataNames<-rbind(dataNames,in2)
  }
  
  names(list_allTOG)<-dataNames[,1]
  return(list_allTOG)
  
}

referenceDatabase<-function(modality, resReadFiles,whole_database){
  
  in1<-names(resReadFiles)[which(str_detect(names(resReadFiles),"_input_refData")==TRUE)]
  in2<-in1[which(str_detect(in1, modality)==TRUE)]
  finalTM<-resReadFiles[[in2]][which(resReadFiles[[in2]]$drug.name %in% whole_database$drug.name),]
  
  
}


com2Lists<-function(joinTables,modality,allCom,diseaseName){
  
  for(i in 1:nrow(allCom)){
    
    res_diseaseINDEX<-names(joinTables)[which(str_detect(names(joinTables),diseaseName)==TRUE)]
    resM_1_nameINDEX<-res_diseaseINDEX[which(str_detect(res_diseaseINDEX, allCom[i,1])==TRUE)]
    resM_2_nameINDEX<-res_diseaseINDEX[which(str_detect(res_diseaseINDEX, allCom[i,2])==TRUE)]
    
    resM_1_ALL<<-joinTables[[resM_1_nameINDEX]]
    resM_2_ALL<<-joinTables[[resM_2_nameINDEX]]
    
    m_UNIQUE<<-data.frame('drug name'=unique(resM_1_ALL$drug.name))#kept unique drug name (single column)
    m2_UNIQUE<<-data.frame('drug name'=unique(resM_2_ALL$drug.name))#kept unique drug name (single column)
    
    #setdiff:find only the differences between the 2 lists (pairwise)
    resM_DIFF<<-setdiff(m_UNIQUE$drug.name,m2_UNIQUE$drug.name) #keeps the ones that appear in moas_UNIQUE but do not appear in indi_UNIQUE
    list2D_M<<-findD(resM_DIFF,m_UNIQUE,resM_1_ALL,resM_2_ALL)
    joinTables[[resM_1_nameINDEX]]<-list2D_M[[1]]
    joinTables[[resM_2_nameINDEX]]<-list2D_M[[2]]  #inserts zeros and NAs (indis) to the drugs that did not have MoAs
    
    temp1<<-list2D_M[[1]]
    temp2<<-list2D_M[[2]]  
   
  }
  
  return (joinTables)
}

aggreF<-function(com2ListsOutput){
  
  semiMat<-lapply(com2ListsOutput,function(x){
    
    aggregate(. ~ drug.name,data=x,paste,collapse=",",na.action = na.pass)
    
  })
  
}

joinAggr<-function(com2ListsOutput,modality){
  
  df<-com2ListsOutput[[1]]
  
  for (i in 2:length(modality)){
    
    df <- df %>% inner_join(com2ListsOutput[[i]],by="drug.name")
    
  }
  return (df)
}

finalScore<-function(com2ListsOutput,selDrug,modality,inUserW){
  
  listALL_modalities<-list()
  finalALLdata<-data.frame(NA,NA)#create an empty dataframe
  colnames(finalALLdata)<-c('drug.name','composite score')
  
  
  for (j in 1:length(modality)){
    
    res_modaINDEX<-names(com2ListsOutput)[which(str_detect(names(com2ListsOutput),modality[j])==TRUE)]
    modalitySCORE<-calcDrug(com2ListsOutput[[res_modaINDEX]],selDrug,modality[j],'p_value','score')
    
    #print(modalitySCORE)
    listALL_modalities[[j]]<-modalitySCORE
    
  }
  names(listALL_modalities)<-c(modality)
  composite_score<-matrix(nrow=length(modality))
  
  
  
  for (j in 1:length(modality)){
    
    indTable<-names(inUserW)[which(names(inUserW)==modality[j])]
    
    res_modaINDEX<-names(listALL_modalities)[which(names(listALL_modalities)==modality[j])]
    composite_score[j,1]<-as.numeric(inUserW[indTable][modality[j]])*as.numeric(listALL_modalities[[res_modaINDEX]])
    
  }
  
  all_composite_score<-sum(composite_score[,1])
  
  finalALLdata<-rbind(finalALLdata,c(selDrug,all_composite_score))
  
  
  return (finalALLdata)
  
}

##############main program########################

pathFiles<-paste0(getwd(),"/data/") #set your directory


#whole database
whole_database<-read.delim(paste0(pathFiles,"whole_database.txt"))
#gia na theoreite ena file input prepei na exei _input
filelist <- list.files(pathFiles,pattern = "*[_input_]*.txt")
filelist
#read Files
resReadFiles<-readFiles(pathFiles,filelist)
names(resReadFiles)<- c(filelist)



#change
source("./data/welcome_page_file.R", local=TRUE)
source("./data/help_page_file.R", local=TRUE)

my_autocomp_list<- read.delim("./all_files/AD_PD_MS_HD_trials_repurposed_joined_FINAL.txt")


ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  # shinythemes::themeSelector(),
  fluidRow(               tags$style(".col-sm-12 {width: 100%;
                          background-color:black;
                          color:white;
                          }"),
   headerPanel(title = "DReAmocracy",
              # tags$li(".main-header {background-color: black;}")

  #               #  titleWidth = 300
   )
  
  
  ),
  sidebarPanel(
    #theme = shinythemes::shinytheme("cosmo"),
    width = 3,
    # theme = "paper",  # <--- To use a theme, uncomment this
    # "shinythemes",
    fluidRow(
    tabPanel("DReAmocracy",
        
               selectInput("disease", "Diseases Available:", selected = NULL,
                           choices = list("Choose a disease" = "Choose a disease","Alzheimer's Disease" = "Alzheimer's Disease", "Parkinson's Disease" = "Parkinson's Disease",
                                          "Huntington's Disease" = "Huntington's Disease", "Multiple Sclerosis" = "Multiple Sclerosis")),
               
               select2Input("drug", "Drug of interest:", choices = c(my_autocomp_list$drug.me), type=c("input","select")),
               numericInput("num1", "Weight: Mechanism of Action", value = NA, min = 0, max = 1, step = 0.1),
               numericInput("num2", "Weight: Initial Indications", value = NA, min = 0, max = 1, step = 0.1),
               numericInput("num3", "Weight: Pathways", value = NA, min = 0, max = 1, step = 0.1),
               p("Note: Sum of weights must equal to 1!!"),
               
               br(),
               checkboxGroupInput("Method_collection1", "Method collection:", 
                                  choices = list("Comp. drug repurposing lists" = "Comp. drug repurposing lists"), selected = NULL),
               checkboxGroupInput("Method_collection2", "Method collection:", 
                                  choices = list("Clinical trial lists" = "Clinical trial lists"), selected = NULL),
               conditionalPanel(
                 condition = "input.Method_collection1 == 'Comp. drug repurposing lists'",
                 numericInput("weight_repurp", "Weight: Comp. drug repurposing lists", value = NA, min = 0, max = 1, step = 0.1)),
               
               
               conditionalPanel(
                 condition = "input.Method_collection2 == 'Clinical trial lists'",
                 numericInput("weight_trial", "Weight: Clinical trials", value = NA, min = 0, max = 1, step = 0.1)
               ),
               
               
               actionButton("action", "Search")
             )
             
    )
  ),
  
  mainPanel(
    shinyjs::useShinyjs(),
    tabsetPanel(
      tabPanel( "Welcome", welcomePage ),
      tabPanel("Drug Output",
               
               tableOutput("table_1"),
               textOutput('safeError'),
               textOutput('safeError2'),
               
      ),
      tabPanel( "Help", icon = icon("question"), helpPage )
    )
  )
  # )
)
server = function(input, output, session) {
  
  observeEvent(list(input$num1,input$num2,input$num3),{
    
    index_na<-which(is.na(c(input$num1,input$num2,input$num3)))
    temp_d<-data.frame(input$num1,input$num2,input$num3)
    temp_d[,index_na]<-0
    
    
    output$safeError<-renderText({
      sum_weights<-sum(temp_d[1,])
      if(sum_weights>1){
        shinyjs::disable("action")
        stop(safeError("error: check the weights. They need to be equal to 1"))
      }else{
        shinyjs::enable("action")
        
      }
      
    }) 
    # sum_weights<-sum(temp_d[1,])
    # if (sum_weights>1){
    #   safeError("error")
    #   print(sum_weights)
    #   print("ERROR")
    # }
    
  })
  
  observeEvent(list(input$weight_repurp,input$weight_trial),{
    index_na2<-which(is.na(c(input$weight_repurp,input$weight_trial)))
    temp_d2<-data.frame(input$weight_repurp,input$weight_trial)
    temp_d2[,index_na2]<-0
    
    output$safeError2<- renderText({
      sum_weights2<-sum(temp_d2[1,])
      if (sum_weights2>1){
        shinyjs::disable("action")
        stop(safeError("error: check the weights. They need to pe equal to 1"))
    
      }else{
        shinyjs::enable("action")
      }
    })
    
    # 
    # sum_weights2<-sum(temp_d2[1,])
    # if (sum_weights2>1){
    #   safeError("error")
    #   print(sum_weights2)
    #   print("ERROR2")
    # }
    
    
    
  })
  
  tr<-eventReactive(input$action, {
    
    withProgress(message = 'Calculating scores', value = 0, {
    finalComTRIAL<-data.frame(NA,NA)
    finalComRep<-data.frame(NA,NA)
    
    if (input$disease!="Choose a disease"){
      
      if (!is.na(input$weight_trial)){
        
        in1<-names(resReadFiles)[which(str_detect(names(resReadFiles),"trials_input_refDis")==TRUE)]
        n<-str_split(in1, pattern = "_")
        modaTemp<-lapply(n,function(x){ data.frame(modality=x[2])})
        
        modalitiesN<-rbindlist(modaTemp)
        modalitiesU<-unique(modalitiesN$modality)
        
        #find diseases
        diseaseTemp<-lapply(n,function(x){ data.frame(disease=x[1])})
        diseasesN<-rbindlist(diseaseTemp)
        diseaseU<-unique(diseasesN$disease)
        diseaseUSER<-input$disease
        
        resReadFilesInp<-resReadFiles[in1]
        incProgress(0.25, "joint tables_trials")
        #Join tables
        resjoinTables<-joinTables(modalitiesU,resReadFiles,resReadFilesInp,whole_database,diseaseUSER)
        
        #diff combos
        allCom<-data.frame(expand.grid(x = modalitiesU, y = modalitiesU))
        allCom<-allCom[which(allCom[,1]!=allCom[,2]),]
        allCom$x<-as.character(allCom$x)
        allCom$y<-as.character(allCom$y)
        
        rescom2ListsOutput<-com2Lists(resjoinTables,modalitiesU,allCom,diseaseUSER)
        incProgress(0.5, "computation of lists")
        
        #collapse version - TEMP not used
        aggreFOutput<-aggreF(rescom2ListsOutput)
        #joinAggrOutput<-joinAggr(rescom2ListsOutput,modalitiesU)
        
        incProgress(0.75, "aggregation of lists")
        selDrugU<-input$drug
        inUserW<-list("indication"=input$num2,"moa"=input$num1,"pathway"=input$num3)
        
        finalScoreOutput<-finalScore(rescom2ListsOutput,selDrugU,modalitiesU,inUserW)
        finalScoreOutput<-na.omit(finalScoreOutput)
        finalComTRIAL<-finalScoreOutput
        
        
      }
      if (!is.na(input$weight_repurp)){
        
        in1<-names(resReadFiles)[which(str_detect(names(resReadFiles),"repurposed_input_refDis")==TRUE)]
        n<-str_split(in1, pattern = "_")
        modaTemp<-lapply(n,function(x){ data.frame(modality=x[2])})
        modalitiesN<-rbindlist(modaTemp)
        modalitiesU<-unique(modalitiesN$modality)
        #find diseases
        diseaseTemp<-lapply(n,function(x){ data.frame(disease=x[1])})
        diseasesN<-rbindlist(diseaseTemp)
        diseaseU<-unique(diseasesN$disease)
        diseaseUSER<-input$disease
        
        resReadFilesInp<-resReadFiles[in1]
        #Join tables
        resjoinTables<-joinTables(modalitiesU,resReadFiles,resReadFilesInp,whole_database,diseaseUSER)
        
        #diff combos
        allCom<-data.frame(expand.grid(x = modalitiesU, y = modalitiesU))
        allCom<-allCom[which(allCom[,1]!=allCom[,2]),]
        allCom$x<-as.character(allCom$x)
        allCom$y<-as.character(allCom$y)
        
        
        rescom2ListsOutput<-com2Lists(resjoinTables,modalitiesU,allCom,diseaseUSER)
        #collapse version - TEMP not used
        aggreFOutput<-aggreF(rescom2ListsOutput)
        #joinAggrOutput<-joinAggr(rescom2ListsOutput,modalitiesU)
        
        selDrugU<-input$drug
        inUserW<-list("indication"=input$num2,"moa"=input$num1,"pathway"=input$num3)
        finalScoreOutput<-finalScore(rescom2ListsOutput,selDrugU,modalitiesU,inUserW)
        finalScoreOutput<-na.omit(finalScoreOutput)
        finalComRep<-finalScoreOutput
        
      }
      incProgress(1, "prepare output tables")
      
      if ((nrow(finalComTRIAL)>0) && (nrow(finalComRep)>0)){
        
        allScores<-cbind(finalComTRIAL,finalComRep)
        
        allScores<-allScores[,-3]
        colnames(allScores)<-c("drug name","composite score trials","composite score repurposed")
        
        finalComScore<-sum(as.numeric(input$weight_trial)*as.numeric(allScores[1,2]),as.numeric(input$weight_repurp)*as.numeric(allScores[1,3]))
        allScores<-cbind(allScores,final_composite_score=finalComScore)
        
        
      }else if (nrow(finalComTRIAL)>0 && nrow(finalComRep)==0){
        
        allScores<-finalComTRIAL
        
      }else if (nrow(finalComRep)>0 && nrow(finalComTRIAL)==0){
        allScores<-finalComRep
        
        
      }else{
        
        return (NULL)
      }
    }else{
      return (NULL)
    }
    }) 
  })
  
 
  
  output$table_1 <- renderTable({ 
    
    allScores<-tr()
    
    if (is.null(allScores)){
      
      return (NULL)
      
    }else{
      return (allScores)
    }
    
    
  })
  
  
  
  
}


shinyApp(ui, server)
