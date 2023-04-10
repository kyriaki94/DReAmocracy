library(dplyr)
library(stringr)
library(data.table)
library(compare)

#####functions#############

#this function can r-bind the specific drug name and NAs and 0s, if there is not in the specific list
findD<-function(ch1,uniqueD,data1,data2){
  
  if (length(ch1)!=0){
    for (i in 1:length(ch1)){
      if (!(ch1[i] %in%  uniqueD$drug.name)){
        data1<-rbind(data1,c(ch1[i],NA,rep(0,8)))
      }else{
        data2<-rbind(data2,c(ch1[i],NA,rep(0,8)))
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


readFiles<-function(pathFiles,filelist){
  
  a<-lapply(filelist, function(k){
    texts<-read.delim(paste0(pathFiles, k), header=TRUE)
    print(colnames(texts))
    
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
    
    m_UNIQUE<<-data.frame('drug name'=unique(resM_1_ALL$drug.name))
    m2_UNIQUE<<-data.frame('drug name'=unique(resM_2_ALL$drug.name))
    
    #setdiff:find only the differences between the 2 lists (pairwise)
    resM_DIFF<<-setdiff(m_UNIQUE$drug.name,m2_UNIQUE$drug.name) 
    list2D_M<<-findD(resM_DIFF,m_UNIQUE,resM_1_ALL,resM_2_ALL)
    joinTables[[resM_1_nameINDEX]]<-list2D_M[[1]]
    joinTables[[resM_2_nameINDEX]]<-list2D_M[[2]]  
    
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
  finalALLdata<-data.frame(NA,NA)
  colnames(finalALLdata)<-c('drug.name','composite score')
  
  
  for (j in 1:length(modality)){
    
    res_modaINDEX<-names(com2ListsOutput)[which(str_detect(names(com2ListsOutput),modality[j])==TRUE)]
    modalitySCORE<-calcDrug(com2ListsOutput[[res_modaINDEX]],selDrug,modality[j],'p_value','score')
    
    
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

##############main program#########################
pathFiles<-"./data"

#whole database
whole_database<-read.delim(paste0(pathFiles,"whole_database.txt"))
#input files should have the name file ending with _input
filelist <- list.files(pathFiles,pattern = "*[_input_]*.txt")
filelist
#read Files
resReadFiles<-readFiles(pathFiles,filelist)
names(resReadFiles)<- c(filelist)

#select trials and/or repurposed
#find modalities
inUserTrialRep<-list("trial"=TRUE,"repurposed"=TRUE)
inUserWeightTrialRep<-list("trials"=0.5,"repurposed"=0.5)

finalComTRIALLIST_ALL<-list()
if (inUserTrialRep[[1]]==TRUE){
  in1<-names(resReadFiles)[which(str_detect(names(resReadFiles),"trials_input_refDis")==TRUE)]
  n<-str_split(in1, pattern = "_")
  modaTemp<-lapply(n,function(x){ data.frame(modality=x[2])})
  modalitiesN<-rbindlist(modaTemp)
  modalitiesU<-unique(modalitiesN$modality)
  #find diseases
  diseaseTemp<-lapply(n,function(x){ data.frame(disease=x[1])})
  diseasesN<-rbindlist(diseaseTemp)
  diseaseU<-unique(diseasesN$disease)
 
  
  resReadFilesInp<-resReadFiles[in1]
  
  for(i in 1:length(diseaseU)){
    diseaseUSER<-diseaseU[i]
    resjoinTables<-joinTables(modalitiesU,resReadFiles,resReadFilesInp,whole_database,diseaseUSER)
    allCom<-data.frame(expand.grid(x = modalitiesU, y = modalitiesU))
    allCom<-allCom[which(allCom[,1]!=allCom[,2]),]
    allCom$x<-as.character(allCom$x)
    allCom$y<-as.character(allCom$y)
  
    rescom2ListsOutput<-com2Lists(resjoinTables,modalitiesU,allCom,diseaseUSER)
    aggreFOutput<-aggreF(rescom2ListsOutput)
    
    inUserW<-list("indication"=0.2,"moa"=0.4,"pathway"=0.4)
    all_finalScoreOutput<-data.frame(drug.name=NA,composite.score=NA)
    for (j in 1:length(whole_database$drug.name)){
     
      selDrugU<-whole_database$drug[j]
      finalScoreOutput<-finalScore(rescom2ListsOutput,selDrugU,modalitiesU,inUserW)
      finalScoreOutput<-na.omit(finalScoreOutput)
      all_finalScoreOutput<-rbind(all_finalScoreOutput,data.frame(finalScoreOutput))
     
     
    }
    finalComTRIALLIST_ALL[[i]]<-all_finalScoreOutput
    }
  
 names(finalComTRIALLIST_ALL)<-c(diseaseU)
}

finalComREPLIST_ALL<-list()
if (inUserTrialRep[[2]]==TRUE){
  
  in1<-names(resReadFiles)[which(str_detect(names(resReadFiles),"repurposed_input_refDis")==TRUE)]
  n<-str_split(in1, pattern = "_")
  modaTemp<-lapply(n,function(x){ data.frame(modality=x[2])})
  modalitiesN<-rbindlist(modaTemp)
  modalitiesU<-unique(modalitiesN$modality)
  #find diseases
  diseaseTemp<-lapply(n,function(x){ data.frame(disease=x[1])})
  diseasesN<-rbindlist(diseaseTemp)
  diseaseU<-unique(diseasesN$disease)
  #diseaseUSER<-"Huntington's Disease"
  

  resReadFilesInp<-resReadFiles[in1]
  
  
  for(i in 1:length(diseaseU)){
    diseaseUSER<-diseaseU[i]
    resjoinTables<-joinTables(modalitiesU,resReadFiles,resReadFilesInp,whole_database,diseaseUSER)
    allCom<-data.frame(expand.grid(x = modalitiesU, y = modalitiesU))
    allCom<-allCom[which(allCom[,1]!=allCom[,2]),]
    allCom$x<-as.character(allCom$x)
    allCom$y<-as.character(allCom$y)
    
    rescom2ListsOutput<-com2Lists(resjoinTables,modalitiesU,allCom,diseaseUSER)
    #collapse version - TEMP not used
    aggreFOutput<-aggreF(rescom2ListsOutput)
  
    inUserW<-list("indication"=0.2,"moa"=0.4,"pathway"=0.4)
    all_finalScoreOutput<-data.frame(drug.name=NA,composite.score=NA)
  for (j in 1:length(whole_database$drug.name)){
    selDrugU<-whole_database$drug[j]
    
    finalScoreOutput<-finalScore(rescom2ListsOutput,selDrugU,modalitiesU,inUserW)
    finalScoreOutput<-na.omit(finalScoreOutput)
    all_finalScoreOutput<-rbind(all_finalScoreOutput,data.frame(finalScoreOutput))
    
  }
  finalComREPLIST_ALL[[i]]<-all_finalScoreOutput
  }
  names(finalComREPLIST_ALL)<-c(diseaseU)
}


#common diseases
comL<-which(names(finalComTRIALLIST_ALL) %in% names(finalComREPLIST_ALL))

allScoresFLIST<-list()

for (i in 1:length(comL)){
  allScores<-data.frame()
  
  allScores<-cbind(finalComTRIALLIST_ALL[[comL[i]]],finalComREPLIST_ALL[[comL[i]]])
  allScores<-na.omit(allScores)
  
  allScores<-allScores[,-3]
  
  colnames(allScores)<-c("drug name","composite score trials","composite score repurposed")
  allScoresF<-data.frame()
  
  for(j in 1:nrow(whole_database)){
    
    finalComScore<-sum(inUserWeightTrialRep[[1]]*as.numeric(allScores[j,2]),
                       inUserWeightTrialRep[[2]]*as.numeric(allScores[j,3]))
  
    allScoresT<-cbind(allScores[j,],final_composite_score=finalComScore)
    allScoresF<-rbind(allScoresF,allScoresT)
  }
  allScoresFLIST[[i]]<-allScoresF
}

names(allScoresFLIST)<-c(names(finalComTRIALLIST_ALL)[comL])


uniTR<-names(finalComTRIALLIST_ALL)[-(comL)]
uniREP<-names(finalComREPLIST_ALL)[-(comL)]

if(length(uniTR)>0){
  allTRIAL<-list()
  for (i in 1:length(uniTR)){
    index<-which(names(finalComTRIALLIST_ALL)==uniTR[i])
    finalComTRIALLIST_ALL[[index]]<-na.omit( finalComTRIALLIST_ALL[[index]])
    allTRIAL[[i]]<-finalComTRIALLIST_ALL[[index]]
    
  }
  names(allTRIAL)<-c(uniTR)
}


if(length(uniREP)>0){
  allREP<-list()
  for (i in 1:length(uniREP)){
    index<-which(names(finalComREPLIST_ALL)==uniREP[i])
    finalComREPLIST_ALL[[index]]<-na.omit(finalComREPLIST_ALL[[index]])
    allREP[[i]]<-finalComREPLIST_ALL[[index]]
    
  }
  names(allREP)<-c(uniREP)
}


#################


# write files as outputs
write_csv <- function(df, file_name) {
  write.csv(df, file = file_name, row.names = FALSE)
}

lapply(names(allScoresFLIST), function(name) {
  write_csv(allScoresFLIST[[name]], paste0(name, ".csv"))
})


