library(Dict)
library(dplyr)
library(funr)
library(readr)
library(svDialogs)

readFullDataSet<-function(dataSetPath)
{
  df<-read_csv(dataSetPath)
  return (select_if(df, is.numeric))
}


##set/import data set (make sure only numeric columns are included)

df<-readFullDataSet("7COM1079introsurvey.txt")


students<-read_csv("students.txt", col_names=FALSE)

## set the max number of columns you want to enquire about
maxCols<-10
##set how many rows in each csv you generate
maxRows<-12
##set the number of csvs to be generated
maxCsvs<-length(students$X1)
##set the prefix





#this is a list of operations with their friendly names to be done on columns
operations<- Dict$new(
  sd = "Standard Deviation",
  median = "Median",
  max = "Maximum Value",
  IQR = "Inter Quartile Range",
  mean = "Mean"
)

#this function will return the result for a particular op on a particular column
doOpOnCol<-function(df,op,col) 
{
  colData<-pull(df,col)
  switch(op, 
         sd={
           return(sd(colData,na.rm=TRUE  ))
         },
         median={
           return(median(colData,na.rm=TRUE  )) 
         },
         max=
           {
             return(max(colData,na.rm=TRUE  ))
           },
         IQR=
           {
             return (IQR(colData,na.rm=TRUE))
           },
         mean=
           {
             return (mean(colData,na.rm=TRUE))
           }
  )
}



clearExistingCsvs<-function()
{
  pth<-funr::get_script_path()
  pat<-paste0(csvPrefix,"*")
  print(paste("***",pth,"***",pat))
  junk <- dir(path=pth,  pattern=pat) # dir
  junk <- append(junk, "answers.csv")
  file.remove(junk) # file.remove
}

#this function will get generate a complete list of operations on columns
#override it to get a specialized set of names and columns
getOpNameTbl<-function(ops,cnames)
{
  OpNomTbl <- data.frame(op = character(), cnom= character())
  
  approvedCols<-dlg_list(
    cnames,
    preselect = NULL,
    multiple = TRUE,
    title = "Choose Columns To Be Interrogated",
    gui = .GUI
    )$res
  
  
  
  for(cname in approvedCols)
  {
    for (op in ops)
    {
     OpNomTbl [nrow(OpNomTbl ) + 1,] = c(op,cname)
    }
  }
  
  return (OpNomTbl)
}


#this creates the column names in the answer csv
makeColNames<-function(onss) 
{
  retval<-c("csvId")
  for (row in 1:nrow(onss)) 
  {
    op<-onss[row,"op"]
    cnom<-onss[row,"cnom"]
    retval<-append(retval, paste0(op, "_of_", cnom))
  }
  return (retval)
}

#produces a printout of the questions being asked to help create a form
makeQuestionList<-function(onss,operations)
{
  qList<-""
  
  for (row in 1:nrow(onss)) 
  {
    op<-onss[row,"op"]
    cnom<-onss[row,"cnom"]
    q<-paste("Put the", operations[op],"of column *", cnom, "*")
    qList<-paste(qList,row,q,"\r\n")
  }
  
  return(qList)
}

# CREATES A LINE OF RIGHT ANSWERS TO THE OPS ON COLS OF THE SUBSAMPLED DF
makeLine <- function(df,opColTbl, csvId)
{
  retval<-c(csvId)
  for (row in 1:nrow(opColTbl)) 
  {
    op<-opColTbl[row,"op"]
    cnom<-opColTbl[row,"cnom"]
    result<-unlist(doOpOnCol(df,op,cnom))
    retval <- append(retval, round(result,2))
  }
  return (retval)
}

##RANDOMIZED OPERATIONS AND COLUMNS

ops = sample(operations$keys)
colnamez = sample(colnames(df))

#####CREATE LIST OF OPS ON COLUMNS###########


opNameTbl<-getOpNameTbl(ops,colnamez)


opNameSubset<-sample_n(opNameTbl, maxCols)

cat(makeQuestionList(opNameSubset,operations))


answers <- list()
for (i in 1:maxCsvs)
{
  set.seed(23+i)
  tmp_df <- df[sample(nrow(df), size=maxRows), ]
  fn<-students$X1[i]
  write.csv(tmp_df,paste(fn,".csv",sep=""))
  answers[[i]] <- makeLine(tmp_df,opNameSubset, paste0(fn,".csv"))
}

answersDf<-as.data.frame(do.call(rbind, answers))
cnames <- makeColNames(opNameSubset)
colnames(answersDf)<-cnames



write.csv(answersDf, "answers.csv")



#####GENERAL PRINT OUT######################

# for(cname in colnamez)
# {
#   for (op in ops)
#   {
#     english <-friendly[op]
#     print(paste("Find ", english, " on column ", cname))
#     print(doOpOnCol(df,op,cname));
#   }
# }

#############################################

