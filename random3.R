set.seed(23)

#goal is to take a dataset
#make a number of subsamples of that dataset
#take a subset of the columns
#iterate over the 6 things in the summary
#ask students some summary statistics of subset
#need to generate n subsets (csvs), n "true answers" - (row in master.csv)
#and one set of questions
#e.g in your dataset, what is the minimum number in the column
#maybe mail merge for feedback



############setup############

##set/import data set
df <- mtcars
## set the max number of columns you want to enquire about
maxCols<-8
## set the max number of things  you want to enquire about (e.g mean 1st qu etc)
maxFeatures<-6
##set how many rows in each csv you generate
maxRows<-10
##set the number of csvs to be generated
maxCsvs<-10

#############functions######################

makeColNames<-function(featsIdx,colsIdx) 
{
  retval<-c()
  for (i in 1:length(featsIdx))
  {
    retval<-append(retval, paste(featNamez[i], " of ", colNamez[i]))
  }
  return (retval)
}

makeLine <- function(df,featsIdx,colsIdx,csvId) 
{ 
  sm<-summary(df)
  retval<-c()
  for (i in 1:length(featsIdx))
  {
    #print(paste("feats+cols", featsIdx, " ", colsIdx))
    offset=(colsIdx[i]-1)*6 + featsIdx[i]
    #print(paste(i,getSumVal(sm,offset)))
    retval <- append(retval, getSumVal(sm,offset))
  }
  return (retval)
}

getSumVal <-function(sm,offset)
{
  #print (paste("Offset:",offset))
  #print (as.numeric(gsub('.*:', '', sm[[offset]])))
  return (as.numeric(gsub('.*:', '', sm[[offset]])))
}

#######################################################
set.seed(NULL)   
numCols <- length(colnames(df))
colIndexes <- sample(x=1:numCols,size=maxCols)
featIndexes <- sample(x=append(1:maxFeatures,1:maxFeatures),size=maxCols) 



####################################

featNamez<-c()
sumry = summary(df)
for (k in 1:length(featIndexes))
{
  st<-gsub(':.*$', '', sumry[[featIndexes[k]]])
  st<-gsub(' ','',st)
  featNamez<-append(featNamez,st)
}

colNamez<-c()
for (k in 1:length(colIndexes))
{
  idx <- colIndexes[k]
  colNamez<-append(colNamez,colnames(df)[idx])
}


sample(colnames(df),length(colnames(df)))

####################################
###ask questions###

colCt<-0
loopCt<-0
for (j in featIndexes)
{
  loopCt<-loopCt+1  
  colCt <- max(loopCt %% maxCols,1)
  #print(paste("What is the ", featNamez[j], " of column" , colNamez[j]))
  #print(paste("What is the feature ", featIndexes[j], " of column" , colIndexes[j]))
}




#select random sample  from data frame
rand_df <- df[sample(nrow(df), size=maxRows), ]

fullList<-list(rand_df)


answers <- list()
for (i in 1:maxCsvs)
{
  set.seed(23+i)
  tmp_df <- df[sample(nrow(df), size=maxRows), ]
  fullList<-append(fullList,list(tmp_df))
  write.csv(tmp_df,paste("testz",i,".csv",sep=""))
  l <-summary(tmp_df)
  answers[[i]] <- makeLine(tmp_df,featIndexes,colIndexes, i)
}

answersDf<-as.data.frame(do.call(rbind, answers))
cnames <- makeColNames(featIndexes,colIndexes)
colnames(answersDf)<-cnames

write.csv(answersDf, "answers3.csv")


