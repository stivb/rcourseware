library(readr)
setwd(dirname(normalizePath(sys.frame(1)$ofile)))
df <- read_csv("datasets.csv")

print(colnames(df))
colnames(df)[1] <- "dataset"
colnames(df)[2] <- "shortname"  
colnames(df)[3] <- "url"
colnames(df)[4] <- "filename"
colnames(df)[5] <- "filesize"
colnames(df)[6] <- "details"
colnames(df)[7] <- "subjectarea"  
colnames(df)[8] <- "available"

qTemplate <- "{n}.  <h1>{dataset}</h1> For assignment 1 you have been allocated the dataset '{dataset}' ({shortname}). <p>You MUST use this dataset for your assignment.
£££You can get your dataset from <i>{url}</i> 
£££The file(s) is called '{filename}' and is {filesize} in size.
£££It contains data about {details}. Subject Area: {subjectarea}.
£££<b>You MUST type or copy/paste <span style='background-color:yellow'>{dataset}</span> in the box below to confirm you have read this question and understand which dataset you must use.</b>
£££This is essential for your assignment to be considered valid.
£££If you have any problems with downloading or using the dataset please raise this on Slack in the #Assignments channel
£££The full assignment (and the place where you will submit your work) is at: https://herts.instructure.com/courses/124322/assignments/382268</p>
... Go back to assignment 1 at: https://herts.instructure.com/courses/124322/assignments/382268 where the full requirements are laid out
___
"
quiz_questions <- ""
for (i in 1:10) {
    spacer<-paste0(strrep(" ", 3 + nchar(i)), collapse = "")
    quiz_questions <- paste0(quiz_questions,     
    gsub("\\{n\\}",     i,
    gsub("£££", paste0(spacer,"</p>","<p>"),
    gsub("\\{dataset\\}",     df$dataset[i], 
    gsub("\\{shortname\\}",     df$shortname[i], 
    gsub("\\{url\\}",     df$url[i], 
    gsub("\\{filename\\}",     df$filename[i], 
    gsub("\\{filesize\\}",     df$filesize[i], 
    gsub("\\{details\\}", df$details[i], 
    gsub("\\{subjectarea\\}", df$subjectarea[i], 
    gsub("\\{available\\}", df$available[i], qTemplate)))))))))))
}


writeLines(quiz_questions, "datasets.qtitext")




