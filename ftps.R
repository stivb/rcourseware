library(RCurl)



publicFTPRoot<-"sftp://pedro.cs.herts.ac.uk:22/cygdrive/c/inetpub/wwwroot/comqsjb/public/"

publicHTTPSRoot<-"https://pedro.cs.herts.ac.uk/comqsjb/public/"

opts <- curlOptions(
  dirlistonly = FALSE,
  sslversion = 6L,
  verbose = TRUE,
  ftp.use.epsv = FALSE,
  ssl.verifypeer=TRUE,
  ftp.ssl = TRUE,
  ftp.create.missing.dirs=TRUE
)

checkConnection<-function()
{
  x <- getURL(paste0(publicRoot,"diagnostics.php"), userpwd = pw,
              .opts = opts)
  print(x)
}

uploadLocalFile<-function(localFileName, remoteFolder) 
{
ftpUpload(localFileName, paste0(publicRoot,remoteFolder,"/",localFileName),
            userpwd=pw, .opts = opts)
return (paste0(publicHTTPSRoot,remoteFolder,"/",localFileName))
}

makeDir<-function(remoteDirName)
{
  
  curlPerform(publicRoot,quote=paste("MKD",remoteDirName),userpwd=pw, .opts = opts)
}

getRemoteFolderCSVListing<-function(remoteDirName)
{
  remoteFolder<-paste0(publicFTPRoot,remoteDirName,"/")
  filenames = getURL(remoteFolder, ftp.use.epsv = FALSE, dirlistonly = TRUE, userpwd = pw)
  fnz<-unlist(strsplit(filenames,"\n"))
  remoteCsvs<-fnz[grepl("^.*\\.csv$", fnz)]
  return(remoteCsvs)
}

getLocalCsvListing<-function()
{
  return (list.files(pattern="*.csv$"))
}

uploadLocalFiles<-function(remoteDirName)
{
  localFiles<-getLocalCsvListing()
  for (localFile in localFiles) {
    if (localFile="answers.csv")  next
    uploadLocalFile(localFile, remoteDirName)
  }
  remoteFiles<-getRemoteFolderCSVListing(remoteDirName)
  diagnostics<-setdiff(localFiles,remoteFiles)
  print("Files not on both servers are: ")
  print(diagnostics)
}
