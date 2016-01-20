# RVCtools
R Vegetation &amp; Climate Tools

### Installation
execute 'R CMD build RVCTools' from the parent directory

### Uplading modifications
Before uploading modifications, make sure there are no error messages by executing
R CMD check RVCTools

git commit -a -m  "short text about the modifications"
git push origin master

### Automatic version and date increment
For an automatic version increment on each commit add the following content to '.git/hooks/post-commit':

```
#!/usr/bin/env Rscript

doIncrement <- TRUE

# get the environment variable and modify if necessary
tmpEnv <- as.logical(Sys.getenv("doIncrement"))
if (!is.na(tmpEnv)) {
    doIncrement <- tmpEnv
}

if (doIncrement) {
  DCF                <- read.dcf("DESCRIPTION")
  Version            <- DCF[1, "Version"]
  splitVersion       <- strsplit(Version, ".", fixed = TRUE)[[1]]
  nVer               <- length(splitVersion)
  EndVersion         <- as.integer(splitVersion[nVer])
  newEndVersion      <- as.character(EndVersion + 1)
  splitVersion[nVer] <- newEndVersion
  newVersion         <- paste(splitVersion, collapse = ".")
  DCF[1, "Date"]     <- format(Sys.time(), "%Y-%m-%d")
  DCF[1, "Version"]  <- newVersion
  write.dcf(DCF, "DESCRIPTION")
}
```
To disable it, set the environmental bash variable 'doIncrement=FALSE', before executing a git commit.
