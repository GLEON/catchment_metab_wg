# #Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
#
# #Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)


floorMins <- function(dataIn, timeStep)
{
  #Pull out datetime column and name it x
  x <- dataIn$datetime
  nRows <- length(x)
  #Truncate each datetime to hour; convert to class numeric
  floorHour <- as.POSIXct(trunc(x[1:nRows],"hour"))
  floorNumeric <- as.numeric(floorHour)
  #Create sequence from floorNumeric to next hour by timeStep (in seconds)
  seqSec <- seq(0,3600,60*timeStep)
  #Create matrix where each row is floorNumeric + the elements of seqSec
  matSec <- matrix(rep(seqSec,nRows),nrow=nRows,byrow=T)
  matSec <- floorNumeric + matSec
  #Calculate abs(time difference) between each element of x and the timeStep intervals
  difs <- abs(as.numeric(x) - matSec)
  #Find the minimum absolute difference in each row and select the corresponding time from matSec
  whichMin <- apply(difs,1,which.min)
  if(class(whichMin) == "list"){whichMin = unlist(whichMin)}
  rowNames <- as.numeric(rownames(data.frame(whichMin)))
  matIndex <- (whichMin-1)*nRows + rowNames
  matSecFlat <- matrix(matSec,ncol=1)
  outTime <- as.POSIXct(matSecFlat[matIndex],origin="1970-01-01")
  #Return outTime
  return(outTime)
}

#Function to find duplicate datetime stamps
findNotDupRows <- function(dataIn)
{
  #This function returns the indexes of rows where the datetime is NOT a duplicate
  #of the datetime in a previous row
  #dataInName is character, e.g. "dataPAR"
  # dataIn <- eval(parse(text=dataInName))
  #Find duplicated time stamps
  dups <- duplicated(dataIn$datetime)
  #If no duplicated time stamps, notDupRows=all rows in dataIn
  if (all(dups==FALSE))
  {
    notDupRows <- c(1:dim(dataIn)[1])
  } else
    #If at least one time stamp is duplicated, warn, and notDupRows is indexes
    #of rows where datetime is not duplicate of the datetime in a previous row
  {
    notDupRows <- which(dups==FALSE)
    nDups <- dim(dataIn)[1]-length(notDupRows)
    print(paste("Warning:",nDups,"rows with duplicate time stamps in will be removed"))
  }
  #Return dupRows
  return(notDupRows)
}
