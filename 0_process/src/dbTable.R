# getting table from database 
# JAZ, 2014-12-10; updated 2017-03-08 
# 
# Arguments: 
# table - the data table to be returned 
# dbname - name of database, default is 'MFEdb_20170202.db' ; maybe change to current version so we don't have to continue to update this function
# lakeID - vector of lakeID's that you want; default returns all lakeID's
# depthClass - vector of depthClass that you want; default returns all depthClass (i.e. PML, Hypo, etc..)
# minDate - character of minimum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)
# maxDate - character of maximum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)
# dateFormat - character of date format if not in standard unambiguous format (i.e. '%y/%m/%d') 

dbTable<-function(table,fpath=file.path('C:/Users/jzwart/Documents/Jake/Database'),
                  dbname='MFEdb_20180223.db',lakeID=c(),depthClass=c(),minDate=c(),maxDate=c(),dateFormat=c()){
  #set file path to the location of the database (defaults to Jake's database location)
  table=as.character(table)
  library(RSQLite)
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection
  #query an entire table
  table<-dbGetQuery(con,paste('SELECT * FROM',table)) #note that capitalization doesn't matter so LAKES=lakes
  if(!is.null(lakeID)){
    table<-table[table$lakeID%in%lakeID,]
  }
  if(!is.null(depthClass)){
    table<-table[table$depthClass%in%depthClass,]
  }
  if(!is.null(table$dateTimeSample)){
    table$dateTimeSample<-as.POSIXct(table$dateTimeSample,tz='GMT') # changing from character to POSIXct 
  if(!is.null(minDate)){
    if(!"POSIXct"%in%class(minDate)){
      if(is.null(dateFormat)){
        minDate <- as.POSIXct(minDate)
      }else{
        minDate <- as.POSIXct(minDate, format=dateFormat)
      }
    }
    table<-table[table$dateTimeSample>minDate,]
  }
  if(!is.null(maxDate)){
    if(!"POSIXct"%in%class(maxDate)){
      if(is.null(dateFormat)){
        maxDate <- as.POSIXct(maxDate)
      }else{
        maxDate <- as.POSIXct(maxDate, format=dateFormat)
      }
    }
    table<-table[table$dateTimeSample<maxDate,]
  }
  }
  return(table)
}


