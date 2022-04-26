# JAZ; 2015-11-30 
# listing table names in MFE database 

# fpath - file path of database file location; currently defaults to Jake's computer location 
# dbname - name of database, default is 'MFEdb_20170202.db' ; maybe change to current version so we don't have to continue to update this function

dbTableList<-function(fpath=file.path('C:/Users/jzwart/Documents/Jake/Database'),
                  dbname='MFEdb_20180223.db'){
  #set file path to the location of the database (defaults to Jake's database location)
  library(RSQLite)
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection    
  #list tables in database
  tables=dbListTables(con) 
  return(tables)
}
