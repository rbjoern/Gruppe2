library("stringr")



                        ################################ Data cleaning ################################ 

# Reads player data
players = read.csv("~/Documents/Polit/Valgfag/SDS/Gruppe2/Exam project/Data/players.csv" )
# Reads transfer pricing data
transfers = read.csv("~/Documents/Polit/Valgfag/SDS/Gruppe2/Exam project/Data/transfers.csv")


# Preparing player data
  
  # Creates a year variable based on season (as character)
  players$year = substr(as.character(players$season), nchar(as.character(players$season))-4+1, nchar(as.character(players$season)))

  
  
  
#Preparing transfer data
  
  # Transforms some categories of transferfee to £0k
  transfers$fee = ifelse(transfers$transferfee=='Loan',
                                                      '£0k', 
            ifelse(transfers$transferfee=='End of loan', 
                                                      '£0k',
            ifelse(transfers$transferfee=='Free transfer',
                                                      '£0k',
            as.character(transfers$transferfee)
                                                        ))) 
 
  
  # Isolates multiplier for transferfee
  transfers$multiplierfee = substr(as.character(transfers$fee), nchar(as.character(transfers$fee))-1+1, nchar(as.character(transfers$fee)))
  
  # Cleans multiplier for transferfee
  transfers$factorfee = ifelse(transfers$multiplierfee=="m",
                                                  as.numeric(1000000),
                      ifelse(transfers$multiplierfee=="k",
                                                   as.numeric(1000),
                                                   as.numeric(0)
                                                    ))
                            
  # Extracts only digits from transfer fee
  transfers$fee = as.numeric(str_extract(transfers$fee, "\\d+\\.*\\d*"))
  
  # Creates price variable as product of transferpricing and the factor
  transfers$price = transfers$fee*transfers$factorfee
  
  # Cleans market value
  transfers$MV = ifelse(transfers$marketvalue=="-", NA, as.character(transfers$marketvalue))
  
  # Isolates multiplier for market value
  transfers$multiplierMV = substr(as.character(transfers$MV), nchar(as.character(transfers$MV))-1+1, nchar(as.character(transfers$MV)))
  
  # Cleans multiplier
  transfers$factorMV = ifelse(transfers$multiplierMV=="m",
                            as.numeric(1000000),
                            ifelse(transfers$multiplierMV=="k",
                                   as.numeric(1000),
                                   as.numeric(0)
                            ))
  
  # Extracts only digits from transfer pricing
  transfers$MV = as.numeric(str_extract(transfers$MV, "\\d+\\.*\\d*"))
  
  # Creates price variable as product of transferpricing and the factor
  transfers$marketvalue = transfers$MV*transfers$factorMV
  
  # Removes irrelevant variables
  transfers$transferfee = NULL
  transfers$test = NULL
  transfers$multiplier = NULL
  transfers$factor = NULL
  transfers$fee = NULL
  transfers$multiplierfee = NULL
  transfers$factorfee = NULL
  transfers$MV =NULL
  transfers$multiplierMV = NULL
  transfers$factorMV = NULL
  
  
  
  
  
  
  