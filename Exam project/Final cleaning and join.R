library("stringr")



                        ################################ Data preperation ################################ 

# Reads player data
players = read.csv("~/Documents/Polit/Valgfag/SDS/Gruppe2/Exam project/Data/players.csv", )
# Reads transfer pricing data
transfers = read.csv("~/Documents/Polit/Valgfag/SDS/Gruppe2/Exam project/Data/transfers.csv")


# Preparing player data
  
  # Creates a year variable based on season (as character)
  players$year = substr(as.character(players$season), nchar(as.character(players$season))-4+1, nchar(as.character(players$season)))

#Preparing transfer data
  
  # Transforms some categories of transferfee to £0k
  transfers$test = ifelse(transfers$transferfee=='Loan',
                                                      '£0k', 
            ifelse(transfers$transferfee=='End of loan', 
                                                      '£0k',
            ifelse(transfers$transferfee=='Free transfer',
                                                      '£0k',
            as.character(transfers$transferfee)
                                                        ))) 
  
  
  # Cleans the transfer pricing variable
  transfers$test = gsub('£', "", transfers$test) 
  transfers$test = gsub('Loan fee:', "", transfers$test) 
 
  
  # Isolates multiplier
  transfers$multiplier = substr(as.character(transfers$test), nchar(as.character(transfers$test))-1+1, nchar(as.character(transfers$test)))
  
  # Cleans multiplier
  transfers$factor = ifelse(transfers$multiplier=="m",
                                                  as.numeric(1000000),
                      ifelse(transfers$multiplier=="k",
                                                   as.numeric(1000),
                                                   as.numeric(0)
                                                    ))
                            
  # Extracts only digits from transfer pricing
  transfers$test = as.numeric(str_extract(transfers$test, "\\d+\\.*\\d*"))
  
  # Creates price variable as product of transferpricing and the factor
  transfers$price = transfers$test*transfers$factor
  
  # Cleans market value
  transfers$test = ifelse(transfers$marketvalue=="-", NA, as.character(transfers$marketvalue))
  
  # Removes irrelevant variables
  transfers$transferfee = NULL
  transfers$test = NULL
  transfers$multiplier = NULL
  transfers$factor = NULL
  
  
  
  
  
  
  
  