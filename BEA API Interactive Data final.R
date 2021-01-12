# User sets folder where files are written

#setwd("/Users/Tshepo1/Desktop/R")
setwd("\\\\hq-fs-1/arthur.chambers$/Desktop/BEA API")

#user sets file name

file_name1 <- "Interactive_data.csv"
file_name2 <- "All_Data.csv"
file_name3 <- "Missing_Data.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   c("EU","Canada","Mexico","Japan","China")
countries2 <- as.vector(unlist(strsplit(countries,",")),mode="list")
affiliation <- "AllAffiliations"
years <- 2010:2015
dataset <- "IntlServTrade"
trade_direction <- "Exports,Imports"

indicators <- c("AccountAuditBookkeep",
                "Advertising",
                "AllServiceTypes",
                "ArchAndEng",
                "ArchAndEngAbroadUs",
                "ArchAndEngExpend",
                "ArchAndEngUsAbroad",
                "BusMgmtConsPubRel",
                "ChargesForTheUseOfIpNie",
                "CipAudVisRelated",
                "CipBooksSoundRecord",
                "CipBroadcastLiveRecord",
                "CipCompSoft",
                "CipCompSoftIct",
                "CipFranchiseFees",
                "CipIndProcess",
                "CipMoviesTv",
                "CipOth",
                "CipTrademarks",
                "Comp",
                "Const",
                "ConstAbroadUs",
                "ConstExpend",
                "ConstFgnExpend",
                "Financial",
                "FinCredCardOthCredRelated",
                "FinFinAdv",
                "FinFinMan",
                "FinFinManFinAdvCust",
                "FinOth",
                "FinSecBrok",
                "FinSecBrokUwRelated",
                "FinSecLendEftOth",
                "FinUwRelated",
                "GovtGoodsAndServicesNie",
                "IctServ",
                "IndEng",
                "Info",
                "InsDirectPremiumsPaid",
                "InsDirectPremiumsReceived",
                "InsLossesPaid",
                "InsLossesPaidDirect",
                "InsLossesPaidRe",
                "InsLossesRecovered",
                "InsLossesRecoveredDirect",
                "InsLossesRecoveredRe",
                "InsPremiumsPaid",
                "InsPremiumsReceived",
                "InsRePremiumsPaid",
                "InsRePremiumsReceived",
                "Insurance",
                "InsuranceAuxIns",
                "InsuranceDirect",
                "InsuranceDirectAuxIns",
                "InsurancePremSupp",
                "InsuranceReIns",
                "InsuranceRiskPool",
                "Legal",
                "MaintenanceAndRepairNie",
                "Mining",
                "MiningAbroadUs",
                "MiningExpend",
                "OperatingLeasing",
                "OthBusinessNie",
                "OthBusinessNieCtry",
                "OtherBusiness",
                "PotIctEnServ",
                "PotIctEnServOthBusServ",
                "PotIctEnServOthTechTrdOthBus",
                "PotIctEnServTechTrdOthBus",
                "ProfMgmtConsult",
                "ResearchAndDev",
                "SportsPerformArts",
                "TechTradeRelatedOth",
                "Telecom",
                "TelecomCompAndInfo",
                "TradeRelated",
                "Training",
                "Transport",
                "TransportAir",
                "TransportAirFreight",
                "TransportAirPass",
                "TransportAirPort",
                "TransportOth",
                "TransportPostal",
                "TransportRoadAndOth",
                "TransportSea",
                "TransportSeaFreight",
                "TransportSeaPort",
                "Travel",
                "TravelBusiness",
                "TravelBusinessOth",
                "TravelBusinessPersonalOth",
                "TravelEducation",
                "TravelHealth",
                "TravelPersonal",
                "TravelPersonalOth",
                "TravelShortTermWork")

#Loads neccessary libraries

library(jsonlite)

library(dplyr)

library(reshape2)



#Concatenates years

if (years == "X") {
  year_list <- "X"
} else {
  year_list <- paste(years, collapse = ",")
}

if (countries == "X") {
  country_list <- "X"
} else {
  country_list <- paste(countries, collapse = ",")
}

#URL for calling BEA API
#user must input ID for this to run successfully

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=UserID=YOUR_BEA_API_USER_ID",
  
  "&method=GetData&DataSetName=", dataset,
  
  "&TypeOfService=")


url2 <- paste0(
  
  "&TradeDirection=", trade_direction,
  
  "&Affiliation=", affiliation,
  
  "&AreaOrCountry=", country_list,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
  
)

#creates a table to store results

final.result1 <- data.frame()

#Loop that calls API for each sector and creates a table of data


for (i in 1:length(indicators)) {
  this.indicator <- indicators[[i]]
  this.call <- paste0(url1, this.indicator, url2)	
  this.raw.result <- readLines(this.call,encoding="UTF-8", warn=FALSE)
  this.raw.result1 <- fromJSON(this.raw.result)
  
  this.result2 <- this.raw.result1$BEAAPI$Results$Data
  
  final.result1 <- rbind.data.frame(final.result1, this.result2)
}

#Formats table

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")

Combined_table1 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue)
Combined_table2  <- split(Combined_table1, Combined_table1$AreaOrCountry)  
Combined_table3 <- lapply(seq_along(Combined_table2), function(x) as.data.frame(Combined_table2[[x]])[, 1:6]) 
names(Combined_table3) <- countries
list2env(Combined_table3, envir = .GlobalEnv)



function.format.aggregate.calculate <- function(df,TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue) {
  df <- dcast(df, TypeOfService + TradeDirection + Affiliation + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0)
  df$Unique_ID <- do.call(paste, c(df[c("TypeOfService", "TradeDirection", "Affiliation")], sep = "."))
  df <- as.data.frame.matrix(df, row.names=df$Unique_ID)
  df[5:(4 + length(years))] <- lapply(df[5:(4 + length(years))], as.numeric)
  df <- as.data.frame.matrix(df, col.names=TRUE)
  df <- df[,-(5 + length(years))]
  df[is.na(df)] <- 0
  Distribution_Exports <- df["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))] #+
    #df["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Distribution_Exports) <- "Distribution Services Exports"
  
  Electronic_Exports <- df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Exports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Exports) <- "Electronic Services Exports"
  
  Finance_Exports <- df["Financial.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Finance_Exports) <- "Financial Services Exports"
  
  Travel_Exports <- df["Travel.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))]
   row.names(Travel_Exports) <- "Travel Services Exports"
  
  ChargesForTheUseOfIpNie_Exports <- df["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(ChargesForTheUseOfIpNie_Exports) <- "Charges for the use of Intellectual Property Exports"
  
  Professional_Exports <- df["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["IndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
    #df["Training.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["MaintenanceAndRepairNie.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Legal.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Advertising.Exports.AllAffiliations", 5:(4 + length(years))]
   row.names(Professional_Exports) <- "Professional Services Exports"
  
  Other_Exports <- df["Const.Exports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Exports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Other_Exports) <- "Other Services Exports"
  
  TotalPrivate_Exports <- df["AllServiceTypes.Exports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(TotalPrivate_Exports) <- "Total Private Services Exports"
  
  Distribution_Imports <- df["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))] #+
    #df["TradeRelated.Exports.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Distribution_Imports) <- "Distribution Services Imports"
  
  Electronic_Imports <- df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Imports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Imports) <- "Electronic Services Imports"
  
  Finance_Imports <- df["Financial.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Finance_Imports) <- "Financial Services Imports"
  
  Travel_Imports <- df["Travel.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Travel_Imports) <- "Travel Services Imports"
  
  ChargesForTheUseOfIpNie_Imports <- df["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(ChargesForTheUseOfIpNie_Imports) <- "Charges for the use of Intellectual Property Imports"
  
  Professional_Imports <- df["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["IndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
    #df["Training.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["MaintenanceAndRepairNie.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Legal.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Advertising.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Professional_Imports) <- "Professional Services Imports"
  
  Other_Imports <- df["Const.Imports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Imports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Other_Imports) <- "Other Services Imports"
  
  TotalPrivate_Imports <- df["AllServiceTypes.Imports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(TotalPrivate_Imports) <- "Total Private Services Imports"
  
  df <- rbind(Distribution_Exports,Electronic_Exports,Finance_Exports,Professional_Exports,ChargesForTheUseOfIpNie_Exports,Travel_Exports,Other_Exports,TotalPrivate_Exports,
              Distribution_Imports,Electronic_Imports,Finance_Imports,Professional_Imports,ChargesForTheUseOfIpNie_Imports,Travel_Imports,Other_Imports,TotalPrivate_Imports)
  
  # Performs aggregations and calculations
  function.cagr <- function(df,col_name1,end_date,start_date,periods) {
    df[[col_name1]] <- ((df[[end_date]]/df[[start_date]])^(1/periods))-1
    df
  }
  
  function.growth <- function(df,col_name2,final_year,end_date) {
    df[[col_name2]] <- (df[[final_year]]-df[[end_date]])/df[[end_date]]
  }

  
  CAGR <- function.cagr(df, "CAGR", end_date,beginning_date,length(years)-2)
  Year_on_Year <- function.growth(df, "Year-on-Year Growth", final_year, end_date)
  
  df <- cbind(CAGR, Year_on_Year)
}

final.countries <- data.frame()

for (i in 1:length(countries)) {
  arg1 <- "TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue"
  arg2 <- "function.format.aggregate.calculate("
  arg3 <- paste0(arg2, countries[[i]], ", ",arg1,")", sep="")
  final.countries1 <- eval(parse(text=arg3))
  final.countries2 <- cbind(countries[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)

}

final.countries$CAGR <- final.countries$CAGR*100
final.countries$Year_on_Year <- final.countries$Year_on_Year*100
round(final.countries$CAGR, 2)
round(final.countries$Year_on_Year, 2)
names(final.countries)[names(final.countries)=="countries[[i]]"] <- "Countries"

#use levels of final.countries$Countries to name the split files
country.names <- levels(final.countries$Countries)
final.countries2  <- split(final.countries, final.countries$Countries)
names(final.countries2) <- country.names
list2env(final.countries2, envir = .GlobalEnv)
#rm(final.countries1, final.countries2, final.result1, Combined_table1)
na_test5 <- Combined_table1[rowSums(is.na(Combined_table1)) > 0,]

sapply(names(final.countries2), 
       function (x) write.csv(final.countries2[[x]], file=paste(x, "csv", sep=".")))
write.csv(na_test5, file = file_name3, row.names = TRUE)

#_____________________________________________________________________________________



#Create industry groups

Distribution_Exports_sum <- rbind(Distribution_Exports, Combined_table3["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))], 
                                    Combined_table3["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Distribution_Exports_sum) <- c("Total Distribution Exports", "Sea Transport Exports", "Air Transport (freight) Exports", "Air Transport (port) Exports",
"Other modes of transport Exports", "Trade related services Exports")


Distribution_Imports_sum <- rbind(Distribution_Imports, Combined_table3["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TradeRelated.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Distribution_Imports_sum) <- c("Total Distribution Imports", "Sea Transport Import", "Air Transport (freight) Imports", "Air Transport (port) Imports",
                                         "Other modes of transport Imports", "Trade related services Imports")

Electronic_Exports_sum <- rbind(Electronic_Exports, Combined_table3["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Telecom.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Comp.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Info.Exports.AllAffiliations", 5:(4 + length(years))] 
)

row.names(Electronic_Exports_sum) <- c("Total Electronic Exports", "Audio-visual and Related Products Exports", "Telecommunications Services Exports",
                                       "Computer Services Exports", "Information Services Exports")

Electronic_Imports_sum <- rbind(Electronic_Imports, Combined_table3["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Telecom.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Comp.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Info.Imports.AllAffiliations", 5:(4 + length(years))] 
)

row.names(Electronic_Imports_sum) <- c("Total Electronic Imports", "Audio-visual and Related Products Imports", "Telecommunications Services Imports",
                                        "Computer Services Imports", "Information Services Imports")

Finance_Exports_sum <- rbind(Finance_Exports, Combined_table3["Financial.Exports.AllAffiliations", 5:(4 + length(years))],
                               Combined_table3["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Finance_Exports_sum) <- c("Total Financial Exports", "Financial Exports", "Insurance Exports")

Finance_Imports_sum <- rbind(Finance_Imports, Combined_table3["Financial.Imports.AllAffiliations", 5:(4 + length(years))],
                               Combined_table3["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Finance_Imports_sum) <- c("Total Financial Imports", "Financial Imports", "Insurance Imports")

Professional_Exports_sum <- rbind(Professional_Exports, Combined_table3["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["IndEng.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Training.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["MaintenanceAndRepairNie.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Legal.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Advertising.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Professional_Exports_sum) <- c("Total Professional Exports", "Business and Mangement Consulting Exports", "Architecture and Engineering Exports", 
                                          "Industrial Engineering Exports","Training Exports", "Research and Development Exports", 
                                         "Maintenance and Repair Exports", "Accounting and Auditing Exports", "Legal and Accounting Exports",
                                         "Advertising Exports")
                                         
Professional_Imports_sum <- rbind(Professional_Imports, Combined_table3["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["IndEng.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Training.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["MaintenanceAndRepairNie.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Legal.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Advertising.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Professional_Exports_sum) <- c("Total Professional Imports", "Business and Mangement Consulting Imports", "Architecture and Engineering Imports", 
                                         "Industrial Engineering Imports","Training Imports", "Research and Development Imports", 
                                         "Maintenance and Repair Imports", "Accounting and Auditing Imports", "Legal and Accounting Imports",
                                         "Advertising Imports")

ChargesForTheUseOfIpNie_Exports_sum <- rbind(ChargesForTheUseOfIpNie_Exports, Combined_table3["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(ChargesForTheUseOfIpNie_Exports_sum) <- c("Total Charges for the use of Intellectual Property Exports",
                                                    "Industrial Processes Exports", "Computer Software Exports", "Trademarks Exports", 
                                                    "Franchise Fees Exports", "Other Intellectual Property Exports")

ChargesForTheUseOfIpNie_Imports_sum <- rbind(ChargesForTheUseOfIpNie_Imports, Combined_table3["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(ChargesForTheUseOfIpNie_Imports_sum) <- c("Total Charges for the use of Intellectual Property Imports",
                                                    "Industrial Processes Imports", "Computer Software Imports", "Trademarks Imports", 
                                                    "Franchise Fees Imports", "Other Intellectual Property Imports")

Travel_Exports_sum <- rbind(Travel_Exports, Combined_table3["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelBusiness.Exports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelPersonal.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Travel_Exports_sum) <- c("Total Travel Exports", "Air Passenger Fares Exports", "Business Travel Exports", "Personal Travel Exports")

Travel_Imports_sum <- rbind(Travel_Imports, Combined_table3["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelBusiness.Imports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelPersonal.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Travel_Imports_sum) <- c("Total Travel Imports", "Air Passenger Fares Imports", "Business Travel Imports", "Personal Travel Imports")

Other_Exports_sum <- rbind(Other_Exports, Combined_table3["Const.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["Mining.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["OthBusinessNie.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Other_Exports_sum) <- c("Total Other Exports", "Construction Exports", "Mining Exports",
                                  "Sports and Performing Arts Exports", "Operating and Leasing Exports",
                                  "Other Business Services Exports")

Other_Imports_sum <- rbind(Other_Imports, Combined_table3["Const.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["Mining.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["OthBusinessNie.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Other_Imports_sum) <- c("Total Other Imports", "Construction Imports", "Mining Imports",
                                  "Sports and Performing Arts Imports", "Operating and Leasing Imports",
                                  "Other Business Services Imports")

Summary_Table <- rbind(TotalPrivate_Exports, TotalPrivate_Imports, Distribution_Exports_sum, Distribution_Imports_sum, Electronic_Exports_sum, Electronic_Imports_sum, Finance_Exports_sum, Finance_Imports_sum,
                       ChargesForTheUseOfIpNie_Exports_sum, ChargesForTheUseOfIpNie_Imports_sum, Travel_Exports_sum, Travel_Imports_sum,
                       Professional_Exports_sum, Professional_Imports_sum, Other_Exports_sum, Other_Imports_sum)

CAGR2 <- function.cagr2(Summary_Table, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year2 <- function.growth2(Summary_Table, "Year-on-Year Growth", final_year, end_date)

Summary_Table <- cbind(CAGR2, Year_on_Year2)

Summary_Table$CAGR <- Summary_Table$CAGR*100
Summary_Table$CAGR <- round(Summary_Table$CAGR,2)
Summary_Table$Year_on_Year <- Summary_Table$Year_on_Year*100
Summary_Table$Year_on_Year <- round(Summary_Table$Year_on_Year,2)
Total_aggregate$CAGR <- Total_aggregate$CAGR*100
Total_aggregate$CAGR <- round(Total_aggregate$CAGR,2)
Total_aggregate$Year_on_Year <- Total_aggregate$Year_on_Year*100
Total_aggregate$Year_on_Year <- round(Total_aggregate$Year_on_Year,2)

#exports final file to csv
write.csv(Total_aggregate, file = file_name1, row.names = TRUE)
write.csv(Summary_Table, file = file_name2, row.names = TRUE)

#-------------------------------------------------------------------------------------


aggregate_comp <- Combined_table3[c("TransportSea.Exports.AllAffiliations", 
                                    "TransportAirFreight.Exports.AllAffiliations",
                                    "TransportAirPort.Exports.AllAffiliations",
                                    "TransportOth.Exports.AllAffiliations",
                                    "TradeRelated.Exports.AllAffiliations",
                                    "CipAudVisRelated.Exports.AllAffiliations",
                                    "Telecom.Exports.AllAffiliations",
                                    "Comp.Exports.AllAffiliations",
                                    "Info.Exports.AllAffiliations",
                                    "Financial.Exports.AllAffiliations",
                                    "Insurance.Exports.AllAffiliations",
                                    "Travel.Exports.AllAffiliations",
                                    "TransportAirPass.Exports.AllAffiliations",
                                    "ChargesForTheUseOfIpNie.Exports.AllAffiliations",
                                    "ProfMgmtConsult.Exports.AllAffiliations",
                                    "ArchAndEng.Exports.AllAffiliations",
                                    "IndEng.Exports.AllAffiliations",
                                    "Training.Exports.AllAffiliations",
                                    "ResearchAndDev.Exports.AllAffiliations",
                                    "MaintenanceAndRepairNie.Exports.AllAffiliations",
                                    "Const.Exports.AllAffiliations",
                                    "Mining.Exports.AllAffiliations",
                                    "SportsPerformArts.Exports.AllAffiliations",
                                    "OperatingLeasing.Exports.AllAffiliations",
                                    "OperatingLeasing.Exports.AllAffiliations",
                                    "OthBusinessNieCtry.Exports.AllAffiliations", 
                                    "TransportSea.Imports.AllAffiliations", 
                                    "TransportAirFreight.Imports.AllAffiliations",
                                    "TransportAirPort.Imports.AllAffiliations",
                                    "TransportOth.Imports.AllAffiliations",
                                    "TradeRelated.Imports.AllAffiliations",
                                    "CipAudVisRelated.Imports.AllAffiliations",
                                    "Telecom.Imports.AllAffiliations",
                                    "Comp.Imports.AllAffiliations",
                                    "Info.Imports.AllAffiliations",
                                    "Financial.Imports.AllAffiliations",
                                    "Insurance.Imports.AllAffiliations",
                                    "Travel.Imports.AllAffiliations",
                                    "TransportAirPass.Imports.AllAffiliations",
                                    "ChargesForTheUseOfIpNie.Imports.AllAffiliations",
                                    "ProfMgmtConsult.Imports.AllAffiliations",
                                    "ArchAndEng.Imports.AllAffiliations",
                                    "IndEng.Imports.AllAffiliations",
                                    "Training.Imports.AllAffiliations",
                                    "ResearchAndDev.Imports.AllAffiliations",
                                    "MaintenanceAndRepairNie.Imports.AllAffiliations",
                                    "Const.Imports.AllAffiliations",
                                    "Mining.Imports.AllAffiliations",
                                    "SportsPerformArts.Imports.AllAffiliations",
                                    "OperatingLeasing.Imports.AllAffiliations",
                                    "OperatingLeasing.Imports.AllAffiliations",
                                    "OthBusinessNieCtry.Imports.AllAffiliations"),
                                  5:(4 + length(years))]

n_a_test <- aggregate_comp[rowSums(is.na(aggregate_comp)) > 0,]

