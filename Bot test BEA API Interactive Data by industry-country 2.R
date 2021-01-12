# User sets folder where files are written


setwd("Z:/BEA API R/data/Interactive data by industry/Test")
#setwd("~/Drive/R")

#user sets file name


file_name3 <- "Missing_Data_Industry_Interactive.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "All"
affiliation <- "AllAffiliations"
years <- 2011:2016
Years_for_CAGR <- 2011:2015
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

library(readr)

#Concatenates years

if (years == "X") {
  year_list <- "X"
} else {
  year_list <- paste(years, collapse = ",")
}


#URL for calling BEA API
#must insert your user ID

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=your_user_ID",
  
  "&method=GetData&DataSetName=", dataset,
  
  "&TypeOfService=")


url2 <- paste0(
  
  "&TradeDirection=", trade_direction,
  
  "&Affiliation=", affiliation,
  
  "&AreaOrCountry=", countries,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
  
)

#creates a table to store results

final.result1 <- data.frame()

#Loop that calls API for each sector and creates a table of data


for (i in 1:length(indicators)) {
  this.indicator <- indicators[[i]]
  this.call <- paste0(url1, this.indicator, url2)	
  this.raw.result1 <- readLines(this.call,encoding="UTF-8", warn=FALSE)
  this.raw.result2 <- fromJSON(this.raw.result1)
  
  this.result <- this.raw.result2$BEAAPI$Results$Data
  
  final.result1 <- rbind.data.frame(final.result1, this.result)
}

Combined_table1 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue) 
Combined_table1 <- dcast(Combined_table1, TypeOfService + TradeDirection + Affiliation + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0) 
Combined_table1[5: (4 + length(years))] = lapply(Combined_table1[5:(4 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
Combined_table1$Unique_ID <- do.call(paste, c(Combined_table1[c("TypeOfService", "TradeDirection", "Affiliation")], sep = ".")) 
Combined_table1 <- as.data.frame.matrix(Combined_table1, row.names=Combined_table1$Unique_ID) 
Combined_table1[5:(4 + length(years))] <- lapply(Combined_table1[5:(4 + length(years))], as.numeric) 
Combined_table1 <- as.data.frame.matrix(Combined_table1, col.names=TRUE) 
Combined_table2 <- Combined_table1[,-(5 + length(years))] 
Combined_table6 <- is.na(Combined_table2)
Combined_table2[Combined_table6] <- rep(0, sum(Combined_table6))
countries2 <- unique(Combined_table2$AreaOrCountry, incomparables = FALSE)


Combined_table3  <- split(Combined_table2, Combined_table1$AreaOrCountry)    
Combined_table3 <- lapply(seq_along(Combined_table3), function(x) as.data.frame(Combined_table3[[x]])[, 1:(4 + length(years))])  
names(Combined_table3) <- countries2 
list2env(Combined_table3, envir = .GlobalEnv)  

end_date <- paste0("",(Years_for_CAGR[length(Years_for_CAGR)]),"",sep="")
beginning_date <- paste0("",Years_for_CAGR[1],"",sep="")
final_year <- paste0("",(years[length(years)]),"",sep="")

CAGR_name <- paste0("CAGR ", beginning_date, " to ", end_date)
Year_to_year_name <- paste0("Growth from ", end_date, " to ", final_year)

function.format.aggregate.calculate <- function(df) {
  Distribution_Exports <- df["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))] #+
  #df["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Distribution_Exports) <- "Distribution Services Exports"
  
  Logistics_exports <- df["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Logistics_exports) <- "Logistics exports"
  
  Maritime_exports <- df["TransportSeaFreight", 5:(4 + length(years))] + 
    df["TransportSeaPort", 5:(4 + length(years))]
  row.names(Maritime_exports) <- "Maritime Transport exports"
  
  Electronic_Exports <- df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Exports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Exports) <- "Electronic Services Exports"
  
  Electronic_Exports2 <- df["TelecomCompAndInfo.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Electronic_Exports2) <- "Electronic Services Exports"
  
  Audio_visual_exports <- df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Audio_visual_exports) <- "Audio visual exports"
  
  Telecom_exports <- df["Telecom.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Telecom_exports) <- "Telecommuncations exports"
  
  Computer_services_exports <- df["Comp.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Computer_services_exports) <- "computer services exports"
  
  Finance_Exports <- df["Financial.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Finance_Exports) <- "Financial Services Exports"
  
  Banking_exports <- df["FinFinManFinAdvCust.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["FinCredCardOthCredRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Banking_exports) <- "Banking exports"
  
  Securities_exports <- df["FinSecBrokUwRelated.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["FinSecLendEftOth.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Securities_exports) <- "Securities exports"
  
  Insurance_exports <- df["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Insurance_exports) <- "Insurance exports"
  
  Travel_Exports <- df["Travel.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Travel_Exports) <- "Travel Services Exports"
  
  ChargesForTheUseOfIpNie_Exports <- df["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(ChargesForTheUseOfIpNie_Exports) <- "Charges for the use of Intellectual Property Exports"
  
  ChargesForTheUseOfIpNie_Exports2 <- df["ChargesForTheUseOfIpNie.Exports.AllAffiliations", 5:(4 + length(years))] -
    df["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))] -
    df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(ChargesForTheUseOfIpNie_Exports2) <- "Charges for the use of Intellectual Property Exports alt calc"
  
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
  
  Professional_Exports2 <- df["ProfMgmtConsult.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TechTradeRelatedOth.Exports.AllAffiliations", 5:(4 + length(years))] -
    df["Const.Exports.AllAffiliations", 5:(4 + length(years))] -
    df["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))] - 
    df["OthBusinessNieCtry.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Professional_Exports2) <- "Professional Services Exports"
  
  Architecture_exports <- df["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["IndEng.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Architecture_exports) <- "Architecture and Engineering exports"
  
  Accounting_exports <- df["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Accounting_exports) <- "Accounting exports"
  
  Legal_exports <- df["Legal.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Legal_exports) <- "Legal exports"
  
  Management_exports <- df["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Management_exports) <- "Management Consulting exports"
  
  
  Other_Exports <- df["Const.Exports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Exports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Other_Exports) <- "Other Services Exports"
  
  TotalPrivate_Exports <- df["AllServiceTypes.Exports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(TotalPrivate_Exports) <- "Total Private Services Exports"
  
  Total_Exports <- df["AllServiceTypes.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Total_Exports) <- "Total Services Exports"
  
  Distribution_Imports <- df["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))] #+
  #df["TechTradeRelatedOth.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Distribution_Imports) <- "Distribution Services Imports"
  
  Logistics_imports <- df["TransportAirFreight.imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Logistics_imports) <- "Logistics imports"
  
  Maritime_imports <- df["TransportSeaFreight.imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportSeaPort.imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Maritime_imports) <- "Maritime Transport imports"
  
  Electronic_Imports <- df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Imports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Imports) <- "Electronic Services Imports"
  
  Electronic_Imports2 <- df["TelecomCompAndInfo.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Electronic_Imports2) <- "Electronic Services Imports"
  
  Audio_visual_imports <- df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Audio_visual_imports) <- "Audio visual imports"
  
  Telecom_imports <- df["Telecom.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Telecom_imports) <- "Telecommuncations imports"
  
  Computer_services_imports <- df["Comp.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Computer_services_imports) <- "computer services imports"
  
  Finance_Imports <- df["Financial.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Finance_Imports) <- "Financial Services Imports"
  
  Banking_imports <- df["FinFinManFinAdvCust.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["FinCredCardOthCredRelated.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Banking_imports) <- "Banking imports"
  
  Securities_imports <- df["FinSecBrokUwRelated.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["FinSecLendEftOth.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Securities_imports) <- "Securities imports"
  
  Insurance_imports <- df["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Insurance_imports) <- "Insurance imports"
  
  Travel_Imports <- df["Travel.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Travel_Imports) <- "Travel Services Imports"
  
  ChargesForTheUseOfIpNie_Imports <- df["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(ChargesForTheUseOfIpNie_Imports) <- "Charges for the use of Intellectual Property Imports"
  
  ChargesForTheUseOfIpNie_Imports2 <- df["ChargesForTheUseOfIpNie.Imports.AllAffiliations", 5:(4 + length(years))] -
    df["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))] -
    df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Electronic_Imports2) <- "Charges for the use of Intellectual Property Imports"
  
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
  
  Professional_Imports2 <- df["ProfMgmtConsult.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TechTradeRelatedOth.Imports.AllAffiliations", 5:(4 + length(years))] -
    df["Const.Imports.AllAffiliations", 5:(4 + length(years))] -
    df["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))] - 
    df["OthBusinessNieCtry.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Professional_Imports2) <- "Professional Services Imports"
  
  Architecture_imports <- df["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["IndEng.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Architecture_imports) <- "Architecture and Engineering imports"
  
  Accounting_imports <- df["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Accounting_imports) <- "Accounting imports"
  
  Legal_imports <- df["Legal.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Legal_imports) <- "Legal imports"
  
  Management_imports <- df["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Management_imports) <- "Management Consulting imports"
  
  Other_Imports <- df["Const.Imports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Imports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Other_Imports) <- "Other Services Imports"
  
  TotalPrivate_Imports <- df["AllServiceTypes.Imports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(TotalPrivate_Imports) <- "Total Private Services Imports"
  
  Total_Imports <- df["AllServiceTypes.Imports.AllAffiliations", 5:(4 + length(years))]
  row.names(Total_Imports) <- "Total Services Imports"
  
  Other_Exports2 <- TotalPrivate_Exports - Distribution_Exports - Electronic_Exports2 - Finance_Exports - 
    Professional_Exports2 - ChargesForTheUseOfIpNie_Exports2 - Travel_Exports
  row.names(Other_Exports2) <- "Other Services Exports (includes suppressed data)"  
  
  Other_Imports2 <- TotalPrivate_Imports - Distribution_Imports - Electronic_Imports2 - Finance_Imports - 
    Professional_Imports2 - ChargesForTheUseOfIpNie_Imports2 - Travel_Imports
  row.names(Other_Imports2) <- "Other Services Imports (includes suppressed data)"
  
  df <- rbind(Distribution_Exports,Electronic_Exports2,Finance_Exports,Professional_Exports2,ChargesForTheUseOfIpNie_Exports2,Travel_Exports,Other_Exports2,TotalPrivate_Exports,
              Distribution_Imports,Electronic_Imports2,Finance_Imports,Professional_Imports2,ChargesForTheUseOfIpNie_Imports2,Travel_Imports,Other_Imports2,TotalPrivate_Imports,
              Audio_visual_exports, Telecom_exports, Computer_services_exports,Audio_visual_imports, Telecom_imports, Computer_services_imports, Total_Exports, Total_Imports)
  
  # Performs aggregations and calculations
  function.cagr <- function(df,col_name1,end_date,start_date,periods) {
    df[[col_name1]] <- ((df[[end_date]]/df[[start_date]])^(1/(length(periods)-1)))-1
    df
  }
  
  function.growth <- function(df,col_name2,final_year,end_date) {
    df[[col_name2]] <- (df[[final_year]]-df[[end_date]])/df[[end_date]]
    df
  }
  
  
  CAGR <- function.cagr(df, CAGR_name, end_date,beginning_date,Years_for_CAGR)
  Year_on_Year <- function.growth(df, Year_to_year_name, final_year, end_date)
  
  df <- cbind(CAGR, Year_on_Year)
  df <- df %>% subset(., select=which(!duplicated(names(.)))) 
}

final.countries <- data.frame()

for (i in 1:length(countries2)) {
  
  arg1 <- "function.format.aggregate.calculate(" 
  arg2 <- paste0(arg1,countries2[[i]],")", sep="")
  final.countries1 <- eval(parse(text=arg2))
  final.countries2 <- cbind(countries2[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)
}

rm(Africa,AllCountries,AsiaAndPac,Australia,Austria,Bahrain,Belgium,Bermuda,Brazil,Brunei,
   Bulgaria,CaftaDrCountries,Canada,Chile,China,Colombia,CostaRica,Croatia,Cyprus,CzechRep,Denmark,
   DominicanRep,ElSalvador,Estonia,EU,EuroArea,Europe,Finland,France,Germany,Greece,Guatemala,Honduras,
   HongKong,Hungary,India,Indonesia,IntOrgAndUnalloc,Ireland,Israel,Italy,Japan,Jordan,KoreaRepOf,
   LatAmAndOthWestHem,Latvia,Lithuania,Luxembourg,Malaysia,Malta,Mexico,MiddleEast,Morocco,NaftaCountries,
   Netherlands,NewZealand,Nicaragua,Nigeria,Norway,Oman,OthAfricaIst,OthAsiaAndPacIst,OthEuropeIst,
   OthMiddleEastIst,OthSouthAndCenAmIst,OthWestHem,OthWestHemOthIst,Panama,Peru,Philippines,Poland,Portugal,
   Romania,Russia,SaudiArabia,Singapore,Slovakia,Slovenia,SouthAfrica,SouthAndCenAm,Spain,Sweden,Switzerland,
   Taiwan,Thailand,Turkey,UkIslandsCarib,UnitedKingdom,Venezuela,Vietnam)

final.countries[,length(years)+2] <- final.countries[,length(years)+2]*100
final.countries[,length(years)+3]<- final.countries[,length(years)+3]*100
round(final.countries[,length(years)+2], 2)
round(final.countries[,length(years)+3], 2)
final.countries <- cbind(row.names(final.countries), final.countries)
names(final.countries)[names(final.countries) == "countries2[[i]]"] <- "Countries"
names(final.countries)[names(final.countries) == "row.names(final.countries)"] <- "Industries"
final.countries$Industries = gsub("[[:digit:]]", "", final.countries$Industries)
last_column <- paste0("final.countries[order(final.countries$Industries, -final.countries$'", final_year, "'),]", sep="")
final.countries2 <- eval(parse(text=last_column))
regions <- c("Africa","AsiaAndPac","CaftaDrCountries","EuroArea","Europe","IntOrgAndUnalloc","LatAmAndOthWestHem","MiddleEast",
             "NaftaCountries","OthAfricaIst","OthAsiaAndPacIst","OthEuropeIst","OthMiddleEastIst","OthSouthAndCenAmIst","OthWestHem",
             "OthWestHemOthIst","SouthAndCenAm")
final.countries3 <- final.countries2[!grepl(paste(regions, collapse = "|"), final.countries2$Countries),]

countries <- unique(final.countries3$Countries, incomparables = FALSE)
industries2 <- unique(final.countries3$Industries, incomparables = FALSE)
industries2 <- industries2[order(industries2)]

#need to combine exports and imports for each industry into single column
#keep second column for exports/imports
# for each unique value in industries, do the report generator

for (i in 1:length(industries2)) {
  df <- industries2[[i]]
  industry.exports <- df["AllCountries", 2 + length(years)]
  

#final.countries3  <- split(final.countries3, final.countries3$Industries)

#names(final.countries3) <- industries2
#list2env(final.countries, envir = .GlobalEnv)  

  
  #code for report generator may not work
  
  dd <- if (abs(df["Total Private Services Exports", Year_to_year_name]) < 3) {
    print(comp_1)
  } else if (abs(df["Total Private Services Exports", Year_to_year_name]) > 5) {
    print(comp_3)
  } else print(comp_2)
  
  ee <- if (df["Total Private Services Exports", Year_to_year_name] < 0) {
    print(down[1])
  } else print(up[1])
  
  ff <- if (abs(df["Total Private Services Imports", Year_to_year_name]) < 3) {
    print(comp_1)
  } else if (abs(df["Total Private Services Imports", Year_to_year_name]) > 5) {
    print(comp_3)
  } else print(comp2)
  
  gg <- if (df["Total Private Services Imports", Year_to_year_name] < 0) {
    print(ran_down[1])
  } else print(ran_up[1])
  
  hh <- if (df["Total Private Services Imports", Year_to_year_name] < 0) {
    print(down_3[1])
  } else print(up_3[1])
  
  ii <- if (df[2,3] < 0) {
    print(ran_down[2])
  } else print(ran_up[2])
  
  kk <- if (df["Total Private Services Exports",length(years)] > df["Total Private Services Imports",length(years)]) {
    print("surplus ")
  } else print("deficit ")
  
  ll <- if (df["Total Private Services Exports",length(years)]+df["Total Private Services Imports",length(years)] >
            df["Total Private Services Exports",1]+df["Total Private Services Imports",1]) {
    print(up[2])
  } else print(down[2])
  
  mm <- if ((df["Total Private Services Exports",length(years)] - df["Total Private Services Imports",length(years)]) > 
            ( df["Total Private Services Exports",1]+df["Total Private Services Imports",1])) {
    print (up_3[1])
  } else print(down_3[1])
  
  
  df2 <- df2[order(df2[,length(years)], decreasing = TRUE),]
  df2.1 <- subset(df2, df2$Year_to_year_name<0)
  
  df3 <- df3[order(df3[,length(years)], decreasing = TRUE),]
  df3.1 <- subset(df3, df3$Year_to_year_name<0)
  
  # Works but need to redo text for industries
  
  report_text_1 <- paste0("U.S. exports of ",  ee, " by ", round((df["Total Private Services Exports", Year_to_year_name])*100, 2),
                          " percent annually to $", df["Total Private Services Exports", length(years)], " billion in ", years[length(years)])
  
  report_text_2 <- paste0("U.S. imports ", gg, " by ", round((df["Total Private Services Imports", Year_to_year_name])*100, 2),
                          " percent annually to $", df["Total Private Services Imports", length(years)], " billion in ", years[length(years)])
  
  report_text_3 <- paste0("Total services trade between the United States and ", countries[[i]], " ", ll, " during ",
                          years[1], "--", years[length(years)],". U.S. exports of services ", ee ," by $", abs(df["Total Private Services Exports", final_year]-
                                                                                                                 df["Total Private Services Exports", beginning_date]), 
                          " billion, while U.S. imports ", gg, " $", abs(df["Total Private Services Imports", final_year]-
                                                                                                                                                                                                                         
                          df["Total Private Services Imports", beginning_date]), " billion. The United States recorded a services trade ", kk, "with ", countries[[i]],
                          " of $", abs(df["Total Private Services Exports",length(years)] - df["Total Private Services Imports",length(years)])," billion in ", final_year,
                          ", ", mm, " from $", abs(df["Total Private Services Exports",beginning_date] - df["Total Private Services Imports",beginning_date]), 
                          " billion in ", beginning_date, ". ", rownames(df2[1,]), " accounted for the largest category of U.S. services exports to ",  countries[[i]], ", followed by, ",
                          rownames(df2[2,])," while ", rownames(df3[1,]), " and ", rownames(df3[2,]), " accounted for the largest shares of U.S. services imports, respectively.")
  
  
  file_name4 <- paste0(countries[[i]], ".txt")
  sink(file_name4)
  report_text_final <- cat( report_text_1, report_text_2, report_text_3, sep="\n")
  sink()
  file.show(file_name4)
  
}

final.countries <- data.frame()

for (i in 1:length(countries2)) {

  arg1 <- "function.format.aggregate.calculate(" 
  arg2 <- paste0(arg1,countries2[[i]],")", sep="")
  final.countries1 <- eval(parse(text=arg2))
  final.countries2 <- cbind(countries2[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)
}

