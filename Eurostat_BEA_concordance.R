#import data table in standard wide format
#need to fill in blank cells as 0s before import - tkae out letter flags too
Eurostat.for.R.inward <- read.csv("//s1p-fsc-01/Home/arthur.chambers/Desktop/TPA Retro 2/Digital modeling/Eurostat/Eurostat for R inward_test.csv")
library(data.table)
library(dplyr)
#rename columns (as appropriate)
inward_1 <- Eurostat.for.R.inward
lapply(inward_1[,3:12], as.numeric)
names(inward_1)[1] <- "industry"
#names(inward_1)[7] <- "inward_rev"
#names(inward_1)[6] <- "year"

#create id variable
inward_1$ID_var <- paste0(inward_1$partner.country,".", inward_1$reporting.country)

inward_1$indicator <- NULL
inward_1$partner.country <- NULL
inward_1$reporting.country <- NULL
inward_1 <- subset(inward_1, inward_1$nace.rev.2 != "D35")
inward_1 <- subset(inward_1, inward_1$nace.rev.2 != "L68")
inward_1$nace.rev.2 <- NULL
inward_1$X <- NULL
#split by country pair
inward_2 <- split(inward_1, inward_1$ID_var)

final_result <- data.frame()

# be careful to count the columns when updating as adding years will change column numbering

#test_2 <- sapply(inward_2, function.aggregate)

list_names <- names(inward_2)
#list2env(names(inward_2) envir = .GlobalEnv)
final_inward <- data.frame()

for (i in 1:length(inward_2)) {
  df <- inward_2[[i]]
  row.names(df) <- df[,1]
  df[,1] <- NULL
  country_pair <- df[1,11]
  df[,11] <- NULL
  #df <- as.numeric(df)
  mining <- df["Mining and quarrying",1:10 ]
  manufacturing <- df["Manufacturing", 1:10]
  food <- df["Manufacture of food products", 1:10]
  beverages_tobacco <-  df["Manufacture of beverages", 1:10] + df["Manufacture of tobacco products", 1:10]
  textiles_apparel_leather <- df["Other professional, scientific and technical activities",1:10] + df["Veterinary activities",1:10] + df["Manufacture of leather and related products", 1:10]
  wood_prod <- df["Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",1:10]
  paper <- df["Manufacture of paper and paper products", 1:10]
  printing <- df["Printing and reproduction of recorded media", 1:10]
  petro_coke <- df["Manufacture of coke and refined petroleum products", 1:10]
  chemicals <- df["Manufacture of chemicals and chemical products", 1:10] + df["Manufacture of basic pharmaceutical products and pharmaceutical preparations", 1:10]
  basic_chems <- df["Manufacture of chemicals and chemical products", 1:10]
  pharma <- df["Manufacture of basic pharmaceutical products and pharmaceutical preparations", 1:10]
  rubber_plastic <- df["Manufacture of rubber and plastic products",1:10]
  non_metal_minerals <- df["Manufacture of other non-metallic mineral products",1:10]
  prim_fab_metals <- df["Manufacture of basic metals", 1:10] + df["Manufacture of fabricated metal products, except machinery and equipment",1:10]
  machinery <- df["Manufacture of machinery and equipment n.e.c.",1:10]
  manuf_computer <- df["Manufacture of computer, electronic and optical products", 1:10]
  eletric_equip <- df["Manufacture of computer, electronic and optical products",1:10]
  trans_equip <- df["Manufacture of motor vehicles, trailers and semi-trailers",1:10] + df["Manufacture of other transport equipment",1:10]
  motor_vehic <- df["Manufacture of motor vehicles, trailers and semi-trailers",1:10]
  other_trans_equip <- df["Manufacture of other transport equipment",1:10]
  furniture <- df["Manufacture of furniture",1:10]
  wholesale <- df["Wholesale and retail trade and repair of motor vehicles and motorcycles",1:10] + df["Wholesale trade, except of motor vehicles and motorcycles",1:10]
  retail <- df["Retail trade, except of motor vehicles and motorcycles",1:10]
  general_merch <- df["Retail sale in non-specialised stores",1:10]
  food_bev_store <- df["Retail sale of food, beverages and tobacco in specialised stores",1:10]
  nonstore_retail <- df["Retail trade not in stores, stalls or markets",1:10]
  information <- df["Information and communication",1:10]
  publishing <- df["Publishing activities",1:10]
  news_book_pub <- df["Publishing activities",1:10]
  software_pub <- df["Software publishing",1:10]
  motion_pic <- df["Motion picture, video and television programme production, sound recording and music publishing activities",1:10]
  telecoms <- df["Telecommunications",1:10]
  broadcasting <- df["Programming and broadcasting activities",1:10]
  data_process <- df["Data processing, hosting and related activities; web portals",1:10]
  other_info_serv <- df["Other information service activities",1:10]
  fin_isr <- df["Financial service activities, except insurance and pension funding",1:10] + df["Insurance, reinsurance and pension funding, except compulsory social security",1:10] + df["Activities auxiliary to financial services and insurance activities",1:10]
  finance <- df["Financial service activities, except insurance and pension funding",1:10]
  insurance <- df["Insurance, reinsurance and pension funding, except compulsory social security",1:10]
  real_estate_rental <- df["Real estate activities",1:10] +df["Rental and leasing activities",1:10]
  real_estate <- df["Real estate activities",1:10]
  rental_lease <- df["Rental and leasing activities",1:10]
  professional <- df["Professional, scientific and technical activities",1:10] + df["Computer programming, consultancy and related activities",1:10]
  ag_engineering <- df["Architectural and engineering activities; technical testing and analysis",1:10]
  comp_sys_design <- df["Computer programming, consultancy and related activities",1:10]
  legal_account <- df["Legal and accounting activities",1:10]
  r_d <- df["Scientific research and development",1:10]
  advertising <- df["Advertising and market research",1:10]
  other_prof_serv <- df["Other professional, scientific and technical activities",1:10] + df["Veterinary activities",1:10]
  construction <- df["Construction",1:10]
  transport_warehouse <- df["Transportation and storage",1:10]
  air_trans <- df["Air transport",1:10]
  rail_truck_trans <- df["Land transport and transport via pipelines",1:10]
  water_trans <- df["Water transport",1:10]
  support_trans <- df["Warehousing and support activities for transportation",1:10]
  admin_support_waste <- df["Administrative and support service activities",1:10] + df["Water supply; sewerage, waste management and remediation activities",1:10] - df["Rental and leasing activities",1:10]
  admin_support <- df["Administrative and support service activities",1:10] - df["Rental and leasing activities",1:10]
  waste_man <- df["Water supply; sewerage, waste management and remediation activities",1:10]
  
  result_1 <- rbind(  mining ,
                manufacturing ,
                food ,
                beverages_tobacco ,
                textiles_apparel_leather ,
                wood_prod ,
                paper ,
                printing ,
                petro_coke ,
                chemicals ,
                basic_chems ,
                pharma ,
                rubber_plastic ,
                non_metal_minerals ,
                prim_fab_metals ,
                machinery ,
                manuf_computer ,
                eletric_equip ,
                trans_equip ,
                motor_vehic ,
                other_trans_equip ,
                furniture ,
                wholesale ,
                retail ,
                general_merch ,
                food_bev_store ,
                nonstore_retail ,
                information ,
                publishing ,
                news_book_pub ,
                software_pub ,
                motion_pic ,
                telecoms ,
                broadcasting ,
                data_process ,
                other_info_serv ,
                fin_isr ,
                finance ,
                insurance ,
                real_estate_rental ,
                real_estate ,
                rental_lease ,
                professional ,
                ag_engineering ,
                comp_sys_design ,
                legal_account ,
                r_d ,
                advertising ,
                other_prof_serv ,
                construction ,
                transport_warehouse ,
                air_trans ,
                rail_truck_trans ,
                water_trans ,
                support_trans ,
                admin_support_waste ,
                admin_support ,
                waste_man
  )
  
  
  row.names(result_1) <- c("mining_inward" ,
                     "manufacturing_inward" ,
                     "food_inward",
                     "beverages_tobacco" ,
                     "textiles_apparel_leather" ,
                     "wood_prod" ,
                     "paper" ,
                     "printing" ,
                     "petro_coke" ,
                     "chemicals" ,
                     "basic_chems" ,
                     "pharma" ,
                     "rubber_plastic" ,
                     "non_metal_minerals" ,
                     "prim_fab_metals" ,
                     "machinery" ,
                     "manuf_computer" ,
                     "eletric_equip" ,
                     "trans_equip" ,
                     "motor_vehic" ,
                     "other_trans_equip" ,
                     "furniture" ,
                     "wholesale" ,
                     "retail" ,
                     "general_merch" ,
                     "food_bev_store" ,
                     "nonstore_retail",
                     "information" ,
                     "publishing" ,
                     "news_book_pub" ,
                     "software_pub" ,
                     "motion_pic" ,
                     "telecoms" ,
                     "broadcasting" ,
                     "data_process" ,
                     "other_info_serv" ,
                     "fin_isr" ,
                     "finance" ,
                     "insurance" ,
                     "real_estate_rental" ,
                     "real_estate" ,
                     "rental_lease" ,
                     "professional" ,
                     "ag_engineering" ,
                     "comp_sys_design" ,
                     "legal_account" ,
                     "r_d" ,
                     "advertising" ,
                     "other_prof_serv" ,
                     "construction" ,
                     "transport_warehouse" ,
                     "air_trans" ,
                     "rail_truck_trans" ,
                     "water_trans" ,
                     "support_trans" ,
                     "admin_support_waste" ,
                     "admin_support" ,
                     "waste_man" )
  country_pair2 <- rep(country_pair, times = 58)
  as.data.frame(country_pair2)
  result_2 <- cbind(result_1, country_pair2)
  final_result <- rbind.data.frame(final_result, result_2)
  
}


  
  #inward_4 <- cbind(inward_2[[i]],inward_3)
  #final_inward <- rbind(inward_3,inward_4)




#reshape long
ind_names <- row.names(final_result)
final_result$industry <- ind_names
setDT(final_result)
inward_eurostat_all<- melt(final_result, id.vars=c("industry", "country_pair2"), measure.vars = c("X2017", "X2016", "X2015", "X2014", "X2013", "X2012", "X2010", "X2009", "X2008"))

colnames(inward_eurostat_all)[3] <- "year"
colnames(inward_eurostat_all)[4] <- "inward_revenue"

write.csv(inward_eurostat_all, file = "inward_eurostat_all.csv", row.names = TRUE)
# now remove X from year column and use text to columns to break out country pair into iso codes
