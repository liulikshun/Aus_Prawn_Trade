install.packages("readxl")

install.packages("dplyr")

install.packages("tidyverse")

library(readxl)

library(tidyverse)

library(dplyr)

Prawns_Shrimp_AUS <- read_csv("model0_2000_2018_AUS.csv")

Prawn.List <- paste(c('prawn', 'shrimp'), collapse = '|')

Data <- Prawns_Shrimp_AUS %>% filter(str_detect(common_name, Prawn.List)) %>% view()

Data$cn = Data$common_name

sum(Data$quantity)

Data %>% filter(exporter == "AUS") %>% group_by(prod_method) %>% summarise(sum(quantity))

#Total list of prawn species

Prawn_Species <- Data %>% select(species, common_name) %>% unique() 

write_csv(Prawn_Species, file = "Prawn_Species_Gephart.csv")

#List of countries

Data %>% select(importer) %>% unique() %>% view()

#Calculations for sankey

Data[grepl("nei", Data$common_name, ignore.case=TRUE),] %>% filter(importer == "AUS") %>% group_by(prod_method) %>% summarize(quantity = sum(quantity))

#121737 are all the nei values for prawns imported into AUS, Aquaculture = 25909, Capture = 95828

#How many prawns does Australia trade from 2000-2018

Data %>% filter(importer == "AUS") %>% group_by(prod_method) %>% summarize(quantity = sum(quantity)) %>% view()

#To determine Two-way Trade of Prawns
Data_Two_Way_trade <- semi_join(x = Data, y = Data, by = c("importer" = "exporter", "exporter" = "importer", "common_name" = "cn", "cn" = "common_name", "year" = "year")) %>% view()

#List of Species

Data_Two_Way_trade %>% group_by(importer,exporter,year,cn,species,prod_method) %>% summarize(quantity = sum(quantity)) %>% ungroup() %>% view()

Two_way_trade_summarized <- Data_Two_Way_trade %>% group_by(importer,exporter,year,cn,species,prod_method) %>% summarize(quantity = sum(quantity)) %>% ungroup() %>% view()

write.csv(x = Two_way_trade_summarized, file = "Gephart_Two_way_trade_summarized.csv", row.names = FALSE)

#Species list

Two_way_trade_summarized %>% filter(importer == "AUS") %>% group_by(cn,prod_method) %>% summarize(quantity = sum(quantity)) %>% view()

#countries List
Two_way_trade_summarized %>% group_by(importer,exporter) %>% unique() %>% summarize(quantity = sum(quantity)) %>% view()

Redundant_Trade_Quantity <- Two_way_trade_summarized %>% group_by(across(!quantity)) %>%
  summarise(quantity = sum(quantity)) %>% 
  ungroup %>% view()

Redundant_AUS <- Redundant_Trade_Quantity %>% left_join(., ., by = c("importer" = "exporter",
                                                    "exporter" = "importer","prod_method",
                                                    "year",
                                                    "cn")) %>% # ... to calculate net quantity
  mutate(redundant_qty = pmin(quantity.x ,quantity.y)*2) %>%
  select(!c(quantity.x, quantity.y)) %>% 
  filter(importer == "AUS") %>% mutate(abs(redundant_qty)) %>% view()

Redundant_AUS <-select(Redundant_AUS,-c(8))

#Remove all rows that have zeros and NA
Redundant_AUS[Redundant_AUS==0] <-  NA
Redundant_AUS <- Redundant_AUS[complete.cases(Redundant_AUS),]

Redundant_AUS <- Redundant_AUS %>% group_by(`abs(redundant_qty)`) %>% slice() %>% view()

Redundant_AUS$`abs(redundant_qty)` <- 2*(Redundant_AUS$`abs(redundant_qty)`)

Redundant_AUS %>% group_by(year,importer,exporter) %>% summarise(quantity = sum(`abs(redundant_qty)`)) %>% view()

#Based on species

Redundant_AUS %>% filter(importer =="AUS") %>% group_by(cn,prod_method) %>% summarise(quantity = sum(`abs(redundant_qty)`)) %>% view()

#Calculating Trade Flows
#How much does Australia Export and Import based on prod_method

Trade_Flow_Prod_Method_Import <- Two_way_trade_summarized %>% group_by(year,prod_method,importer,exporter) %>% filter(importer == "AUS") %>% summarise(quantity = sum(quantity)) %>% view()

Trade_Flow_Prod_Method_Export <- Two_way_trade_summarized %>% group_by(year,prod_method,importer,exporter) %>% filter(exporter == "AUS") %>% summarise(quantity = sum(quantity)) %>% view()

Trade_Flow_Prod_Method_Import %>% group_by(prod_method) %>% summarise(quantity = sum(quantity)) %>% view()

Trade_Flow_Prod_Method_Export %>% group_by(year,prod_method) %>% summarise(quantity = sum(quantity)) %>% view()


Two_way_trade_summarized %>% filter(exporter == "AUS") %>% group_by(cn,prod_method) %>% summarize(quantity = sum(quantity)) %>% view()


Two_way_trade_summarized %>% select(importer) %>% unique() %>% view()



Two_way_trade_summarized %>% summarize(quantity = sum(quantity))

#Redundant trade of prawns per country/ trading partner

Redundant_AUS %>% select(importer,exporter) %>% view()

Redundant_AUS %>% group_by(importer,exporter) %>% summarise(quantity = sum(`abs(redundant_qty)`)) %>% view()

#Carbon Emissions Calculations

Redundant_AUS %>% group_by(cn,prod_method) %>% summarise(quantity = sum(`abs(redundant_qty)`)) %>% view()

Carbon_Emissions_Redundant_Trade_Annual <- Redundant_AUS %>% group_by(year,importer,exporter,cn,prod_method) %>% summarise(quantity = sum(`abs(redundant_qty)`))

write.csv(Carbon_Emissions_Redundant_Trade_Annual, "Carbon_Emissions_Redundant_Trade.csv")
  
#Flow Map Calculations

Redundant_AUS_2 <- Redundant_Trade_Quantity %>% left_join(., ., by = c("importer" = "exporter",
                                                                       "exporter" = "importer","prod_method",
                                                                       "year",
                                                                       "cn")) %>% view()

Redundant_AUS_2[Redundant_AUS_2==0] <-  NA
Redundant_AUS_2 <- Redundant_AUS_2[complete.cases(Redundant_AUS_2),]

Redundant_Trade_Flow <- Redundant_AUS_2 %>% mutate(Flow = ifelse(quantity.y > quantity.x, "Export Heavy", "Import Heavy")) %>% mutate(redundant_qty = pmin(quantity.x ,quantity.y)*2) %>%
  select(!c(quantity.x, quantity.y)) %>% 
  filter(importer == "AUS") %>% mutate(abs(redundant_qty)) %>% group_by(`abs(redundant_qty)`) %>% slice()

Redundant_Trade_Flow$`abs(redundant_qty)` <- 2*(Redundant_Trade_Flow$`abs(redundant_qty)`)

Redundant_Trade_Flow %>% group_by(importer,exporter,Flow) %>% summarise(quantity = sum(`abs(redundant_qty)`)) %>% view()
