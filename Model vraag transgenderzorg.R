#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Model voor het schatten van de omvang vraag transgenderzorg in opdracht van ZonMw
#Versie:  1
#Structuur: 
#         0.  Module 0: Inlezen data
#         A.  Percentage dubbele aanmeldingen voor indicatiestelling
#         B.  Verhoudingen zorgvraag uit enquête 
#         1.  Module 1: Aantal trans personen
#         2.  Module 2: Vraag naar indicatiestelling
#         3.  Module 3: Vraag naar endocrinologische en chirurgische zorg
#-----------------------------------------------------------------------

#Module A en B zijn als losse delen gecodeerd, omdat de berekeningen in deze modules 
#onder andere gebaseerd zijn op de data uit een enquête, die niet gedeeld kan worden.
#De resultaten van deze twee modules worden apart opgeslagen en ingeladen in Module 0. 

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Library's en mappen definieren -----------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Alle variabelen uit R's geheugen verwijderen
rm(list=ls())
options(scipen = 999)

#Benodigde library's laden
library(haven)
library(readxl)
library(writexl)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

#Alle mappen definiëren
SiRMer            = #naam
inputmap          = #naam_inputmap
resultatenmap     = #naam_resultatenmap
sogizmap          = #naam_sogizmap
wdir              = paste0('C:/Users/',SiRMer,inputmap)
resultaten        = paste0('C:/Users/',SiRMer,resultatenmap)
sogiz             = paste0('C:/Users/',SiRMer,sogizmap)

code              = paste0(wdir,'/Kwantitatief/Code/')
bron              = paste0(wdir,'/Inputs/bron/')
data_indelingen   = paste0(wdir,'/Inputs/indelingen/')
shapefiles        = paste0(wdir,'/Inputs/shapefiles/')
input             = paste0(wdir,'/Kwantitatief/Inputs/')
werkbestanden     = paste0(wdir,'/werkbestanden/')
output            = paste0(wdir,'/Kwantitatief/Outputs/')

#Om het overzicht te bewaren gebruiken we de volgende afkortingen voor de resultaten:
#R1: Huidig aantal trans personen
#R2: Toekomstig aantal trans personen
#R3: Huidige vraag naar indicatiestelling
#R4: Toekomstige vraag naar indicatiestelling
#R5: Huidige andere zorgvragen
#R6: Toekomstige andere zorgvragen

#We zetten 'MC' initieel op 0 zodat bij de eerste run alle delen van de code worden meegenomen
#We zetten na de eerste run 'MC' gelijk aan 1. Dit zorgt ervoor dat code die niet van parameters afhangt bij de Monte Carlo-simulatie worden overgeslagen
MC = 0

#------------------------------------------------------------------------------------------------------------------------
#---------------------------------- Module 0: datasets inlezen -------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

source(paste0(code,"Module 0 - inlezen datasets.R"))

#We pakken hier voor alle parameters een willekeurige waarde uit de normaalverdeling. Zie Monte Carlo-deel voor uitleg
parameters$waarde_mc = rnorm(length(parameters$waarde), parameters$waarde, parameters$sigma)

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Module 1: Huidig aantal transpersonen ----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

source(paste0(code,"Module 1 - aantal trans personen.R"))

R1     = transpersonen
R2     = toekomst_transpersonen

#------------------------------------------------------------------------------------------------------------------------
#---------------------------- Module 2: Vraag naar indicatiestelling ----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

source(paste0(code, "Module 2 - vraag naar indicatiestellingstrajecten.R"))

R3    = aantal_nieuw_totaal_resultaten
R4    = toekomstscenarios_opslaan

#------------------------------------------------------------------------------------------------------------------------
#--------------------Module 3: Vraag naar endocrinologische en chirurgische transgenderzorg-----------------------------------------
#------------------------------------------------------------------------------------------------------------------------

source(paste0(code, "Module 3 - vraag naar endocrinologische en chirurgische transgenderzorg.R"))

R5    = zorgvraag
R6    = zorgvraag_2027

#------------------------------------------------------------------------------------------------------------------------
#---------------------------------------Monte Carlo-simulatie------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

MC = 0

#Om te kijken hoe gevoelig de resultaten zijn voor veranderingen in de parameters, voeren we een Monte Carlo-simulatie uit. 
#Daarbij wordt het model x aantal keer gerund, elke keer met andere waarden voor deze parameters.
#Na elke iteratie slaan we de resultaten op en aan het eind berekenen we daarvan het gemiddelde resultaat en de standaarddeviatie.

#Omdat we de parameters random kiezen moet er een set.seed worden gekozen zodat we als we de for loop nog een keer runnen we hetzelfde resultaat krijgen.
#Meest recent gebruikte waarde is 123
set.seed(#waarde)

#Hier bepalen we hoevaak we het model draaien:
#meest recent gebruikte waarde is 100.000
runs = #aantal runs

start_time <- Sys.time()
#Hier start de for loop die alle modules runt en vervolgens de resultaten opslaat
for(i in 1:runs){
  #Waarde bepalen van variabelen die we gaan variëren. 
  #Parameter uit normaalverdeling
  parameters$waarde_mc = rnorm(length(parameters$waarde), parameters$waarde, parameters$sigma)
  #Alle modules runnen
  source(paste0(code, "Module 1 - aantal trans personen.R"))
  source(paste0(code, "Module 2 - vraag naar indicatiestellingstrajecten.R"))
  source(paste0(code, "Module 3 - vraag naar endocrinologische en chirurgische transgenderzorg.R"))
  #Resultaten worden als nieuwe rijen toegevoegd aan onderstaande dataframes
  R1       = rbind(R1, transpersonen)
  R2       = rbind(R2, toekomst_transpersonen)
  R3       = rbind(R3, aantal_nieuw_totaal_resultaten)
  R4       = rbind(R4, toekomstscenarios_opslaan)
  R5       = rbind(R5, zorgvraag)
  R6       = rbind(R6, zorgvraag_2027)
}
end_time <- Sys.time()
end_time - start_time

#Vervolgens berekenen we de gemiddelde resultaten van alle iteraties
gemiddeld_R1        = aggregate(R1$transpersonen,by=list(R1$leeftijdscategorie, R1$geboorte_geslacht, R1$jaar),FUN=mean, na.rm=T)
names(gemiddeld_R1) = c("leeftijdscategorie", "geboorte_geslacht", "jaar", "aantal")
gemiddeld_R2        = aggregate(R2$aantal, by=list(R2$scenario, R2$jaar), FUN=mean, na.rm=T)
names(gemiddeld_R2) = c("scenario", "jaar", "aantal")
gemiddeld_R3        = aggregate(R3$aantal_nieuw, by=list(R3$leeftijdscategorie, R3$geboorte_geslacht),FUN=mean, na.rm=T)
names(gemiddeld_R3) = c("leeftijdscategorie", "geboorte_geslacht", "aantal_nieuw")
gemiddeld_R4        = aggregate(R4$steady_states, by=list(R4$scenario), FUN=mean, na.rm=T)
names(gemiddeld_R4) = c("scenario", "steady_states")
gemiddeld_R5        = aggregate(R5$aantal, by=list(R5$leeftijdscategorie,R5$geboorte_geslacht, R5$zorgvraag, R5$maand, R5$jaar), FUN=mean, na.rm=T)
names(gemiddeld_R5) = c("leeftijdscategorie", "geboorte_geslacht", "zorgvraag", "maand", "jaar", "aantal")
gemiddeld_R6        = aggregate(R6$aantal_2027, by=list(R6$leeftijdscategorie, R6$geboorte_geslacht, R6$zorgvraag, R6$scenario), FUN=mean, na.rm=T)
names(gemiddeld_R6) = c("leeftijdscategorie", "geboorte_geslacht", "zorgvraag", "scenario", "aantal")

#Vervolgens berekenen we de standaarddeviatie van alle iteraties
sd_R1               = aggregate(R1$transpersonen, by=list(R1$leeftijdscategorie, R1$geboorte_geslacht, R1$jaar), FUN=sd, na.rm=T)
names(sd_R1)        = c("leeftijdscategorie", "geboorte_geslacht", "jaar", "sd")
sd_R2               = aggregate(R2$aantal, by=list(R2$scenario, R2$jaar), FUN=sd, na.rm=T)
names(sd_R2)        = c("scenario", "jaar", "sd")
sd_R3               = aggregate(R3$aantal_nieuw, by=list(R3$leeftijdscategorie, R3$geboorte_geslacht), FUN=sd, na.rm=T)
names(sd_R3)        = c("leeftijdscategorie", "geboorte_geslacht", "sd")
sd_R4               = aggregate(R4$steady_states, by=list(R4$scenario), FUN=sd, na.rm=T)
names(sd_R4)        = c("scenario", "sd")
sd_R5               = aggregate(R5$aantal, by=list(R5$leeftijdscategorie,R5$geboorte_geslacht, R5$zorgvraag, R5$maand, R5$jaar), FUN=sd, na.rm=T)
names(sd_R5)        = c("leeftijdscategorie", "geboorte_geslacht", "zorgvraag", "maand", "jaar", "sd")
sd_R6               = aggregate(R6$aantal_2027, by=list(R6$leeftijdscategorie, R6$geboorte_geslacht, R6$zorgvraag, R6$scenario), FUN=sd, na.rm=T)
names(sd_R6)        = c("leeftijdscategorie", "geboorte_geslacht", "zorgvraag", "scenario","sd")

#Tabellen opslaan
setwd(paste0(output, "/Resultaten 8 december"))
write_xlsx(gemiddeld_R1, "Trans personen.xlsx")
write_xlsx(gemiddeld_R2, "Toekomst trans personen.xlsx")
write_xlsx(gemiddeld_R3, "Indicatie.xlsx")
write_xlsx(gemiddeld_R4, "Indicatie scenarios.xlsx")
write_xlsx(gemiddeld_R5, "Zorgvraag.xlsx")
write_xlsx(gemiddeld_R6, "Zorgvraag 2027.xlsx")

write_xlsx(sd_R1, "SD Trans personen.xlsx")
write_xlsx(sd_R2, "SD Toekomst trans personen.xlsx")
write_xlsx(sd_R3, "SD Indicatie.xlsx")
write_xlsx(sd_R4, "SD Indicatie scenarios.xlsx")
write_xlsx(sd_R5, "SD Zorgvraag.xlsx")
write_xlsx(sd_R6, "SD Zorgvraag 2027.xlsx")

#------------------------------------------------------------------------------------------------------------------------
#---------------------------------------Bandbreedte ---------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

MC = 1

#------------------------------------------------------------------------------------------------------------------------
#---------------------------------- Module 0: datasets inlezen -------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#We zetten eerst alle parameters op de minimumwaarde
parameters$waarde_mc = parameters$min_waarde

source(paste0(code, "Module 1 - aantal trans personen.R"))

R1_min     = transpersonen
R2_min     = toekomst_transpersonen

source(paste0(code, "Module 2 - vraag naar indicatiestellingstrajecten.R"))

R3_min    = aantal_nieuw_totaal_resultaten
R4_min    = toekomstscenarios_opslaan

source(paste0(code, "Module 3 - vraag naar endocrinologische en chirurgische transgenderzorg.R"))

R5_min    = zorgvraag
R6_min    = zorgvraag_2027

#En vervolgens alle parameters op de maximumwaarde
parameters$waarde_mc = parameters$max_waarde

source(paste0(code, "Module 1 - aantal trans personen.R"))

R1_max     = transpersonen
R2_max     = toekomst_transpersonen

source(paste0(code, "Module 2 - vraag naar indicatiestellingstrajecten.R"))

R3_max    = aantal_nieuw_totaal_resultaten
R4_max    = toekomstscenarios_opslaan

source(paste0(code, "Module 3 - vraag naar endocrinologische en chirurgische transgenderzorg.R"))

R5_max    = zorgvraag
R6_max    = zorgvraag_2027

R1_bandbreedte = merge(R1_min, R1_max, by=c("leeftijdscategorie","geboorte_geslacht","jaar"))
R2_bandbreedte = merge(R2_min, R2_max, by=c( "jaar","scenario"))
R3_bandbreedte = merge(R3_min, R3_max, by=c("leeftijdscategorie", "geboorte_geslacht"))
R4_bandbreedte = merge(R4_min, R4_max, by=c("scenario"))
R5_bandbreedte = merge(R5_min, R5_max, by=c("geboorte_geslacht",  "leeftijdscategorie", "zorgvraag","maand" , "jaar"))
R6_bandbreedte = merge(R6_min, R6_max, by=c("geboorte_geslacht",  "leeftijdscategorie", "zorgvraag", "scenario"))
