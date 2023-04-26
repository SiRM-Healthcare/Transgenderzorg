#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Verhoudingen zorgvragen uit enquête Kwartiermaker verkrijgen
#Versie:  1
#---------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Library's en mappen definieren -----------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(haven)
library(readxl)
library(writexl)
library(data.table)
library(dplyr)
library(stringr)

SiRMer = #naam
inputmap = #naam_inputmap
brongegevensmap = #naam_brongegevensmap

input = paste0("C:/Users/",SiRMer,inputmap)
bron  = paste0("C:/Users/",SiRMer,brongegevensmap)

echte_enq                = data.table(read_sav(paste0(bron, "Data kwartiermaker/220620 SiRM databestand Zelfmedicatie & zorg in het buitenland - Opgeschoond voor exclusie.sav")))

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Juiste data uit de enquête selecteren ----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Alleen de respondenten selecteren die vragen over zorgvraag hebben ingevuld
echte_enq$Fase                = rowSums(echte_enq[,c(37:57)], na.rm=T)
echte_enq                     = echte_enq[!echte_enq$Fase == 0,]

#We berekenen het percentage respondenten wat bij geboorte mannelijk geslacht toegekend hebben gekregen (AMAB)(dit hebben we later in het model nodig)
gebgesl                       = subset(echte_enq, select=c("ResponseId", "Geboorte_geslacht"))
gebgesl                       = unique(gebgesl)
gebgesl                       = gebgesl[!is.na(gebgesl$Geboorte_geslacht),]
gebgesl$aantal                = 1
sum(gebgesl$aantal[gebgesl$Geboorte_geslacht == 1])/sum(gebgesl$aantal)

#We maken twee leeftijdscategorieën: onder en boven 18, omdat dit ook de categorieën zijn die in de wachtlijst/volume data worden gebruikt
echte_enq$leeftijdscategorie  = ifelse(echte_enq$Leeftijd>=18, ">=18", "<18")

#We passen voor een eenvoudigere interpretatie de codering van geboortegeslacht aan naar AMAB/AFAB ipv 1/2
echte_enq$Geboorte_geslacht   = ifelse(as.numeric(echte_enq$Geboorte_geslacht) == 1, "AMAB", "AFAB")

#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------ Stap 1: Verhoudingen zorgvraag -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#We selecteren de kolommen die over zorgvragen gaan
gegevens                      = subset(echte_enq, select=c("ResponseId","Geboorte_geslacht", "leeftijdscategorie"))
zorgvragen                    = echte_enq[,c(37:57)]

#We vervangen alle lege cellen door 6, 'weet ik niet'
zorgvragen[is.na(zorgvragen)] = 6
zorgvragen                    = cbind(gegevens, zorgvragen)

#We selecteren eerst alleen de personen die op de wachtlijst staan voor een indicatiestellingstraject
indicatie_wachtlijst          = zorgvragen[zorgvragen$Fase_psychdiag == 3,]
indicatie_wachtlijst$Fase_psychdiag = NULL
indicatie_wachtlijst                = melt(indicatie_wachtlijst, id.vars=c("ResponseId", "Geboorte_geslacht", "leeftijdscategorie"),
                                           variable.name = "zorgvraag", value.name="antwoord")
#Van de mensen die op de wachtlijst staan willen we weten welke zorgvragen ze hebben
#We selecteren op antwoord '2 en 3', dus 'gewenst' en 'wachtlijst'
indicatie_wachtlijst$aantal = 1
#We berekenen het totaal aantal personen per categorie om vervolgens aandelen te kunnen berekenen
aantallen                     = subset(indicatie_wachtlijst, select=c("ResponseId", "Geboorte_geslacht", "leeftijdscategorie", "aantal"))
aantallen                     = unique(aantallen)
aantallen                     = aggregate(aantallen$aantal, by=list(aantallen$Geboorte_geslacht, aantallen$leeftijdscategorie),
                                        FUN=sum, na.rm=T)
names(aantallen)              = c("geboorte_geslacht", "leeftijdscategorie", "aantal_totaal")
indicatie_wachtlijst          = indicatie_wachtlijst[indicatie_wachtlijst$antwoord %in% c(2,3),]
indicatie_wachtlijst$antwoord = NULL

#We berekenen per zorgvraag het aandeel van de mensen die deze zorgvraag heeft
indicatie_wachtlijst          = aggregate(indicatie_wachtlijst$aantal, by=list(indicatie_wachtlijst$Geboorte_geslacht, indicatie_wachtlijst$leeftijdscategorie,
                                                                               indicatie_wachtlijst$zorgvraag), FUN=sum, na.rm=T)
names(indicatie_wachtlijst)   = c("geboorte_geslacht", "leeftijdscategorie", "zorgvraag", "aantal")
indicatie_wachtlijst          = merge(indicatie_wachtlijst, aantallen, by=c("geboorte_geslacht", "leeftijdscategorie"))
indicatie_wachtlijst$aandeel  = indicatie_wachtlijst$aantal/indicatie_wachtlijst$aantal_totaal
indicatie_wachtlijst$aantal = NULL
indicatie_wachtlijst$aantal_totaal = NULL

#Tabel opslaan
write_xlsx(indicatie_wachtlijst, paste0(input, "verhoudingen_enquete_wachtlijst.xlsx"))
