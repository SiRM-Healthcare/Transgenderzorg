#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Bijlage A - percentage dubbele aanmeldingen
#Versie:  1
#Structuur: 
#              Stap 1: Vraag 13 uit de enquête gebruiken
#              Stap 2: Vraag 15 uit de enquête gebruiken
#------------------------------------------------------------------------------------------------------------------------

#In de enquête van de Kwartiermaker zitten twee vragen die informatie bevatten over het aandeel dubbele aanmeldingen:
#Vraag 13: bij hoeveel aanbieders sta je op de wachtlijst?
#Vraag 15: bij welke aanbieders sta je op de wachtlijst?
#Omdat deze vragen niet consistent zijn ingevuld, komen we in de berekening op verschillende percentages uit.
#Om die reden nemen we het gemiddelde van de twee percentages, en gebruiken we de percentages als minimum en maximum voor de bandbreedte van de resultaten.

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
inputmap        = #naam_inputmap
brongegevensmaps  = #naam_resultatenmap

input = paste0("C:/Users/",SiRMer,inputmap)
bron  = paste0("C:/Users/",SiRMer,brongegevensmaps)

#Data:
echte_enq                = data.table(read_sav(paste0(bron, "Data kwartiermaker/220620 SiRM databestand Zelfmedicatie & zorg in het buitenland - Opgeschoond voor exclusie.sav")))
wachtlijst_echt          = data.table(read_excel(paste0(input, "dataset_kwartiermaker_wachtlijst.xlsx")))
koppel_instelling        = data.table(read_excel(paste0(input, "koppeltabel namen instellingen.xlsx")))

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Relevante data uit de enquête selecteren -------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Alleen de respondenten selecteren die vragen over zorgvraag hebben ingevuld
echte_enq$Fase                = rowSums(echte_enq[,c(37:57)], na.rm=T)
echte_enq                     = echte_enq[!echte_enq$Fase == 0,]
#Onderstaande dataset bevat de kolommen uit de enquête die gaan over wachtlijsten:
#Per instelling is aangegeven of een respondent op de wachtlijst voor die instelling staat, dus iedere instelling heeft zijn eigen kolom. Door te selecteren op alle variabelen met "psychdiag" hebben we in ieder geval de instelling te pakken die de indicatiestelling doen.
instelling_wachten_indicatie  = echte_enq %>% select(contains("psychdiag")| contains("ResponseId")) 

#We gebruiken ook de daadwerkelijke wachtlijst van de instellingen
wachtlijst                    = wachtlijst_echt
wachtlijst_echt$schatting     = NULL

#Selectie van personen die hebben aangegeven op de wachtlijst te staan voor de indicatiestelling
instelling_wachten_indicatie  = instelling_wachten_indicatie[Fase_psychdiag ==3,]

#Door te filteren op alle kolommen waar het woord "psychdiag" in voorkomt, krijgen we ook andere kolommen dan de instellingen, deze andere kolommen behouden we hier als kolommen, de instellingen zetten we naar rijen.
instelling_wachten_indicatie  = data.table(melt(instelling_wachten_indicatie, id=c("ResponseId","Fase_psychdiag", "Toelichting_behoefte_psychdiag",
                                                                                   "Wachtlijst_buitenlandopen_psychdiag","Wachtlijst_buitenland_psychdiag",
                                                                                   "Wachtlijst_andersopen_psychdiag", "Wachttijd_psychdiag",
                                                                                   "Wachttijd_ervaring_psychdiag", "Wachttijd_last_psychdiag","Wachttijd_behoefte_psychdiag",
                                                                                   "Wachtlijst_aanmelding_psychdiag",
                                                                                   "Zorgbuitenland_psychdiag_tekst",
                                                                                   "Zorgbuitenland_psychdiag_kwaliteit",
                                                                                   "Zorgbuitenland_psychdiag_wachttijd",
                                                                                   "Zorgbuitenland_psychdiag_kosten",
                                                                                   "Zorgbuitenland_psychdiag_beschikbaarheid",
                                                                                   "Zorgbuitenland_psychdiag_verwezen",
                                                                                   "Zorgbuitenland_psychdiag_anders",
                                                                                   "Locatie_psychdiag_overig",
                                                                                   "Locatie_psychdiag_buitenland"), variable.name = "instelling", value.name = "aantal")) 

#Als persoon x niet op de wachtlijst voor instelling y staat, is de cel 'aantal' gelijk aan NA. Deze verwijderen we.
#Per persoon zijn meerdere rijen mogelijk, namelijk een rij per instelling waar je op wachtlijst staat
instelling_wachten_indicatie = instelling_wachten_indicatie[is.na(aantal)==F,]

#-------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------ Stap 1: Vraag 13 uit de enquête gebruiken -------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#We tellen per instelling hoeveel personen er op alleen bij die instelling op de wachtlijst staan, hoeveel op de  wachtlijst van die instelling + nog een andere, hoeveel op de wachtlijst van die instelling + nog 2 andere wachtlijsten etc.-------
per_instelling_wachten            = instelling_wachten_indicatie[,.(aantal = sum(aantal)),.(instelling,Wachtlijst_aanmelding_psychdiag)]  # Tel het aantal wachtenden per instelling en per op hoeveel wachtlijsten
per_instelling_wachten            = per_instelling_wachten[is.na(Wachtlijst_aanmelding_psychdiag)==F]                                     # Verwijder alle rijen (wachtenden) die niet op de wachtlijst voor een indicatie staan
per_instelling_wachten            = per_instelling_wachten[,`:=`(aandeel = aantal/sum(aantal)),by=.(instelling)]                          # Aandeel van wachtenden dat op 1, 2, 3, 4 etc wachtlijsten staan per instelling  
per_instelling_wachten$instelling = as.character(per_instelling_wachten$instelling)                                               # instelling is een factor, hier een character variabele van maken
per_instelling_wachten$instelling = substring(per_instelling_wachten$instelling,12,nchar(per_instelling_wachten$instelling)-10)   # Alleen de naam extraheren van de instelling

#Selecteer wachtenden in 2022 per insteling vanuit de data van de instellingen
wachtenden_2022_indicatie         = wachtlijst_echt[jaar==2022 & maand == "januari",.(aantal = sum(aantal, na.rm=T)),.(koppel)]
#Selecteer instelling met wachtenden (sommige instellingen hebben geen wachtenden):
wachtenden_2022_indicatie         = wachtenden_2022_indicatie[aantal>0]

#We kijken alleen naar de zorgaanbieders die voorkomen in de wachtlijstdata van de instellingen
#We gebruiken een koppeltabel om de namen van de instellingen van de datasets te kunnen koppelen
#Voor het gemak werken we vanaf hier met een vierletterige code voor elke instelling ('koppel')
per_instelling_wachten            = merge(per_instelling_wachten, koppel_instelling,by="instelling")
per_instelling_wachten$instelling = NULL

per_instelling_wachten            = merge(per_instelling_wachten, wachtenden_2022_indicatie, by="koppel", all.x=T)
per_instelling_wachten[is.na(per_instelling_wachten$aantal),] = 0

#De aantallen uit de data van de instellingen vermenigvuldigen met de aandelen uit de enquete data (per op hoeveel wachtlijsten)
per_instelling_wachten$aantal_echt = per_instelling_wachten$aantal*per_instelling_wachten$aandeel

#Aantallen sommeren over de instellingen en dan delen door op hoeveel wachtlijsten de personenen staan om te schatten hoeveel unieke wachtenden er zijn
per_instelling_wachten_tot       = per_instelling_wachten[,.(aantal_echt = sum(aantal_echt)), by=.(Wachtlijst_aanmelding_psychdiag)]
per_instelling_wachten_tot$uniek = per_instelling_wachten_tot$aantal_echt/per_instelling_wachten_tot$Wachtlijst_aanmelding_psychdiag
per_instelling_wachten_tot       = per_instelling_wachten_tot[!is.nan(per_instelling_wachten_tot$uniek),]
schat_wachten_indicatie_1        = sum(per_instelling_wachten_tot$uniek)/sum(per_instelling_wachten_tot$aantal_echt)

#-------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------ Stap 2: Vraag 15 uit de enquête gebruiken -------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#We bepalen obv de enqueteresultaten voor alle mogelijke combinatie van inschrijvingen op wachtlijsten van instellingen het aantal wachtenden
#(dus hoeveel personen staan er op wachtlijst van instelling A, hoeveel op wachtlijst van instelling A + instelling B, etc.).
#Vervolgens bekijken we per instelling (beginnend bij de grootste) hoe het "echte" aantal (vanuit ontvangen data van de instellingen) verdeeld is 
#over de combinaties van mogelijkheden obv verhoudingen uit de enquete (dus aantallen uit stap hierboven)
#Steeds als we de volgende instelling gaan bekijken, houden we rekening met het deel dat al wordt meegenomen door de overlap met de instelling die al is meegneomen in de vorige stappen.
#In principe beginnen we bij de grootste instelling, maar omdat er dan aan het eind voor een aantal instellingen de overlap met die grote instelling groter is dan het totaal van die instelling, beginnen we met de instelling waar dat voor geldt.

instelling_wachten_indicatie            = instelling_wachten_indicatie[,.(ResponseId, instelling, aantal)]
instelling_wachten_indicatie$aantal     = as.numeric(as.character(instelling_wachten_indicatie$aantal))
instelling_wachten_indicatie$instelling = as.character(instelling_wachten_indicatie$instelling)
instelling_wachten_indicatie$instelling = substring(instelling_wachten_indicatie$instelling,12,nchar(instelling_wachten_indicatie$instelling)-10)
instelling_wachten_indicatie    = instelling_wachten_indicatie[, `:=`(aantal_instelling = sum(aantal)),.(instelling)]

#we verwijderen instellingen met hele lage aantallen (<=2)
instelling_wachten_indicatie    = instelling_wachten_indicatie[aantal_instelling>2,]

#we selecteren instellingen uit de enquete die ook in de wachtlijstdata van de Kwartiermaker voorkomen:
#We selecteren alleen instellingen die een wachtlijst>0 hebben in 2022
instelling_wachten_indicatie        = merge(instelling_wachten_indicatie, koppel_instelling, by="instelling")
instelling_wachten_indicatie        = instelling_wachten_indicatie[instelling_wachten_indicatie$koppel %in% wachtenden_2022_indicatie$koppel,]
instelling_wachten_indicatie$aantal_instelling = NULL
instelling_wachten_indicatie$instelling        = NULL
instelling_wachten_indicatie        = dcast(instelling_wachten_indicatie, ResponseId ~ koppel, value.var = "aantal", fun.aggregate=length)
instelling_wachten_indicatie$aantal = 1

#we bepalen hoe vaak combinaties voorkomen (celwaarde 1 = patienten staan op wachtlijst van deze instelling)
instelling_wachten_indicatie        = instelling_wachten_indicatie[,.(aantal = sum(aantal)),by=.(AUMC, GHEA, JIJG, PSYT,
                                                                                                 RUMC, UMCG, VAAR, YOUZ, 
                                                                                                 ZUNL)]

#we beginnen bij PSYT, genderhealthcare, JIJG, De Vaart en YOUZ, want door de hoge aantallen van het AUMC, is de overlap met AUMC al groter dan het aantal dat er op wachtlijst staat voor deze instelling

#Selectie PSyTrans
setnames(wachtenden_2022_indicatie, "koppel", "instelling")
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$PSYT==1, instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$PSYT==1]),0)
instelling_wachten_indicatie$aantal_wachtenden_PSYT  = round(instelling_wachten_indicatie$aandeel*wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="PSYT"],0)

#GenderHealthCare
wachten_GHEA                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$GHEA==1])
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$GHEA ==1 & 
                                                              instelling_wachten_indicatie$PSYT!=1, 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$GHEA==1 & 
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1]),0)
instelling_wachten_indicatie$aantal_wachtenden_GHEA  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="GHEA"]-wachten_GHEA),0)

#BUROJIJ
wachten_JIJG                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$JIJG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$JIJG==1])
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$JIJG==1 & 
                                                             (instelling_wachten_indicatie$GHEA!=1 & 
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$JIJG==1 & 
                                                                                                                                         (instelling_wachten_indicatie$GHEA!=1 & 
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_JIJG  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="JIJG"]-wachten_JIJG),0)

#De Vaart
wachten_VAAR                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$VAAR==1]) + 
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$VAAR==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$VAAR==1])
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$VAAR==1 &
                                                             (instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$VAAR==1 &
                                                                                                                                         (instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                          instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_VAAR  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="VAAR"]-wachten_VAAR),0)

#Youz
wachten_YOUZ                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$YOUZ==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$YOUZ==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$YOUZ==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_VAAR[instelling_wachten_indicatie$VAAR==1 & instelling_wachten_indicatie$YOUZ==1])
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$YOUZ==1 & 
                                                             (instelling_wachten_indicatie$VAAR!=1 &
                                                              instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$YOUZ==1 & 
                                                                                                                                         (instelling_wachten_indicatie$VAAR!=1 &
                                                                                                                                          instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                          instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_YOUZ  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="YOUZ"]-wachten_YOUZ),0)

#AUMC
wachten_AUMC                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$AUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$AUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$AUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_VAAR[instelling_wachten_indicatie$VAAR==1 & instelling_wachten_indicatie$AUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_YOUZ[instelling_wachten_indicatie$YOUZ==1 & instelling_wachten_indicatie$AUMC==1]) 
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$AUMC==1 & 
                                                             (instelling_wachten_indicatie$YOUZ!=1 &
                                                              instelling_wachten_indicatie$VAAR!=1 &
                                                              instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$AUMC==1 & 
                                                                                                                                         (instelling_wachten_indicatie$YOUZ!=1 &
                                                                                                                                          instelling_wachten_indicatie$VAAR!=1 &
                                                                                                                                          instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                          instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_AUMC  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="AUMC"]-wachten_AUMC),0)

#RUMC
wachten_RUMC                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$RUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$RUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$RUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_VAAR[instelling_wachten_indicatie$VAAR==1 & instelling_wachten_indicatie$RUMC==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_YOUZ[instelling_wachten_indicatie$YOUZ==1 & instelling_wachten_indicatie$RUMC==1]) + 
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_AUMC[instelling_wachten_indicatie$AUMC==1 & instelling_wachten_indicatie$RUMC==1]) 
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$RUMC==1 & 
                                                             (instelling_wachten_indicatie$AUMC!=1 & 
                                                              instelling_wachten_indicatie$YOUZ!=1 &
                                                              instelling_wachten_indicatie$VAAR!=1 &
                                                              instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$RUMC==1 & 
                                                                                                                                         (instelling_wachten_indicatie$AUMC!=1 & 
                                                                                                                                          instelling_wachten_indicatie$YOUZ!=1 &
                                                                                                                                          instelling_wachten_indicatie$VAAR!=1 &
                                                                                                                                          instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                          instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_RUMC  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="RUMC"]-wachten_RUMC),0)

#UMCG
wachten_UMCG                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$UMCG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$UMCG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$UMCG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_VAAR[instelling_wachten_indicatie$VAAR==1 & instelling_wachten_indicatie$UMCG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_YOUZ[instelling_wachten_indicatie$YOUZ==1 & instelling_wachten_indicatie$UMCG==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_AUMC[instelling_wachten_indicatie$AUMC==1 & instelling_wachten_indicatie$UMCG==1]) + 
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_RUMC[instelling_wachten_indicatie$RUMC==1 & instelling_wachten_indicatie$UMCG==1]) 
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$UMCG==1 & 
                                                             (instelling_wachten_indicatie$RUMC!=1 &
                                                              instelling_wachten_indicatie$AUMC!=1 &
                                                              instelling_wachten_indicatie$YOUZ!=1 &
                                                              instelling_wachten_indicatie$VAAR!=1 &
                                                              instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                              instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$UMCG==1 & 
                                                                                                                                         (instelling_wachten_indicatie$RUMC!=1 & 
                                                                                                                                          instelling_wachten_indicatie$AUMC!=1 & 
                                                                                                                                          instelling_wachten_indicatie$YOUZ!=1 &
                                                                                                                                          instelling_wachten_indicatie$VAAR!=1 &
                                                                                                                                          instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                          instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                          instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_UMCG  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="UMCG"]-wachten_UMCG),0)

#GenderteamZuid
wachten_ZUNL                                         = sum(instelling_wachten_indicatie$aantal_wachtenden_PSYT[instelling_wachten_indicatie$PSYT==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_GHEA[instelling_wachten_indicatie$GHEA==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_JIJG[instelling_wachten_indicatie$JIJG==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_VAAR[instelling_wachten_indicatie$VAAR==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_YOUZ[instelling_wachten_indicatie$YOUZ==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_AUMC[instelling_wachten_indicatie$AUMC==1 & instelling_wachten_indicatie$ZUNL==1]) +
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_RUMC[instelling_wachten_indicatie$RUMC==1 & instelling_wachten_indicatie$ZUNL==1]) + 
                                                       sum(instelling_wachten_indicatie$aantal_wachtenden_UMCG[instelling_wachten_indicatie$UMCG==1 & instelling_wachten_indicatie$ZUNL==1]) 
instelling_wachten_indicatie$aandeel                 = ifelse(instelling_wachten_indicatie$ZUNL==1 & 
                                                             (instelling_wachten_indicatie$UMCG!=1 &
                                                              instelling_wachten_indicatie$RUMC!=1 &
                                                              instelling_wachten_indicatie$AUMC!=1 &
                                                              instelling_wachten_indicatie$YOUZ!=1 &
                                                              instelling_wachten_indicatie$VAAR!=1 &
                                                              instelling_wachten_indicatie$JIJG!=1 &
                                                              instelling_wachten_indicatie$GHEA!=1 &
                                                              instelling_wachten_indicatie$PSYT!=1), 
                                                             instelling_wachten_indicatie$aantal/sum(instelling_wachten_indicatie$aantal[instelling_wachten_indicatie$ZUNL==1 &
                                                                                                                                        (instelling_wachten_indicatie$UMCG!=1 & 
                                                                                                                                         instelling_wachten_indicatie$RUMC!=1 &
                                                                                                                                         instelling_wachten_indicatie$AUMC!=1 & 
                                                                                                                                         instelling_wachten_indicatie$YOUZ!=1 &
                                                                                                                                         instelling_wachten_indicatie$VAAR!=1 &
                                                                                                                                         instelling_wachten_indicatie$JIJG!=1 &
                                                                                                                                         instelling_wachten_indicatie$GHEA!=1 &
                                                                                                                                         instelling_wachten_indicatie$PSYT!=1)]),0)
instelling_wachten_indicatie$aantal_wachtenden_ZUNL  = round(instelling_wachten_indicatie$aandeel*(wachtenden_2022_indicatie$aantal[wachtenden_2022_indicatie$instelling=="ZUNL"]-wachten_ZUNL),0)

#Percentage unieke aanmeldingen volgens methode 2
schat_wachten_indicatie_2                            = sum(instelling_wachten_indicatie[, rowSums(.SD), .SDcols=patterns("aantal_wachtenden_")])/sum(wachtenden_2022_indicatie$aantal)
