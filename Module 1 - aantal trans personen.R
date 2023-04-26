#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Huidig en toekomstig aantal trans personen inschatten
#---------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------
#---------------- Stap 1: Inschatten aantal trans personen o.b.v. demografie --------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

if(MC == 0){
#We berekenen het aantal inwoners per leeftijdscategorie en geboortegeslacht van 2022 t/m 2027
bevolking                = bevolking_origineel
prognose                 = prognose_origineel
bevolking                = bevolking[bevolking$jaar == 2022,]
bevolking                = subset(bevolking, select=c("leeftijd", "geslacht", "pc4", "inwoners"))
prognose                 = subset(prognose, select=c("leeftijd", "geslacht", "pc4", "inw_2023", "inw_2024", "inw_2025", "inw_2026", "inw_2027"))
bevolking                = merge(bevolking, prognose, by=c("leeftijd", "geslacht", "pc4"))
setnames(bevolking, "inwoners", "inw_2022")
#Format omzetten van wide naar long
bevolking                = melt(bevolking, id.vars=c("leeftijd", "geslacht", "pc4"), variable.name="jaar", value.name="aantal")
bevolking$jaar           = substr(bevolking$jaar, 5, 8)
bevolking$jaar           = as.numeric(bevolking$jaar)

#We splitsen de leeftijdsgroep 15 tot 20 jaar op om ze te verdelen over de groepen <18 en >=18
bevolking_15_18          = bevolking[bevolking$leeftijd == "15 tot 20 jaar",]
bevolking_15_18$aantal   = bevolking_15_18$aantal*(3/5)
bevolking_15_18$leeftijd = "15 tot 18 jaar"
bevolking_18_20          = bevolking[bevolking$leeftijd == "15 tot 20 jaar",]
bevolking_18_20$aantal   = bevolking_18_20$aantal*(2/5)
bevolking_18_20$leeftijd = "18 tot 20 jaar"
bevolking                = bevolking[!bevolking$leeftijd == "15 tot 20 jaar",]
bevolking                = rbind(bevolking, bevolking_15_18, bevolking_18_20)

#We gebruiken de koppeltabel om van leeftijdscategorieÃ«n van 5 jaar naar <18 en >=18 te gaan
bevolking                = merge(bevolking, koppel, by="leeftijd")
bevolking                = aggregate(cbind(bevolking$aantal), by=list(bevolking$leeftijdscategorie,
                                                                             bevolking$geslacht, bevolking$jaar), FUN=sum, na.rm=T)
names(bevolking)         = c("leeftijdscategorie", "geboorte_geslacht", "jaar", "inwoners")
bevolking$geboorte_geslacht[bevolking$geboorte_geslacht == "Mannen"]  = "AMAB"
bevolking$geboorte_geslacht[bevolking$geboorte_geslacht == "Vrouwen"] = "AFAB"
rm(prognose, bevolking_15_18, bevolking_18_20)
}else{}

#We bepalen prevalentiepercentages per leeftijd/geboortegeslacht. 'Prevalentie' wordt hier gebruikt voor 'het aantal trans personen per 100.000 inwoners'
#Met 'aantal trans personen' doelen we op het aantal personen wat zich als niet-cis identificeert in bijvoorbeeld enquêtes, zie het rapport voor meer toelichting 
prevalentie              = parameters[parameters$parameter == "prevalentie"]
prevalentie              = subset(prevalentie, select=c("leeftijdscategorie", "geboorte_geslacht", "waarde_mc"))

#We vermenigvuldigen het aantal inwoners met de prevalentiecijfers om een inschatting te maken van het aantal trans personen van 2022 tot en met 2027
prevalentie                    = merge(bevolking, prevalentie, by=c("leeftijdscategorie", "geboorte_geslacht"))
prevalentie$transpersonen      = prevalentie$inwoners*prevalentie$waarde_mc  

#Het aantal trans personen per leeftijd/geboortegeslacht van 2022 tm 2027 slaan we op:
transpersonen                  = subset(prevalentie, select=c("leeftijdscategorie", "geboorte_geslacht", "jaar", "transpersonen"))
transpersonen_2022        = sum(transpersonen[transpersonen$jaar == 2022,]$transpersonen)
transpersonen_2027        = sum(transpersonen[transpersonen$jaar == 2027,]$transpersonen)
#We berekenen het demografische groeipercentage dat we nodig hebben in module 2
demo_groei                = (transpersonen_2027 - transpersonen_2022)/transpersonen_2022

#------------------------------------------------------------------------------------------------------------------------
#---------- Stap 2: Berekenen aantal trans personen in vier verschillende scenario's ------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#We gaan ervan uit dat het aantal personen wat zich als trans identificeert niet alleen zal veranderen door demografische veranderingen
#Om mogelijke gevolgen hiervan te onderzoeken, stelden we vier scenario's op, elk met eigen groeipercentages en steady states
#Zie het rapport voor een uitgebreide toelichting.
#Tussen 2022 en 2027 verandert het aantal transpersonen met het jaarlijkse groeipercentage (naast demografie)
#In de steady state verandert het aantal transpersonen en de zorgvragen alleen door demografische groei

#Aantal jaar voordat de steady state wordt bereikt
T = 5

#Scenarios en bijbehorende groeipercentages
toekomstscenarios              = toekomstscenarios_origineel
toekomstscenarios              = toekomstscenarios[toekomstscenarios$`behandeling/aantal` == "aantal nieuw per jaar",]
toekomstscenarios              = subset(toekomstscenarios, select=c("scenario", "groeifactor"))
#Het aantal trans personen in 2027 na doorrekenen demografie:
toekomstscenarios$transpersonen_2027             = transpersonen_2027
#Het aantal trans personen in 2027 na doorrekenen scenario:
toekomstscenarios$aantal_transpersonen_scenarios = toekomstscenarios$transpersonen_2027*as.numeric(toekomstscenarios$groeifactor)^T

#We nemen aan dat het verloop tussen 2022 en 2027 volgens een S-curve is
#Om het aantal transpersonen tussen 2022 en 2027 te berekenen, gebruiken we onderstaande functie met formule voor een S-curve
aantal_0       = transpersonen_2022
functie_1      = function(aantal_T){
  aantal       = rep(0,6)
  aantal[1]    = aantal_0
  for(i in 2:length(aantal)){
    aantal[i]  = aantal_0 + (aantal_T-aantal_0)/(1+exp(-i + (5/2)))
  }
  aantal_nieuw = cbind(aantal)
}
#We runnen de functie voor de steady states (2027) van alle scenarios om het verloop te berekenen
verloop_scenarios = data.frame(sapply(toekomstscenarios$aantal_transpersonen_scenarios, functie_1))
names(verloop_scenarios)   = c(1,2,3,4)
verloop_scenarios$jaar     = seq(2022, 2027, by=1)  
#Resultaten omzetten naar long format
verloop_scenarios          = melt(verloop_scenarios, id.vars="jaar", variable.name="scenario", value.name ="aantal" )
verloop_scenarios$scenario = as.factor(verloop_scenarios$scenario)
#Opslaan resultaten
toekomst_transpersonen     = verloop_scenarios

#We berekenen per scenario met welk percentage het aantal personen wat als trans identificeert is veranderd tussen 2022 en 2027
#Dit is nodig in module 2
groei             = toekomst_transpersonen[toekomst_transpersonen$jaar == 2027,]
groei$aantal_2022 = transpersonen_2022
groei$groei       = (groei$aantal - groei$aantal_2022)/groei$aantal_2022
