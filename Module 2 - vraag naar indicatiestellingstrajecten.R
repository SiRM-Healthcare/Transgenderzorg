#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Module 1B: Huidig aantal trans personen en vraag naar transgenderzorg
#Versie:  1
#---------------------------------------------------------------------------------------------------------------------

if(MC == 0){
wachtlijst                               = wachtlijst_echt
volume                                   = volume_echt
wachtlijst$schatting                     = NULL
volume$schatting                         = NULL

#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------- Stap 1: Vraag naar indicatiestelling bepalen -------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#Vanuit de data van de instellingen voor transgenderzorg ontvingen we de volgende gegevens over indicatiestellingstrajecten:
# -Wachtlijsten voor juli 2021, januari 2022, en juli 2022
# -(Verwachte) behandelcapaciteit van 2020 t/m 2023
#We kunnen hiermee per instelling bepalen hoeveel nieuwe aanmeldingen voor een indicatiestelling er tussen juli 2021 en juli 2022 zijn bijgekomen
#Voor de behandelcapaciteit (hierna: volume) nemen we het gemiddelde volume over 2021 en 2022

wachtlijst                               = wachtlijst[wachtlijst$maand == "juli",]
wachtlijst$maand                         = NULL
wachtlijst                               = dcast(wachtlijst, koppel+leeftijdscategorie ~ jaar)
setnames(wachtlijst, c("2021", "2022"), c("w_2021", "w_2022")) 
volume                                   = volume[volume$jaar %in% c(2021, 2022),]
volume                                   = dcast(volume, koppel+leeftijdscategorie ~ jaar)
names(volume)                            = c("koppel", "leeftijdscategorie", "v_2021", "v_2022") 

#We koppelen de data over wachtlijsten en volumes
data                                     = merge(wachtlijst, volume, by=c("koppel", "leeftijdscategorie"), all=T) 
#We berekenen we het gemiddelde volume van 2021 en 2022
data$volume_gem                          = (data$v_2021 + data$v_2022)/2
#We berekenen het aantal nieuwe aanmeldingen tussen juli 2021 en juli 2022
data$aantal_nieuw                        = data$w_2022 - data$w_2021 + data$volume_gem

}else{}

#We vermenigvuldigen het aantal nieuwe aanmeldingen met het percentage unieke aanmeldingen:
#Zo corrigeren we voor het gegeven dat een deel van de betrokken personen zich bij meerdere instellingen aanmelden.
percentage_uniek                           = parameters$waarde_mc[which(parameters$parameter=="percentage_uniek") ]
data$uniek_nieuw                           = data$aantal_nieuw*percentage_uniek

#Vervolgens berekenen we het totaal aantal nieuwe aanmeldingen over alle instellingen per leeftijdscategorie (<18 en >=18)
aantal_nieuw_totaal                        = aggregate(cbind(data$uniek_nieuw), 
                                           by=list(data$leeftijdscategorie), FUN=sum, na.rm=T)
names(aantal_nieuw_totaal)                 = c("leeftijdscategorie", "aantal_nieuw")

#We verdelen dit naar geslacht zoals toegewezen bij de geboorte (AFAB/AMAB)zoals bekend uit beschikbare literatuur
percentage_AMAB                            = parameters$waarde_mc[parameters$parameter == "verhouding_geboortegeslacht" 
                                           & parameters$geboorte_geslacht == "AMAB"]
percentage_AFAB                            = 1-percentage_AMAB
aantal_nieuw_totaal_AFAB                   = aantal_nieuw_totaal
aantal_nieuw_totaal_AFAB$aantal_nieuw      = aantal_nieuw_totaal_AFAB$aantal_nieuw*percentage_AFAB
aantal_nieuw_totaal_AFAB$geboorte_geslacht = "AFAB"
aantal_nieuw_totaal_AMAB                   = aantal_nieuw_totaal
aantal_nieuw_totaal_AMAB$aantal_nieuw      = aantal_nieuw_totaal_AMAB$aantal_nieuw*percentage_AMAB
aantal_nieuw_totaal_AMAB$geboorte_geslacht = "AMAB"
aantal_nieuw_totaal                        = rbind(aantal_nieuw_totaal_AFAB, aantal_nieuw_totaal_AMAB)

#We slaan deze tabel op voor de resultaten, zodat we de resultaten van elke MC-iteratie kunnen vergelijken
aantal_nieuw_totaal_resultaten             = aantal_nieuw_totaal

#We berekenen de verhoudingen voor leeftijdscategorie/geboortegeslacht die we nodig hebben in Module 3
verhoudingen_gebgesl_leeftijd              = aantal_nieuw_totaal
verhoudingen_gebgesl_leeftijd$percentage   = verhoudingen_gebgesl_leeftijd$aantal_nieuw/sum(verhoudingen_gebgesl_leeftijd$aantal_nieuw)
verhoudingen_gebgesl_leeftijd$aantal_nieuw = NULL

#-------------------------------------------------------------------------------------------------------------------------------------
#----------------------------- Stap 2: Hoe groeit de vraag naar indicatiestellingstrajecten-------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------
#---------------- Stap 2A: Berekenen steady states met demografische groei en groeipercentages per scenario ------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#Nu berekenen we de steady states met de groeipercentages van de verschillende scenario's
#De functie 'T' geeft het aantal jaar tot het bereiken van de steady state. Standaardwaarde is 5
T=#tijd tot steady state
toekomstscenarios                            = toekomstscenarios_origineel
toekomstscenarios                            = toekomstscenarios[toekomstscenarios$`behandeling/aantal` == "Indicatiestelling" & toekomstscenarios$behandelwens == "ja",]
toekomstscenarios                            = subset(toekomstscenarios, select=c("scenario", "groeifactor", "sd"))
#Om schijnnauwkeurigheid te voorkomen, voeren we de berekeningen niet uit per leeftijd/geslacht zoals toegekend bij geboorte
toekomstscenarios$aantal_nieuw               = sum(aantal_nieuw_totaal$aantal_nieuw)
toekomstscenarios$scenario = as.character(toekomstscenarios$scenario)
groei$scenario             = as.character(groei$scenario)

#We vermenigvuldigen het aantal nieuwe aanmeldingen eerst met groeipercentages van aantallen trans personen
toekomstscenarios                            = merge(toekomstscenarios, groei, by="scenario")
toekomstscenarios$steady_states              = toekomstscenarios$aantal_nieuw*(1+as.numeric(toekomstscenarios$groei))
#Vervolgens vermenigvuldigen we met de groeifactor voor indicatiestelling en slaan de resultaten op
toekomstscenarios$steady_states              = toekomstscenarios$steady_states*as.numeric(toekomstscenarios$groeifactor)^T
toekomstscenarios_opslaan                    = subset(toekomstscenarios, select=c(scenario, steady_states))

#-------------------------------------------------------------------------------------------------------------------------------------
#------------------------ Stap 2B: Hoe verloopt de vraagcurve? -----------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

if(MC == 0){

#In stap 1 hebben we berekend hoeveel mensen er tussen juli 2021 en juli 2022 bij zijn gekomen op de wachtlijst
#We nemen aan dat dit aantal groeit volgens een S-curve, en dat dit na een aantal jaar een evenwicht bereikt
#We doen de berekening niet apart per leeftijdscategorie.
#We nemen aan dat na 5 jaar de steady state is bereikt
#De functie berekent dan de S-curve, oftewel het jaarlijks aantal nieuwe aanmeldingen tussen 2022 en 2022 + T
vooruitblik           = #aantal jaar
aantal_0       = sum(aantal_nieuw_totaal$aantal_nieuw)
functie_2 = function(aantal_T){
  aantal       = rep(0,vooruitblik)
  aantal[1]    = NA
  aantal[2]    = aantal_0
  for(i in 3:length(aantal)){
    aantal[i]  = aantal_0 + (aantal_T-aantal_0)/(1+exp(-i + (5/2)))
  }
  aantal_nieuw = cbind(aantal)
}

#MC-resultaten als verdere input:
#De functie 'steady_states' gebruikt hard copy resultaten van de meest recente MC-simulatie. Hieronder zijn gecommente dummywaarden ingevuld.
steady_states = c(#7000,#10000,#3000,#4000,#5000)
verloop_vraag          = data.frame(sapply(steady_states, functie_2))
names(verloop_vraag)   = c(1,2,3,4,5)
verloop_vraag$jaar     = seq(2021, 2021 + vooruitblik -1, by=1)
#Resultaten omzetten in long format
verloop_vraag          = melt(verloop_vraag, id.vars="jaar", variable.name="scenario", value.name ="aantal" )
verloop_vraag$scenario = as.factor(verloop_vraag$scenario)

#-------------------------------------------------------------------------------------------------------------------------------------
#--------------- Stap 3: Gevraagde behandelcapaciteit voor de zorgvraag --------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#Gegevens:
#t=0: juli 2022
#We willen in x jaar naar een lege wachtlijst, hoeveel volume is daarvoor nodig?
#Onder volume 2021 verstaan we het volume van juli 2020 tot juli 2021, dus we nemen het gemiddelde volume van 2020 en 2021
volume_2021     = (sum(volume_echt[volume_echt$jaar == "2020",]$aantal, na.rm=T) + sum(volume_echt[volume_echt$jaar == "2021",]$aantal, na.rm=T))/2
#Onder volume 2022 verstaan we het volume van juli 2021 tot juli 2022, dus we nemen het gemiddelde volume van 2021 en 2022
#Dit is al berekend in stap 1
volume_2022     = sum(data$volume_gem)
wachtlijst_2021 = sum(data$w_2021)
wachtlijst_2022 = sum(data$w_2022)
#Helling van het behandelaanbod (volume)
helling         = volume_2022 - volume_2021

#Voor de verandering in aanbod, is de aanname dat dit eerst groeit tot het gestelde maximum. 
#Daarna blijft het aanbod (volume) X jaar hoger dan de nieuwe jaarlijkse zorgvraag zodat de wachtlijsten krimpen.
#Vervolgens wordt de capaciteit gelijk aan de vraag. Zie het rapport voor meer toelichting op de gekozen aannames.

#Gekozen parameters:
aantal_jaar_hoog      = #aantal jaar_(5)
overshoot_percentage  = #overshootpercentage_(0,1-0,3)
daling_door_uitval    = #jaarlijks verlooppercentage_(0,1)
#Keuze voor welk scenario we de wachtlijst, wachttijd en nodige volume berekenen
#We kijken vanaf nu alleen naar scenario 1,2,4
#Scenario 5 is het demografiescenario
scenario              = #scenarionummer(1-5)

#Bij elk scenario past een andere steady state
steady_state          = steady_states[scenario]
#Voor alleen demografie
steady_state         = sum(aantal_nieuw_totaal$aantal_nieuw)*(1+demo_groei)
overshoot             = (1+overshoot_percentage)*steady_state

# Stap 1:
volume = c(volume_2021, volume_2022)

# Stap 2:
#Helling is de groei in volume tussen 21 en 22
vol                 = volume_2022
while(vol + helling < overshoot){
  vol               = vol + helling
  volume            = c(volume, vol)
}
# Stap 3:
#Als het gewenste volume is bereikt, blijft het volume 'aantal_jaar_hoog' jaar op dat level. 
volume              = c(volume, rep(overshoot, aantal_jaar_hoog))

# Stap 4:
#In deze periode nemen we niemand meer aan, dus het volume neemt af ivm verloop door bv pensioen/vertrek
#We laten het volume afnemen tot het gelijk is aan de steady state vraag
vol      = tail(volume, n=1)
while((1-daling_door_uitval)*vol > steady_state){
  vol    = (1-daling_door_uitval)*vol 
  volume = c(volume, vol)
}

# Stap 5:
#Het volume heeft de steady state bereikt, en blijft op dat level voor de rest van de periode waar we naar kijken
volume = c(volume, rep(steady_state, vooruitblik - length(volume)))

#-------------------------------------------------------------------------------------------------------------------------------------
#---------- Stap 4: Hoe verloopt de wachtlijst/wachttijd bij dit gekozen volume? -----------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

aantal_nieuwe_aanmeldingen        = verloop_vraag$aantal[verloop_vraag$scenario == scenario]

#Vervolgens berekenen we hoe de wachtlijst verloopt met het hierboven gekozen doel voor het volume
wachtlijst                        = rep(NA, length(volume))
wachtlijst[1]                     = wachtlijst_2021*percentage_uniek
wachtlijst[2]                     = wachtlijst_2022*percentage_uniek
#De wachtlijst in periode t is gelijk aan de wachtlijst in periode t-1 + het aantal nieuwe aanmeldingen - het volume
for(i in 3:length(wachtlijst)){
  if(wachtlijst[i-1] - volume[i-1] + aantal_nieuwe_aanmeldingen[i] > 0){
    wachtlijst[i] = wachtlijst[i-1] - volume[i-1] + aantal_nieuwe_aanmeldingen[i]
  }
  else{wachtlijst[i] = 0}
}

#We voegen de data voor volume, vraag en wachtlijst samen
plot_data             = data.frame(cbind(seq(2021, 2021+vooruitblik-1, by=1), volume, wachtlijst, aantal_nieuwe_aanmeldingen))
names(plot_data)      = c("jaar","volume", "wachtlijst", "nieuw")
#We berekenen de wachttijd: wachttijd=wachtlijst/volume
plot_data$wachttijd   = plot_data$wachtlijst/plot_data$volume
scale                = 0.0003
ggplot(plot_data, aes(x=jaar)) + 
  geom_line(aes(y=volume, colour="Volume")) + 
  geom_line(aes(y=wachtlijst, colour="Wachtlijst")) +
  geom_line(aes(y=nieuw, colour= "Vraag")) + 
  geom_line(aes(y=wachttijd/scale, colour="Wachttijd")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Wachttijd"))+
  theme_bw() + 
  labs(x="", y="Aantal")

#Van alle combinaties data opslaan
#Uitzetten bij runnen MC
setwd(paste0(output, "/Resultaten met nieuwe overshoot%"))
write_xlsx(plot_data, "Scenario demo, optie 3.xlsx")
}else{}
