#------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Vraag naar endocrinologische en chirurgische zorg bepalen
#Versie:  1
#---------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------
#-----------------Vraag naar endocrinologische en chirurgische zorg bepalen ----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#We weten hoeveel mensen er in juli 2022 op de wachtlijst stonden voor een diagnose
#Vervolgens berekenen we wat de vraag naar somatische transgenderzorg in 2022-2023 zou zijn bij toereikend aanbod
percentage_diagnose                    = parameters$waarde_mc[parameters$parameter == "percentage_diagnose"]
wachtlijst2022                         = aggregate(data$w_2022, by=list(data$leeftijdscategorie), FUN=sum, na.rm=T)
names(wachtlijst2022)                  = c("leeftijdscategorie", "aantal")
wachtlijst2022$aantal                  = wachtlijst2022$aantal*percentage_uniek
wachtlijst2022$percentage_diagnose     = percentage_diagnose
wachtlijst2022$aantal_diagnose         = wachtlijst2022$aantal*wachtlijst2022$percentage_diagnose

#Dit nog verdelen naar geslacht zoals toegewezen bij de geboorte (AMAB/AFAB)
aantal_diagnose_AFAB                   = subset(wachtlijst2022, select=c("leeftijdscategorie", "aantal_diagnose"))
aantal_diagnose_AFAB$aantal_diagnose   = aantal_diagnose_AFAB$aantal_diagnose*percentage_AFAB
aantal_diagnose_AFAB$geboorte_geslacht = "AFAB"
aantal_diagnose_AMAB                   = subset(wachtlijst2022, select=c("leeftijdscategorie", "aantal_diagnose"))
aantal_diagnose_AMAB$aantal_diagnose   = aantal_diagnose_AMAB$aantal_diagnose*percentage_AMAB
aantal_diagnose_AMAB$geboorte_geslacht = "AMAB"
aantal_diagnose                        = rbind(aantal_diagnose_AFAB, aantal_diagnose_AMAB)

#Psychologische zorg
#De dataset 'vraag_behandelingen' bevat de verhoudingen van behandelwensen van de personen van de enquête die op een wachtlijst staan
psych                                  = vraag_behandelingen[vraag_behandelingen$zorgvraag == "Fase_psychzorg",]
psych                                  = merge(psych, aantal_diagnose, by=c("geboorte_geslacht", "leeftijdscategorie"))
psych$aantal                           = psych$aandeel*psych$aantal_diagnose
psych$maand                            = "juli"
psych$jaar                             = 2022
psych$aandeel                          = NULL
psych$aantal_diagnose                  = NULL

#Endocrinologische transgenderzorg:
#We nemen aan dat personen die een diagnose krijgen, geen onvrijwilige wachttijd hebben tot start van endocrinologische zorg
endo                                   = vraag_behandelingen[vraag_behandelingen$zorgvraag %in% c("Fase_hormonen", "Fase_pubremming"),]
endo                                   = merge(endo, aantal_diagnose, by=c("geboorte_geslacht", "leeftijdscategorie"))
endo$aantal                            = endo$aandeel*endo$aantal_diagnose
endo$maand                             = "juli"
endo$jaar                              = 2022
endo$aandeel                           = NULL
endo$aantal_diagnose                   = NULL

#Chirurgische transgenderzorg
#Omdat een eerder gestarte endocrinologische behandeling een voorwaarde is voor veel chirurgische behandelingen,
#verwacht je dat chirurgische transgenderzorg pas gemiddeld een jaar later gevraagd is
chirurgische_behandelingen             = c("Fase_borstverw", "Fase_fertiliteit", "Fase_gynchi", "Fase_genchi_verwpenis", "Fase_borstvergr",
                                           "Fase_genchi_verwvagina", "Fase_aangezichtschi")
chirurgie                              = vraag_behandelingen[vraag_behandelingen$zorgvraag %in% chirurgische_behandelingen,]
chirurgie                              = merge(chirurgie, aantal_diagnose, by=c("geboorte_geslacht", "leeftijdscategorie"))
chirurgie$maand                        = "juli"
chirurgie$jaar                         = 2023
chirurgie$aantal                       = chirurgie$aandeel*chirurgie$aantal_diagnose
chirurgie$aandeel                      = NULL
chirurgie$aantal_diagnose              = NULL
zorgvraag                              = data.frame(rbind(psych, endo, chirurgie))

#Nu maken we voor alle vormen van zorg een prognose voor 2027
#We doen dit apart voor endocrinologische, psychologische, chirurgische en fertiliteitszorg
#We hebben chirurgische zorgvraag berekend voor 2023, dus die rekenen we 4 jaar vooruit
#We hebben endocrinologische zorgvraag berekend voor 2022, dus die rekenen we 5 jaar vooruit
endo_2027                              = endo
psych_2027                             = psych
chir_2027                              = chirurgie[!chirurgie$zorgvraag == "Fase_fertiliteit",]
fert_2027                              = chirurgie[chirurgie$zorgvraag == "Fase_fertiliteit",]

#We rekenen eerst de demografische groei door
endo_2027$aantal                       = endo_2027$aantal *(1+demo_groei)
chir_2027$aantal                       = chir_2027$aantal*(1+(4/5)*demo_groei)
fert_2027$aantal                       = fert_2027$aantal*(1+(4/5)*demo_groei)
psych_2027$aantal                      = psych_2027$aantal*(1+demo_groei)

#We pakken de groeifactoren uit de tabel
toekomstscenarios_groei                = toekomstscenarios_origineel[toekomstscenarios_origineel$`behandeling/aantal` %in% c("aantal nieuw per jaar", 
                                                                                                                             "Indicatiestelling",
                                                                                                                             "Chirurgische zorg", 
                                                                                                                             "Psychologische zorg", 
                                                                                                                             "Endocrinologische zorg",
                                                                                                                             "Fertiliteitszorg"),]
toekomstscenarios_groei                = toekomstscenarios_groei[toekomstscenarios_groei$behandelwens %in% c("ja", "n.v.t."),]
toekomstscenarios_groei                = subset(toekomstscenarios_groei, select=c("scenario", "behandeling/aantal", "groeifactor"))
toekomstscenarios_groei$groeifactor    = as.numeric(toekomstscenarios_groei$groeifactor)
toekomstscenarios_groei                = dcast(toekomstscenarios_groei, scenario ~`behandeling/aantal`)
toekomstscenarios_groei$jaar           = 2027

#Het aantal zorgvragen in 2027 zijn het aantal zorgvragen nu x de groeifactor voor aantal transpersonen x de groeifactor voor indicatiestelling x de groeifactor voor de betreffende zorgvraag
#Endocrinologische transgenderzorg:
endo_2027$jaar                         = 2027
endo_2027                              = merge(endo_2027, toekomstscenarios_groei,by="jaar", allow.cartesian=T)
endo_2027$aantal                       = endo_2027$aantal*(endo_2027$`aantal nieuw per jaar`)^5*endo_2027$`Indicatiestelling`^5*endo_2027$`Endocrinologische zorg`^5

#Chirurgische transgenderzorg:
chir_2027$jaar                         = 2027
chir_2027                              = merge(chir_2027, toekomstscenarios_groei, by="jaar", allow.cartesian=T)
chir_2027$aantal                       = chir_2027$aantal*(chir_2027$`aantal nieuw per jaar`)^4*chir_2027$`Indicatiestelling`^4*chir_2027$`Chirurgische zorg`^4

#Psychologische transgenderzorg:
#Voor psychologische transgenderzorg is geen indicatiestelling nodig
psych_2027$jaar                        = 2027
psych_2027                             = merge(psych_2027, toekomstscenarios_groei, by="jaar", allow.cartesian=T)
psych_2027$aantal                      = psych_2027$aantal*(psych_2027$`aantal nieuw per jaar`)^5*psych_2027$`Psychologische zorg`^5

#Fertiliteitszorg
fert_2027$jaar                         = 2027
fert_2027                              = merge(fert_2027, toekomstscenarios_groei, by="jaar", allow.cartesian=T)
fert_2027$aantal                       = fert_2027$aantal*(fert_2027$`aantal nieuw per jaar`)^4*fert_2027$`Indicatiestelling`^4*fert_2027$`Fertiliteitszorg`^4

#We combineren alle soorten zorg in een tabel
zorgvraag_2027                         = rbind(endo_2027[,1:7], chir_2027[,1:7], psych_2027[,1:7], fert_2027[,1:7])
zorgvraag_2027$jaar                    = NULL
zorgvraag_2027$maand                   = NULL
setnames(zorgvraag_2027, "aantal", "aantal_2027")