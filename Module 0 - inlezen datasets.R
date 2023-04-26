#-------------------------------------------------------------------------------------------------------------------------
#Project: Omvang transgender zorg
#Auteur:  SiRM
#Datum:   maart 2023
#Doel:    Module 0: inlezen datasets
#Versie:  1
#-------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Inlezen originele datasets -------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Dataset volume inlezen - een overzicht met de volume per instelling, behandeling en leeftijdscategorie
volume_echt                        = data.table(read_excel(paste0(input,"Dataset_kwartiermaker_volume.xlsx")))
#Dataset wachttijden inlezen - een overzicht met wachttijden per instelling, behandeling en leeftijdscategorie
wachtlijst_echt                    = data.table(read_excel(paste0(input,"Dataset_kwartiermaker_wachtlijst.xlsx")))
#Excel-bestand met alle losse parameters
parameters                         = data.table(read_excel(paste0(wdir,"/Kwantitatief/Inputs/parameters_model_transgenderzorg.xlsx")))
#Excel-bestand met ontwikkelingen en ontwikkelfactoren per scenario
toekomstscenarios_origineel        = data.table(read_excel(paste0(wdir,"/Kwantitatief/Excel bewerkingen/input_toekomstscenarios.xlsx")))
#Resultaten enquête - vraag naar endocrinologische en chirurgische zorg
vraag_behandelingen                = data.table(read_excel(paste0(input, "verhoudingen_enquete_wachtlijst.xlsx")))
#Koppeltabel om verschillende leeftijdscategorieën te koppelen
koppel                             = data.table(read_excel(paste0(input, "koppeltabel leeftijd bevolkingsprognose.xlsx")))
#Koppeltabel om namen instellingen uit verschillende datasets te koppelen
koppel_instelling                  = data.table(read_excel(paste0(input, "/koppeltabel namen instellingen.xlsx")))

#------------------------------------------------------------------------------------------------------------------------
#----------------------------- Openbare datasets -------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Bevolking - CBS
bevolking_origineel                = data.table(read.csv(paste0(sogiz,'Datasets/Bevolking (CBS).csv'), 
                                     sep=";",dec=',', stringsAsFactors=FALSE))   # Aantal inwoners per pc4, leeftijdscategorie en geslacht
#Prognose bevolkingsgroei - CBS
prognose_origineel                 = data.table(read.csv(paste0(sogiz,'Datasets/bevolkingsprognose (CBS).csv'), 
                                     sep=";",dec=',', stringsAsFactors=FALSE))   # Prognose bevolking per pc4, leeftijdscategorie en geslacht
