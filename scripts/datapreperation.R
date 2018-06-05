library(readr)

regios <- read_delim("datasets/82949NED_TypedDataSet_17052018_142705.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#voeg criminaliteit toe aan regios
criminaliteit <- read_delim("datasets/criminaliteit.csv",";", escape_double = FALSE, col_types = cols(GeregistreerdeMisdrijvenPer1000Inw_3 = col_number()),  trim_ws = TRUE)
compleet <- left_join(criminaliteit, regios, by="RegioS") %>% filter(!is.na(GeregistreerdeMisdrijvenPer1000Inw_3)&GeregistreerdeMisdrijvenPer1000Inw_3 != ".")
compleet$SoortMisdrijf <- NULL
compleet$Perioden <- NULL

#voeg zorgkosten toe aan regios
zorgkosten <- read_delim("datasets/Vektis Open Databestand Zorgverzekeringswet 2015 - gemeente (3).csv",";", escape_double = FALSE, trim_ws = TRUE)
zg <- zorgkosten
zg$total1 <- zg$KOSTEN_EERSTELIJNS_ONDERSTEUNING + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_FARMACIE + zg$KOSTEN_GENERALISTISCHE_BASIS_GGZ + zg$KOSTEN_HUISARTS_INSCHRIJFTARIEF + zg$KOSTEN_GERIATRISCHE_REVALIDATIEZORG
zg$total2 <- zg$KOSTEN_GRENSOVERSCHRIJDENDE_ZORG + zg$KOSTEN_HUISARTS_CONSULT + zg$KOSTEN_HUISARTS_MDZ + zg$KOSTEN_HUISARTS_OVERIG + zg$KOSTEN_HULPMIDDELEN + zg$KOSTEN_KRAAMZORG + zg$KOSTEN_LANGDURIGE_GGZ + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG 
zg$total3 <- zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_MONDZORG + zg$KOSTEN_OVERIG + zg$KOSTEN_PARAMEDISCHE_ZORG_FYSIOTHERAPIE + zg$KOSTEN_PARAMEDISCHE_ZORG_OVERIG + zg$KOSTEN_SPECIALISTISCHE_GGZ + zg$KOSTEN_VERLOSKUNDIGE_ZORG + zg$KOSTEN_VERPLEGING_EN_VERZORGING + zg$KOSTEN_ZIEKENVERVOER_LIGGEND +  zg$KOSTEN_ZIEKENVERVOER_ZITTEND
#alles bij elkaar
zg$totalpp <- (zg$total1 + zg$total2 + zg$total3) / zg$AANTAL_BSN
#alle leeftijd en geslachten bij elkaar
zg <- zg %>% 
  select(gemeente = GEMEENTENAAM, totalpp = totalpp, aantal = AANTAL_BSN, leeftijd = LEEFTIJDSKLASSE, geslacht = GESLACHT) %>% 
  filter(!is.na(gemeente))

zg <- aggregate(totalpp ~ gemeente, data=zg, mean)

zg <- zg %>% 
  select(gemeente = gemeente, number = totalpp)%>% filter(!is.na(number))


compleet$gemeente <- compleet$Naam_2
compleet <- compleet %>%  mutate(gemeente = toupper(gemeente))%>% 
  left_join(zorgkostentotaal ,by = "gemeente")

