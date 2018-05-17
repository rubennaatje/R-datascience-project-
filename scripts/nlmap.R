#libraries
library(sp)
library(tibble)
library(dplyr)
library(ggplot2)
library(mapproj)
library(rgdal)
library(rgeos)
library(maptools)

#packages voor libraries installeren 
install.packages("sp")
install.packages("gpclib", type="source")
install.packages("gpclib")
install.packages("rgeos")
install.packages('rgdal', type='source')
install.packages("mapproj")
install.packages("maptools")
#kaart spatialpolygonsdataframe inladen
NLD <- readRDS("~/zorgkosten db4/maps/gadm36_NLD_2_sp.rds")
#zorgkosten inladen
zorgkosten <- read_delim("datasets/Vektis Open Databestand Zorgverzekeringswet 2015 - gemeente (3).csv",";", escape_double = FALSE, trim_ws = TRUE)
#ijsselmeer en zeeuwsemeren eruit halen
NLD_fixed <- subset(NLD, !NLD$NAME_1  %in% c("Zeeuwse meren", "IJsselmeer"))
NLD_fixed <- fortify(NLD_fixed,region="NAME_2")

#zorgkosten berekenen, opgedeeld in verschillende variabelen omdat het anders te lang werd, misschien opdelen en categorien die bij elkaar passen.
zg <- zorgkosten
zg$total1 <- zg$KOSTEN_EERSTELIJNS_ONDERSTEUNING + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_FARMACIE + zg$KOSTEN_GENERALISTISCHE_BASIS_GGZ + zg$KOSTEN_HUISARTS_INSCHRIJFTARIEF + zg$KOSTEN_GERIATRISCHE_REVALIDATIEZORG
zg$total2 <- zg$KOSTEN_GRENSOVERSCHRIJDENDE_ZORG + zg$KOSTEN_HUISARTS_CONSULT + zg$KOSTEN_HUISARTS_MDZ + zg$KOSTEN_HUISARTS_OVERIG + zg$KOSTEN_HULPMIDDELEN + zg$KOSTEN_KRAAMZORG + zg$KOSTEN_LANGDURIGE_GGZ + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG 
zg$total3 <- zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_MONDZORG + zg$KOSTEN_OVERIG + zg$KOSTEN_PARAMEDISCHE_ZORG_FYSIOTHERAPIE + zg$KOSTEN_PARAMEDISCHE_ZORG_OVERIG + zg$KOSTEN_SPECIALISTISCHE_GGZ + zg$KOSTEN_VERLOSKUNDIGE_ZORG + zg$KOSTEN_VERPLEGING_EN_VERZORGING + zg$KOSTEN_ZIEKENVERVOER_LIGGEND +  zg$KOSTEN_ZIEKENVERVOER_ZITTEND
#alles bij elkaar
zg$totalpp <- (zg$total1 + zg$total2 + zg$total3) / zg$AANTAL_BSN
#alle leeftijd en geslachten bij elkaar
zg2 <- zg %>% 
  select(gemeente = GEMEENTENAAM, totalpp = totalpp, aantal = AANTAL_BSN, leeftijd = LEEFTIJDSKLASSE, geslacht = GESLACHT) %>% 
  filter(!is.na(gemeente))

zg3 <- aggregate(totalpp ~ gemeente, data=zg2, mean)

test <- zg3 %>% 
  select(gemeente = gemeente, number = totalpp)%>% filter(!is.na(number))

names_and_numbers <- data_frame(id=rownames(NLD@data), gemeente=NLD@data$NAME_2)  %>% 
  mutate(gemeente = toupper(gemeente)) %>% 
  left_join(test, by = "gemeente")

NLD_fixed$id <- toupper(NLD_fixed$id)

final_map <- left_join(NLD_fixed, names_and_numbers, by = c("id" = "gemeente"))

ggplot(final_map)+
  theme_minimal()+
  geom_polygon( aes(x = long, y = lat, group = group, fill= number),
                color = "grey", alpha = 1/4) +
  coord_map()+
  scale_fill_distiller(name = "zorgkosten per gemeente", # change titel legend
                       palette = "Spectral")+ # change the color scheme
  #theme(legend.position = "bottom",  legend.text = element_text(vjust = 45))+  # chagne the legend position
  labs(title="gemiddelde zorgkosten p.p. per gemeente",  caption="Bron: vektis")

