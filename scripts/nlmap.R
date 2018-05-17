#libraries
install.packages("sp")
library(sp)
library(tibble)
library(dplyr)
library(ggplot2)
library(mapproj)
library(rgdal)
library(rgeos)
library(maptools)
install.packages("gpclib", type="source")
install.packages("gpclib")
install.packages("rgeos")
install.packages('rgdal', type='source')
gpclibPermit()

install.packages("mapproj")
install.packages("maptools")
NLD <- readRDS("~/zorgkosten db4/maps/gadm36_NLD_2_sp.rds")

plot(NLD)

NLD@data %>% as_tibble()
NLD@data %>% tail(2)

NLD@polygons %>% length()
## [1] 491
NLD@polygons[[10]] %>% # read the tenth object 
  slotNames()  # and give me the slotnames 

ggplot()+
  geom_polygon(data = NLD, 
               aes(x = long, y = lat, group = group)) +
  coord_fixed()

ggplot()+
  geom_polygon(data = NLD, aes(x = long, y = lat, group = group)) +
  coord_map()

ggplot()+
  geom_polygon(data = NLD, aes(x = long, y = lat, group = group)) +
  coord_map()

NLD@data$NAME_2 %>%  unique()

NLD_fixed <- subset(NLD, !NLD$NAME_1  %in% c("Zeeuwse meren", "IJsselmeer"))
NLD_fixed <- fortify(NLD_fixed,region="NAME_2")
#NLD_fixed <- merge(fortify(NLD_fixed), as.data.frame(NLD_fixed), by.x="id", by.y=0)

NLD_IDK@data$id %>% unique()

ggplot(NLD_fixed) +
  geom_polygon( aes(x = long, y = lat, group = group))+
  coord_map()

ggplot(NLD_fixed) +
  theme_minimal()+  # no backgroundcolor
  geom_polygon( aes(x = long, y = lat, group = group),
                color = "white",   # color is the lines of the region
                fill = "#9C9797")+ # fill is the fill of every polygon.
  coord_map()
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
  theme(legend.position = "bottom")  # chagne the legend position


NLD %>%  length()
NLD_fix <- toupper(NLD_fixed$id)

NLD@data$CC_2


zg <- zorgkosten
zg$total1 <- zg$KOSTEN_EERSTELIJNS_ONDERSTEUNING + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_FARMACIE + zg$KOSTEN_GENERALISTISCHE_BASIS_GGZ + zg$KOSTEN_HUISARTS_INSCHRIJFTARIEF + zg$KOSTEN_GERIATRISCHE_REVALIDATIEZORG
zg$total2 <- zg$KOSTEN_GRENSOVERSCHRIJDENDE_ZORG + zg$KOSTEN_HUISARTS_CONSULT + zg$KOSTEN_HUISARTS_MDZ + zg$KOSTEN_HUISARTS_OVERIG + zg$KOSTEN_HULPMIDDELEN + zg$KOSTEN_KRAAMZORG + zg$KOSTEN_LANGDURIGE_GGZ + zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG 
zg$total3 <- zg$KOSTEN_MEDISCH_SPECIALISTISCHE_ZORG + zg$KOSTEN_MONDZORG + zg$KOSTEN_OVERIG + zg$KOSTEN_PARAMEDISCHE_ZORG_FYSIOTHERAPIE + zg$KOSTEN_PARAMEDISCHE_ZORG_OVERIG + zg$KOSTEN_SPECIALISTISCHE_GGZ + zg$KOSTEN_VERLOSKUNDIGE_ZORG + zg$KOSTEN_VERPLEGING_EN_VERZORGING + zg$KOSTEN_ZIEKENVERVOER_LIGGEND +  zg$KOSTEN_ZIEKENVERVOER_ZITTEND

zg$totalpp <- (zg$total1 + zg$total2 + zg$total3) / zg$AANTAL_BSN

zg2 <- zg %>% 
  select(gemeente = GEMEENTENAAM, totalpp = totalpp, aantal = AANTAL_BSN, leeftijd = LEEFTIJDSKLASSE, geslacht = GESLACHT) %>% 
  filter(!is.na(gemeente))
sumOfAll <- sum(zg2)

zg2 <- data.frame(aggregate(totalpp ~ gemeente, data=zg2, mean))

zg3 <- aggregate(totalpp ~ gemeente, data=zg2, mean)
