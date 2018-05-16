#libraries
install.packages("sp")
library(sp)
library(tibble)
library(dplyr)
library(ggplot2)
install.packages("mapproj")
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
NLD_fixed <- fortify(NLD_fixed, gemeente = "NAME_2")

ggplot(NLD_fixed) +
  geom_polygon( aes(x = long, y = lat, group = group))+
  coord_map()

ggplot(NLD_fixed) +
  theme_minimal()+  # no backgroundcolor
  geom_polygon( aes(x = long, y = lat, group = group),
                color = "white",   # color is the lines of the region
                fill = "#9C9797")+ # fill is the fill of every polygon.
  coord_map()
test <- zorgkosten %>% 
  select(gemeente = GEMEENTENAAM, number = KOSTEN_FARMACIE, leeftijd = LEEFTIJDSKLASSE, geslacht = GESLACHT)%>% filter(leeftijd == "20 t/m 24 jaar"& !is.na(number)& geslacht == "M")

names_and_numbers <- data_frame(id=rownames(NLD@data), gemeente=NLD@data$NAME_2) %>% 
  mutate(gemeente = toupper(gemeente)) %>% 
  left_join(test, by = "gemeente")

final_map <- left_join(NLD_fixed, names_and_numbers, by = "id")

ggplot(final_map)+
  theme_minimal()+
  geom_polygon( aes(x = long, y = lat, group = group, fill= number),
                color = "grey", alpha = 1/4) +
  coord_map()+
  scale_fill_distiller(name = "Number of people in gemeente", # change titel legend
                       palette = "RdBu", min(0), max(max.col(final_map$number)) )+ # change the color scheme
  theme(legend.position = "bottom")  # chagne the legend position


final_map$gemeente %>%  unique()
