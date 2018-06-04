criminaliteit <- read_delim("datasets/criminaliteit.csv", ";", escape_double = FALSE, trim_ws = TRUE)
criminaliteit2 <- read_delim("datasets/criminaliteit.csv",";", escape_double = FALSE, col_types = cols(GeregistreerdeMisdrijvenPer1000Inw_3 = col_number()),  trim_ws = TRUE)
regios <- read_delim("datasets/82949NED_TypedDataSet_17052018_142705.csv", ";", escape_double = FALSE, trim_ws = TRUE)

crimi <- left_join(criminaliteit2, regios, by="RegioS") %>% filter(!is.na(GeregistreerdeMisdrijvenPer1000Inw_3)&GeregistreerdeMisdrijvenPer1000Inw_3 != ".")

NLD <- readRDS("~/zorgkosten db4/maps/gadm36_NLD_2_sp.rds")
NLD_fixed <- subset(NLD, !NLD$NAME_1  %in% c("Zeeuwse meren", "IJsselmeer"))
NLD_fixed <- fortify(NLD_fixed,region="NAME_2")
names_and_numbers <- data_frame(id=rownames(NLD@data), Naam_2=NLD@data$NAME_2)  %>% 
  left_join(crimi, by = "Naam_2") 

final_map <- left_join(NLD_fixed, names_and_numbers, by = c("id" = "Naam_2"))


ggplot(final_map)+
  theme_minimal()+
  geom_polygon( aes(x = long, y = lat, group = group, fill= GeregistreerdeMisdrijvenPer1000Inw_3),
                color = "grey", alpha = 1/4) +
  coord_map()+
  scale_fill_distiller(name = "criminaliteit per gemeente", # change titel legend
                       palette = "Spectral")+ # change the color scheme
  #theme(legend.position = "bottom",  legend.text = element_text(vjust = 45))+  # change the legend position
  labs(title="Criminaliteit per gemeente",  caption="Bron: vektis")
