# 3. faza: Vizualizacija podatkov

# Graf delovno aktivnih moških glede na izobrazbo
graf_izobrazba.moski <- ggplot(filter(tabela_po_izobrazbi, spol == 'Moški'), 
                          aes(x=leto, y=stevilo, col=izobrazba)) + geom_point(size=3) + 
  ggtitle("Delovno aktivni moški glede na izobrazbo") + ylab("Število v 1000") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Graf delovno aktivnih žensk glede na izobrazbo
graf_izobrazba.zenske <- ggplot(filter(tabela_po_izobrazbi, spol == 'Ženske'), 
                           aes(x=leto, y=stevilo, col=izobrazba)) + geom_point(size=3) +
  ggtitle("Delovno aktivne ženske glede na izobrazbo") + ylab("Število v 1000") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Graf delovno aktivnih v letu 2019 glede na poklicno skupino in spol
graf_poklicna_skupina.2019 <- ggplot(filter(tabela_po_poklicni_skupini, leto == 2019), aes(x=stevilo, y=poklicna_skupina, fill=spol)) + 
  geom_bar(stat='identity', position='dodge') + theme_minimal() + 
  labs(title="Delovno aktivni v 2019", x="Število v 1000", y = "Poklicna skupina") +
  scale_fill_manual(values=c('skyblue1', 'plum3'))

# Graf delovno aktivnih tehnikov in drugih strokovnih sodelavcev glede na spol
graf_tehniki <- ggplot(filter(tabela_po_poklicni_skupini, poklicna_skupina == 'Tehniki in drugi strokovni sodelavci'), 
       aes(x=leto, y=stevilo, fill=spol)) + 
  geom_bar(stat='identity', position='dodge') + theme_minimal() + 
  labs(title="Delovno aktivni tehniki in drugi strokovni sodelavci", x="Leto", y = "Število v 1000") + 
  coord_cartesian(ylim=c(50,90)) + scale_fill_manual(values=c('skyblue1', 'plum3'))

# Zemljevida

zemljevid <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", 
                             "gadm36_SVN_1", encoding = "UTF-8")

zemljevid$NAME_1[zemljevid$NAME_1 == "GoriĹˇka"] <- "Goriška"
zemljevid$NAME_1[zemljevid$NAME_1 == "KoroĹˇka"] <- "Koroška"
zemljevid$NAME_1[zemljevid$NAME_1 == "Notranjsko-kraĹˇka"] <- "Notranjsko-kraška"
zemljevid$NAME_1[zemljevid$NAME_1 == "Obalno-kraĹˇka"] <- "Obalno-kraška"


glede_na_povrsino <- inner_join(povrsina_regij, filter(tabela_po_statisticni_regiji, leto == 2019),
                     by = NULL, copy = FALSE, suffix = c(".regija", ".regija")) %>% select(-leto) %>% 
  mutate(Število = (stevilo / povrsina))

narisi_glede_na_povrsino <- tm_shape(merge(zemljevid, glede_na_povrsino, by.x='NAME_1', by.y='regija')) + 
  tm_polygons('Število', palette = "Purples") + tm_layout(main.title = "Zemljevid števila delovno aktivnih \nna kvadratni kilometer regije") + 
  tm_text(text='NAME_1', size=0.6)

glede_na_prebivalce <- inner_join(filter(prebivalstvo_regij, leto == 2019), filter(tabela_po_statisticni_regiji, leto == 2019),
                                by = NULL, copy = FALSE, suffix = c(".regija", ".regija")) %>% select(-leto) %>% 
  mutate(Delež = ((stevilo * 100) / prebivalstvo))

narisi_glede_na_prebivalce <- tm_shape(merge(zemljevid, glede_na_prebivalce, by.x='NAME_1', by.y='regija')) + 
  tm_polygons('Delež', palette = "Purples") + tm_layout(main.title = "Zemljevid deleža delovno aktivnih \nglede na število vseh prebivalcev regije") + 
  tm_text(text='NAME_1', size=0.6)
