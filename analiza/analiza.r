# 4. faza: Analiza podatkov

# Graf za napoved števila delovno aktivnih moških z visokošolsko ali višješolsko izobrazbo

visokoizobrazeni_moski <- tabela_po_izobrazbi %>% filter(izobrazba == 'Višješolska, visokošolska') %>%
  filter(spol == 'Moški')
visokoizobrazeni_moski$leto = as.numeric(as.character(visokoizobrazeni_moski$leto))

model_visokoizobrazeni.moski <- lm(stevilo ~ leto, data=visokoizobrazeni_moski)
l <- data.frame(leto=c(2020, 2021, 2022))
napoved_moski <- mutate(l, stevilo=predict(model_visokoizobrazeni.moski,l))

graf_napovedi_visokoizobrazeni.moski <- ggplot(visokoizobrazeni_moski, aes(x=leto, y=stevilo)) +
  geom_point() + geom_smooth(method=lm, fullrange=TRUE) +
  geom_point(data=napoved_moski, aes(x=leto, y=stevilo), color='plum1', size=3) +
  scale_x_continuous(breaks=2008:2022) + theme_minimal() + 
  labs(title = 'Napoved števila delovno aktivnih moških \nz visokošolsko ali višješolsko izobrazbo', 
       x = 'Leto', y = 'Število v 1000')

# Graf za napoved števila delovno aktivnih žensk z visokošolsko ali višješolsko izobrazbo

visokoizobrazene_zenske <- tabela_po_izobrazbi %>% filter(izobrazba == 'Višješolska, visokošolska') %>%
  filter(spol == 'Ženske')
visokoizobrazene_zenske$leto = as.numeric(as.character(visokoizobrazene_zenske$leto))

model_visokoizobrazene.zenske <- lm(stevilo ~ leto, data=visokoizobrazene_zenske)
napoved_zenske <- mutate(l, stevilo=predict(model_visokoizobrazene.zenske,l))

graf_napovedi_visokoizobrazene.zenske <- ggplot(visokoizobrazene_zenske, aes(x=leto, y=stevilo)) +
  geom_point() + geom_smooth(method=lm, fullrange=TRUE) +
  geom_point(data=napoved_zenske, aes(x=leto, y=stevilo), color='plum1', size=3) +
  scale_x_continuous(breaks=2008:2022) + theme_minimal() + 
  labs(title = 'Napoved števila delovno aktivnih žensk \nz visokošolsko ali višješolsko izobrazbo', 
       x = 'Leto', y = 'Število v 1000')
