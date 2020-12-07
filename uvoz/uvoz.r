# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

uvozi.povrsina_regij <- function() {
  link <- "https://sl.wikipedia.org/wiki/Statisti%C4%8Dne_regije_Slovenije"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec="")
  colnames(tabela) <- c("stevilo", "regija", "prebivalci_2018", "povrsina")
  for (col in c("prebivalci_2018", "povrsina")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  tabela$regija[tabela$regija == "Pomurska regija"] <- "Pomurska"
  tabela$regija[tabela$regija == "Podravska regija"] <- "Podravska"
  tabela$regija[tabela$regija == "Koroška regija"] <- "Koroška"
  tabela$regija[tabela$regija == "Savinjska regija"] <- "Savinjska"
  tabela$regija[tabela$regija == "Zasavska regija"] <- "Zasavska"
  tabela$regija[tabela$regija == "Posavska regija"] <- "Posavska"
  tabela$regija[tabela$regija == "Jugovzhodna regija"] <- "Jugovzhodna"
  tabela$regija[tabela$regija == "Osrednjeslovenska regija"] <- "Osrednjeslovenska"
  tabela$regija[tabela$regija == "Gorenjska regija"] <- "Gorenjska"
  tabela$regija[tabela$regija == "Primorsko-notranjska regija"] <- "Primorsko-notranjska"
  tabela$regija[tabela$regija == "Goriška regija"] <- "Goriška"
  tabela$regija[tabela$regija == "Obalno-kraška regija"] <- "Obalno-kraška"
  tabela <- tabela[c(2, 4)]
  return(tabela)
}

uvozi.izobrazba <- function() {
  po_izobrazbi <- read_csv2("podatki/po_stopnjah_dosezene_izobrazbe_in_spolu.csv", skip=2,
                            locale=locale(encoding="Windows-1250"))
  colnames(po_izobrazbi) <- c("drzava", "spol", "izobrazba", 2008:2019)
  for (col in c(4:15)) {
    if (is.character(po_izobrazbi[[col]])) {
      po_izobrazbi[[col]] <- parse_number(po_izobrazbi[[col]], na="-", locale=sl)
    }
  }  
  po_izobrazbi <- po_izobrazbi[ -c(1)] %>%
    pivot_longer(!c('spol', 'izobrazba'), names_to = 'leto', values_to = "stevilo")
  return(po_izobrazbi)
}

uvozi.poklicna_skupina <- function() {
  po_poklicni_skupini <- read_csv2("podatki/po_poklicnih_skupinah_in_spolu.csv", skip=2,
                            locale=locale(encoding="Windows-1250"), na = c("N", "NA"))
  colnames(po_poklicni_skupini) <- c("drzava", "spol", "poklicna_skupina", 2008:2019)
  for (col in c(4:15)) {
    if (is.character(po_poklicni_skupini[[col]])) {
      po_poklicni_skupini[[col]] <- parse_number(po_poklicni_skupini[[col]], na="-", locale=sl)
    }
  }
  po_poklicni_skupini <- po_poklicni_skupini[ -c(1)] %>%
    pivot_longer(!c('spol', 'poklicna_skupina'), names_to = 'leto', values_to = "stevilo")
  return(po_poklicni_skupini)
}

uvozi.regije <- function() {
  po_statisticni_regiji <- read_csv2("podatki/po_statisticnih_regijah.csv", skip=2,
                                   locale=locale(encoding="Windows-1250"), na = c("N", "NA"))
  colnames(po_statisticni_regiji) <- c("regija", "spol", 2005:2019)
  for (col in c(3:17)) {
    if (is.character(po_statisticni_regiji[[col]])) {
      po_statisticni_regiji[[col]] <- parse_number(po_statisticni_regiji[[col]], na="-", locale=sl)
    }
  }
  po_statisticni_regiji <- po_statisticni_regiji[ -c(2:5)] %>% 
    pivot_longer(!regija, names_to = "leto", values_to = "stevilo")
  return(po_statisticni_regiji)
}

uvozi.prebivalstvo_regij <- function() {
  prebivalstvo_regij <- read_csv2("podatki/prebivalstvo_statisticnih_regij.csv", skip=2,
                                     locale=locale(encoding="Windows-1250"))
  colnames(prebivalstvo_regij) <- c("spol", "regija", 1990:2020)
  for (col in c(3:33)) {
    if (is.character(prebivalstvo_regij[[col]])) {
      prebivalstvo_regij[[col]] <- parse_number(prebivalstvo_regij[[col]], na="-", locale=sl)
    }
  }
  prebivalstvo_regij <- prebivalstvo_regij[ -c(1, 3:20, 33)] %>% 
    pivot_longer(!regija, names_to = "leto", values_to = "prebivalstvo")
  return(prebivalstvo_regij)
}


# podatki, zapisani v tabelo regije - površina regij v km^2
povrsina_regij <- uvozi.povrsina_regij()

# podatki, zapisani v tabela_po_izobrazbi (številski podatki so predstavljeni v 1000)
tabela_po_izobrazbi <- uvozi.izobrazba()

# podatki, zapisani v tabela_po_poklicni_skupini (številski podatki so predstavljeni v 1000)
tabela_po_poklicni_skupini <- uvozi.poklicna_skupina()

# podatki, zapisani v tabela_po_statisticni_regiji
tabela_po_statisticni_regiji <- uvozi.regije()

# podatki, zapisani v tabelo prebivalstvo_regij
prebivalstvo_regij <- uvozi.prebivalstvo_regij()


# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))