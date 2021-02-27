# LIBEK ----
library(tidyverse)
library(readxl)
library(dplyr)
library(plotly)
library(datasets)
library(psych)


# BEOLVASÁS ----

df <- read_excel("test_data.xlsx")

# ÁTTEKINTÉS ----
str(df)
head(df)

# ADATOK ELŐKÉSZÍTÉSE ----

## a numerikus valtozok kivetelevel minden valtozo faktor
for (i in setdiff(1:18, c(1,3,7,8,16,18)))
  df[[i]] <- factor(df[[i]])

## faktorokon belüli szintek meghatározása
levels(df$nem) <- c(
  "férfi",
  "nő")

levels(df$jatszik) <- c(
  "igen",
  "nem")

levels(df$jatek_neve) <- c(
  "Call of Duty",
  "Candy Crush",
  "CS GO",
  "Diablo 3",
  "Fifa",
  "Forza Horizon",
  "GTA V.",
  "Hearth Stone",
  "League of Legends",
  "Mahjong",
  "Nem játszik",
  "Need for Speed: Carbon",
  "Pasziánsz",
  "Pirates of the Caribbean",
  "Rocket League",
  "Subway Surfers",
  "World of Warcraft"
)

levels(df$foglalkozas) <- c(
  "dolgozik",
  "egyéb",
  "nyugdíjas",
  "tanul"
)

levels(df$eszkoz) <- c (
  "konzol",
  "nem játszik",
  "PC",
  "telefon"
)

levels(df$streamer) <- c (
  "igen",
  "nem"
)

levels(df$youtuber) <- c (
  "igen",
  "nem"
)

levels(df$f2p_fizetos) <- c (
  "fizetős",
  "ingyenes",
  "nem játszik"
)

levels(df$elofizetes) <- c (
  "igen",
  "nem"
)

levels(df$vasarlasolt_valaha) <- c (
  "igen",
  "nem"
)

levels(df$mit_vasarol) <- c (
  "autó",
  "fizetőeszköz",
  "kredit",
  "loot crate",
  "nem vásárolt",
  "paklik",
  "riot pont",
  "skin",
  "szolgáltatás",
  "wow token"
)


# ÖSSZEFOGLALÓK: ----
## nemek eloszlása : ----
  # forrás : https://www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf

# az attach utasítással elérhetőek lesznek az egyes változók
attach(df)

nem_szazalek <- table(nem)
nem_szazalek
addmargins(nem_szazalek)
  # férfi:  38
  # nő:     25
  #összesen:63

round(100*prop.table(nem_szazalek), digits = 2)
  # férfi:  60,32
  # nő:     39,68

## összefoglalók életkor szerint ----

## átlagéletkor az összes rekordra nézve

mean(df$kor)
  # átlagéletkor: 28,23 év

  ## nemenkénti átlagéletkor

by(df$kor, df$nem, summary)
  # átlagéletkor a férfiaknál: 26,21
  # átlagéletkor a nőknél: 31,32


## összefoglalók aszerint, hogy hányan játszanak ----

jatszik_szazalek <- table(jatszik)
jatszik_szazalek
addmargins(jatszik_szazalek)
  # játszik:      45
  # nem játszik:  18
  # összesen:     63

round(100*prop.table(jatszik_szazalek), digits = 2)
  # jatszik:      71,43
  # nem játszik:  28,57

## összefoglalók aszerint, hogy milyen játékkal játszanak: ----
jatek_neve_szazalek <- table(jatek_neve)
jatek_neve_szazalek
addmargins(jatek_neve_szazalek)
  # call of duty:       1         # Mahjong:            2
  # candy crush:        1         # Nem játszik:        17 
  # cs go:              3         # Need for Speed:     1
  # diablo 3:           1         # Pasziánsz:          3
  # fifa:               4         # Pirates.:           1
  # forza horizon:      1         # Rocket League:      5
  # GTA V.:             1         # Subway Surfers:     1
  # Hearth Stone:       1         # World of Warcraft:  7
  # League of Legends: 13         # összesen:           63


round(100*prop.table(jatek_neve_szazalek), digits = 2)
  # Call of Duty:       1,59%     # Mahjong:            3,17%
  # Candy Crush:        1,59%     # Nem játszik:        26,98% 
  # CS GO:              4,76%     # Need for Speed:     1,59%
  # Diablo 3:           1,59%     # Pasziánsz:          4,76%
  # Difa:               6,35%     # Pirates.:           1,59%
  # Forza Horizon:      1,59%     # Rocket League:      7,94%
  # GTA V.:             1,59%     # Subway Surfers:     1,59%
  # Hearth Stone:       1,59%     # World of Warcraft:  11,11%
  # League of Legends:  20,63%    # összesen:           100%


## összefoglalók a játékkal töltött órák száma szerint (átlagosan): ----
df %>%
  group_by(jatek_neve) %>%
  summarise_each(mean, jatek_orak_num) %>%
  arrange(desc(jatek_orak_num))


# VIZUALIZÁCIÓK: ----

# nemek arányai (histogram)
plot_ly(x = ~df$nem, type = "histogram")%>%
  layout(title="Nemek arányai")

# nemek aránya (kördiagram)
nem_labels = c('férfi', 'nő')
nem_values = c(38, 25)

nemek_piechart <- plot_ly(type='pie', labels=nem_labels, values=nem_values, 
                      textinfo='label+percent',
                      insidetextorientation='radial') %>%
  layout(title="Férfiak és nők arányai")
nemek_piechart


# kitöltők életkora
plot_ly(x = ~df$kor) %>%
  layout(title="A kitöltők életkora")

# kitöltők életkora
plot_ly(x = ~df$ID, y= ~df$kor) %>%
  layout(title="A kitöltők életkora (hisztogram)")

plot_ly(data = df, x = ~ID, y = ~kor, color = ~nem, colors = "Set2") %>%
  layout(title="A kitöltők életkora (scatter)")


# a kitöltők arányai aszerint, hogy mivel játszanak:

plot_ly(x = ~df$jatek_neve, type = "histogram")%>%
  layout(title="A kitöltők ezekkel játszanak")

# mivel játszanak (kördiagram)
jatek_labels = c('Call of Duty', 'Candy Crush', 'CS GO', 'Diablo 3',
                 'Fifa','Forza','GTA V.','Hearth Stone','LoL','Mahjong',
                 'nem játszik','NFS','pasziánsz','Pirates of the Caribbean','Rocket League',
                 'Subway Surfers','WoW')
jatek_values = c(1, 1, 3, 1, 4, 1, 1, 1, 13, 2, 17, 1, 3, 1, 5, 1,7)

jatekok_piechart <- plot_ly(type='pie', labels=jatek_labels, values=jatek_values, 
                      textinfo='label+percent',
                      insidetextorientation='radial') %>%
  layout(title="Játékok arányai")
jatekok_piechart


# MODELLEK : ----




# valószínűségi mintavétel ----
nonprobabilitic = df[1:63,]
summary(nonprobabilitic)


# adatok előkészítése a további munkához ----
set.seed(1234)

training = nonprobabilitic[1:45, -5]
testing = nonprobabilitic[46:63, -5]
summary(training)
summary(testing)

# lineáris regressziós modellek létrehozása ----

model = lm(vasarlas_erteke~., data = training)
summary(model)

  # az eszköz, előfizetés és éz hogy játszik-e bizonyultak fontos változóknak

# modell plottolása
p = plot(training$eszkoz, training$vasarlas_erteke)
abline(model, col="red")
pairs.panels(df[1:63,2:5])

