## Estimation des salaires par rapport à l'avance ------

library("Hmisc")
library("dplyr")
library("ggplot2")

## Lecture du fichier -----

v.avance <- read.csv("/Users/gaston/Desktop/ifp/advance_balance.csv", sep = ";")

v.avance$diff_balance <- v.avance$Balance.2014 - v.avance$Balance.2013

names(v.avance)

v.avance %>%
  ggplot(aes(Balance.2013)) +
  geom_density( alpha = 0.5, adjust = 1)

v.avance %>%
  ggplot(aes(diff_balance)) +
  geom_density( alpha = 0.5, adjust = 1)

d.avance <-
  rbind(d04.avance %>% mutate(date = as.factor("2004 ajusté"),
                              Advance = Advance*2.27,
                              Balance = Balance*2.27),
        d14.avance %>% mutate(date = as.factor(2014)))


d04.avance.04 <-
  d04.avance %>%
  mutate( id.interview =as.integer(as.numeric(levels(id.interview))[id.interview])) %>%
  filter( year == "2004") %>%
  arrange(id.interview) %>%
  select(id.interview, Advance, Balance, year, Total.memb)



d04.avance.04$id.interview

d14.avance.14 <-
  d14.avance %>%
  select(id.interview, Advance, Balance, year, Total.memb) %>%
  filter( year == "2014") %>%
  arrange(id.interview)


inner_join(d04.avance.04, d14.avance.14, by = "id.interview")

dv.avance <-
  rbind(d04.avance.04 %>%
          mutate(year = as.factor("2004 ajusté"),
                 Advance = Advance*2.27,
                 Balance = Balance*2.27),
        v.avance %>%
          mutate(year = as.factor(2014),
                 Balance = diff_balance,
                 Advance = Advance.2014,
                 Total.memb = d04.avance.04$Total.memb) %>%
          select(id.interview, Advance, Balance, year, Total.memb))
  

dv.avance %>%
  ggplot(aes(Balance, fill = year)) +
  geom_density( alpha = 0.5, adjust = 2)

summary( dv.avance %>% filter( year == 2014))
summary( dv.avance %>% filter( year == "2004 ajusté"))

# avance Pre Venkat COPIE ---------------


d04.avance <- d04$`23-advance`
d14.avance <- d14$`23-advance`

summary(d04.avance$Advance)
summary(d14.avance$Advance)

summary(d04.avance$Balance)
summary(d14.avance$Balance)

summary(d04.avance$Total.memb)
summary(d14.avance$Total.memb)

d04.avance %>%
  group_by(year) %>%
  summarise(mean(Total.memb))

d14.avance %>%
  group_by(year) %>%
  summarise(mean(Total.memb))

names(d04.avance)
names(d14.avance)

d.avance <-
  rbind(d04.avance %>% mutate(date = as.factor("2004 ajusté"),
                              Advance = Advance*2.27,
                              Balance = Balance*2.27),
        d14.avance %>% mutate(date = as.factor(2014)))

d.avance %>%
  ggplot(aes(Advance, fill = date)) +
  geom_density( alpha = 0.5, adjust = 3)

d.avance %>%
  ggplot(aes(Balance, fill = date)) +
  geom_density( alpha = 0.5, adjust = 1)

d.avance %>%
  filter(date == "2004 ajusté") %>%
  ggplot(aes(Balance)) +
  geom_density( alpha = 0.5, adjust = 1)


d.avance %>%
  filter(date == "2014") %>%
  ggplot(aes(Balance)) +
  geom_density( alpha = 0.5, adjust = 1)


d.avance %>%
  filter(date == "2014", year == "2013") %>%
  ggplot(aes(Balance)) +
  geom_density( alpha = 0.5, adjust = 1)

summary(d04.avance$Balance)

## Calcul d'un salaire ------

dv.avance <-
  dv.avance %>%
  mutate( salaire = (Advance + Balance)/Total.memb,
          salaire.menage = (Advance + Balance))
# attention: diviser par VRAIS tot.m


dv.avance %>%
  ggplot(aes(salaire, fill = year)) +
  geom_density( alpha = 0.5, adjust = 2)

dv.avance %>%
  ggplot(aes(salaire.menage, fill = year)) +
  geom_density( alpha = 0.5, adjust = 2)


summary( dv.avance %>% filter( year == "2004 ajusté"))
summary( dv.avance %>% filter( year == 2014))


# Balances négatives -----------

100*length(which(v.avance$Balance.2014 < 0))/nrow(v.avance)
# 41% repartent avec une avance négative

100*length(which((d04.avance %>% filter(year == "2004"))$Balance < 0))/nrow(v.avance)
# 38%