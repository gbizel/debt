## Estimation des salaires par rapport à l'avance ------

library("Hmisc")
library("dplyr")
library("ggplot2")

## Lecture du fichier -----

v.avance <- read.csv("/Users/gaston/Desktop/ifp/advance_balance.csv", sep = ";")

v.avance$diff_balance <- v.avance$Balance.2014 - v.avance$Balance.2013

names(v.avance)

v.avance %>%
  ggplot(aes(Balance.2014)) +
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

dv.avance$salaire.indiv[dv.avance$salaire.indiv == Inf] <- NA

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

summary(d04.avance.04$Total.memb)
summary(d14.avance.14$Total.memb)

2.367+0.94
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

# GRAPH FINAL AVANCE 14 04:::::::

d.avance.median <-
  d.avance %>%
  group_by(date) %>%
  summarize( median = median(Advance, na.rm = T))

d.avance %>%
  ggplot(aes(Advance, fill = date)) +
  geom_density( alpha = 0.5, adjust = 3) +
  coord_cartesian(xlim = c(0,120000)) +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) +
  geom_vline(data=d.avance.median, aes(xintercept=median,  colour=date),
             linetype="dashed", size=1)

##::::::::::::::::

## Stat pour Slide 2:

# 2004: 90% in what
quantile((d.avance %>% filter(date == "2004 ajusté"))$Advance, c(1:10)/10) 

# 2014: 10% above what
quantile((d.avance %>% filter(date == "2014"))$Advance, c(.10, .90))

var((d.avance %>% filter(date == "2014"))$Advance)/var((d.avance %>% filter(date == "2004 ajusté"))$Advance)

##::::::::::::::::

d.avance %>%
  filter(date == "2014") %>%
  arrange(Advance) %>%
  select(Advance)

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
  rbind(
    dv.avance %>%
      filter(year=="2004 ajusté") %>%
      mutate( salaire.indiv = (Advance + Balance  + 800*34)/(Total.memb),
              salaire.menage = (Advance + Balance)),
    dv.avance %>%
      filter(year=="2014") %>%
      mutate( salaire.indiv = (Advance + Balance + 800*34)/(Total.memb + 1),
              salaire.menage = (Advance + Balance))
    )
    
    
# attention: diviser par VRAIS tot.m

#SLIDE VARIANCE:::::
var((dv.avance %>% filter(year == 2014))$salaire.menage, na.rm = T)/var((dv.avance %>% filter(year == "2004 ajusté"))$salaire.menage, na.rm = T)
length(which((dv.avance %>% filter(year == 2014))$salaire.menage > 0))

median((dv.avance %>% filter(year == 2014))$salaire.menage, na.rm = T)
median((dv.avance %>% filter(year == "2004 ajusté"))$salaire.menage, na.rm = T)
59950/20430

dv.avance.median.i <-
  dv.avance %>%
  group_by(year) %>%
  summarize( median = median(salaire.indiv, na.rm = T))

dv.avance.median.m <-
  dv.avance %>%
  group_by(year) %>%
  summarize( median = median(salaire.menage, na.rm = T))


# Salaire individuel:::::::::::::::
dv.avance %>%
  ggplot(aes(salaire.indiv, fill = year)) +
  geom_density( alpha = 0.5, adjust = 2) +
  geom_vline(data=dv.avance.median.i, aes(xintercept=median,  colour=year),
             linetype="dashed", size=1)  +
  coord_cartesian(xlim = c(0,76000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  

## Salaire menage:::::::::::::::::
dv.avance %>%
  ggplot(aes(salaire.menage, fill = year)) +
  geom_density( alpha = 0.5, adjust = 2) +
  geom_vline(data=dv.avance.median.m, aes(xintercept=median,  colour=year),
             linetype="dashed", size=1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())



summary( dv.avance %>% filter( year == "2004 ajusté"))
summary( dv.avance %>% filter( year == 2014))


# Balances négatives -----------

100*length(which(v.avance$Balance.2014 < 0))/nrow(v.avance)
# 41% repartent avec une avance négative

100*length(which((d04.avance %>% filter(year == "2004"))$Balance < 0))/nrow(v.avance)
# 38%

v.avance %>%
  filter(Balance.2014 <= 0) %>%
  summarise(mean(Balance.2014), median(Balance.2014))
# mean(Balance.2014) median(Balance.2014)
# 1          -22693.91               -21000

d04.avance %>%
  filter(Balance <= 0,
         year == "2004") %>%
  summarise(mean(Balance), median(Balance))
# mean(Balance) median(Balance)
# 1     -674.0659            -500


# Cercle vicieux ------


inner_join(
d04.avance %>%
  filter(year == "2004",
         Balance < 0) %>%
  select(id.interview, Balance)
,
d04.avance %>%
  filter(year == "2003",
         Balance < 0) %>%
  select(id.interview, Balance)
,
by = "id.interview")


inner_join(
  v.avance %>%
    filter(Balance.2013 < 0) %>%
    select(id.interview, Balance.2013)
  ,
  d04.avance %>%
    filter(year == "2003",
           Balance < 0) %>%
    select(id.interview, Balance)
  ,
  by = "id.interview")

v.avance %>%
  filter(Balance.2013 < 0 & Balance.2014 < 0) %>%
  select(id.interview, Balance.2013, Balance.2014)


61/145

-(21886-24078)/24078
# augmentation de 9%



## SLIDE BALANCE2014:::::::::::::::::::: ----------

  v.avance %>%
    ggplot(aes(Balance.2014)) +
    geom_density() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
  #  geom_vline(xintercept = c(-29000, -4700))
  #  geom_vline(xintercept = c(-36750, -9300.0))
    geom_vline(xintercept = c(-29000, -9300.0),linetype="dashed") #, color = "e44f2f")
# 25%

quantile(v.avance$Balance.2014, na.rm = T, c(1:20)/20)

# 30% entre -29000 et -4700


