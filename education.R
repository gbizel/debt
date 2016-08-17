## Education -------

library(dplyr)
library(ggplot2)

d04.household <-
  d04.household %>%
  mutate(id.indiv = paste(id.interview,Nbr.member," "))

d14.household <-
  d14.household %>%
  mutate(id.indiv = paste(id.interview,Nbr.member," "))

d.household <-
  rbind(d04.household %>% mutate(date = as.factor(2004)),
        d14.household %>% mutate(date = as.factor(2014)))

names(d.household)


d.household %>%
  mutate( education = match_levels( data = d.household$education,
                                    names = d04$`X_5_E-Education`)) %>%
#  filter( age < 20 ) %>%
  ggplot(aes(education, fill = date)) +
  geom_bar( position = "dodge", alpha = .5)

d04.children <- d04$`33-14 Child labour`
d14.children <- d14$`33-14 Child labour`

d.children <-
  rbind(d04.children %>% mutate(date = as.factor(2004)),
        d14.children %>% mutate(date = as.factor(2014)))
names(d.children)

d04.household <-
  d04.household %>%
  mutate( id.indiv = as.numeric(paste(id.interview, Nbr.member, sep =""))) %>%
  arrange(id.indiv)

d14.household <-
  d14.household %>%
  mutate( id.indiv = as.numeric(paste(id.interview, Nbr.member, sep =""))) %>%
  arrange(id.indiv)

d.j.household <-
  inner_join(d04.household, d14.household, by = "id.indiv")

which(d.j.household$education.x != d.j.household$education.y)

d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  mutate(a = age.y - age.x)
  filter( education.y != education.x, education.y == 1 ) 
# 100% des enfants dont l'éducation a évolué est maintenant 2.
# 386 passent de 0,1 a 2
# 2 passent de 0 à 1

d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  mutate(ed = education.y-education.x) %>%
  filter( education.y < 2)
# 100% des 0,1 en 2014 l'étaient déjà en 2004.

# 100% des 0,1 de moins de 11 ans en 2004 sont maintenant 2.
d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  filter(age.x <= 11)
# Il y a 411 enfants

d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  filter(age.x <= 11, education.y < 2)
# 27 enfants n'ont pas atteint le niveau college

386/411
# 94% d'éducation

d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  filter(age.x == 10)

d.j.household %>%
  select(age.x,age.y,education.x,education.y) %>%
  mutate(a = age.y - age.x) %>%
  filter( a != 10 ) 
 


names(d.household)



d.children %>%
  group_by(date) %>%
  summarise(mean(X33.1.no.children))

# inner_join(d04.children, d14.children, by = "id.indiv")


# rume --------


names(r.family)

summary(as.factor(r.family$X1.G.Education))/19.28

100*summary(as.factor(d04.household$education))/(nrow(d04.household))


6.2759336+2.8526971+3.2157676+0.6224066+0.9336100
length(r.family$X1.G.Education)