## Etude descriptive des pratiques d'empreint =========

## PROBLEME:
## Les sources ont été copiées et les chiffres modifiées
## Attention: cela ressemble à du random: toutes les moyennes sont ~égales...
## Il existe des 0 et des NA dans source.of.cash 2004,
## Ils ont été remplacés par des sources en 2014,
## créant souvent des doublons de source incohérents (foyer 1010 par exemple)

library("Hmisc")
library("dplyr")
library("ggplot2")

# Importation des données --------

d04.practices <-
  d04$`12-borrowing practice` %>%
  arrange(id.interview) %>%
  filter(source.of.cash > 0) %>%
  mutate(lender = match_levels(data = d04.practices$source.of.cash,
                                names = d04$`X_12-source-cash`)) %>%
  select(id.interview, source.of.cash,  lender, outstanding) 


d14.practices <-
  d14$`12-borrowing practice` %>%
  arrange(id.interview) %>%
  mutate(lender = match_levels(data = d14.practices$source.of.cash,
                               names = d14$`X_12-source-cash`)) %>%
  select(id.interview, source.of.cash,  lender, outstanding) 
  


# Etude descriptive -----------


d04.practices %>%
  group_by(lender) %>%
  summarise(sum(outstanding, na.rm = T)/41024.61)

# 1  Relative/friends                           3.71667153
# 2    Wealthy people                          16.98151427
# 3     money lenders                           8.75876699
# 4           maistry                          44.58055787
# 5        snopkeeper                           0.46313664
# 6      grocery shop                           2.41077246
# 7       pawnbrocker                          18.41699409
# 8               SHG                           0.05362635
# 9              bank                           3.82575240
# 10            other                           0.79220741

d14.practices %>%
  group_by(lender) %>%
  summarise(sum(outstanding, na.rm = T)/385477)

# 1  Relative/friends                         17.6578629
# 2    Wealthy people                         12.8295592
# 3     money lenders                         12.6381081
# 4           maistry                         23.0532561
# 5        snopkeeper                          7.6580444
# 6      grocery shop                          8.6905315
# 7       pawnbrocker                         15.5205110
# 8               SHG                          0.5707215
# 9              bank                          1.0208132
# 10            other                          0.3605922

d14.practices %>%
  summarise(sum(outstanding, na.rm = T))

d04.practices %>%
  group_by(lender) %>%
  summarise(med = median(outstanding, na.rm = T),
            mean = mean(outstanding, na.rm = T))

d14.practices %>%
  group_by(lender) %>%
  summarise(med = median(outstanding, na.rm = T),
            mean = mean(outstanding, na.rm = T))

summary(d14.practices %>%
          filter(source.of.cash == 1) )

d04.practices %>%
  ggplot( aes(x=outstanding, fill = lender) ) +
  geom_density(alpha = .5, position = "identity", adjust = 6) +
  coord_cartesian(xlim = c(0,10000))

d14.practices %>%
  ggplot( aes(x=outstanding, fill = lender )) +
  geom_density(alpha = .5, position = "identity", adjust = 4) 


nrow(d04.practices)
nrow(d14.practices)

d04.practices %>%
  group_by(lender) %>%
  summarise(n())

d14.practices %>%
  group_by(lender) %>%
  summarise(n())

nrow(d14.practices)/length(unique(d14.practices$id.interview))
# 4.5 sources différentes en moyenne.

## Very similar ratio -- etude manuelle : les données ont été copiées
## NE PAS UTILISER SOURCE.OF.CASH pour 2014

typeof(d04.practices$source.of.cash)

# Comparaison avec RUME ---------


r.X.source <- data.frame( id = c(1:13),
                          from = c("Well known people",
                                   "Relatives",
                                   "employer",
                                   "maistry",
                                   "colleague",
                                   "pawn broker",
                                   "shop keeper",
                                   "finance",
                                   "friends",
                                   "SHG",
                                   "Banks",
                                   "Coop bank",
                                   "Sugar mills"))



names(r.main)

r.main$X6.1.C.From

r.practices <-
  r.main %>%
  mutate(from.code = as.factor(X6.2.B.From), outstanding = X6.1.I.Amount.of.loan) %>%
  mutate(from = match_levels(data = r.main$X6.2.B.From,
                             names = r.X.source)) %>%
  select(Code.family, from, from.code, outstanding) %>%
  arrange(Code.family)

r.practices %>%
  filter(from.code %in% c(1:9)) %>%
  ggplot( aes(x=outstanding, fill = from )) +
  geom_density(alpha = .5, position = "identity", adjust = 2) +
  coord_cartesian(xlim = c(0,100000))

r.practices %>%
  group_by(from) %>%
  summarise(med = median(outstanding, na.rm = T),
            mean = mean(outstanding, na.rm = T))
