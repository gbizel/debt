## Etude de l'explosion de l'endettement

## Attention :: les salaires sont probablement FAUX. Données copiées
## --> se focaliser sur les assets

library("Hmisc")
library("dplyr")
library("ggplot2")

# Debt setup


d04 <- mdb.get("/Users/gaston/Desktop/ifp/base_2004/debtbondage/debt-fusion-15-6-05-9.24am.mdb")
d14 <- mdb.get("/Users/gaston/Desktop/ifp/base_2014/debt-fusion-data2014_FINAL.mdb")


# Database ----

d04.borrow <-
  d04$`12-borrowing practice` %>%
  group_by(id.interview) %>%
  summarize(total.outstanding = sum(outstanding))

d14.borrow <-
  d14$`12-borrowing practice` %>%
  group_by(id.interview) %>%
  summarize(total.outstanding = sum(outstanding))

d.borrow <-
  rbind(d04.borrow %>% mutate(date = as.factor("2004 ajusté"),
                              total.outstanding = total.outstanding*2.27),
        d14.borrow %>% mutate(date = as.factor(2014)))

d.borrow.median <-
  d.borrow %>%
  group_by(date) %>%
  summarize( median = mean(total.outstanding, na.rm = T))

d.borrow %>%
  ggplot( aes(x=total.outstanding, fill = date) ) +
  geom_density(alpha = .5, position = "identity", adjust = 2) +
  geom_vline(data=d.borrow.median, aes(xintercept=median,  colour=date),
             linetype="dashed", size=1)


# comparaison aux assets -----

d14.borrow.assets <-
  inner_join(d14.borrow,
           d14$`7-assets` %>%
             group_by(id.interview) %>%
             summarize(assets.value = sum(approxi.value)) %>%
             mutate( date = as.factor(2014)),
           by = "id.interview")

d04.borrow.assets <-
  inner_join(d04.borrow,
           d04$`7-assets` %>%
             group_by(id.interview) %>%
             summarize(assets.value = sum(approxi.value)) %>%
             mutate( date = as.factor(2004)),
           by = "id.interview")

d.borrow.assets <-
  rbind(d04.borrow.assets, d14.borrow.assets) %>%
  mutate(ratio = total.outstanding/assets.value)


d.borrow.assets.median <-
  d.borrow.assets %>%
  group_by(date) %>%
  summarize( median = median(ratio, na.rm = T))

d.borrow.assets.mean <-
  d.borrow.assets %>%
  group_by(date) %>%
  summarize( mean = mean(ratio, na.rm = T))

## FINAL GRAPH
## TO EXPLAIN. ###########

d.borrow.assets %>%
  group_by(date) %>%
  summarise(max(ratio, na.rm = T))

temp <-
  d.borrow.assets %>%
  arrange(desc(ratio)) %>%
  filter(date == 2004)

d.borrow.assets %>%
#  filter(date == "2004") %>%
  ggplot( aes(x = ratio, fill = date) ) +
  geom_density(alpha = .5, position = "identity", adjust = 1) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(data=d.borrow.assets.median, aes(xintercept=median,  colour=date),
#  geom_vline(data=d.borrow.assets.mean, aes(xintercept=mean,  colour=date),
               linetype="dashed", size=1)


## SLIDES::::::

length(which((d.borrow.assets %>% filter(date =="2014"))$ratio > 2))/nrow(d.borrow.assets)
length(which((d.borrow.assets %>% filter(date =="2004"))$ratio > 2))/nrow(d.borrow.assets)


## PREMIER DECILE

quantile( ( d.borrow.assets %>% filter(date == "2004") )$assets.value,
          prob = seq(0, 1, length = 11))

quantile( ( d.borrow.assets %>% filter(date == "2014") )$assets.value,
          prob = seq(0, 1, length = 11))


d.borrow.first.decile <-
  rbind(d.borrow.assets %>%
        filter( date == 2004, assets.value < 4500  ),
      d.borrow.assets %>%
        filter( date == 2014, assets.value < 19000  ))

d.borrow.first.decile %>%
  filter(date == "2004") %>%
  ggplot( aes(x = ratio, fill = date) ) +
  geom_density(alpha = .5, position = "identity", adjust = 2)
#  coord_cartesian(xlim = c(0, 10)) +

## pic de densité en 2004 pour 3
## en 2014 pour 50!!
## tout ce qu'ils consomment

## Mediane par décile -------------

q.asset.04 <-
  quantile( ( d.borrow.assets %>% filter(date == "2004") )$assets.value,
          prob = seq(0, 1, length = 11))

q.asset.14 <-
  quantile( ( d.borrow.assets %>% filter(date == "2014") )$assets.value,
          prob = seq(0, 1, length = 11))

# Dec1
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[2]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[2]) %>%
  summarise(median(ratio, na.rm = T))

2.952381
60.57955

# Dec2
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[3],
         assets.value >= q.asset.04[2]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[3],
         assets.value >= q.asset.04[2]) %>%
  summarise(median(ratio, na.rm = T))

1.663985
10.80214

# Dec3
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[4],
         assets.value >= q.asset.04[3]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[4],
         assets.value >= q.asset.04[3]) %>%
  summarise(median(ratio, na.rm = T))

1.141111
7.083333

# Dec5
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[5],
         assets.value >= q.asset.04[4]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[5],
         assets.value >= q.asset.04[4]) %>%
  summarise(median(ratio, na.rm = T))

0.36
5.792952

# Dec6
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[6],
         assets.value >= q.asset.04[5]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[6],
         assets.value >= q.asset.04[5]) %>%
  summarise(median(ratio, na.rm = T))

0.3
4.318182

# Dec7
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[7],
         assets.value >= q.asset.04[6]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[7],
         assets.value >= q.asset.04[6]) %>%
  summarise(median(ratio, na.rm = T))

0.2302632
3.078947

# Dec8
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[8],
         assets.value >= q.asset.04[7]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[8],
         assets.value >= q.asset.04[7]) %>%
  summarise(median(ratio, na.rm = T))

0.2173913
1.9189

# Dec9
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[9],
         assets.value >= q.asset.04[8]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[9],
         assets.value >= q.asset.04[8]) %>%
  summarise(median(ratio, na.rm = T))

0.5142857
1.058659

# Dec10
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[10],
         assets.value >= q.asset.04[9]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[10],
         assets.value >= q.asset.04[9]) %>%
  summarise(median(ratio, na.rm = T))

0.3325877
0.5862745

# Dec10
d.borrow.assets %>%
  filter(date == 2004,
         assets.value < q.asset.04[11],
         assets.value >= q.asset.04[10]) %>%
  summarise(median(ratio, na.rm = T))

d.borrow.assets %>%
  filter(date == 2014,
         assets.value < q.asset.14[11],
         assets.value >= q.asset.04[10]) %>%
  summarise(median(ratio, na.rm = T))

0.1916667
0.3819444

## try differently : follow the first decile.

d.borrow.assets %>%
  filter( id.interview %in% (d.borrow.assets %>%
                               filter( date == 2004, assets.value < 4500  ))$id.interview )


which(d.borrow.assets$id.interview %in% (d.borrow.assets %>%
                     filter( date == 2004, assets.value < 4500  ))$id.interview)


## SUITE AVEC PRECAUTION... NON EXPLOITABLE
# comparaison aux salaires -----

# a faire plus tard:additionner tout ?

names(d14$`5-household structure`)

d04.household <- d04$`5-household structure`
d14.household <- d14$`5-household structure`

d14.borrow.salary <-
  inner_join(d14.borrow,
             d14$`5-household structure` %>%
               group_by(id.interview) %>%
               summarize(earnings.1 = sum(Earnings.1.1),
                         earnings.2 = sum(Earnings.2.1),
                         earnings.3 = sum(Earnings.3.1),
                         earnings.4 = sum(Earnings.4.1),
                         earnings.5 = sum(Earnings.5.2)) %>%
               mutate( date = as.factor(2014),
                       earnings.non = earnings.1 + earnings.2,
                       earnings.mig = earnings.3 + earnings.4 + earnings.5) %>%
               mutate( earnings.tot = earnings.non + earnings.mig),
             by = "id.interview")


d04.borrow.salary <-
  inner_join(d04.borrow,
             d04$`5-household structure` %>%
               group_by(id.interview) %>%
               summarize(earnings.1 = sum(Earnings.1.1),
                         earnings.2 = sum(Earnings.2.1),
                         earnings.3 = sum(Earnings.3.1),
                         earnings.4 = sum(Earnings.4.1),
                         earnings.5 = sum(Earnings.5.2)) %>%
               mutate( date = as.factor(2004),
                       earnings.non = earnings.1 + earnings.2,
                       earnings.mig = earnings.3 + earnings.4 + earnings.5) %>%
               mutate( earnings.tot = earnings.non + earnings.mig),
             by = "id.interview")

d.borrow.salary <-
  rbind(d04.borrow.salary, d14.borrow.salary) %>%
  mutate(ratio.salary = total.outstanding/earnings.tot)


d.borrow.salary.median <-
  d.borrow.salary %>%
  group_by(date) %>%
  summarize( median = median(ratio.salary, na.rm = T))

d.borrow.salary %>%
  ggplot( aes(x = ratio.salary, fill = date) ) +
  geom_density(alpha = .5, position = "identity", adjust = 2) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(data=d.borrow.salary.median, aes(xintercept=median,  colour=date),
             linetype="dashed", size=1)

# types de salaires ---------

names(d.borrow.salary)

ratio_mig_non = as.double(d.borrow.salary$earnings.non) / as.double(d.borrow.salary$earnings.mig)
ratio_mig_non[which(!is.finite(ratio_mig_non))] <- c(0,0)

d.borrow.salary <-
  d.borrow.salary %>%
  mutate(  ratio_nonmig_mig = ratio_mig_non )

summary(
  ( d.borrow.salary %>%
    filter(date == 2014) )$ratio_nonmig_mig
)

summary(
  ( d.borrow.salary %>%
      filter(date == 2004) )$ratio_nonmig_mig
)

d.borrow.salary %>%
  filter(date == 2014) %>%
  ggplot(aes(ratio_nonmig_mig)) +
  geom_density()

summary(
  as.factor(
    (d.borrow.salary %>%
       filter(date == 2004))$earnings.non
  ))

summary(
  as.factor(
    (d.borrow.salary %>%
       filter(date == 2014))$earnings.non
  ))


## En 2004, seulement 3% des gens avaient une activité Non_migration
## En 2014, 100% des individus ont une activité non_migratoire


summary(as.factor(d14.household$Non.mig.occup.1)) / nrow(d14.household)
# main (30%): Agriculture work on own land
# 15%: NREGA, Agriculture coolie, Self Employment, Transport workers
summary(as.factor(d14.household$Non.mig.occup.2))
#nothing

summary(as.factor(d14.household$Mig.occup.1))
summary(as.factor(d14.household$Mig.occup.2))

summary(as.factor(d04.household$Mig.occup.1))
summary(as.factor(d04.household$Mig.occup.2))


# etude specifique pour ceux qui travaillent brick_sugar :

i04_brick = c(which(d04.household$Mig.occup.1 == 2),
              which(d04.household$Mig.occup.2 == 2))
i04_sugar = which(d04.household$Mig.occup.1 == 8)

i14_brick = c(which(d14.household$Mig.occup.1 == 2),
              which(d14.household$Mig.occup.2 == 2))
i14_sugar = which(d14.household$Mig.occup.1 == 8)

id04_brick = unique((d04.household[i04_brick,])$id.interview)
id04_sugar = unique((d04.household[i04_sugar,])$id.interview)  

id14_brick = unique((d14.household[i14_brick,])$id.interview)
id14_sugar = unique((d14.household[i14_sugar,])$id.interview)  

#brick:
d04.brick <-
  d04.household %>%
  filter( Mig.occup.1 == 2 | Mig.occup.2 == 2 ) %>%
  group_by(id.interview) %>%
  summarize( bricks1 =  sum(Earnings.3.1),
             bricks2 = sum(Earnings.4.1)) %>%
  mutate(bricks = bricks1 + bricks2) %>%
  select(id.interview, bricks1, bricks2, bricks)

d14.brick <-
  d14.household %>%
  filter( Mig.occup.1 == 2 | Mig.occup.2 == 2 ) %>%
  group_by(id.interview) %>%
  summarize( bricks1 =  sum(Earnings.3.1),
             bricks2 = sum(Earnings.4.1)) %>%
  mutate(bricks = bricks1 + bricks2) %>%
  select(id.interview, bricks1, bricks2, bricks)

d.brick <-
  rbind(d04.brick %>% mutate(date = as.factor(2004)),
        d14.brick %>% mutate(date = as.factor(2014)))

d.brick %>%
  ggplot(aes(bricks, fill = date)) +
  geom_density(alpha = .5, adjust = .6)

## les données sont identiques, des zeros en plus.

summary(d04.household$Earnings.3.1)
summary(d14.household$Earnings.3.1)
# = .

summary(d04.household$Earnings.4.1)
summary(d14.household$Earnings.4.1)
# = .


# stat des: assets ----------

d04.asset <-
  d04$`7-assets` %>%
  arrange(id.interview)

d14.asset <-
  d14$`7-assets` %>%
  arrange(id.interview)

names(d04.asset)

d.asset <-
  rbind(d04.asset %>% mutate(date = as.factor(2004)),
        d14.asset %>% mutate(date = as.factor(2014)))

summary(d04.asset)
summary(d14.asset)
# toutes variables identiques a part asset.value....

d.asset %>%
  ggplot(aes(assets, fill = date)) +
  geom_bar(position = "dodge")


d.asset.test <-
  inner_join(d04.asset, d14.asset, by = "id.interview")


names(d04.asset)

d14.asset %>%
  ggplot(aes(approxi.date)) +
  geom_density(adjust = 2)

# avance ---------------


d04.avance <- d04$`23-advance`
d14.avance <- d14$`23-advance`

summary(d04.avance$Advance)
summary(d14.avance$Advance)

summary(d04.avance$Balance)
summary(d14.avance$Balance)


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




