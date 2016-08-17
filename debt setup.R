library("Hmisc")
library("dplyr")
library("ggplot2")

match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

d04 <- mdb.get("/Users/gaston/Desktop/ifp/base_2004/debtbondage/debt-fusion-15-6-05-9.24am.mdb")

d14 <- mdb.get("/Users/gaston/Desktop/ifp/base_2014/debt-fusion-data2014_FINAL.mdb")

d04.household <- d04$`5-household structure`
d14.household <- d14$`5-household structure`


mean.04 <- d04.household %>%
          group_by(id.interview) %>%
          summarise(nombre = n()) 
mean(mean.04$nombre)

mean.14 <- d14.household %>%
  group_by(id.interview) %>%
  summarise(nombre = n()) 
mean(mean.14$nombre)

  d04.household %>%
  group_by(id.interview) %>%
  summarise(nombre = n()) %>%
    summarise(moyenne = mean(nombre))
  
# mean(mean.04$nombre)


d04.household =
  d04.household %>%
  arrange(id.interview, Nbr.member) %>%
  mutate( id.person = paste(id.interview, Nbr.member))

d14.household =
  d14.household %>%
  arrange(id.interview, Nbr.member) %>%
  mutate( id.person = paste(id.interview, Nbr.member))

household = inner_join(d04.household,d14.household, by = "id.person")

household <-
  household %>%
  arrange(id.person) %>%
  select(id.person, id.interview.x, id.interview.y, relation.x, relation.y, name.x, name.y)

which(is.na(household$name.y))

household %>%
  group_by(id.interview.x) %>%

names(household)

# household =
#   household[ , c(26,1,27,2,28,3,29,4,30,5,31,6,32,7,33,8,34,9,35,10,36,11,37,12,38,13,39,40,14,15,16,17,18,19,20,21,22,23,24)]

match(
  substr(names(household),0,nchar(names(household))-2),
  names(d04.household)
)

str("dddd")

household.change =
  household %>%
  mutate(
    education = education.y - education.x,
    marital.status = marital.status.y - marital.status.x,
    nbr.year = nbr.year.y - nbr.year.x,
    Non.mig.occup = Non.mig.occup.1.y - Non.mig.occup.1.x,
    No.days.1.1 = No.days.1.1.y - No.days.1.1.x
  ) %>%
  select(
    id.person,
    relation.x,
    sexe.x,
    age.x,
    education,
    nbr.year,
    marital.status,
    Non.mig.occup,
    No.days.1.1 
    )

household.change %>%
  mutate( relation = match_levels(household.change$relation.x,d04$`X_5-relation_head`)) %>%
  ggplot(aes(x = nbr.year, fill = sexe.x)) +
  geom_bar(position = "identity", alpha = .3)

which()

1:51

rle( household$id.person )$lengths
# le foyer 169 a des doublons




household %>%
  group_by(id.interview.x) %>%
  summarise(nombre = n()) %>%
  summarise(moyenne = mean(nombre))





