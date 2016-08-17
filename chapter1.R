


# get data ----
db_path = "/Users/gaston/Desktop/ifp/base_2010/Database_Base_400_Final_Work.mdb"
rume <- mdb.get(db_path)

# functions ----
match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

# build databases ----

r.general <- rume$`T 1 General informations`
r.occupation <- rume$`T 2 Occupations`
r.family <- rume$`T 1-1 Family members`

r.chp1 <-
  r.occupation %>%
  right_join(r.general, by = c("Code.Family" = "Code.family")) %>%
  mutate( income = as.double(X2.D.Annual.Income),
          caste.code = as.factor(X1.6.b.Caste.Code)) %>%
  arrange(Code.Family, X2.A.Code.id.member)

# incomes ----

# individual incomes
r.chp1 %>%
  ggplot(aes(x = income, fill = caste.code)) +
  geom_density(position = "identity", alpha = .5, adjust = 5) +
  scale_x_continuous(limits = c(0,200000))

r.chp1 %>%
  ggplot(aes(x = income, fill = X2.A.Code.id.member)) +
  geom_density(position = "identity", alpha = .5)


# head income
r.chp1 %>%
  filter(X2.A.Code.id.member == "F1") %>%
  ggplot(aes(x = X2.D.Annual.Income, fill = caste.code)) +
  geom_density(position = "identity", alpha = .3, adjust = 5)

# household db
r.chp1.household <-
  r.chp1 %>%
  group_by(Code.Family) %>%
  summarize(total.income = sum(income),
            caste.code = first(caste.code))

# household mean
r.chp1.household.mean <-
  r.chp1.household %>%
  group_by(caste.code) %>%
  summarize(mean.income = mean(total.income))

# household incomes
r.chp1.household %>%
  ggplot(aes(x = total.income, fill = caste.code)) +
  geom_density(position = "identity", alpha = .3, adjust = 4) +
  geom_vline(data=r.chp1.household.mean, aes(xintercept=mean.income,  colour=caste.code),
             linetype="dashed", size=1)



# tests ----
r.chp1 %>%
  filter( Code.Family == "ADEP41")

test1 <-
  full_join(r.chp1.household, r.chp1, by = "Code.Family") %>%
  select(caste.code.x, caste.code.y) %>%
  mutate(diff = as.numeric(caste.code.x) - as.numeric(caste.code.y))

test1$diff

r.chp1 %>%
  filter(X2.A.Code.id.member == "F2") %>%
  ggplot(aes(x = X2.D.Annual.Income, fill = caste.code)) +
  geom_density(position = "identity", alpha = .3, adjust = 5)
# femmes des basses castes ont plus tendance a travailler

names(r.chp1)

r.chp1 %>%
  group_by(caste.code) %>%
  summarize()

# education ----

r.education.caste <-
  r.family %>%
  inner_join(r.general, by = "Code.family" )

r.education.caste %>%
  mutate(
    caste.code =
      match_levels(
        data  = r.education.caste$X1.6.b.Caste.Code,
        names = rume$X_Caste_Code),
    education =
      match_levels(
        data  = r.education.caste$X1.G.Education,
        names = rume$X_Education)
  ) %>%
  filter( X1.G.Education %in% c(9,1,2) ) %>%
  ggplot(aes(x = education,
             fill = as.factor(caste.code))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           position = "fill", alpha = .5) +
  scale_x_discrete() +
  coord_polar(theta = "y")


r.education.caste %>%
  mutate(
    caste.code =
      match_levels(
        data  = r.education.caste$X1.6.b.Caste.Code,
        names = rume$X_Caste_Code),
    education =
      match_levels(
        data  = r.education.caste$X1.G.Education,
        names = rume$X_Education)
  ) %>%
  filter( X1.G.Education %in% c(9,1,2) ) %>%
  ggplot(aes(x = caste.code,
             fill = as.factor(education))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           position = "fill", alpha = .5) +
  scale_x_discrete() +
  coord_polar(theta = "y")

# who works in the family ? - what part of the total salary? ----


names(r.chp1)

r.caste.member = 
  r.chp1 %>%
  group_by(X2.A.Code.id.member, caste.code) %>%
  summarize( income.member = mean(income) )
  
r.caste.member.percentage <-
  r.caste.member%>%
  group_by(caste.code) %>%
  summarise(total = sum(income.member))

r.pct <-
  inner_join(r.caste.member, r.caste.member.percentage, by = "caste.code") %>%
  mutate(prt = income.member/total*100)

r.pct %>%
  filter(caste.code == 1) %>%
  ggplot(aes(x = prt)) +
  geom_histogram() +
  scale_x_discrete() +
  coord_polar(theta = "y")


r.chp1 %>%
  filter(X2.A.Code.id.member == "F2") %>%
  group_by(caste.code) %>%
  summarize(sum(is.na(X2.B.Person.involved))/n())

r.chp1 %>%
  filter(X2.A.Code.id.member == "F2") %>%
  group_by(caste.code) %>%
  summarise(n())


length(which(is.na((r.chp1 %>% filter(X2.A.Code.id.member == "F2"))$X2.B.Person.involved)))
length((r.chp1 %>% filter(X2.A.Code.id.member == "F2"))$X2.B.Person.involved)
385/2394

100-23



