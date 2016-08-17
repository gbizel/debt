# install.packages("sqldf")
# install.packages("RODBC")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("plyr")

library("Hmisc")
library("dplyr")
library("ggplot2")

list.files("/Users/gaston/Desktop/ifp/base_2010")

db_path = "/Users/gaston/Desktop/ifp/base_2010/Database_Base_400_Final_Work.mdb"

rume <- mdb.get(db_path)


match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

match_levels(data = r_general$X1.6.b.Caste.Code, names = rume$X_Caste_Code)

# Match -----

r_general <- rume$`T 1 General informations`

r_family <- rume$`T 1-1 Family members`

r_family.head <-
  r_family %>%
  filter( X1.D.Relation == 1)

r_family.head = 
  r_family.head[match(r_general$Code.family, r_family.head$Code.family), ]

r_general.head = inner_join(r_general, r_family.head, by = "Code.family")

r_occupation.headmult =
  rume$`T 2 Occupations` %>%
  filter(X2.A.Code.id.member == "F1" )

r_occupation.head = 
  r_occupation.headmult %>%
  group_by(Code.Family) %>%
  summarize( total_income = sum(X2.D.Annual.Income))

r_general.head2 = inner_join(r_general.head, r_occupation.head, by = c("Code.family"="Code.Family"))

r_general.head$Code.family
r_occupation.head$Code.Family


r_gold = rume$`T13 Gold_corrected`

r_general.head3 = inner_join(r_general.head2, r_gold, by = "Code.family")

r_house = rume$`T15 House property`

r_general.head4 = inner_join(r_general.head3, r_house, by = "Code.family")
  
r_general.head %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
    ) %>%
  ggplot(aes(x = X1.E.Age, fill = caste.code)) +
  geom_density(position = "identity", alpha = 0.5, adjust = 2)


r_general.head2 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  ggplot(aes(x = total_income, fill = caste.code)) +
  geom_density(position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0,250000))


r_general.head3 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  ggplot(aes(x = X13.1.C.Amount.pledge..Rs., fill = caste.code)) +
  geom_density(position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0,50000))

r_general.head4 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  ggplot(aes(x = X15.1.D.Estimated.value.house, fill = caste.code)) +
  geom_density(position = "identity", alpha = 0.5)  


## Test avec moyenne -------


## Income mean ----------
head.income.mean <- r_general.head3 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  group_by(caste.code) %>%
  summarize(asset.mean= mean(total_income))

r_general.head3 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code),
    head_income = total_income
  ) %>%
  ggplot(aes(x = head_income, fill = caste.code)) +
  geom_density(position = "identity", alpha = 0.5, adjust = 2) +
  geom_vline(data=head.income.mean, aes(xintercept=asset.mean,  colour=caste.code),
             linetype="dashed", size=1) +
  scale_x_continuous(limits = c(0,250000)) 
  


# House mean -------------
house.mean <- r_general.head4 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  group_by(caste.code) %>%
  summarize(house.mean= mean(X15.1.D.Estimated.value.house))

r_general.head4 %>%
  mutate(caste.code = match_levels(
    data  = r_general$X1.6.b.Caste.Code,
    names = rume$X_Caste_Code)
  ) %>%
  ggplot(aes(x = X15.1.D.Estimated.value.house, fill = caste.code)) +
  geom_density(position = "identity",
               alpha = 0.5,
               adjust = 2.5) +
  geom_vline(data=house.mean, aes(xintercept=house.mean,  colour=caste.code),
             linetype="dashed", size=1)