## New members ----


v.structure <-
  read.csv2(file = "/Users/gaston/Desktop/ifp/new_member_structure.csv")


v.structure %>%
  filter(is.na(id.interview), name != "") %>%
  select(name)

# 263 new members

new.members <-
  v.structure %>%
  filter(is.na(id.interview), name != "")

summary(new.members)

new.members$age
new.members$No.days.1.1

## nouveau nombre d'individus moyen -----

i.new <-
  which(is.na(v.structure$id.interview) & v.structure$name != "")

v.structure$id.interview[i.new - 1]
# attention il y a quelques NA.


## old structure -------


summary(d14.household)

length(unique(d14.household$id.interview))

263/280
# 0.94


(nrow(d14.household) + 263) / 280

# combien de membres dans l'ancienne structure?


summary(d14.household %>%
  group_by(id.interview) %>%
  summarise(n = n()))
# med = 5, moyenne = 4.689

summary(d04.household %>%
          group_by(id.interview) %>%
          summarise(n = n()))
# med = 5, moyenne = 4.689
