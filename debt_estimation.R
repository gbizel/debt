## chp2 ---

install.packages("tidyr")
library("tidyr")

## source vs frequence ---------


## Pause: xcl venkat -----------


d14.avance.2013 <-
  d14.avance %>%
  filter( year == 2013) %>%
  select(id.interview,year,Advance,Balance) %>%
  arrange(year,id.interview)

d14.avance.2014 <-
  d14.avance %>%
  filter( year == 2014) %>%
  select(id.interview,year,Advance,Balance) %>%
  arrange(year,id.interview)

  
d14.heads <-
  d14$`5-household structure` %>%
  select(id.interview,relation,Nbr.member,name) %>%
  arrange(id.interview,relation) %>%
  group_by(id.interview) %>%
  summarize(head.name = first(name))

venkat_advance <-
  full_join(d14.heads, d14.avance.2013, by = "id.interview") %>%
  full_join(d14.avance.2014, by = "id.interview")

venkat_advance <-
  venkat_advance %>%
  select(id.interview, head.name, Advance.2013 = Advance.x, Balance.2013 = Balance.x,
         Advance.2014 = Advance.y, Balance.2014 = Balance.y)

write.table(venkat_advance, file = "/Users/gaston/Desktop/advance_2013_2014.csv", sep = ";")



