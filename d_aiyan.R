

d14$Interviews$X1.3.Panchayat

interview.aiyat <-
  d14$Interviews %>%
  filter(X1.3.Panchayat == 9 ) %>%
  arrange(id.interview)

write.csv2(interview.aiyat, file = "/Users/gaston/Desktop/ifp/interview.aiyat.csv", sep = ";", dec = ".")

id.aiyat = which(d14.borrow$id.interview %in% interview.aiyat$id.interview )

d14.borrow.aiyat <-
  (d14.borrow %>%
  mutate( source.name = match_levels(data = d14.borrow$source.of.cash,
                                                    names = d14$`X_12-source-cash`)
          )
  )[id.aiyat,]

d14.borrow.aiyat$name <- interview.aiyat$X2.2.name

write.csv2(d14.borrow.aiyat, file = "/Users/gaston/Desktop/ifp/borrow.aiyat.csv")

d14.household

id.h.aiyat = which(d14.household$id.interview %in% interview.aiyat$id.interview )


d14.household.aiyat <-
  (d14.household
  )[id.h.aiyat,]

d14.household.aiyat <-
  d14.household.aiyat %>%
  arrange(id.interview, Nbr.member)

write.csv2(d14.household.aiyat, file = "/Users/gaston/Desktop/ifp/household.aiyat.csv")


#### ---------- tests- -------

d14.household <-
  d14.household %>%
  arrange(id.interview, Nbr.member)

d04.household <-
  d04.household %>%
  arrange(id.interview, Nbr.member)

d14.household$age - d04.household$age

household[which(as.double(household$name.x) - as.double(household$name.y) != 0),]
## 169 : à supprimer

household[
  which(
    as.double(household$nbr.year.y) - as.double(household$nbr.year.x)
        < 10)
  ,]