## Chapitre 2 propre.


## fonctions -----

match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

q_plot <- function(vect) {
  return(qplot(seq_along(vect),vect) +
           labs(x = "", y = deparse(substitute(vect))))
}

## build database =======

# get rum
db_path = "/Users/gaston/Desktop/ifp/base_2010/Database_Base_400_Final_Work.mdb"
rume <- mdb.get(db_path)

# r.

r.general <- rume$`T 1 General informations`
r.occupation <- rume$`T 2 Occupations`
r.family <- rume$`T 1-1 Family members`

# r.mainloan contruction ----

r.mainloan <- rume$`T 6-2 Main loans` # décallage ici, correction:
names.mainloan <- names(r.mainloan)
# [19] "amount.received"
# [38] "amount" 
# [60] "how.much"
names.mainloan = names.mainloan[-c(19,38,60)]
r.mainloan[,81] = NULL
r.mainloan[,81] = NULL
r.mainloan[,81] = NULL
names(r.mainloan) = names.mainloan
# décallage corrigé
# r.mainloan[1,]

r.loan <- rume$`T 6-1 Loans` # décallage ici, correction:
names.loan <- names(r.loan) 
names.loan = names.loan[-8] # 8 = specify
r.loan$X6.1.L.When.loan.was.contracted.Year = NULL
names(r.loan) = names.loan
# décallage corrigé
# r.loan[1,]

r.loan$loan.id <- paste( r.loan$Code.Family, r.loan$X6.1.A.Code.id.loan )
r.mainloan$loan.id <- paste( r.mainloan$Code.family, r.mainloan$X6.2.A.Code.id.loan )

r.main <- inner_join(r.mainloan, r.loan, by = "loan.id")


# reshape variables ----

r.main$X6.2.AO.Amount.principal <-
  as.numeric(levels(r.main$X6.2.AO.Amount.principal))[r.main$X6.2.AO.Amount.principal]
# attention : ne pas utiliser cette vairiable.

r.main$X6.2.AO.If.yes[is.na(r.main$X6.2.AO.If.yes)] <- 0

# create date variables ----

r.main$date <- 
  as.Date(
    paste("01",
          as.character(r.mainloan$X6.2.U.Credit.taken.Month),
          as.character(r.mainloan$X6.2.U.Credit.taken.Year),
          sep = " "),
    "%d %b %Y")

r.main$days <- as.integer(as.Date("2010-04-01") - r.main$date)

# create freq variables ------

f.freq <- as.factor(r.main$X6.2.AB.Frequency)

levels(f.freq) <- c(7,30,365,182,0,0,0)

freq <- as.double(levels(f.freq))[f.freq]

r.main$day.interest <- as.double(r.main$X6.2.AA.If.yes) / freq

r.main$day.interest[!is.finite(r.main$day.interest)] <- 0




#############
# Estimation =============

estimation <-   as.double( r.main$X6.2.AD.Principal.Amount.Repaid
                           + r.main$days*(r.main$day.interest) ) /
  as.double(r.main$X6.1.I.Amount.of.loan)

q_plot(estimation)
## beaucoup de 1, pourquoi ?                    ??

estimation.real <- as.double(
  r.main$X6.2.AD.Principal.Amount.Repaid + r.main$days*(r.main$day.interest)
)

est.interest <-   r.main$days*(r.main$day.interest)

q_plot(est.interest)

est.principal <- as.double(r.main$X6.2.AD.Principal.Amount.Repaid) /
  as.double(r.main$X6.1.I.Amount.of.loan)

q_plot(est.principal)



# frequence vs source -------


names(r.main)

r.main$X6.2.AB.Frequency

r.main %>%
  filter(X6.2.B.From == 2) %>%
  ggplot(aes(as.factor(X6.2.AB.Frequency))) +
  geom_bar()

r.main %>%
  filter(X6.2.AB.Frequency == 2) %>%
  ggplot(aes(as.factor(X6.2.B.From))) +
  geom_bar()


r.main %>%
  filter(X6.2.AB.Frequency == 5) %>%
  ggplot(aes(as.factor(X6.2.B.From), y = (..count..)/sum(..count..))) +
  geom_bar()
# 50% main known, 25% relatives

r.main %>%
  filter(X6.2.AB.Frequency == 5,
         X6.2.B.From %in% c(1,2)) %>%
  ggplot(aes(X6.1.J.Balance, fill = as.factor(X6.2.B.From))) +
  geom_density(position = "identity", alpha = .5, adjust = 3)

r.main %>%
  filter(X6.2.AB.Frequency == 5,
         X6.2.B.From %in% c(1,2),
         X6.1.J.Balance < 90000) %>%
  select(X6.1.J.Balance,X6.1.I.Amount.of.loan,X6.2.B.From) %>%
  ggplot(aes(X6.1.I.Amount.of.loan, fill = as.factor(X6.2.B.From))) +
  geom_density(position = "identity", alpha = .5, adjust = 3)


r.main %>%
  filter(X6.2.AB.Frequency == 5) %>%
  group_by(X6.2.B.From) %>%
  summarize( mean.amount = mean(X6.2.AC.Amount.interest)  )

r.main %>%
  filter(X6.2.AB.Frequency == 5) %>%
  group_by(X6.2.B.From) %>%
  summarize( n(),
             mean.amount = mean(X6.1.I.Amount.of.loan)  )



## Construction du modele --------


