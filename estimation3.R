## Estimation des remboursements mensuels des ménages


library("Hmisc")
library("dplyr")
library("ggplot2")


## Chargement des données -------

match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

q_plot <- function(vect) {
  return(qplot(seq_along(vect),vect) +
           labs(x = "", y = deparse(substitute(vect))))
}

list.files("/Users/gaston/Desktop/ifp/base_2010")
db_path = "/Users/gaston/Desktop/ifp/base_2010/Database_Base_400_Final_Work.mdb"
rume <- mdb.get(db_path)

## Creation des bases de données --------

r.general <- rume$`T 1 General informations`
r.occupation <- rume$`T 2 Occupations`
r.family <- rume$`T 1-1 Family members`

## Construction de r.mainloan, r.loan, r.main ----

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

r.main$X6.2.AO.Amount.principal <-
  as.numeric(levels(r.main$X6.2.AO.Amount.principal))[r.main$X6.2.AO.Amount.principal]
# attention : ne pas utiliser cette vairiable.

r.main$X6.2.AO.If.yes[is.na(r.main$X6.2.AO.If.yes)] <- 0

r.main$date <- 
  as.Date(
    paste("01",
          as.character(r.mainloan$X6.2.U.Credit.taken.Month),
          as.character(r.mainloan$X6.2.U.Credit.taken.Year),
          sep = " "),
    "%d %b %Y")

r.main$days <- as.integer(as.Date("2010-04-01") - r.main$date)


f.freq <- as.factor(r.main$X6.2.AB.Frequency)
levels(f.freq) <- c(7,30,365,182,0,0,0)
freq <- as.double(levels(f.freq))[f.freq]

r.main$day.interest <- as.double(r.main$X6.2.AA.If.yes) / freq
r.main$day.interest[!is.finite(r.main$day.interest)] <- 0

# Settled Loans -- time to repay
time.repay <- r.main$X6.2.Y.Time.to.repay

levels(time.repay) <-
  30*c(1,6,0.25,12,10,11,
    12,13,14,0.5,15,16,
    18,2,24,20,23,24,
    25,27,3,36,30,1,
    4,4,5,5,6,6,
    7,7,8,8,9,NA,
    NA,NA)

r.main$time.repay <- as.numeric(levels(time.repay))[time.repay]

q_plot(as.numeric(r.main$time.repay))

r.main %>%
  filter( X6.1.K.Loan.settled == 1) %>%
  select(time.repay, X6.2.Y.Time.to.repay)


## Estimation de la somme déjà remboursée -----

names(r.main)


estimation <-
  as.double( r.main$X6.2.AD.Principal.Amount.Repaid
             + r.main$days*(r.main$day.interest) ) /
  as.double(r.main$X6.1.I.Amount.of.loan)

q_plot(estimation)
## beaucoup de 1, pourquoi ?                    ??

estimation.real <-
  as.double(
  r.main$X6.2.AD.Principal.Amount.Repaid + r.main$days*(r.main$day.interest) )

q_plot(estimation.real)

est.interest <-   r.main$days*(r.main$day.interest)

q_plot(est.interest)

est.principal <-
  as.double(r.main$X6.2.AD.Principal.Amount.Repaid) /
  as.double(r.main$X6.1.I.Amount.of.loan)

q_plot(est.principal)


## Monthly estimation -------
## Restriction aux monthly/weekly

i.calculable <- which(r.main$X6.2.AB.Frequency %in% c(1,2))
i.f1 <- which(r.main$X6.2.AB.Frequency == 1)
i.f2 <- which(r.main$X6.2.AB.Frequency == 2) 
i.f5 <- which(r.main$X6.2.AB.Frequency == 5)

from <- as.factor(r.main$X6.2.B.From[i.calculable])
caste <- as.factor(r.main$X6.2.D.Lender.s.caste[i.calculable])
freq <- as.factor(r.main$X6.2.AB.Frequency[i.calculable])
levels(freq) = c("weekly","monthly","yearly","6months")

qplot(
  r.main$days[i.calculable],
  estimation[i.calculable],
  colour = freq ) + 
  geom_smooth(method = lm)

names(r.main)

r.main <-
  r.main %>%
  mutate(monthly_payment = ( (estimation.real) / days) * 30)
# attention: monthly_payment a titre indicatif
# faux pour tout autre que 1,2.


## Estimation Weekly
qplot(
  r.main$X6.1.I.Amount.of.loan[i.f1],
  r.main$monthly_payment[i.f1] 
) +
  geom_smooth(method = lm)

summary(lm( r.main$monthly_payment[i.f1] ~ r.main$X6.1.I.Amount.of.loan[i.f1] ))

alpha_w <- summary(lm( r.main$monthly_payment[i.f1] ~
                       r.main$X6.1.I.Amount.of.loan[i.f1] ))$coeff[1,1]
beta_w <-  summary(lm( r.main$monthly_payment[i.f1] ~
                       r.main$X6.1.I.Amount.of.loan[i.f1] ))$coeff[2,1]

## Estimation Monthly

qplot(
  r.main$X6.1.I.Amount.of.loan[i.f2],
  r.main$monthly_payment[i.f2] 
) +
  geom_smooth(method = lm)

summary(lm( r.main$monthly_payment[i.f2] ~ r.main$X6.1.I.Amount.of.loan[i.f2] ))

alpha_m <- summary(lm( r.main$monthly_payment[i.f2] ~
                         r.main$X6.1.I.Amount.of.loan[i.f2] ))$coeff[1,1]
beta_m <-  summary(lm( r.main$monthly_payment[i.f2] ~
                         r.main$X6.1.I.Amount.of.loan[i.f2] ))$coeff[2,1]

## Estimation Whenever

alpha_5 <- 3*alpha_m
beta_5 <- 3*beta_m


## Estimation du montant remboursé par mois
## En fonction uniquement du montant

r.main$est.amount <- NULL

r.main$est.amount[i.f2] <-
  alpha_m + beta_m*as.double(r.main$X6.1.I.Amount.of.loan[i.f2])

r.main$est.amount[i.f5] <-
  alpha_5 + beta_5*as.double(r.main$X6.1.I.Amount.of.loan[i.f5])

r.main$est.amount[i.f1] <-
  alpha_w + beta_w*as.double(r.main$X6.1.I.Amount.of.loan[i.f1])

q_plot(r.main$est.amount)



qplot(
  r.main$X6.1.I.Amount.of.loan,
  r.main$est.amount,
  colour = as.factor(r.main$X6.2.AB.Frequency) ) + 
  geom_smooth(method = lm)
# montre les coefficients

qplot(
  r.main$monthly_payment,
  r.main$ount,
  colour = as.factor(r.main$X6.2.AB.Frequency) ) + 
  geom_smooth(method = lm)
# un peu sousestimé pour 1,2 <- pourquoi ?
# pas de sens pour 5


## Proportion des frequences dans r.main -----

summary(as.factor(r.main$X6.2.AB.Frequency))/nrow(r.main)*100
# ~
# 14 weekly
# 39 monthly
# 26 whenever
# 17 no interest

r.loan$est.monthly.payment <-
  ((14*alpha_w + 39*alpha_m + 26*alpha_5) +
     (14*beta_w + 39*beta_m + 26*beta_5)*as.double(r.loan$X6.1.I.Amount.of.loan))/(14+39+26)

names(r.loan)

qplot(r.loan$X6.1.I.Amount.of.loan,
      r.loan$est.monthly.payment)
  
######## Suite ------------
## Ponderer les trois estimation par rapport a la frequence
## Appliquer ce rapport a la base totale


## Attention dans la pondération: certaines personnes ont des attitudes de remboursement differentes?
## Etudier: caste/freq || et autre.
## Autre facon de classer les gens: regarder directement charac./freq

## Relier aux salaires:

family.income <-
  r.occupation %>%
  group_by(Code.Family) %>%
  summarise(family.income = sum(X2.D.Annual.Income))

names(r.loan)
names(r.occupation)
names(family.income)

# attention ici : que les 3 prets principaux

r.loan.income <-
  left_join(r.loan, family.income, by = "Code.Family") %>%
  group_by(Code.Family) %>%
  summarise(n_loan = n(),
            amount = sum(X6.1.I.Amount.of.loan),
            monthly = sum(est.monthly.payment),
            family.income = first(family.income))


r.loan.income$part_of_income <-
  r.loan.income$monthly / r.loan.income$family.income


names(r.loan.income)

##
summary(r.loan.income$part_of_income)

r.loan.income =
  left_join(r.loan.income, r.general, by = c("Code.Family"="Code.family"))

caste = as.factor(r.loan.income$X1.6.b.Caste.Code)

levels(caste) = c("Lowest","Middle","Upper")

rume$X_Caste_Code

qplot(r.loan.income$family.income,
      r.loan.income$part_of_income,
      color = caste) +
  scale_y_continuous(lim = c(0,1)) +
  scale_x_continuous(lim = c(0,300000),
                     labels = scales::comma) +
  geom_smooth(method = lm, se = F, formula = y ~ x) +
  labs(x = "household income", y = "part of income spent on debt")

##
summary(r.loan.income$family.income)

### Parenthese -----
### Total loans

r.total.loan <-
  r.loan %>%
  group_by(Code.Family) %>%
  summarise(total.loan = sum(X6.1.I.Amount.of.loan))

r.total.loan <-
  inner_join(r.loan.income, r.total.loan, by = "Code.Family")

r.total.loan$family.income.total.loan <-
  r.total.loan$amount / r.total.loan$family.income

names(r.total.loan)

caste2 = as.factor(r.total.loan$X1.6.b.Caste.Code)
levels(caste2) = c("Lowest","Middle","Upper")

qplot(r.total.loan$family.income,
      r.total.loan$total.loan,
      color = caste2) +
  scale_y_continuous(lim = c(0,75000)) +
  scale_x_continuous(lim = c(0,200000),
                     labels = scales::comma) +
  geom_smooth(method = lm,se = F, formula = y ~ x)  

qplot(r.total.loan$family.income,
      r.total.loan$family.income.total.loan,
      color = caste2) +
  scale_y_continuous(lim = c(0,10)) +
  scale_x_continuous(lim = c(0,200000),
                     labels = scales::comma) +
  geom_smooth(method = lm,se = F, formula = y ~ x)

summary(r.total.loan$family.income.total.loan)

####### Fin parenthese



## Loan Settled --------

summary(as.factor(r.main$X6.1.K.Loan.settled))

r.main %>%
  group_by(X6.1.K.Loan.settled) %>%
  summarise( d = mean(days), am = mean(X6.1.I.Amount.of.loan),est = mean(monthly_payment),
              time = mean(time.repay, na.rm = T))
# amount legerment plus élevé et durée légèrement plus longue
# mais monthly payment beaucoup plus élevé!
# montant final important! SETTLEMENT.

r.main$X6.2.Y.Time.to.repay

summary(time.repay)

r.main %>%
  filter( X6.1.K.Loan.settled == 1) %>%
  select(time.repay, X6.2.Y.Time.to.repay)

r.main %>%
  group_by( X6.1.K.Loan.settled) %>%
  summarise( mean(X6.2.B.From), mean(X6.2.AB.Frequency))

100*summary(as.factor(r.main$X6.2.AB.Frequency))/nrow(r.main)

100*summary(as.factor(unlist((r.main %>%
                    filter( X6.1.K.Loan.settled == 1) %>%
                    select(X6.2.AB.Frequency)))))/160


### SCORING ------------------ ######


# Table des Source/Freq

names(r.main)
names(r.loan)

100*summary(as.factor(r.main$X6.2.AB.Frequency))/nrow(r.main)

100*summary(as.factor(r.loan$X6.1.C.From))/nrow(r.loan)



i.calculable <- which(r.main$X6.2.AB.Frequency %in% c(1,2))
i.f1 <- which(r.main$X6.2.AB.Frequency == 1)
i.f2 <- which(r.main$X6.2.AB.Frequency == 2) 
i.f5 <- which(r.main$X6.2.AB.Frequency == 5)

from <- as.factor(r.main$X6.2.B.From[i.calculable])
freq <- as.factor(r.main$X6.2.AB.Frequency[i.calculable])
levels(freq) = c("weekly","monthly","yearly","6months")

qplot(
  r.main$days[i.calculable],
  estimation[i.calculable],
  colour = freq ) + 
  geom_smooth(method = lm)


# Visualiser les regressions freq(1,2) VS from
r.main %>%
  filter(X6.2.AB.Frequency %in% c(1,2),
         X6.2.B.From == 8) %>%
  ggplot(aes(x = as.double(days), y = as.double(estimation.reg), color = as.factor(X6.2.AB.Frequency))) +
  geom_point() +
  geom_smooth( method = lm )




r.main$estimation.reg <-
  as.double( r.main$X6.2.AD.Principal.Amount.Repaid
             + r.main$days*(r.main$day.interest) ) /
  as.double(r.main$X6.1.I.Amount.of.loan)

## Attention : r.temp n'a de sens que pour freq 1,2.

r.temp <-
  r.main %>%
  filter(X6.2.AB.Frequency == 1,
         X6.2.B.From == 10)

nrow(r.temp)

summary(lm( r.temp$monthly_payment ~ r.temp$X6.1.I.Amount.of.loan ))
summary(lm( r.temp$monthly_payment ~ r.temp$X6.1.I.Amount.of.loan ))$coeff[2,1]



# fq,fm
# 1,1 : 0.05975971
# 2,1 : 0.05334108 ref
# 1,2 : 0.05963733
# 2,2 : 0.04613038
# 1,6 : no
# 1,8 : 0.09627316
# 1,10: no
# 2,6 : no
# 2,8 : 0.04813388 / 38 obs
# 2,10: 0.1335655 / 17 obs


0.09627316/0.05334108

6/5.33