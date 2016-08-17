## Chapitre 2 ======
library(scales)

q_plot <- function(vect) {
  return(qplot(seq_along(vect),vect) +
           labs(x = "", y = deparse(substitute(vect))))
}

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

r.loan$loan.id <- paste( r.loan$Code.Family, r.loan$X6.1.A.Code.id.loan)
r.mainloan$loan.id <- paste( r.mainloan$Code.family, r.mainloan$X6.2.A.Code.id.loan)


r.main <- inner_join(r.mainloan, r.loan, by = "loan.id")

# reshape variables
r.main$X6.2.AO.Amount.principal <-
  as.numeric(levels(r.main$X6.2.AO.Amount.principal))[r.main$X6.2.AO.Amount.principal]

# first estimation, looking for a pattern ------

date.taken <-
  as.Date(
  paste("01",
        as.character(r.mainloan$X6.2.U.Credit.taken),
        as.character(r.mainloan$X6.2.U.Credit.taken.Month),
        sep = " "),
  "%d %b %Y")

days.paid <- as.Date("2010-02-01") - date.taken

interest.paid <-
  as.double(r.mainloan$amount)

principal.repaid <-
  as.double(r.mainloan$X6.2.AB.Frequency)

have.to.pay <-
  as.numeric(levels(r.mainloan$X6.2.AM.Used))[r.mainloan$X6.2.AM.Used]

percentage.paid <-
  principal.repaid / have.to.pay

percentage.interest <-
  (interest.paid) / have.to.pay

from <-
  r.mainloan$X6.2.B.From

# r.mainloan[1,]
# Il y a un décalage! Où??!

have.to.pay[1:10]
principal.repaid[1:10]
percentage.paid[1:10]
interest.paid[1:10]


qplot(days.paid, percentage.paid)
qplot(days.paid,principal.repaid)
qplot(days.paid,interest.paid)
qplot(days.paid,percentage.interest)


payments <- tbl_df(as.data.frame(
  cbind(days.paid,
        have.to.pay,
        principal.repaid,
        interest.paid,
        percentage.paid,
        percentage.interest,
        from) ))

pay.no.interest <-
  payments %>%
  filter(percentage.interest != 1)

qplot(
  pay.no.interest$days.paid,
  pay.no.interest$percentage.interest )


payments %>%
  group_by(from) %>%
  summarize( n() )

payments %>%
  filter(percentage.paid >= 5) %>%
  group_by(from) %>%
  summarize(n())

# well known people

pay.well.know <-
  payments %>%
  filter(from == 1)

qplot(
  pay.well.know$days.paid,
  pay.well.know$percentage.interest ) +
  geom_smooth(method = lm)

qplot(
  payments$days.paid,
  payments$percentage.interest ) +
  geom_smooth(method = lm)


qplot(
  pay.well.know$days.paid,
  pay.well.know$percentage.paid ) +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 1, color = "orange")


qplot(
  payments$days.paid,
  payments$percentage.paid ) +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 1, color = "orange")


# relatives

pay.relative <-
  payments %>%
  filter(from == 2)

qplot(
  pay.relative$days.paid,
  pay.relative$percentage.paid ) +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 1, color = "orange")

qplot(
  payments$days.paid,
  payments$percentage.paid ) +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 1, color = "orange")


payments %>%
  filter(from == 6)

payments %>%
  filter(is.na(have.to.pay))

payments %>%
  filter(percentage.paid>2)

## Monthly estimation ---------------

# utilisation de r.main

names(r.main)

r.main$date <- 
  as.Date(
    paste("01",
          as.character(r.mainloan$X6.2.U.Credit.taken.Month),
          as.character(r.mainloan$X6.2.U.Credit.taken.Year),
          sep = " "),
    "%d %b %Y")

r.main$days <- as.Date("2010-04-01") - date.taken

q_plot(r.main$days)


r.main$X6.2.AO.Amount.principal
r.main$X6.1.I.Amount.of.loan
r.main$X6.1.J.Balance


q_plot(
  r.main$X6.1.I.Amount.of.loan - r.main$X6.2.AO.Amount.principal
)

q_plot(
  r.main$X6.1.I.Amount.of.loan - r.main$X6.1.J.Balance
)

r.main$X6.2.AO.If.yes[is.na(r.main$X6.2.AO.If.yes)] <- 0

q_plot(
  as.double(r.main$X6.2.AO.Amount.principal + r.main$X6.2.AO.If.yes)
  / as.double(r.main$X6.1.I.Amount.of.loan)
)

q_plot(
  as.double(r.main$X6.2.AO.Amount.principal)
  / as.double(r.main$X6.1.I.Amount.of.loan)
)


f.freq <- as.factor(r.main$X6.2.AB.Frequency)

levels(f.freq) <- c(7,30,365,182,0,0,0)


cbind(
  r.main$X6.2.AO.If.yes,
  freq,
  as.double(r.main$X6.2.AO.If.yes) / as.double(levels(f.freq))[f.freq]
)

r.main$day.interest <- as.double(r.main$X6.2.AA.If.yes) / as.double(levels(f.freq))[f.freq]

r.main$day.interest[!is.finite(r.main$day.interest)] <- 0


## estimation ::

estimation <-   as.double( r.main$X6.2.AD.Principal.Amount.Repaid
                           + r.main$days*(r.main$day.interest) ) /
  as.double(r.main$X6.1.I.Amount.of.loan)
# estimation pour ceux qui payent Z

estimation.real <- as.double(
  r.main$X6.2.AD.Principal.Amount.Repaid + r.main$days*(r.main$day.interest)
)

cbind(
  r.main$days*(r.main$day.interest),
  r.main$X6.2.AD.Principal.Amount.Repaid,
  r.main$days*(r.main$day.interest) + r.main$X6.2.AD.Principal.Amount.Repaid
)

est.interest <-   r.main$days*(r.main$day.interest)

est.principal <- as.double(r.main$X6.2.AD.Principal.Amount.Repaid) /
  as.double(r.main$X6.1.I.Amount.of.loan)

qplot(
  r.main$days[i.calculable],
  est.interest[i.calculable],
  colour = freq
)

qplot(
  r.main$days[i.calculable],
  est.principal[i.calculable],
  colour = freq
)

qplot(
  r.main$days[i.calculable],
  estimation.real[i.calculable],
  colour = freq
)

qplot(
  r.main$days[i.calculable],
  as.double(r.main$X6.1.I.Amount.of.loan)[i.calculable],
  colour = freq
)

qplot(
  as.double(r.main$X6.1.I.Amount.of.loan)[i.calculable],
  fill = freq
)


cbind(
  r.main$X6.2.AO.If.yes,
  r.main$X6.2.AB.Frequency,
  r.main$day.interest,
  estimation
)

r.main$X6.2.AC.Amount.interest
r.main$X6.2.AO.If.yes
r.main$X6.2.AB.Frequency

# attention : 2 sources d'info: Z,AA,AB,AC,AD et AO...

## estimation Z,AA,AB,AC,AD:

estimation

nrow(
  r.main %>%
  filter(X6.2.AB.Frequency %in% c(1,2))
)

nrow(r.main)

i.calculable <- which(r.main$X6.2.AB.Frequency %in% c(1,2))

length(i.calculable) #689

from = as.factor(r.main$X6.2.B.From[i.calculable])
caste = as.factor(r.main$X6.2.D.Lender.s.caste[i.calculable])
freq = as.factor(r.main$X6.2.AB.Frequency[i.calculable])
levels(freq) <- c("weekly","monthly","yearly","6months")


qplot(
  r.main$days[i.calculable],
  estimation[i.calculable],
  colour = freq
) + 
  geom_smooth(method = lm)


qplot(
  r.main$days[i.calculable],
  estimation[i.calculable]
) +
  geom_smooth(method = lm)


## r.pi : individus dont on peut calculer l'endettement:

r.pi <- r.main[i.calculable,]

r.pi$days = as.double(r.pi$days)

r.pi$estimation.real <- as.double(
  r.pi$X6.2.AD.Principal.Amount.Repaid + r.pi$days*(r.pi$day.interest) )

r.pi$monthly_payment <- ( (r.pi$estimation.real) / r.pi$days ) * 30

qplot(
  r.pi$days,
  r.pi$monthly_payment,
  colour = freq
) + 
  geom_smooth(method = lm)

summary(r.pi$monthly_payment)

cbind(
  r.pi$X6.1.I.Amount.of.loan,
  r.pi$monthly_payment, 
  r.pi$X6.2.AB.Frequency
)

qplot(
  r.pi$X6.1.I.Amount.of.loan,
  r.pi$monthly_payment 
) +
  geom_smooth(method = lm)

summary(lm( r.pi$monthly_payment ~ r.pi$X6.1.I.Amount.of.loan ))

alpha <- summary(lm( r.pi$monthly_payment ~ r.pi$X6.1.I.Amount.of.loan ))$coeff[1,1]
beta <-  summary(lm( r.pi$monthly_payment ~ r.pi$X6.1.I.Amount.of.loan ))$coeff[2,1]

summary(r.main$X6.1.I.Amount.of.loan)

alpha + beta*12000

low_loan <- r.pi %>%
  filter( X6.1.I.Amount.of.loan < 15000) %>%
  select(X6.1.I.Amount.of.loan, monthly_payment)

qplot(low_loan[,1], low_loan[,2]) + geom_smooth(method = lm)

summary(lm(low_loan[,2] ~ low_loan[,1]))


# estimation par tranches ? plus précis ?
# verifier si notre échantillon pi est biaisé (distributions differentes) -- comment?
#

r.main$monthly.estimation <-
  alpha + beta*as.double(r.main$X6.1.I.Amount.of.loan)

qplot(r.main$monthly.estimation[i.calculable], r.pi$monthly_payment)

## relier avec les revenus des ménages ------------

family.income <-
  r.occupation %>%
  group_by(Code.Family) %>%
  summarize(family.income = sum(X2.D.Annual.Income))

names(r.main)

# attention ici : que les 3 prets principaux

r.main.income <-
  left_join(r.main, family.income, by = "Code.Family") %>%
  group_by(Code.family) %>%
  summarize(n_loan = n(),
            amount = sum(X6.1.I.Amount.of.loan),
            monthly = sum(monthly.estimation),
            family.income = first(family.income))

qplot(r.main.income$amount,
      r.main.income$family.income)

qplot(r.main.income$family.income,
      r.main.income$monthly ) +
  geom_abline(intercept = 0)


summary(r.main.income$family.income)
summary(r.main.income$monthly/r.main.income$family.income)

i.lowsalary = which(r.main.income$family.income<45000)

summary(r.main.income$monthly[i.lowsalary]/r.main.income$family.income[i.lowsalary])

pct.debt <- r.main.income$monthly/r.main.income$family.income

qplot(r.main.income$family.income,
      pct.debt) 

nrow(r.main.income)


# tous les prets 

r.loan$monthly.estimation <-
  alpha + beta*r.loan$X6.1.I.Amount.of.loan

r.loan.income <-
  left_join(r.loan, family.income, by = "Code.Family") %>%
  group_by(Code.Family) %>%
  summarize( n_loan = n(),
             amount = sum(X6.1.I.Amount.of.loan),
             monthly = sum(monthly.estimation),
             family.income = first(family.income))

r.loan.income$part_of_income <-
  r.loan.income$monthly / r.loan.income$family.income

r.loan.income =
  left_join(r.loan.income, r.general, by = c("Code.Family"="Code.family"))

names(r.loan.income)

qplot(r.loan.income$family.income,
  r.loan.income$monthly) +
  geom_abline(intercept = 0)

summary(r.loan.income$part_of_income)

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

summary(r.loan.income$part_of_income)

names(r.loan.income)

quantile(r.loan.income$family.income)

i.25 <- which(r.loan.income$family.income < 45000)
i.50 <- which(r.loan.income$family.income < 68000 & r.loan.income$family.income >= 45000)
i.75 <- which(r.loan.income$family.income < 98000 & r.loan.income$family.income >= 68000)
i.100 <- which(r.loan.income$family.income < 738000 & r.loan.income$family.income >= 98000)


# median mean
summary(r.loan.income$part_of_income[i.25]) # 0.18530 0.30030
summary(r.loan.income$part_of_income[i.50]) # 0.11960 0.12690
summary(r.loan.income$part_of_income[i.75]) # 0.08848 0.11520
summary(r.loan.income$part_of_income[i.100])# 0.06231 0.09432

quantile(r.loan.income$family.income, prob = seq(0, 1, length = 11), type = 5)


i.d10 <- which(r.loan.income$family.income < 33000)
i.d20 <- which(r.loan.income$family.income < 42000 & r.loan.income$family.income >= 33000)
i.d30 <- which(r.loan.income$family.income < 50000 & r.loan.income$family.income >= 42000)
i.d40 <- which(r.loan.income$family.income < 59000 & r.loan.income$family.income >= 50000)
i.d50 <- which(r.loan.income$family.income < 68000 & r.loan.income$family.income >= 59000)
i.d60 <- which(r.loan.income$family.income < 90000 & r.loan.income$family.income >= 68000)
i.d70 <- which(r.loan.income$family.income < 108000 & r.loan.income$family.income >= 90000)
i.d80 <- which(r.loan.income$family.income < 145000 & r.loan.income$family.income >= 108000)
i.d90 <- which(r.loan.income$family.income < 738000 & r.loan.income$family.income >= 145000)

summary(r.loan.income$part_of_income[i.d10]) # 0.2230  0.4579
summary(r.loan.income$part_of_income[i.d20]) # 0.17570 0.21480



# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01256 0.06915 0.11160 0.15710 0.17610 6.81200 

qplot(r.loan.income)



r.main$X6.2.AC.Amount.interest

r.main %>%
  group_by(X6.2.AB.Frequency) %>%
  summarize( median(X6.2.AC.Amount.interest) )

# comparer la balance pour les differentes frequences en fonction du temps

r.main$proportion <-  
  as.double(r.main$X6.1.I.Amount.of.loan - r.main$X6.1.J.Balance)/
  as.double(r.main$X6.1.I.Amount.of.loan)

q_plot(r.main$proportion)

qplot( seq_along(r.main$proportion),
       r.main$proportion,
       color = as.factor(r.main$X6.2.AB.Frequency))

qplot( r.main$days,
       r.main$proportion,
       color = as.factor(r.main$X6.2.AB.Frequency)) +
  scale_x_continuous(lim = c(0,2000)) +
  geom_smooth(method = lm)

frequence.main <-
  r.main %>%
  mutate(days = as.double(days)) %>%
  filter(days < 2000, proportion != 0) %>%
  group_by(X6.2.AB.Frequency) %>%
  summarise(n(),
            mean = mean(proportion),
            med = median(proportion),
            zeros = sum(length(which(proportion == 0))),
            days = mean(days, na.rm = T))

# les infos semblent coincider entre 2 et 5.
# mais beaucoup plus d'impayés pour 5.
# peut on faire un coefficient mean/days ou median/days?

# %>%
#   ggplot(aes(x=days, y = med)) +
#   geom_point()

# box plot ? 



r.main %>%
  mutate(days = as.integer(days)) %>%
  filter(X6.2.AB.Frequency == 5, days < 2000) %>%
  select(days,proportion) %>%
  ggplot(aes(x = days, y =proportion)) +
  geom_point()

which(r.main$proportion == 0)

r.main %>%
  filter(X6.2.AB.Frequency %in% c(1:5)) %>%
  ggplot(aes(x= days, y = proportion)) +
  geom_point(alpha=.8, size=2, color = "red") +
  facet_wrap(~X6.2.AB.Frequency) +
  scale_x_continuous(lim = c(0,2000))


r.main %>%
  mutate(days = as.double(days)) %>%
  filter(days < 2000, !(proportion %in% c(0,1))) %>%
  ggplot(aes(x = days, y = proportion)) +
  facet_wrap(~X6.2.AB.Frequency) +
  geom_point() +
  geom_smooth(method = lm)

r.main %>%
  mutate(days = as.double(days)) %>%
  filter(days < 2000, !(proportion %in% c(0,1))) %>%
  lm( days ~ proportion, method = lm)
  
summary(lm(as.integer(r.main$days) ~ r.main$proportion))

q_plot(frequence.main$mean)
q_plot(frequence.main$med)
q_plot(frequence.main$days)


frequence.0 <-
  r.main %>%
  mutate(days = as.double(days)) %>%
  filter(days < 2000) %>%
  group_by(X6.2.AB.Frequency) %>%
  summarise(n(),
            mean = mean(proportion),
            med = median(proportion),
            zeros = sum(length(which(proportion == 0))),
            days = mean(days, na.rm = T))

q_plot(frequence.0$mean)
q_plot(frequence.0$med)
q_plot(frequence.0$days)

r.main %>%
  ggplot(aes(as.factor(X6.2.AB.Frequency), X6.1.I.Amount.of.loan)) +
  geom_boxplot() +
  scale_y_continuous(lim=c(0,50000))


# regarder les prets finis. -------------

summary(r.main$X6.1.K.Loan.settled)
summary(r.loan$X6.1.K.Loan.settled)

r.main.settled <-
  r.main %>%
  filter( X6.1.K.Loan.settled == 1 )

names(r.loan)
names(r.loan.income)

r.loan.settled <-
  r.loan %>%
  filter( X6.1.K.Loan.settled == 1 )


r.main.settled$X6.2.Y.Time.to.repay


r.main$X6.2.AR.Terms.repayment

summary(
  r.main %>%
  filter( X6.2.AP.Frequency == 5 ) %>%
  mutate(repay = as.factor(X6.2.AR.Terms.repayment)) %>%
  select(repay))  
## pour ceux qui sont censés payer irregulierement
## 80: regulier , 280:whenever have money , 50 whenever asked


time.repay <- r.main.settled$X6.2.Y.Time.to.repay

r.main %>%
  filter(X6.1.K.Loan.settled ==1) %>%
  select(X6.2.Y.Time.to.repay)

levels(time.repay) <-
  c(1,6,0.25,12,10,11,
    12,13,14,0.5,15,16,
    18,2,24,20,23,24,
    25,27,3,36,30,1,
    4,4,5,5,6,6,
    7,7,8,8,9,NA,
    NA,NA)



r.main.settled$time.repay <- as.numeric(levels(time.repay))[time.repay]

summary(r.main.settled$time.repay)

qplot(r.main.settled$time.repay,
      r.main.settled$X6.1.I.Amount.of.loan,
      color = as.factor(r.main.settled$X6.2.AB.Frequency)) +
  geom_smooth(method=lm)

100*summary(as.factor(r.main.settled$X6.2.AB.Frequency))/as.double(nrow(r.main.settled))
100*summary(as.factor(r.main$X6.2.AB.Frequency))/as.double(nrow(r.main))
# a peu pres memes proportions de freq. 1,2 plus fort au dertiment de 66.

r.main.settled %>%
  group_by(X6.2.AB.Frequency) %>%
  summarize(n(),
            mean.amount = mean(X6.1.I.Amount.of.loan),
            median.amount = median(X6.1.I.Amount.of.loan),
            mean.time = mean(time.repay, na.rm=T))
# les prets 5 ressemblent beaucoup a 1,2.

r.main %>%
  group_by(X6.2.AB.Frequency) %>%
  summarize(n(),
            mean.amount = mean(X6.1.I.Amount.of.loan),
            median.amount = median(X6.1.I.Amount.of.loan))
  

