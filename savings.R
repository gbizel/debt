# Saving --------


r.saving <- rume$`T12 Saving account in bank/coop`

r.saving$X12.1.E.1.Purpose.having.account <-
  as.factor(r.saving$X12.1.E.1.Purpose.having.account)

r.saving$X12.1.E.2.Purpose.having.account <-
  as.factor(r.saving$X12.1.E.2.Purpose.having.account)

names(r.saving)

nrow(r.saving)

summary(r.saving)

summary(r.saving$X12.1.E.1.Purpose.having.account)
summary(r.saving$X12.1.E.2.Purpose.having.account)


summary(r.saving %>% filter(X12.1.Saving.account == 1))

summary((r.saving %>% filter(X12.1.Saving.account == 1))$X12.1.B.Saving.account)

r.saving %>%
  filter(X12.1.Saving.account == 1) %>%
  group_by(X12.1.B.Saving.account) %>%
  summarize(nb=n()) %>%
  arrange(nb)


summary(as.factor(as.vector(cbind(r.saving$X12.1.E.1.Purpose.having.account,
      r.saving$X12.1.E.2.Purpose.having.account))))/9.74