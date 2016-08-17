library("Hmisc")
library("dplyr")
library("ggplot2")

match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}


d04 <- mdb.get("/Users/gaston/Desktop/ifp/base_2004/debtbondage/debt-fusion-15-6-05-9.24am.mdb")

d14 <- mdb.get("/Users/gaston/Desktop/ifp/base_2014/debt-fusion-data2014_FINAL.mdb")

d04.borrow <- d04$`12-borrowing practice`
d14.borrow <- d14$`12-borrowing practice`

borrow <-
  inner_join(d04.borrow,d14.borrow, by = "id.interview") %>%
  select(id.interview,source.of.cash.x,source.of.cash.y,outstanding.x,outstanding.y)

d14.borrow.sum <-
  d14.borrow %>%
  mutate ( source.of.cash =
             match_levels(data = d14.borrow$source.of.cash,
                          names = d14$`X_12-source-cash`)
           ) %>%
  group_by(source.of.cash) %>%
  summarize(n(),sum(outstanding),mean = mean(outstanding))

d04.borrow.sum <-
  d04.borrow %>%
  mutate ( source.of.cash =
             match_levels(data = d14.borrow$source.of.cash,
                          names = d14$`X_12-source-cash`)
  ) %>%
  group_by(source.of.cash) %>%
  summarize(n(),sum(outstanding, na.rm = T),mean = mean(outstanding, na.rm = T))

d04.borrow.sum

borrow.sum <- 
  inner_join(d04.borrow.sum, d14.borrow.sum, by = "source.of.cash")

# write.table(borrow.sum, file = "/Users/gaston/Desktop/ifp/R/borrowing_habit.csv",sep =";")
  
d14.borrow %>%
  filter(source.of.cash == 5) %>%
  arrange(id.interview)

d04.borrow %>%
  filter(source.of.cash == 5) %>%
  ggplot(aes(x = outstanding)) +
  geom_density(position = "identity", alpha = 0.5)

d04.borrow %>%
  mutate ( source.of.cash =
             match_levels(data = d04.borrow$source.of.cash,
                          names = d04$`X_12-source-cash`),
           outstanding = as.double(outstanding)
  ) %>%
  filter( source.of.cash %in% c("snopkeeper","pawnbrocker","maistry", "Relative/friends")) %>%
  ggplot(aes(x = outstanding, fill = source.of.cash)) +
  geom_density(position = "identity", alpha = 0.5, adjust = 4) +
  scale_y_continuous(limits = c(0,0.0005)) +
  scale_x_continuous(limits = c(0,50000))
#   + geom_vline(data=filter(d04.borrow.sum,
#                          source.of.cash %in% c("snopkeeper","pawnbrocker","maistry", "Relative/friends")) ,
#              aes(xintercept=mean,  colour=source.of.cash),
#              linetype="dashed", size=1)


d14.borrow %>%
  mutate ( source.of.cash =
             match_levels(data = d14.borrow$source.of.cash,
                          names = d14$`X_12-source-cash`),
           outstanding = as.double(outstanding)
  ) %>%
  filter( source.of.cash %in% c("snopkeeper","pawnbrocker","maistry", "Relative/friends")) %>%
  ggplot(aes(x = outstanding, fill = source.of.cash)) +
  geom_density(position = "identity", alpha = 0.5, adjust = 4) +
  scale_y_continuous(limits = c(0,0.0005)) +
  scale_x_continuous(limits = c(0,50000))
# + geom_vline(data=filter(d14.borrow.sum,
#                          source.of.cash %in% c("snopkeeper","pawnbrocker","maistry", "Relative/friends")
#                          ) ,
#              aes(xintercept=mean,  colour=source.of.cash),
#              linetype="dashed", size=1)

