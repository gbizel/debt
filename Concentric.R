## Test: concentric pie charts



wages.matrix = matrix(c("brick kilns",
       "sugar cane",
       "agriculture",
       "construction",
       "NREGA",
       "transport",
       "24000",
       "24000",
       "13000",
       "15000",
       "14000",
       "18000",
       "180",
       "160",
       "170",
       "500",
       "140",
       "450",
       "14",
       "14.5",
       "31",
       "56",
       "46",
       "37.5"), 6,4)


wages = as.data.frame(wages.matrix)
names(wages) = c("occupation", "yearly", "daily", "hourly" )
wages[,2] <- as.numeric(levels(wages[,2]))[wages[,2]]
wages[,3] <- as.numeric(levels(wages[,3]))[wages[,3]]
wages[,4] <- as.numeric(levels(wages[,4]))[wages[,4]]


wages %>%
  ggplot(aes(x = yearly,
             fill = occupation)) +
  geom_bar(
           position = "fill", alpha = .5) +
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
  ggplot(aes(x = education,
             fill = as.factor(caste.code))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           position = "fill", alpha = .5) +
  scale_x_discrete() +
  coord_polar(theta = "y")


