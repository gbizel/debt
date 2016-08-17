

# Debt setup


d04 <- mdb.get("/Users/gaston/Desktop/ifp/base_2004/debtbondage/debt-fusion-15-6-05-9.24am.mdb")

d14 <- mdb.get("/Users/gaston/Desktop/ifp/base_2014/debt-fusion-data2014_FINAL.mdb")


# Enfants . Q33 -----

d04.child <- d04$`33-14 Child labour`
d14.child <- d14$`33-14 Child labour`

# d.child <- inner_join(d04.child, d14.child, by = "id.interview")

d04.child %>%
  mutate(d=4)
d14.child %>%
  mutate(d=14)

d.child <-
  rbind(d04.child %>% mutate(d=4),
        d14.child %>% mutate(d=14))



names(d04.child)
names(d14.child)

d.child$X33.1.no.children
d.child$d <- as.factor(d.child$d)

d.child %>%
  ggplot(aes(x=as.numeric(X33.1.no.children), fill = d)) +
  geom_density(alpha=.5, position="identity")

summary(d04.child$X33.1.no.children)
summary(d14.child$X33.1.no.children)

cbind(d04.child$X33.1.no.children,
      d14.child$X33.1.no.children)

cbind(names(d.child))

# [2,] "X33.1.no.children"       pas de changement
# [3,] "X33.2.same.activity"     pas de changement
# [4,] "X33.3.paid.."            beaucoup plus
# [5,] "X33.4.if.not.1"          
# [6,] "X33.4.if.not.2"          
# [7,] "X33.5.child.beneficial"  less benefical -- pas significatif
# [8,] "X33.6.which.way.1"       
# [9,] "X33.6.which.way.2"       
# [10,] "X33.6.which.way.3"       
# [11,] "X33.7.apprentice"        same as benefical.. -- 
# [12,] "X33.8.child.repay"       
# [13,] "X33.9.without.child"     
# [14,] "X33.10.more.avance"      
# [15,] "X33.11.oblige.child"     
# [16,] "X33.12.prefer"           
# [17,] "X33.13.not.at.school"    
# [18,] "X33.14.child.in.school.1"
# [19,] "X33.14.child.in.school.2"
# [20,] "X33.14.child.in.school.3"
# [21,] "d"      

summary(d.child)

summary(d04.child$X33.3.paid..)
summary(d14.child$X33.3.paid..)

d.child %>%
  ggplot(aes(x=as.numeric(X33.2.same.activity), fill = d)) +
  geom_density(alpha=.5, position="identity")

d.child %>%
  ggplot(aes(x=as.numeric(X33.3.paid..), fill = d)) +
  geom_bar(alpha=.5, position="dodge")


d.child %>%
  ggplot(aes(x=as.numeric(X33.5.child.beneficial), fill = d)) +
  geom_bar(alpha=.5, position="dodge") +
  scale_x_discrete(lim=c(0,1))

d.child %>%
  ggplot(aes(x=as.numeric(X33.7.apprentice), fill = d)) +
  geom_bar(alpha=.5, position="dodge")

d.child %>%
  ggplot(aes(x=as.numeric(X33.8.child.repay), fill = d)) +
  geom_bar(alpha=.5, position="dodge")

d.child %>%
  ggplot(aes(x=as.numeric(X33.9.without.child), fill = d)) +
  geom_bar(alpha=.5, position="dodge")


d.child %>%
  ggplot(aes(x=as.numeric(X33.10.more.avance), fill = d)) +
  geom_bar(alpha=.5, position="dodge")


d.child %>%
  ggplot(aes(x=as.numeric(X33.9.without.child), fill = d)) +
  geom_bar(alpha=.5, position="dodge")


cbind(d04.child$X33.5.child.beneficial,d14.child$X33.5.child.beneficial)

full_join(d04.child,d14.child, by = "id.interview") %>%
  select(X33.5.child.beneficial.x,X33.5.child.beneficial.y)


summary(d04.child$X33.3.paid..)
summary(d14.child$X33.3.paid..)




rbind(summary(d04.child)[4,],
summary(d14.child)[4,])


# X33.3.paid..
# X33.6.which.way.2
# X33.6.which.way.3
# X33.9.without.child
# X33.10.more.avance ?
# X33.13.not.at.school ?
# X33.14.child.in.school.1
# X33.14.child.in.school.2


summary(as.factor(d04.child$X33.6.which.way.3))
summary(as.factor(d14.child$X33.6.which.way.3))

summary(as.factor(d04.child$X33.6.which.way.1))/100
summary(as.factor(d14.child$X33.6.which.way.1))/85

summary(as.factor(d04.child$X33.6.which.way.2))
summary(as.factor(d14.child$X33.6.which.way.2))

summary((d04.child$X33.3.paid..))
summary((d14.child$X33.3.paid..))

summary(as.factor(d04.child$X33.4.if.not.2))
summary(as.factor(d14.child$X33.4.if.not.2))

d.child %>%
  ggplot(aes(x=as.numeric(X33.6.which.way.3), fill = d)) +
  geom_bar(alpha=.5, position="dodge")

summary((d04.child$X33.9.without.child))
summary((d14.child$X33.9.without.child))

d.child %>%
  ggplot(aes(x=as.numeric(X33.9.without.child), fill = d)) +
  geom_bar(alpha=.5, position="dodge")


summary(as.factor(d04.child$X33.13.not.at.school))/100
summary(as.factor(d14.child$X33.13.not.at.school))/85

summary(as.factor(unlist(as.vector(d04.child %>%
                                     filter(X33.13.not.at.school != 0) %>%
                                     select(X33.13.not.at.school)))))/(19+17+49+3)

summary(as.factor(d04.child$X33.14.child.in.school.1))/100
summary(as.factor(d14.child$X33.14.child.in.school.1))/85

summary(as.factor(unlist(as.vector(d04.child %>%
  filter(X33.14.child.in.school.1 != 0) %>%
  select(X33.14.child.in.school.1)))))/(24+10+50+4)


d.child %>%
  filter(X33.13.not.at.school != 0) %>%
  ggplot(aes(x=as.numeric(X33.13.not.at.school), fill = d)) +
  geom_bar(aes(y = ..count.. ),alpha=.5, position="dodge")

summary(as.factor(d04.child$X33.4.if.not.1))/(93)
summary(as.factor(d14.child$X33.4.if.not.1))/79

summary(as.factor(d04.child$X33.4.if.not.2))
summary(as.factor(d14.child$X33.4.if.not.2))


# nombre d'enfants

sum(d04.child$X33.1.no.children)
sum(d14.child$X33.1.no.children)




## Cheating -------

d04.cheat <-
  d04$Interviews %>%
  select(X26.1.accounting,
         X26.2.how..1,
         X26.2.how..2,
         X26.2.how..3,
         X26.3.damage,
         X26.4.cheated,
         X26.5.how.cheated.1,
         X26.5.how.cheated.2,
         X26.5.how.cheated.3,
         X26.6.not.turn)

d14.cheat <-
  d14$Interviews %>%
  select(X26.1.accounting,
         X26.2.how..1,
         X26.2.how..2,
         X26.2.how..3,
         X26.3.damage,
         X26.4.cheated,
         X26.5.how.cheated.1,
         X26.5.how.cheated.2,
         X26.5.how.cheated.3,
         X26.6.not.turn)

names(d04.cheat)
# "X26.1.accounting"         
# [133] "X26.2.how..1"              "X26.2.how..2"              "X26.2.how..3"             
# [136] "X26.3.damage"              "X26.4.cheated"             "X26.5.how.cheated.1"      
# [139] "X26.5.how.cheated.2"       "X26.5.how.cheated.3"       "X26.6.not.turn"

names(d14.cheat)
# "X26.1.accounting"        
# [91] "X26.2.how..1"             "X26.2.how..2"             "X26.2.how..3"            
# [94] "X26.3.damage"             "X26.4.cheated"            "X26.5.how.cheated.1"     
# [97] "X26.5.how.cheated.2"      "X26.5.how.cheated.3"      "X26.6.not.turn"

d04.cheat %>%
  mutate(d=4)
d14.cheat %>%
  mutate(d=14)

d.cheat <-
  rbind(d04.cheat %>% mutate(d=4),
        d14.cheat %>% mutate(d=14))

d.cheat$d <- as.factor(d.cheat$d)

rbind(summary(d04.cheat)[4,],
      summary(d14.cheat)[4,])

summary(as.factor(d04.cheat$X26.2.how..1)) /(83 + 46  +  6  +  5  +  2)
summary(as.factor(d14.cheat$X26.2.how..1)) /(142 + 46  +  6  +  5  +  2)

summary(as.factor(d04.cheat$X26.2.how..2))
summary(as.factor(d14.cheat$X26.2.how..2))

summary(as.factor(d04.cheat$X26.2.how..3))
summary(as.factor(d14.cheat$X26.2.how..3))

cbind(d04.cheat$X26.2.how..1,d14.cheat$X26.2.how..1)


summary(as.factor(d04.cheat$X26.5.how.cheated.1))/177
summary(as.factor(d14.cheat$X26.5.how.cheated.1))/260

summary(as.factor(d04.cheat$X26.5.how.cheated.2))
summary(as.factor(d14.cheat$X26.5.how.cheated.2))

summary(as.factor(d04.cheat$X26.5.how.cheated.3))
summary(as.factor(d14.cheat$X26.5.how.cheated.3))

summary(as.factor(d04.cheat$X26.6.not.turn))/(203+47)
summary(as.factor(d14.cheat$X26.6.not.turn))/(219+63)



# Harassment -----

d04.harass <-
  d04$Interviews %>%
  select(X27.1.harassed.1,
         X27.1.harassed.2,
         X27.1.harassed.3,
         X27.2.why.harassed.1,
         X27.2.why.harassed.2,
         X27.2.why.harassed.3,
         X27.3.how.often,
         X27.4.who.1,
         X27.4.who.2,
         X27.4.who.3,
         X27.5.complaints,
         X27.6.with.whom.1,
         X27.6.with.whom.2,
         X27.6.with.whom.3,
         X27.7.stop.complaints,
         X27.8.no.why.1,
         X27.8.no.why.2,
         X27.8.no.why.3)

d14.harass <-
  d14$Interviews %>%
  select(X27.1.harassed.1,
         X27.1.harassed.2,
         X27.1.harassed.3,
         X27.2.why.harassed.1,
         X27.2.why.harassed.2,
         X27.2.why.harassed.3,
         X27.3.how.often,
         X27.4.who.1,
         X27.4.who.2,
         X27.4.who.3,
         X27.5.complaints,
         X27.6.with.whom.1,
         X27.6.with.whom.2,
         X27.6.with.whom.3,
         X27.7.stop.complaints,
         X27.8.no.why.1,
         X27.8.no.why.2,
         X27.8.no.why.3)

names(d04.harass)
# "X27.1.harassed.1"          "X27.1.harassed.2"          "X27.1.harassed.3"         
# [145] "X27.2.why.harassed.1"      "X27.2.why.harassed.2"      "X27.2.why.harassed.3"     
# [148] "X27.3.how.often"           "X27.4.who.1"               "X27.4.who.2"              
# [151] "X27.4.who.3"               "X27.5.complaints"          "X27.6.with.whom.1"        
# [154] "X27.6.with.whom.2"         "X27.6.with.whom.3"         "X27.7.stop.complaints"    
# [157] "X27.8.no.why.1"            "X27.8.no.why.2"            "X27.8.no.why.3"

names(d14.harass)
# "X27.1.harassed.1"         "X27.1.harassed.2"         "X27.1.harassed.3"        
# [103] "X27.2.why.harassed.1"     "X27.2.why.harassed.2"     "X27.2.why.harassed.3"    
# [106] "X27.3.how.often"          "X27.4.who.1"              "X27.4.who.2"             
# [109] "X27.4.who.3"              "X27.5.complaints"         "X27.6.with.whom.1"       
# [112] "X27.6.with.whom.2"        "X27.6.with.whom.3"        "X27.7.stop.complaints"   
# [115] "X27.8.no.why.1"           "X27.8.no.why.2"           "X27.8.no.why.3"

d04.harass %>%
  mutate(d=4)
d14.harass %>%
  mutate(d=14)

d.harass <-
  rbind(d04.harass %>% mutate(d=4),
        d14.harass %>% mutate(d=14))

d.harass$d <- as.factor(d.harass$d)

rbind(summary(d04.harass)[4,],
      summary(d14.harass)[4,])


summary(as.factor(d04.harass$X27.1.harassed.1))/(282-77)
summary(as.factor(d14.harass$X27.1.harassed.1))/282

summary(as.factor(d04.harass$X27.1.harassed.2))
summary(as.factor(d14.harass$X27.1.harassed.2))

summary(as.factor(d04.harass$X27.1.harassed.3))
summary(as.factor(d14.harass$X27.1.harassed.3))

## LOUCHE!!!
summary(as.factor(d04.harass$X27.2.why.harassed.1))
summary(as.factor(d14.harass$X27.2.why.harassed.1))

summary(as.factor(d04.harass$X27.2.why.harassed.2))
summary(as.factor(d14.harass$X27.2.why.harassed.2))

summary(as.factor(d04.harass$X27.2.why.harassed.3))
summary(as.factor(d14.harass$X27.2.why.harassed.3))


summary(as.factor(d04.harass$X27.3.how.often))/282
summary(as.factor(d14.harass$X27.3.how.often))

summary(as.factor(d04.harass$X27.4.who.1))
summary(as.factor(d14.harass$X27.4.who.1))

summary(as.factor(d04.harass$X27.8.no.why.1))
summary(as.factor(d14.harass$X27.8.no.why.1))

# Maistry ----



d04.maistry <-
  d04$Interviews %>%
  select(X16.1.necessary,
         X16.2.no.maistry,
         X16.3.choose,
         X16.4.change.maistry,
         X16.5.why.1.change,
         X16.5.why.2.change,
         X16.5.why.3.change,
         X16.6.frequency,
         X16.7.not.change1,
         X16.7.not.change2,
         X16.7.not.change3)

d14.maistry <-
  d14$Interviews %>%
  select(X16.1.necessary,
         X16.2.no.maistry,
         X16.3.choose,
         X16.4.change.maistry,
         X16.5.why.1.change,
         X16.5.why.2.change,
         X16.5.why.3.change,
         X16.6.frequency,
         X16.7.not.change1,
         X16.7.not.change2,
         X16.7.not.change3)


names(d04.maistry)
# X16.1.necessary"           "X16.2.no.maistry"         
# [52] "X16.3.choose"              "X16.4.change.maistry"      "X16.5.why.1.change"       
# [55] "X16.5.why.2.change"        "X16.5.why.3.change"        "X16.6.frequency"          
# [58] "X16.7.not.change1"         "X16.7.not.change2"         "X16.7.not.change3"

names(d14.maistry)
# "X16.1.necessary"          "X16.2.no.maistry"         "X16.3.choose"            
# [34] "X16.4.change.maistry"     "X16.5.why.1.change"       "X16.5.why.2.change"      
# [37] "X16.5.why.3.change"       "X16.6.frequency"          "X16.7.not.change1"       
# [40] "X16.7.not.change2"        "X16.7.not.change3"

d.maistry <-
  rbind(d04.maistry %>% mutate(d=4),
        d14.maistry %>% mutate(d=14))

d.maistry$d <- as.factor(d.maistry$d)



summary(as.factor(d04.maistry$X16.1.necessary))
summary((d14.maistry$X16.1.necessary))

summary((d04.maistry$X16.2.no.maistry))
summary((d14.maistry$X16.2.no.maistry))

summary((d04.maistry$X16.3.choose))
summary((d14.maistry$X16.3.choose))

summary((d04.maistry$X16.4.change.maistry))
summary((d14.maistry$X16.4.change.maistry))

summary(as.factor(d04.maistry$X16.5.why.1.change))/(282-33)
summary(as.factor(d14.maistry$X16.5.why.1.change))/282

d.maistry %>%
  ggplot(aes(x=as.numeric(X16.5.why.1.change), fill = d)) +
  geom_bar(aes(y = ..count.. ),alpha=.5, position="dodge")

d.maistry %>%
  ggplot(aes(x=as.numeric(X16.5.why.2.change), fill = d)) +
  geom_bar(aes(y = ..count.. ),alpha=.5, position="dodge")

d.maistry %>%
  ggplot(aes(x=as.numeric(X16.5.why.3.change), fill = d)) +
  geom_bar(aes(y = ..count.. ),alpha=.5, position="dodge")

summary(as.factor(d04.maistry$X16.6.frequency))/(282-40)
summary(as.factor(d14.maistry$X16.6.frequency))/282

summary(as.factor(d04.maistry$X16.7.not.change1))/(282-250)
summary(as.factor(d14.maistry$X16.7.not.change1))/(282-173)

173/282

d.maistry %>%
  ggplot(aes(x=as.numeric(X16.7.not.change1), fill = d)) +
  geom_bar(aes(y = ..count.. ),alpha=.5, position="dodge")

