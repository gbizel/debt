# install.packages("sqldf")
# install.packages("RODBC")

# install.packages("ggplot2")
# install.packages("dplyr")

install.packages("plyr")


# install.packages("data.table")
require("data.table")

library("Hmisc")
# library("RODBC")
# library("sqldf") # ?
library("dplyr")
library("ggplot2")

list.files("/Users/gaston/Desktop/ifp/base_2010")

db_path = "/Users/gaston/Desktop/ifp/base_2010/Database_Base_400_Final_Work.mdb"

rume <- mdb.get(db_path)


# -----------------------------------

head(rume$`T 1 General informations`)

idf <- function ( column ) {
  unlist(lapply(column, as.factor))
}

types_avances = idf(rume$`T25 Migration Full`$X25.2.Q.Advance.through)

summary(types_avances)

avance_travail = which(types_avances == "1")

job_who = rume$`T25 Migration Full`$X25.2.K.How.know.person[avance_travail]

idf(job_who)
summary(idf(job_who))

job_kind = idf(rume$`T25 Migration Full`$X25.2.H.Migration.job)

summary(job_kind)/length(job_kind)*100

summary(job_kind[avance_travail])/length(job_kind[avance_travail])*100

head(rume$`T25 Migration Full`)

rume$`T25 Migration Full`$X25.2.Q.Advance.through
rume$`T25 Migration Full`$X25.2.Q.Advance.through

rume$`X_Advance through`

# qry <- "SELECT * FROM "
# 
# sqldf::sqldf("select * from rume$`T 1-1 Family members`")
# 
# rume$`T 1-1 Family members`
# 
# install.packages("RODBC")
# 
# mdbTables(qry)
# 
# RODBC::sqlTables(rume, tableType = "TABLES")
# 
# 
# mdb.get()


# rume_t = rume$`T 1 General informations`
# 
# head(rume_t)
# 
# sqldf("select Code.family from rume_t")


rume_migration = rume$`T25 Migration Full`
index_advance = which(rume_migration$X25.2.Q.Advance.through == "1" &
                        rume_migration$X25.2.H.Migration.job %in% c("1","2")  )
rume_migration_avance <- rume_migration[index_advance,]

rume_migration_avance$Code.family

rume_occupation = rume$`T 2 Occupations`
index_stop = which(rume_occupation$X2.1.Stop.working.due.to.accident == 1)
rume_stop = rume_occupation[index_stop,]

# il faut aussi matcher le family member
match(rume_migration_avance$Code.family, rume_stop$Code.Family)

match(1:200, c(1,4,4,5,3,30))

rume_migration_avance[1:20,1]

#Code individu


rume_migration$Code.individu <- paste(
  rume_migration$Code.family, rume_migration$X25.2.A.Code.id.member)

# --------------------------------


r_migration = rume$`T25 Migration Full`

index_brick_sugar = which(r_migration$X25.2.Q.Advance.through == "1" &
                          r_migration$X25.2.H.Migration.job %in% c("1","2"))

indiv_brick_sugar = data.frame(0,1,2)
indiv_brick_sugar = as.data.frame(r_migration$Code.family[index_brick_sugar])
indiv_brick_sugar[,2] <- r_migration$X25.2.A.Code.id.member[index_brick_sugar]
colnames(indiv_brick_sugar) <- c("Code.family","Code.member")



indiv_brick_sugar

cat = 26

category_match <- function ( cat ) {
  i = 0
  if (cat < 10) {
    i = match( paste("T", cat, sep = " "), substr(names(rume),1,3) )
  } else {
    i = match( paste("T", cat, sep = "" ), substr(names(rume),1,3) )
  }
  return (i)
}

question_match <- function ( question ) {
}

key_match <- function ( cat, souscat, conditions ) {
  
  if (is.null(souscat)) { souscat = 0 }
  
  i = category_match(cat)
  
  r_category <- rume[[i+souscat-1]]
  names(rume)[i+souscat-1]
  
  return(r_category)
}

test18 <- key_match(18,1, c(1,"1",4,"1"))

mattt = cbind(test18[,2],test18[,3]) == cond(c(1,2), nrow(test18)) #,nrow(test18)

mattt[,3] <- sum(mattt[,1] , mattt[,2])


c(1,2,3,4)


cond <- function (vect,l) {
  sortie = c()
  for (k in vect) {
    sortie = cbind(sortie,rep(k,l))
  }
  return (sortie)
}





names(rume)


## Asservis et accidents --------------

`
# plus judicieux : comparer les assets des foyers..

## cf age-->

r_family = rume$`T 1-1 Family members`

indiv_match.fam = match( indiv_brick_sugar[,3],
                         paste(r_family$Code.family,r_family$X1.A.Code.id.member))

r_family.asservis <- r_family[indiv_match.fam, ]

summary(r_family$X1.E.Age)
summary(r_family.asservis$X1.E.Age)

summary(sapply(r_family$X1.C.Male.Female,as.factor))
summary(sapply(r_family.asservis$X1.C.Male.Female,as.factor))





# Plot age (tests) -------


# 
age <- data.frame(age = as.vector(r_family$X1.E.Age) )
age.asservis <- data.frame(age = as.vector(r_family.asservis$X1.E.Age) )

age.groups <- data.frame(groupe = factor( rep( c("asservi","total"),
                                               c(length(age.asservis$age),length(age$age)) ) ),
                         age = rbind(age.asservis,age) )

age.means <- age.groups %>%
  group_by(groupe) %>%
  summarise(age.mean=mean(age))

age.groups %>%
  ggplot(aes(x=age, fill=groupe)) +
   geom_histogram(aes(y=..density..),
                  binwidth=10,
                  position="dodge",
                  alpha = 0.5) +
  geom_density(alpha=.5, position="identity") +
  geom_vline(data=age.means, aes(xintercept=age.mean,  colour=groupe),
             linetype="dashed", size=1)

age.asservis %>%
  ggplot(aes(x=age)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous( limits = l)


## Plot fonction ----------

density_plot <- function( data, data.asservis, name, width, limits = NULL ) {
  dat <- data.frame(dat = as.vector(data) )
  dat.asservis <- data.frame(dat = as.vector(data.asservis) )
  
  dat.groups <- data.frame(groupe = factor( rep( c("asservi","total"),
                                                 c(length(dat.asservis$dat),length(dat$dat)) ) ),
                           dat = rbind(dat.asservis,dat) )
  
  dat.means <- dat.groups %>%
    group_by(groupe) %>%
    summarise(dat.mean=mean(dat))
  
  dat.groups %>%
    ggplot(aes(x=dat, fill=groupe)) +
    geom_histogram(aes(y=..density..),
                   binwidth=width,
                   position="dodge",
                   alpha = 0.5) +
    geom_density(alpha=.5, position="identity") +
    geom_vline(data=dat.means, aes(xintercept=dat.mean,  colour=groupe),
               linetype="dashed", size=1) +
    labs(x = name) +
    scale_x_continuous(limits = limits)
}

density_plot(r_family$X1.E.Age,
             r_family.asservis$X1.E.Age,
             "age",
             width = 6)

density_plot(r_family$X1.C.Male.Female,
             r_family.asservis$X1.C.Male.Female,
             "sex",
             width = .5)

density_plot(r_family$X1.D.Relation,
             r_family.asservis$X1.D.Relation,
             "relation",
             width = 0.5,
             limits = c(0,14))


# Multivariate plot -------

data = as.factor(r_family$X1.D.Relation)
data.asservis = as.factor(r_family.asservis$X1.D.Relation)

# test <-dat.groups %>%
#   group_by(groupe, dat) %>%
#   summarize(perc = n())
# 
# test <-dat %>%
#   group_by(dat) %>%
#   summarize(perc = n())
# test$perc <- test$perc/nrow(dat)
# 
# test2 <-dat.asservis %>%
#   group_by(dat) %>%
#   summarize(perc = n())


multi_plot <- function( data, data.asservis, level = 0, names = NULL) {
  
  data <- as.factor(data)
  data.asservis <- as.factor(data.asservis)
  
  if (!is.null(names)) {
    data <- match_levels(data,names)
    data.asservis <- match_levels(data.asservis,names)
  }

  
  dat <- data.frame(dat = data )
  dat.asservis <- data.frame(dat = data.asservis )
  
  dat.groups <- data.frame(groupe = factor( rep( c("asservi","total"),
                                                 c(length(dat.asservis$dat),length(dat$dat)) ) ),
                           dat = rbind(dat.asservis,dat) )
  
  DT <- data.table(dat.groups)
  DT.pt <- DT[, grp(dat), by=groupe]
  
  print(DT.pt)
  
#  DT.pt <- DT.pt[-c(8,9,10,11,12,14,22:26,28)]
  
  inutile <- which(DT.pt$percentage < level)
  if (length(inutile) > 0) {
    DT.pt <- DT.pt[-inutile]
  }
  
  DT.pt2 <- within(DT.pt, 
                   x <- factor(x, 
                               levels= names[,2]))
  # attention : dangereux (risque de mélanger les levels?)
  # verifier avec l'impression chiffrée
  
  DT.pt2 %>%
    ggplot() +
    geom_bar(aes(x= x, y=percentage, fill = groupe),
             position="dodge",
             stat="identity") 
  
}



multi_plot(as.factor(r_family$X1.D.Relation),
           as.factor(r_family.asservis$X1.D.Relation),
           level = 0.01,
           names = relation_levels)
# --> 
# father ++
# wife, son -
# daugter: replaced by daughter in law, or sun in law

multi_plot(as.factor(r_family$X1.G.Education),
           as.factor(r_family.asservis$X1.G.Education),
           level = 0.01,
           names = rume$X_Education)


x = factor(c(1,2))

# mutate(x,c(1,2),c(2,3))

plyr::mapvalues(x,c(1,2),c(2,3))

## Levels ------------- -----------

match_levels <- function(data,names) {
  return(plyr::mapvalues(as.factor(data), names[,1],as.vector(names[,2])))
}

match_levels(r_family.asservis$X1.D.Relation,rume$X_Relation)
match_levels(r_family.asservis$X1.D.Relation,relation_levels)



relation_levels = t(matrix(c(01, "father",
         02, "wife",
         03, "mother",
         04, "father",
         05, "son",
         06, "daughter",
         07, "daughter-in-law",
         08, "son-in-law",
         09, "sister",
         10, "mother in law",
         11, "father in law",
         12, "brother elder",
         13, "brother younger",
         14, "others"),2,14))
# relation levels = X_Family...


## -----------------

relation_levels

r_family.asservis[which(r_family.asservis$X1.D.Relation == 7),]
# belles filles: 30 35 26 20


## test dplyr --------------

tbl_df(r_family)

fest_femm <-filter(r_family, X1.C.Male.Female == 1 )

fest_femm_ord <- arrange(fest_femm, X1.D.Relation, X1.E.Age)

r_family %>%
  group_by(X1.D.Relation) %>%
  summarize(mean(X1.E.Age, na.rm= T))

r_family %>%
  mutate(relation = match_levels(r_family$X1.D.Relation, relation_levels)) %>%
  filter(relation %in% c("father","wife", "son", "daughter", "daugther-in-law", "son-in-law")) %>%
  ggplot(aes(x = X1.E.Age, fill = relation)) +
  geom_density(position = "identity", alpha = 0.5)

r_family.asservis %>%
  mutate(relation = match_levels(r_family.asservis$X1.D.Relation, relation_levels)) %>%
  filter(relation %in% c("father","wife", "son", "daughter", "daugther-in-law")) %>%
  ggplot(aes(x = X1.E.Age, fill = relation)) +
  geom_density(position = "identity", alpha = 0.5)


r_family %>%
  mutate( education = match_levels(r_family$X1.G.Education, rume$X_Education ),
          sexe = as.factor(X1.C.Male.Female)) %>%
  filter(education %in% c("Primary","High School","No education")) %>%
  ggplot(aes(x = sexe, fill = education)) +
  geom_bar(position = "stack", alpha = .3)
  
r_family.asservis %>%
  mutate( education = match_levels(r_family.asservis$X1.G.Education, rume$X_Education ),
          sexe = as.factor(X1.C.Male.Female)) %>%
  ggplot(aes(x = sexe, fill = education)) +
  geom_bar(position = "stack", alpha = .3)  


relation_levels  
  
typeof(r_family$X1.H.Student.at.present)
nrow(r_family)

length(which(r_family$X1.C.Male.Female == 1))
length(which(r_family$X1.C.Male.Female == 2))

