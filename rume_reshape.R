install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)


rume_wide = read.csv2("/Users/gaston/Desktop/ifp/SurveyCTO/Rume2/Rume_2_WIDE.csv", sep = ",")



variables = names(rume_wide)
var_end = substr(variables,nchar(variables)-1,nchar(variables))

var_1 = which(var_end == "_1")
var_2 = which(var_end == "_2")
var_3 = which(var_end == "_3")
var_a = which(!(var_end %in% c("_1","_2","_3")))



rume_1 = rume_wide[var_1]
rume_1[var_1,]


names(rume_wide[var_1])

var_1
var_2
var_3


var_n = which(substr(variables,nchar(variables)-1,nchar(variables)-1) == "_")

var_h = which(substr(variables,nchar(variables)-1,nchar(variables)-1) != "_")

names(rume_wide[var_n])

colnames = colnames(rume_wide)

colnames[var_n]

#liste des nouveaux noms a remplacer
name_1 = which(var_end == "_1")
newnames = substr(colnames[name_1],1,nchar(colnames[name_1])-2)

nameswo = colnames[which(var_end )]

testg = gather(rume_wide, namenumber_1,namenumber_2,namenumber_3)



######

# reshape
tidyrume <- rume_wide %>%
  gather(namekey, name, namenumber_1,namenumber_2,namenumber_3)

tidyrume$name

# delete NA
tidyrume <- tidyrume[-c(which(is.na(tidyrume$name))),]

# now: delete extra data (see alternative method below)
time1 = which(tidyrume$time == 1)
time2 = which(tidyrume$time == 2)
time3 = which(tidyrume$time == 3)

variables = names(tidyrume)
var_end = substr(variables,nchar(variables)-1,nchar(variables))
var_1 = which(var_end == "_1")
var_2 = which(var_end == "_2")
var_3 = which(var_end == "_3")

tidyrume[time1,c(var_2,var_3)] <- NA
tidyrume[time2,c(var_1,var_3)] <- NA
tidyrume[time3,c(var_1,var_2)] <- NA


# tidyrume$name <- tidyrume$name_1 tidyrume$name_2

tidyrume2 <- tidyrume %>%
  gather(key2, time2, name_1,name_2,name_3)

tidyrume2 <- tidyrume2[-c(which(is.na(tidyrume2$time2))),]



########

# wide to long

## Ajouter les rows individus

l <- reshape(rume_wide, 
             varying = c("namenumber_1","namenumber_2","namenumber_3"), 
             v.names = "namenumber",
             timevar = "name_number", 
             times = c("namenumber_1","namenumber_2","namenumber_3"), 
             new.row.names = 1:1000,
             direction = "long")


# supprimer les ménage sans individus
# attention: il existe des ménages sans individus ??
excess_row = which(is.na(l$namenumber))
if (length(excess_row) > 0) {
  l <- l[-excess_row,]
}

# ## Fusionner les variables
# 
# # attention: ne marche que pour 1,10, a améliorer
# variables = names(l)
# indiv_var = which(substr(variables,nchar(variables)-1,nchar(variables)-1)=="_")
# premier_var = which(substr(variables,nchar(variables)-1,nchar(variables))=="_1")
# 
# prem_var_name = colnames(l)[premier_var]
# 
# list_new_var = substr(prem_var_name, 1,nchar(prem_var_name)-2)
# 
# new_var = "name"
# 
# # same_var = which(substr(variables,1,nchar(new_var)+1)=="name_")
#    
# # colnames(l) <- c(colnames(l),new_var)
# 
# # add "name" var
# l[ ,new_var] <- NA
# 
# # new_col = which(colnames(l) == new_var)
# 
# length(same_var)
# 
# for (i in 1:nrow(l)) {
#   right_var = paste(new_var,l$namenumber[i],sep="_")
#   l[i ,new_var] <- as.character(l[i, right_var])
# }

## Fusionner les variables

# attention: ne marche que pour 1,10, a améliorer
variables = names(l)
indiv_var = which(substr(variables,nchar(variables)-1,nchar(variables)-1)=="_")
premier_var = which(substr(variables,nchar(variables)-1,nchar(variables))=="_1")

prem_var_name = colnames(l)[premier_var]

list_new_var = substr(prem_var_name, 1,nchar(prem_var_name)-2)

new_var = "name"


# # add "name" var
# l[ ,list_new_var] <- NA

i =7

for (i in 1:nrow(l)) {
  right_var = c(paste(list_new_var,l$namenumber[i],sep="_"))
  right_col = which( colnames(l) %in% right_var)
  
  #attention: 1:9 limite
  
  rempl = match(list_new_var,
                substr(colnames(l)[right_col],1,nchar(right_var)-2)  )
  
  list_rempl = which(!is.na(list_new_var[rempl]))
  
  l[i ,list_new_var[list_rempl]] <- lapply(l[i, right_col],as.character) ## attention : verifier correspondance
}

l

variables = names(l)
indiv_var = which(substr(variables,nchar(variables)-1,nchar(variables)-1)=="_")

l <- l[,-indiv_var]







#########

households
