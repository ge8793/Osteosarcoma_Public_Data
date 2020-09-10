
################################OSA logistic regression

######### Prep environment 

# clear ws()
rm(list = ls())

##set the working directory if not opening an Rproject file (in which case your wd is set to the location of the project)
#setwd(yourworkingdirectoryhere)

# load packages (if not already installed first, install each of these using <install.packages("packagename")>)
library(naniar)
library(data.table)
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(aod)
library(caret)
library(pROC)
library(ResourceSelection)
library(MKmisc)


############### Read in the OS dataset and clean it up to include only breeds with >4 cases 

OS_trim <- as.data.frame(fread("./data/200907_OSA_Public.txt"))
OS_trim <- as_tibble(OS_trim)

#Now we have cleaned up the table, we will use this to remove breeds with
#<1000 non case dogs and no cases, or with <4 cases this prevents zeros in the analysis

# split the data by case and not
OS_split <- split(OS_trim, OS_trim$OSA.case.or.noncase)

###Get the breed info for OS cases into a table

##Breed
breed_case <- as.data.frame(table(OS_split[[1]]$Breed))
breed_case$CPercent <- (breed_case$Freq/sum(breed_case$Freq))*100
colnames(breed_case) <- c("breed", "CNumber", "CPercentage")
breed_case

breed_noncase <- as.data.frame(table(OS_split[[2]]$Breed))
breed_noncase$NCPercent <- (breed_noncase$Freq/sum(breed_noncase$Freq))*100
colnames(breed_noncase) <- c("breed", "NCNumber", "NCPercentage")
breed_noncase

# Rename breeds with <1000 controls or <4 cases as as "Other Purebred"

cases_4 <- filter(breed_case, breed_case$CNumber <4)
"Pomeranian" %in% cases_4$breed
case4 <- as.character(cases_4$breed)

OS <- OS_trim
OS$Breed[OS$Breed %in% case4] <- "Other Purebred"

noncases_1000 <- filter(breed_noncase, breed_noncase$NCNumber <1000)
noncase1000 <- as.character(noncases_1000$breed)
OS$Breed[OS$Breed %in% noncase1000] <- "Other Purebred"

OS_split <- split(OS, OS$OSA.case.or.noncase)

#check only cases with >4 left in
breed_case <- as.data.frame(table(OS_split[[1]]$Breed))

# write.table(OS, "./data/breeds_prepped.txt", col.names = T, row.names = F, sep ='\t')

OS_trim <- OS

############ Part 1- Descriptive statistics- pull these out for each group

###Use mann whitney to compare median age and bodymass, then note down these results in the manuscript

#age
OS_trim$age_combined <- paste(OS_trim$VPN.CaseAnimalAge_Years, OS_trim$VC_age_years_dec31_2016, sep = " ")
OS_trim$age_combined <- gsub("NA", "", OS_trim$age_combined)
OS_trim$age_combined <- as.numeric(OS_trim$age_combined)
wilcox.test(OS_trim$age_combined ~ OS_trim$OSA)

#mass
OS_split <- split(OS_trim, OS_trim$OSA.case.or.noncase)
OS_split[[1]]$mass_combined <- OS_split[[1]]$Mean_wgt_over18mts_for.that.breed_sex
OS_split[[2]]$mass_combined <- OS_split[[2]]$Actual.bodyweight_mean_over18mts_bodyweight
OS_weight <- rbind.data.frame(OS_split[[1]], OS_split[[2]])

wilcox.test(OS_weight$mass_combined ~ OS_weight$OSA)

#Make the data back to how it was before we added these columns
OS_trim <- OS

# split the data by case and not
OS_split <- split(OS_trim, OS_trim$OSA.case.or.noncase)

#check the right breeds are here 
breed_case <- as.data.frame(table(OS_split[[1]]$Breed))

#get the numerical stats for Cases from the data summary
cases_stats <- as.data.frame(summary(OS_split[[1]]))

###repeat for non cases

noncases_stats <- as.data.frame(summary(OS_split[[2]]))


############ Trim the OS data for further analysis 
colnames(OS_trim)
OS <- OS_trim[,c(1,2,5,19,7,9,10,11,12,16,17,18)]
colnames(OS)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Breed", "Purebred_status", "KC_Group", "Skull_Shape", "Dachshund_status", "Spaniel_status", "Bodymass", "Age", "Chondrodystrophic")


############ Check missing values coded as NA and not as anything else or spaces
table(OS$ID == "")
table(OS$ID == " ")
table(OS$OSA)
table(!(OS$OSA %in% c("Case for osteosarcoma", "Noncase for osteosarcoma")))
table(OS$Sex_Neuter)
list_sex <- c("Female_Entire","Female_Neutered","Male_Entire","Male_Neutered")
OS$Sex_Neuter[(!(OS$Sex_Neuter %in% list_sex))] <- "NA"
table(OS$Sex_Neuter)
OS$Sex_Neuter[((OS$Sex_Neuter == "NA"))] <- "Unrecorded"
table(OS$Sex_Neuter)
breed <- as.data.frame(table(OS$Breed))
breed_list <- breed$Var1
OS$Breed[(!(OS$Breed %in% breed_list))] <- "Breed not recorded"
table(OS$Breed)
OS$Breed[((OS$Breed == "NA"))] <- "Breed not recorded"
OS$Breed[((OS$Breed == "#N/A"))] <- "Breed not recorded"
table(OS$Breed == "NA")
table(OS$Breed == "#N/A")
table(OS$Purebred_status)
pure_list <- c("Purebred", "Crossbred")
OS$Purebred_status[(!(OS$Purebred_status %in% pure_list))] <- "NA"
table(OS$Purebred_status)
OS$Purebred_status[((OS$Purebred_status == "NA"))] <- "Unrecorded"
table(OS$Purebred_status)
KC <- as.data.frame(table(OS$KC_Group))
KC <- KC[2:9,]
KC_list <- KC$Var1
OS$KC_Group[(!(OS$KC_Group %in% KC_list))] <- "NA"
table(OS$KC_Group)
OS$KC_Group[((OS$KC_Group == "NA"))] <- "Unrecorded"
OS$KC_Group[((OS$KC_Group == "Not applicable"))] <- "Unrecorded"
table(OS$KC_Group)
OS$Skull_Shape[(OS$Skull_Shape  == "Dolicocephalic")] <- "Dolichocephalic"
table(OS$Skull_Shape)
skull <- c("Brachycephalic", "Dolichocephalic", "Mesocephalic")
OS$Skull_Shape[(!(OS$Skull_Shape %in% skull))] <- "NA"
table(OS$Skull_Shape)
OS$Skull_Shape[((OS$Skull_Shape == "NA"))] <- "Unrecorded"
table(OS$Skull_Shape)
table(OS$Dachshund_status)
dach <- c("Dachshund-type", "Non-Dachshund-type")
OS$Dachshund_status[(!(OS$Dachshund_status %in% dach))] <- "NA"
table(OS$Dachshund_status)
OS$Dachshund_status[((OS$Dachshund_status == "NA"))] <- "Unrecorded"
table(OS$Spaniel_status)
spaniel <- c("Spaniel-type", "Non-spaniel type")
OS$Spaniel_status[(!(OS$Spaniel_status %in% spaniel))] <- "NA"
OS$Spaniel_status[((OS$Spaniel_status == "NA"))] <- "Unrecorded"
table(OS$Spaniel_status)
table(OS$Chondrodystrophic)
OS$Chondrodystrophic[(!(OS$Chondrodystrophic %in% c("Chondystrophic", "No")))] <- "NA"
OS$Chondrodystrophic[((OS$Chondrodystrophic == "NA"))] <- "Unrecorded"
table(OS$Chondrodystrophic)
OS$Age <- OS_trim$age
age <- as.data.frame(table(OS$Age))
age <- age$Var1
table(OS$Age)
OS$Age[(!(OS$Age %in% age))] <- "NA"
OS$Age[((OS$Age == "NA"))] <- "Unrecorded"
table(OS$Age)
table(OS$Bodymass)
mass <- as.data.frame(table(OS$Bodymass))
mass <- mass$Var1
OS$Bodymass[(!(OS$Bodymass %in% mass))] <- "NA"
OS$Bodymass[((OS$Bodymass == "NA"))] <- "Unrecorded"
table(OS$Bodymass)

############ Calculate Further Descriptive Statistics and export in a table 

OS_split <- split(OS, OS$OSA)

##Breed
breed_case <- as.data.frame(table(OS_split[[1]]$Breed))
breed_case$CPercent <- (breed_case$Freq/sum(breed_case$Freq))*100
colnames(breed_case) <- c("breed", "CNumber", "CPercentage")
breed_case

breed_noncase <- as.data.frame(table(OS_split[[2]]$Breed))
breed_noncase$NCPercent <- (breed_noncase$Freq/sum(breed_noncase$Freq))*100
colnames(breed_noncase) <- c("breed", "NCNumber", "NCPercentage")
breed_noncase

breed <- merge(breed_case, breed_noncase, by = "breed", all= T)
breed$CPercentage <- as.numeric(breed$CPercentage)
breed$CPercentage <- format(round(breed$CPercentage, 1), nsmall = 1)
breed$NCPercentage <- as.numeric(breed$NCPercentage)
breed$NCPercentage <- format(round(breed$NCPercentage, 1), nsmall = 1)

#format for publication and export
breed$CaseNopercent <- paste(breed$CNumber, " ", "(", breed$CPercentage, ")", sep = "")
breed$NonCaseNopercent <- paste(breed$NCNumber, " ", "(", breed$NCPercentage, ")", sep = "")

table(is.na(OS_split[[1]]$Breed))
table(OS_split[[2]]$Breed == "NA")
table(is.na(OS_split[[2]]$Breed))

#Sex_Neuter

sex_case <- as.data.frame(table(OS_split[[1]]$Sex_Neuter))
sex_case$CPercent <- (sex_case$Freq/sum(sex_case$Freq))*100
colnames(sex_case) <- c("Sex", "CNumber", "CPercentage")
sex_case

sex_noncase <- as.data.frame(table(OS_split[[2]]$Sex_Neuter))
sex_noncase$NCPercent <- (sex_noncase$Freq/sum(sex_noncase$Freq))*100
colnames(sex_noncase) <- c("Sex", "NCNumber", "NCPercentage")
sex_noncase
sex <- merge(sex_case, sex_noncase, by = "Sex")

sex$CPercentage <- as.numeric(sex$CPercentage)
sex$CPercentage <- format(round(sex$CPercentage, 1), nsmall = 1)
sex$NCPercentage <- as.numeric(sex$NCPercentage)
sex$NCPercentage <- format(round(sex$NCPercentage, 1), nsmall = 1)

#format for publication and export
sex$CaseNopercent <- paste(sex$CNumber, " ", "(", sex$CPercentage, ")", sep = "")
sex$NonCaseNopercent <- paste(sex$NCNumber, " ", "(", sex$NCPercentage, ")", sep = "")
revalue(sex$Sex, c("Unrecorded" = "Sex_Unrecorded")) -> sex$Sex


#Purebred status

purebred_case <- as.data.frame(table(OS_split[[1]]$Purebred_status))
purebred_case$CPercent <- (purebred_case$Freq/sum(purebred_case$Freq))*100
colnames(purebred_case) <- c("purebred", "CNumber", "CPercentage")
purebred_case

purebred_noncase <- as.data.frame(table(OS_split[[2]]$Purebred_status))
purebred_noncase$NCPercent <- (purebred_noncase$Freq/sum(purebred_noncase$Freq))*100
colnames(purebred_noncase) <- c("purebred", "NCNumber", "NCPercentage")
purebred_noncase
purebred <- merge(purebred_case, purebred_noncase, by = "purebred")

purebred$CPercentage <- as.numeric(purebred$CPercentage)
purebred$CPercentage <- format(round(purebred$CPercentage, 1), nsmall = 1)
purebred$NCPercentage <- as.numeric(purebred$NCPercentage)
purebred$NCPercentage <- format(round(purebred$NCPercentage, 1), nsmall = 1)

#format for publication and export
purebred$CaseNopercent <- paste(purebred$CNumber, " ", "(", purebred$CPercentage, ")", sep = "")
purebred$NonCaseNopercent <- paste(purebred$NCNumber, " ", "(", purebred$NCPercentage, ")", sep = "")
revalue(purebred$purebred, c("Unrecorded" = "Purebred_Unrecorded")) -> purebred$purebred


### skullshape
skull_case <- as.data.frame(table(OS_split[[1]]$Skull_Shape))
skull_case$CPercent <- (skull_case$Freq/sum(skull_case$Freq))*100
colnames(skull_case) <- c("skull", "CNumber", "CPercentage")
skull_case

skull_noncase <- as.data.frame(table(OS_split[[2]]$Skull_Shape))
skull_noncase$NCPercent <- (skull_noncase$Freq/sum(skull_noncase$Freq))*100
colnames(skull_noncase) <- c("skull", "NCNumber", "NCPercentage")
skull_noncase
skull <- merge(skull_case, skull_noncase, by = "skull")

skull$CPercentage <- as.numeric(skull$CPercentage)
skull$CPercentage <- format(round(skull$CPercentage, 1), nsmall = 1)
skull$NCPercentage <- as.numeric(skull$NCPercentage)
skull$NCPercentage <- format(round(skull$NCPercentage, 1), nsmall = 1)

#format for publication and export
skull$CaseNopercent <- paste(skull$CNumber, " ", "(", skull$CPercentage, ")", sep = "")
skull$NonCaseNopercent <- paste(skull$NCNumber, " ", "(", skull$NCPercentage, ")", sep = "")
revalue(skull$skull, c("Unrecorded" = "Skull Shape Unrecorded")) -> skull$skull


##Dachshund type 
dachs_case <- as.data.frame(table(OS_split[[1]]$Dachshund_status))
dachs_case$CPercent <- (dachs_case$Freq/sum(dachs_case$Freq))*100
colnames(dachs_case) <- c("dachs", "CNumber", "CPercentage")
dachs_case

dachs_noncase <- as.data.frame(table(OS_split[[2]]$Dachshund_status))
dachs_noncase$NCPercent <- (dachs_noncase$Freq/sum(dachs_noncase$Freq))*100
colnames(dachs_noncase) <- c("dachs", "NCNumber", "NCPercentage")
dachs_noncase
dachs <- merge(dachs_case, dachs_noncase, by = "dachs")

dachs$CPercentage <- as.numeric(dachs$CPercentage)
dachs$CPercentage <- format(round(dachs$CPercentage, 1), nsmall = 1)
dachs$NCPercentage <- as.numeric(dachs$NCPercentage)
dachs$NCPercentage <- format(round(dachs$NCPercentage, 1), nsmall = 1)

#format for publication and export
dachs$CaseNopercent <- paste(dachs$CNumber, " ", "(", dachs$CPercentage, ")", sep = "")
dachs$NonCaseNopercent <- paste(dachs$NCNumber, " ", "(", dachs$NCPercentage, ")", sep = "")
revalue(dachs$dachs, c("Unrecorded" = "Dachshund Status Unrecorded")) -> dachs$dachs

## Spaniel status 
spaniel_case <- as.data.frame(table(OS_split[[1]]$Spaniel_status))
spaniel_case$CPercent <- (spaniel_case$Freq/sum(spaniel_case$Freq))*100
colnames(spaniel_case) <- c("spaniel", "CNumber", "CPercentage")
spaniel_case

spaniel_noncase <- as.data.frame(table(OS_split[[2]]$Spaniel_status))
spaniel_noncase$NCPercent <- (spaniel_noncase$Freq/sum(spaniel_noncase$Freq))*100
colnames(spaniel_noncase) <- c("spaniel", "NCNumber", "NCPercentage")
spaniel_noncase
spaniel <- merge(spaniel_case, spaniel_noncase, by = "spaniel")

spaniel$CPercentage <- as.numeric(spaniel$CPercentage)
spaniel$CPercentage <- format(round(spaniel$CPercentage, 1), nsmall = 1)
spaniel$NCPercentage <- as.numeric(spaniel$NCPercentage)
spaniel$NCPercentage <- format(round(spaniel$NCPercentage, 1), nsmall = 1)

#format for publication and export
spaniel$CaseNopercent <- paste(spaniel$CNumber, " ", "(", spaniel$CPercentage, ")", sep = "")
spaniel$NonCaseNopercent <- paste(spaniel$NCNumber, " ", "(", spaniel$NCPercentage, ")", sep = "")
revalue(spaniel$spaniel, c("Unrecorded" = "Spaniel_Status_Unrecorded")) -> spaniel$spaniel
spaniel1 <- as.character(c("Non-Spaniel-type", "Spaniel-type", "Spaniel Status Unrecorded"))
spaniel[,1] <- spaniel1

## Chondrodystrophic
chond_case <- as.data.frame(table(OS_split[[1]]$Chondrodystrophic))
chond_case$CPercent <- (chond_case$Freq/sum(chond_case$Freq))*100
colnames(chond_case) <- c("chond", "CNumber", "CPercentage")
chond_case

chond_noncase <- as.data.frame(table(OS_split[[2]]$Chondrodystrophic))
chond_noncase$NCPercent <- (chond_noncase$Freq/sum(chond_noncase$Freq))*100
colnames(chond_noncase) <- c("chond", "NCNumber", "NCPercentage")
chond_noncase
chond <- merge(chond_case, chond_noncase, by = "chond")

chond$CPercentage <- as.numeric(chond$CPercentage)
chond$CPercentage <- format(round(chond$CPercentage, 1), nsmall = 1)
chond$NCPercentage <- as.numeric(chond$NCPercentage)
chond$NCPercentage <- format(round(chond$NCPercentage, 1), nsmall = 1)

#format for publication and export
chond$CaseNopercent <- paste(chond$CNumber, " ", "(", chond$CPercentage, ")", sep = "")
chond$NonCaseNopercent <- paste(chond$NCNumber, " ", "(", chond$NCPercentage, ")", sep = "")
chond$chond <- c("Chondrodystrophic", "Non-Chondrodystriphic", "Chondrodystrophy Unrecorded")

##Bodymass
mass_case <- as.data.frame(table(OS_split[[1]]$Bodymass))
mass_case$CPercent <- (mass_case$Freq/sum(mass_case$Freq))*100
colnames(mass_case) <- c("mass", "CNumber", "CPercentage")
mass_case

mass_noncase <- as.data.frame(table(OS_split[[2]]$Bodymass))
mass_noncase$NCPercent <- (mass_noncase$Freq/sum(mass_noncase$Freq))*100
colnames(mass_noncase) <- c("mass", "NCNumber", "NCPercentage")
mass_noncase
mass <- merge(mass_case, mass_noncase, by = "mass")

mass$CPercentage <- as.numeric(mass$CPercentage)
mass$CPercentage <- format(round(mass$CPercentage, 1), nsmall = 1)
mass$NCPercentage <- as.numeric(mass$NCPercentage)
mass$NCPercentage <- format(round(mass$NCPercentage, 1), nsmall = 1)

#format for publication and export
mass$CaseNopercent <- paste(mass$CNumber, " ", "(", mass$CPercentage, ")", sep = "")
mass$NonCaseNopercent <- paste(mass$NCNumber, " ", "(", mass$NCPercentage, ")", sep = "")
revalue(mass$mass, c("Unrecorded" = "Bodymass_Unrecorded")) -> mass$mass

##Age 
age_case <- as.data.frame(table(OS_split[[1]]$Age))
age_case$CPercent <- (age_case$Freq/sum(age_case$Freq))*100
colnames(age_case) <- c("age", "CNumber", "CPercentage")
age_case

age_noncase <- as.data.frame(table(OS_split[[2]]$Age))
age_noncase$NCPercent <- (age_noncase$Freq/sum(age_noncase$Freq))*100
colnames(age_noncase) <- c("age", "NCNumber", "NCPercentage")
age_noncase
age <- merge(age_case, age_noncase, by = "age")

age$CPercentage <- as.numeric(age$CPercentage)
age$CPercentage <- format(round(age$CPercentage, 1), nsmall = 1)
age$NCPercentage <- as.numeric(age$NCPercentage)
age$NCPercentage <- format(round(age$NCPercentage, 1), nsmall = 1)

#format for publication and export
age$CaseNopercent <- paste(age$CNumber, " ", "(", age$CPercentage, ")", sep = "")
age$NonCaseNopercent <- paste(age$NCNumber, " ", "(", age$NCPercentage, ")", sep = "")
revalue(age$age, c("Unrecorded" = "Age_Unrecorded")) -> age$age

##KC_group
KC_case <- as.data.frame(table(OS_split[[1]]$KC_Group))
KC_case$CPercent <- (KC_case$Freq/sum(KC_case$Freq))*100
colnames(KC_case) <- c("KC", "CNumber", "CPercentage")
KC_case

KC_noncase <- as.data.frame(table(OS_split[[2]]$KC_Group))
KC_noncase$NCPercent <- (KC_noncase$Freq/sum(KC_noncase$Freq))*100
colnames(KC_noncase) <- c("KC", "NCNumber", "NCPercentage")
KC_noncase
KC <- merge(KC_case, KC_noncase, by = "KC")

KC$CPercentage <- as.numeric(KC$CPercentage)
KC$CPercentage <- format(round(KC$CPercentage, 1), nsmall = 1)
KC$NCPercentage <- as.numeric(KC$NCPercentage)
KC$NCPercentage <- format(round(KC$NCPercentage, 1), nsmall = 1)

#format for publication and export
KC$CaseNopercent <- paste(KC$CNumber, " ", "(", KC$CPercentage, ")", sep = "")
KC$NonCaseNopercent <- paste(KC$NCNumber, " ", "(", KC$NCPercentage, ")", sep = "")
revalue(KC$KC, c("Unrecorded" = "KC_Group_Unrecorded")) -> KC$KC

# combine these data frames 
desc <- c(purebred, breed, KC, mass, age, sex, dachs, spaniel, chond, skull)
lapply(desc, as.data.frame )
desc <- list(purebred, breed, KC, mass, age, sex, dachs, spaniel, chond, skull)
desc_stats <- rbindlist(desc, use.names = F)
desc_stats <- desc_stats[, c(1,6,7)]

# write.table(desc_stats, "./analysis/descriptive_stats.txt", row.names = F, col.names = T, quote = F, sep = "\t")

################################### Part 2 - Univariable Logistic Regression

#Get the median and IQR of all the columns 
summary(OS)

# regress breed on OSA risk using logistic regression
colnames(OS)

#make the variables into a factor variable 
OS$breedf <- factor(OS$Breed)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$puref <- factor(OS$Purebred_status)
OS$KCf <- factor(OS$KC_Group)
OS$skullf <- factor(OS$Skull_Shape)
OS$dachsf <- factor(OS$Dachshund_status)
OS$spanielf <- factor(OS$Spaniel_status)
OS$massf <- factor(OS$Bodymass)
OS$agef <- factor(OS$Age)
OS$chondf <- factor(OS$Chondrodystrophic)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$breedf)
is.factor(OS$breedf)
#set the base
OS$breedf <- relevel(OS$breedf, "Crossbreed")

table(OS$KCf)
is.factor(OS$KCf)
#set the base 
OS$KCf <- relevel(OS$KCf, "Toy")

table(OS$massf)
is.factor(OS$massf)
#set the base 
OS$massf <- relevel(OS$massf, "<10")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

table(OS$dachsf)
is.factor(OS$dachsf)
#set the base 
OS$dachsf <- relevel(OS$dachsf, "Non-Dachshund-type")

table(OS$spanielf)
is.factor(OS$spanielf)
#set the base 
OS$spanielf <- relevel(OS$spanielf, "Non-spaniel type")

table(OS$chond)
is.factor(OS$chondf)
#set the base 
OS$chondf <- relevel(OS$chondf, "No")

table(OS$skullf)
is.factor(OS$skullf)
#set the base 
OS$skullf <- relevel(OS$skullf, "Mesocephalic")

head(OS)
is.factor(OS$breedf)

model <- glm(OSf ~ breedf, data = OS, family = binomial)

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "Crossbreed"
Breed_res <- Breed_res[,2:8]

# write.table(Breed_res, "./analysis/Breed_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# Breed_res <- as.data.frame(fread("./analysis/Breed_results.txt"))

sex_model <- glm(OSf ~ Sex_Neuterf, data = OS, family = binomial)

sex_res <- cbind(summary(sex_model)$coefficients)
sex_res <- as.data.frame(sex_res)
sex_res$OR <- exp(sex_res$Estimate)
sex_res$OR <- round(sex_res$OR, 2)
UCI <- round(exp(sex_res$Estimate+(1.96*sex_res$`Std. Error`)),2)
LCI <- round(exp(sex_res$Estimate-(1.96*sex_res$`Std. Error`)),2)
sex_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(sex$Sex)
colnames(cat)[1] <- "Category"
cat2 <- cat[order(as.data.frame(cat$Category)),]
sex_res$Category <- cat2
tail(sex_res)
# write.table(sex_res, "./analysis/sex_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# sex_res <- as.data.frame(fread("./analysis/sex_results.txt"))


pure_model <- glm(OSf ~ puref, data = OS, family = binomial)

pure_res <- cbind(summary(pure_model)$coefficients)
pure_res <- as.data.frame(pure_res)
pure_res$OR <- exp(pure_res$Estimate)
pure_res$OR <- round(pure_res$OR, 2)
UCI <- round(exp(pure_res$Estimate+(1.96*pure_res$`Std. Error`)),2)
LCI <- round(exp(pure_res$Estimate-(1.96*pure_res$`Std. Error`)),2)
pure_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(purebred$purebred)
colnames(cat)[1] <- "Category"
cat2 <- cat[order(as.data.frame(cat$Category)),]
pure_res$Category <- cat2
tail(pure_res)
# write.table(pure_res, "./analysis/pure_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# pure_res <- as.data.frame(fread("./analysis/pure_results.txt"))


KC_model <- glm(OSf ~ KCf, data = OS, family = binomial)

KC_res <- cbind(summary(KC_model)$coefficients)
KC_res <- as.data.frame(KC_res)
KC_res$OR <- exp(KC_res$Estimate)
KC_res$OR <- round(KC_res$OR, 2)
UCI <- round(exp(KC_res$Estimate+(1.96*KC_res$`Std. Error`)),2)
LCI <- round(exp(KC_res$Estimate-(1.96*KC_res$`Std. Error`)),2)
KC_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- rownames(KC_res)
cat <- gsub("KCf" , "", cat)
KC_res$Category <- cat
KC_res$Category[((KC_res$Category == "(Intercept)"))] <- "Toy"
tail(KC_res)

# write.table(KC_res, "./analysis/KC_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# KC_res <- as.data.frame(fread("./analysis/KC_results.txt"))

skull_model <- glm(OSf ~ skullf, data = OS, family = binomial)

skull_res <- cbind(summary(skull_model)$coefficients)
skull_res <- as.data.frame(skull_res)
skull_res$OR <- exp(skull_res$Estimate)
skull_res$OR <- round(skull_res$OR, 2)
UCI <- round(exp(skull_res$Estimate+(1.96*skull_res$`Std. Error`)),2)
LCI <- round(exp(skull_res$Estimate-(1.96*skull_res$`Std. Error`)),2)
skull_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- c("Mesocephalic", "Brachycephalic", "Dolichocephalic", "Skull Shape Unrecorded")
skull_res$Category <- cat
tail(skull_res)

# write.table(skull_res, "./analysis/skull_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# skull_res <- as.data.frame(fread("./analysis/skull_results.txt"))


dachs_model <- glm(OSf ~ dachsf, data = OS, family = binomial)

dachs_res <- cbind(summary(dachs_model)$coefficients)
dachs_res <- as.data.frame(dachs_res)
dachs_res$OR <- exp(dachs_res$Estimate)
dachs_res$OR <- round(dachs_res$OR, 2)
UCI <- round(exp(dachs_res$Estimate+(1.96*dachs_res$`Std. Error`)),2)
LCI <- round(exp(dachs_res$Estimate-(1.96*dachs_res$`Std. Error`)),2)
dachs_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- c("Non-Dachshund-type", "Dachshund-type", "Dachshund Status Unrecorded")
dachs_res$Category <- cat
tail(dachs_res)

# write.table(dachs_res, "./analysis/dachs_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# dachs_res <- as.data.frame(fread("./analysis/dachs_results.txt"))

spaniel_model <- glm(OSf ~ spanielf, data = OS, family = binomial)

spaniel_res <- cbind(summary(spaniel_model)$coefficients)
spaniel_res <- as.data.frame(spaniel_res)
spaniel_res$OR <- exp(spaniel_res$Estimate)
spaniel_res$OR <- round(spaniel_res$OR, 2)
UCI <- round(exp(spaniel_res$Estimate+(1.96*spaniel_res$`Std. Error`)),2)
LCI <- round(exp(spaniel_res$Estimate-(1.96*spaniel_res$`Std. Error`)),2)
spaniel_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- c("Non-Spaniel-type", "Spaniel-type", "Spaniel Status Unrecorded")
spaniel_res$Category <- cat
tail(spaniel_res)

# write.table(spaniel_res, "./analysis/spaniel_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# spaniel_res <- as.data.frame(fread("./analysis/spaniel_results.txt"))

mass_model <- glm(OSf ~ massf, data = OS, family = binomial)

mass_res <- cbind(summary(mass_model)$coefficients)
mass_res <- as.data.frame(mass_res)
mass_res$OR <- exp(mass_res$Estimate)
mass_res$OR <- round(mass_res$OR, 2)
UCI <- round(exp(mass_res$Estimate+(1.96*mass_res$`Std. Error`)),2)
LCI <- round(exp(mass_res$Estimate-(1.96*mass_res$`Std. Error`)),2)
mass_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- as.data.frame(mass$mass)
colnames(cat)[1] <- "Category"
cat2 <- cat[order(as.data.frame(cat$Category)),]
mass_res$Category <- cat2
tail(mass_res)
# write.table(mass_res, "./analysis/mass_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# mass_res <- as.data.frame(fread("./analysis/mass_results.txt"))


age_model <- glm(OSf ~ agef, data = OS, family = binomial)

age_res <- cbind(summary(age_model)$coefficients)
age_res <- as.data.frame(age_res)
age_res$OR <- exp(age_res$Estimate)
age_res$OR <- round(age_res$OR, 2)
UCI <- round(exp(age_res$Estimate+(1.96*age_res$`Std. Error`)),2)
LCI <- round(exp(age_res$Estimate-(1.96*age_res$`Std. Error`)),2)
age_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- as.data.frame(age$age)
colnames(cat)[1] <- "Category"
cat2 <- cat[order(as.data.frame(cat$Category)),]
age_res$Category <- cat2
tail(age_res)
# write.table(age_res, "./analysis/age_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# age_res <- as.data.frame(fread("./analysis/age_results.txt"))


chond_model <- glm(OSf ~ chondf, data = OS, family = binomial)

chond_res <- cbind(summary(chond_model)$coefficients)
chond_res <- as.data.frame(chond_res)
chond_res$OR <- exp(chond_res$Estimate)
chond_res$OR <- round(chond_res$OR, 2)
UCI <- round(exp(chond_res$Estimate+(1.96*chond_res$`Std. Error`)),2)
LCI <- round(exp(chond_res$Estimate-(1.96*chond_res$`Std. Error`)),2)
chond_res$CI <- paste(LCI, UCI, sep = " - ")
cat <- c("Non-Chondrodystriphic", "Chondrodystrophic", "Chondrodystrophy Unrecorded")
chond_res$Category <- cat
tail(chond_res)
# write.table(chond_res, "./analysis/chond_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')
# chond_res <- as.data.frame(fread("./analysis/chond_results.txt"))

univ_res <- list(pure_res, Breed_res, KC_res, mass_res, age_res, sex_res, dachs_res, spaniel_res, chond_res, skull_res)

univ_res <- rbindlist(univ_res)
univ_res$OR <- round(univ_res$OR, 2)
univ_res$Pvalue <- round(univ_res$`Pr(>|z|)`,3)
univ_res$Pvalue[univ_res$Pvalue == 0] <- "<0.001"

# write.table(univ_res, "./analysis/univariable_results.txt", row.names = F, col.names = T, quote = F, sep = '\t')

colnames(univ_res)
res_trim <- univ_res[,c(7,5,6,8)]

colnames(desc_stats)[1] <- "Category" 

figure1 <- merge(desc_stats, res_trim, by = "Category", sort = F)

# write.table(figure1, "./analysis/figure1.txt", row.names = F, col.names = T, quote = F, sep = '\t')


############################################## Part 3 - multivariable logistic regression

######### 3a. Breed Model 

# OS <- as.data.frame(fread("./analysis/OS_cleaned_for_regression.txt"))

table(OS$Breed)

#Select chosen variables

colnames(OS)

#make a copy of OS in case 
OS2 <- OS

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Breed, OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Breed", "Age")

#make the variables into a factor variable 
OS$breedf <- factor(OS$Breed)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

#some checks
table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$breedf)
is.factor(OS$breedf)
#set the base
OS$breedf <- relevel(OS$breedf, "Crossbreed")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "breedf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, " Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, " agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)
colnames(Breed_res)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"


Breed_res

# write.table(Breed_res, "./analysis/multi_reg_breeds.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 
pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

#check if any multi-colinearity
car::vif(multi_model)

######### 3b. Purebred Model 

# rm(list=ls())

OS <- OS2

#Select chosen variables

colnames(OS)

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Purebred_status, OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Purebred", "Age")

#make the variables into a factor variable 
OS$purebredf <- factor(OS$Purebred)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$purebredf)
is.factor(OS$purebredf)
#set the base 
OS$purebredf <- relevel(OS$purebredf, "Crossbred")


table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "purebredf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
table(cat$Category)
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)
colnames(Breed_res)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_purebred.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

######### 3c. KC Group Model

OS <- OS2

#Select chosen variables

colnames(OS)

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$KC_Group, OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "KC_Group", "Age")

#make the variables into a factor variable 
OS$KCf <- factor(OS$KC_Group)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$KCf)
is.factor(OS$KCf)
#set the base
OS$KCf <- relevel(OS$KCf, "Toy")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "KCf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_KCgroup.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

######### 3d. Dachshund Model

OS <- OS2

#Select chosen variables

colnames(OS)

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Dachshund_status,OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Dachs", "Age")

#make the variables into a factor variable 
OS$Dachsf <- factor(OS$Dachs)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$Dachsf)
is.factor(OS$Dachsf)
#set the base
OS$Dachsf <- relevel(OS$Dachsf, "Non-Dachshund-type")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "Dachsf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`,3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_Dachs.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)


######### 3e. Spaniel Model

OS <- OS2

#Select chosen variables

colnames(OS)

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Spaniel_status,OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Spaniel", "Age")

#make the variables into a factor variable 
OS$Spanielf <- factor(OS$Spaniel)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$Spanielf)
is.factor(OS$Spanielf)
#set the base
OS$Spanielf <- relevel(OS$Spanielf, "Non-spaniel type")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "Spanielf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_Spaniel.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

######### 3f. Skullshape Model

OS <- OS2

#Select chosen variables

colnames(OS)

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Skull_Shape,OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Skull_Shape", "Age")

#make the variables into a factor variable 
OS$Skullf <- factor(OS$Skull_Shape)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$Skullf)
is.factor(OS$Skullf)
#set the base
OS$Skullf <- relevel(OS$Skullf, "Mesocephalic")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "Skullf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`,3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_Skull.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

######### 3e. Chondrodystrophy Model

OS <- OS2

#Select chosen variables
OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Chondrodystrophic,OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Chondrodystrophic", "Age")

#make the variables into a factor variable 
OS$chondrf <- factor(OS$Chondrodystrophic)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$chondrf)
is.factor(OS$chondrf)
#set the base
OS$chondrf <- relevel(OS$chondrf, "No")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "chondrf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"
# write.table(Breed_res, "./analysis/multi_reg_chondro.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)

######### 3f. Bodymass Model

OS <- OS2

#Select chosen variables

OS <- cbind.data.frame(OS$ID, OS$OSA, OS$Sex_Neuter, OS$Bodymass,OS$Age)
colnames(OS) <- c("ID", "OSA", "Sex_Neuter", "Bodymass", "Age")

#make the variables into a factor variable 
OS$massf <- factor(OS$Bodymass)
OS$OSf <- factor(OS$OSA)
OS$Sex_Neuterf <- factor(OS$Sex_Neuter)
OS$agef <- factor(OS$Age)

table(OS$OSf)
colnames(OS)
is.factor(OS$OSf)

#set the base as non case
OS$OSf <- relevel(OS$OSf, "Noncase for osteosarcoma")
table(OS$OSf)

table(OS$massf)
is.factor(OS$massf)
#set the base
OS$massf <- relevel(OS$massf, "<10")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$Sex_Neuterf)
is.factor(OS$Sex_Neuterf)
#set the base 
OS$Sex_Neuterf <- relevel(OS$Sex_Neuterf, "Female_Entire")

colnames(OS)

OS <- OS[,6:9]

multi_model <- glm(OSf ~. , data = OS, family = binomial(link="logit"))

Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace_all(cat$Category, "chondrf","")
cat$Category <- str_replace_all(cat$Category, "massf", "")
cat$Category <- str_replace_all(cat$Category, "Sex_Neuterf", "")
cat$Category <- str_replace_all(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
colnames(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)

Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

# write.table(Breed_res, "./analysis/multi_reg_bodymass.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#get the area under the roc curve 

pROC_obj <- roc(OS$OSf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

fitted(multi_model)


