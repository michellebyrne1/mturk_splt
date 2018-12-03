setwd("Z:/dsnlab/TAG/projects/sos_splt_mturk/raw_data")
all_mturk <- read.csv("SOS_Pilot_SPLT.csv")
all_mturk[all_mturk==""] <- NA
all_mturk <- all_mturk[-c(1:2), ]

#if no qualtrics code then no data or it's shitty so bye:
good_mturk <- all_mturk[complete.cases(all_mturk$mTurkCode), ]

#qualtrics num output not useful so I recoded qual version:
num_mturk <- good_mturk

#first the agree/disagree for faces:
for (i in 21:198) {
num_mturk[ , i] <- 
  ifelse(num_mturk[ , i] == "Strongly disagree", 1, 
       ifelse(num_mturk[ , i] == "Somewhat disagree", 2,
              ifelse(num_mturk[ , i] == "Neither agree nor disagree", 3,
                     ifelse(num_mturk[ , i] == "Somewhat agree", 4,
                            ifelse(num_mturk[ , i] == "Strongly agree", 5, NA)))))
}

#then items for does this word fit in the domain: 
for (i in 199:220) {
  num_mturk[ , i] <- 
    ifelse(num_mturk[ , i] == "Never", 1, 
           ifelse(num_mturk[ , i] == "Rarely", 2,
                  ifelse(num_mturk[ , i] == "They're not really related", 3,
                         ifelse(num_mturk[ , i] == "Sometimes", 4,
                                ifelse(num_mturk[ , i] == "Always", 5, NA)))))
}

#which female face do we hate:
num_mturk$Q293 <- factor(num_mturk$Q293)
counts_f <- table(num_mturk$Q293)
barplot(counts_f, main="Female Tall Poppy", las = 2)

#which male face do we hate:
num_mturk$Q294 <- factor(num_mturk$Q294)
counts_m <- table(num_mturk$Q294)
barplot(counts_m, main="Male Tall Poppy", las = 2)

#do words fit in domain:
domain_avgs_stat <- colMeans(num_mturk[ ,199:209], na.rm = TRUE, dims = 1)
names(domain_avgs_stat) <- c("popular","wealthy","trendy","flirty","single","looking to date","swimming","siblings","milk","whistle","busy")
barplot(domain_avgs_stat, main = "High Social Status", ylab = "Average rating: 1=Never,3=NotRelated,5=Always",ylim = c(0,5),las = 2)

domain_avgs_rom <- colMeans(num_mturk[ ,210:220], na.rm = TRUE, dims = 1)
names(domain_avgs_rom) <- c("popular","wealthy","trendy","flirty","single","looking to date","swimming","siblings","milk","whistle","busy")
barplot(domain_avgs_rom, main = "Available for new romantic relationships", ylab = "Average rating: 1=Never,3=NotRelated,5=Always",ylim = c(0,5),las = 2)

#what descriptives do we want for these? "This person is popular/I'd be surprised to learn they're not":

#correlations and other tests TBD:
