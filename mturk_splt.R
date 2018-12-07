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


status_popular <- t.test(num_mturk$Q265, mu=3, alternative = 'greater')
(status_popular.l <- t.test(num_mturk$Q265, mu=3.5, alternative = 'less', alpha = .025))
(status_popular.g <- t.test(num_mturk$Q265, mu=2.5, alternative = 'greater', alpha = .025))

status_wealthy <- t.test(num_mturk$Q266, mu=3, alternative = 'greater')
(status_wealthy.l <- t.test(num_mturk$Q266, mu=3.5, alternative = 'less', alpha = .025))
(status_wealthy.g <- t.test(num_mturk$Q266, mu=2.5, alternative = 'greater', alpha = .025))

status_trendy <- t.test(num_mturk$Q273, mu=3, alternative = 'greater')
(status_trendy.l <- t.test(num_mturk$Q273, mu=3.5, alternative = 'less', alpha = .025))
(status_trendy.g <- t.test(num_mturk$Q273, mu=2.5, alternative = 'greater', alpha = .025))

status_flirty <- t.test(num_mturk$Q274, mu=3, alternative = 'greater')
(status_flirty.l <- t.test(num_mturk$Q274, mu=3.5, alternative = 'less', alpha = .025))
(status_flirty.g <- t.test(num_mturk$Q274, mu=2.5, alternative = 'greater', alpha = .025))

status_single <- t.test(num_mturk$Q275, mu=3, alternative = 'greater')
(status_single.l <- t.test(num_mturk$Q275, mu=3.5, alternative = 'less', alpha = .025))
(status_single.g <- t.test(num_mturk$Q275, mu=2.5, alternative = 'greater', alpha = .025))

status_looking <- t.test(num_mturk$Q276, mu=3, alternative = 'greater')
(status_looking.l <- t.test(num_mturk$Q276, mu=3.5, alternative = 'less', alpha = .025))
(status_looking.g <- t.test(num_mturk$Q276, mu=2.5, alternative = 'greater', alpha = .025))

status_swimming <- t.test(num_mturk$Q277, mu=3, alternative = 'greater')
(status_swimming.l <- t.test(num_mturk$Q277, mu=3.5, alternative = 'less', alpha = .025))
(status_swimming.g <- t.test(num_mturk$Q277, mu=2.5, alternative = 'greater', alpha = .025))

status_siblings <- t.test(num_mturk$Q278, mu=3, alternative = 'greater')
(status_siblings.l <- t.test(num_mturk$Q278, mu=3.5, alternative = 'less', alpha = .025))
(status_siblings.g <- t.test(num_mturk$Q278, mu=2.5, alternative = 'greater', alpha = .025))

status_milk <- t.test(num_mturk$Q279, mu=3, alternative = 'greater')
(status_milk.l <- t.test(num_mturk$Q279, mu=3.5, alternative = 'less', alpha = .025))
(status_milk.g <- t.test(num_mturk$Q279, mu=2.5, alternative = 'greater', alpha = .025))

status_whistle <- t.test(num_mturk$Q280, mu=3, alternative = 'greater')
(status_whistle.l <- t.test(num_mturk$Q280, mu=3.5, alternative = 'less', alpha = .025))
(status_whistle.g <- t.test(num_mturk$Q280, mu=2.5, alternative = 'greater', alpha = .025))

status_busy <- t.test(num_mturk$Q281, mu=3, alternative = 'greater')
(status_busy.l <- t.test(num_mturk$Q281, mu=3.5, alternative = 'less', alpha = .025))
(status_busy.g <- t.test(num_mturk$Q281, mu=2.5, alternative = 'greater', alpha = .025))



domain_avgs_rom <- colMeans(num_mturk[ ,210:220], na.rm = TRUE, dims = 1)
names(domain_avgs_rom) <- c("popular","wealthy","trendy","flirty","single","looking to date","swimming","siblings","milk","whistle","busy")
barplot(domain_avgs_rom, main = "Available for new romantic relationships", ylab = "Average rating: 1=Never,3=NotRelated,5=Always",ylim = c(0,5),las = 2)


romantic_popular <- t.test(num_mturk$Q282, mu=3, alternative = 'greater')
(romantic_popular.l <- t.test(num_mturk$Q282, mu=3.5, alternative = 'less', alpha = .025))
(romantic_popular.g <- t.test(num_mturk$Q282, mu=2.5, alternative = 'greater', alpha = .025))

romantic_wealthy <- t.test(num_mturk$Q283, mu=3, alternative = 'greater')
(romantic_wealthy.l <- t.test(num_mturk$Q283, mu=3.5, alternative = 'less', alpha = .025))
(romantic_wealthy.g <- t.test(num_mturk$Q283, mu=2.5, alternative = 'greater', alpha = .025))

romantic_trendy <- t.test(num_mturk$Q284, mu=3, alternative = 'greater')
(romantic_trendy.l <- t.test(num_mturk$Q284, mu=3.5, alternative = 'less', alpha = .025))
(romantic_trendy.g <- t.test(num_mturk$Q284, mu=2.5, alternative = 'greater', alpha = .025))

romantic_flirty <- t.test(num_mturk$Q285, mu=3, alternative = 'greater')
(romantic_flirty.l <- t.test(num_mturk$Q285, mu=3.5, alternative = 'less', alpha = .025))
(romantic_flirty.g <- t.test(num_mturk$Q285, mu=2.5, alternative = 'greater', alpha = .025))

romantic_single <- t.test(num_mturk$Q286, mu=3, alternative = 'greater')
(romantic_single.l <- t.test(num_mturk$Q286, mu=3.5, alternative = 'less', alpha = .025))
(romantic_single.g <- t.test(num_mturk$Q286, mu=2.5, alternative = 'greater', alpha = .025))

romantic_looking <- t.test(num_mturk$Q287, mu=3, alternative = 'greater')
(romantic_looking.l <- t.test(num_mturk$Q287, mu=3.5, alternative = 'less', alpha = .025))
(romantic_looking.g <- t.test(num_mturk$Q287, mu=2.5, alternative = 'greater', alpha = .025))

romantic_swimming <- t.test(num_mturk$Q288, mu=3, alternative = 'greater')
(romantic_swimming.l <- t.test(num_mturk$Q288, mu=3.5, alternative = 'less', alpha = .025))
(romantic_swimming.g <- t.test(num_mturk$Q288, mu=2.5, alternative = 'greater', alpha = .025))

romantic_siblings <- t.test(num_mturk$Q289, mu=3, alternative = 'greater')
(romantic_siblings.l <- t.test(num_mturk$Q289, mu=3.5, alternative = 'less', alpha = .025))
(romantic_siblings.g <- t.test(num_mturk$Q289, mu=2.5, alternative = 'greater', alpha = .025))

romantic_milk <- t.test(num_mturk$Q290, mu=3, alternative = 'greater')
(romantic_milk.l <- t.test(num_mturk$Q290, mu=3.5, alternative = 'less', alpha = .025))
(romantic_milk.g <- t.test(num_mturk$Q290, mu=2.5, alternative = 'greater', alpha = .025))

romantic_whistle <- t.test(num_mturk$Q291, mu=3, alternative = 'greater')
(romantic_whistle.l <- t.test(num_mturk$Q291, mu=3.5, alternative = 'less', alpha = .025))
(romantic_whistle.g <- t.test(num_mturk$Q291, mu=2.5, alternative = 'greater', alpha = .025))

romantic_busy <- t.test(num_mturk$Q292, mu=3, alternative = 'greater')
(romantic_busy.l <- t.test(num_mturk$Q292, mu=3.5, alternative = 'less', alpha = .025))
(romantic_busy.g <- t.test(num_mturk$Q292, mu=2.5, alternative = 'greater', alpha = .025))

#what descriptives do we want for these? "This person is popular/I'd be surprised to learn they're not":


