setwd("Z:/dsnlab/TAG/projects/sos_splt_mturk/raw_data")
all_mturk <- read.csv("SOS_Pilot_SPLT.csv")
all_mturk[all_mturk==""] <- NA
all_mturk <- all_mturk[-c(1:2), ]
install.packages("plotrix")
install.packages("ggplot2")
library(plotrix)
library(ggplot2)

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

#first visually see which one sticks out
num_mturk$Q293 <- factor(num_mturk$Q293)
counts_f <- table(num_mturk$Q293)
barplot(counts_f, main="Female Tall Poppy", las = 2)

#then make sure that one (in this case, #4) is significantly different
fem1_count <- as.numeric(counts_f[1])
fem2_count <- as.numeric(counts_f[2])
fem3_count <- as.numeric(counts_f[3])
fem4_count <- as.numeric(counts_f[4])
total_fem <- length(num_mturk$Q293)

fem4_fem1 <- prop.test(x = c(fem1_count, fem4_count), n = c(total_fem, total_fem))
fem4_fem2 <- prop.test(x = c(fem2_count, fem4_count), n = c(total_fem, total_fem))
fem4_fem3 <- prop.test(x = c(fem3_count, fem4_count), n = c(total_fem, total_fem))

#and then check that the remaining ones are basically the same
fem3_fem1 <- prop.test(x = c(fem1_count, fem3_count), n = c(total_fem, total_fem))
fem3_fem2 <- prop.test(x = c(fem2_count, fem3_count), n = c(total_fem, total_fem))
fem2_fem1 <- prop.test(x = c(fem1_count, fem2_count), n = c(total_fem, total_fem))

#which male face do we hate:
num_mturk$Q294 <- factor(num_mturk$Q294)
counts_m <- table(num_mturk$Q294)
barplot(counts_m, main="Male Tall Poppy", las = 2)

male1_count <- as.numeric(counts_m[1])
male2_count <- as.numeric(counts_m[2])
male3_count <- as.numeric(counts_m[3])
male4_count <- as.numeric(counts_m[4])
total_male <- length(num_mturk$Q294)

male3_male1 <- prop.test(x = c(male1_count, male3_count), n = c(total_male, total_male))
male3_male2 <- prop.test(x = c(male2_count, male3_count), n = c(total_male, total_male))
male3_male4 <- prop.test(x = c(male4_count, male3_count), n = c(total_male, total_male))

male4_male1 <- prop.test(x = c(male1_count, male4_count), n = c(total_male, total_male))
male4_male2 <- prop.test(x = c(male2_count, male4_count), n = c(total_male, total_male))
male2_male1 <- prop.test(x = c(male1_count, male2_count), n = c(total_male, total_male))


#do words fit in domain:
domain_avgs_stat <- colMeans(num_mturk[ ,199:209], na.rm = TRUE, dims = 1)
names(domain_avgs_stat) <- c("popular","wealthy","trendy","flirty","single",
                             "looking to date","swimming","siblings","milk","whistle","busy")
barplot(domain_avgs_stat, main = "High Social Status", 
        ylab = "Average rating: 1=Never,3=NotRelated,5=Always",ylim = c(0,5),las = 2)


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
names(domain_avgs_rom) <- c("popular","wealthy","trendy","flirty","single",
                            "looking to date","swimming","siblings","milk","whistle","busy")
barplot(domain_avgs_rom, main = "Available for new romantic relationships", 
        ylab = "Average rating: 1=Never,3=NotRelated,5=Always",ylim = c(0,5),las = 2)


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

# what's the average ranking for popular, wealthy, avg of pop and wealthy, flirty, single, 
#avg flirty and single, 4 neutral words, across female faces 1, 2, and 3

# every male face:

# popular
male1_pop <- (mean(num_mturk$Q502_1, na.rm = TRUE))
male1_pop_se <- std.error(num_mturk$Q502_1)
male1_pop_sur <- (mean(num_mturk$Q502_2, na.rm = TRUE))
male2_pop <- (mean(num_mturk$Q503_1, na.rm = TRUE))
male2_pop_se <- std.error(num_mturk$Q503_1)
male2_pop_sur <- (mean(num_mturk$Q503_2, na.rm = TRUE))
male3_pop <- (mean(num_mturk$Q504_1, na.rm = TRUE))
male3_pop_se <- std.error(num_mturk$Q504_1)
male3_pop_sur <- (mean(num_mturk$Q504_2, na.rm = TRUE))
male4_pop <- (mean(num_mturk$Q505_1, na.rm = TRUE))
male4_pop_se <- std.error(num_mturk$Q505_1)
male4_pop_sur <- (mean(num_mturk$Q505_2, na.rm = TRUE))

males_pop <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_pop, male2_pop, male3_pop, male4_pop), 
                        "se" = c(male1_pop_se, male2_pop_se, male3_pop_se, male4_pop_se))
males_pop$face_num=as.factor(males_pop$face_num)

males_pop_plot<- ggplot(males_pop, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_pop_plot+labs(title="Mean Rating Popular Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#females too
female1_pop <- (mean(num_mturk$Q2_1, na.rm = TRUE))
female1_pop_se <- std.error(num_mturk$Q2_1)
female1_pop_sur <- (mean(num_mturk$Q2_2, na.rm = TRUE))
female2_pop <- (mean(num_mturk$Q499_1, na.rm = TRUE))
female2_pop_se <- std.error(num_mturk$Q499_1)
female2_pop_sur <- (mean(num_mturk$Q499_2, na.rm = TRUE))
female3_pop <- (mean(num_mturk$Q500_1, na.rm = TRUE))
female3_pop_se <- std.error(num_mturk$Q500_1)
female3_pop_sur <- (mean(num_mturk$Q500_2, na.rm = TRUE))
female4_pop <- (mean(num_mturk$Q501_1, na.rm = TRUE))
female4_pop_se <- std.error(num_mturk$Q501_1)
female4_pop_sur <- (mean(num_mturk$Q501_2, na.rm = TRUE))

females_pop <- data.frame("face_num" = c(1:4), "mean_rating" = c(female1_pop, female2_pop, female3_pop, female4_pop), 
                        "se" = c(female1_pop_se, female2_pop_se, female3_pop_se, female4_pop_se))
females_pop$face_num=as.factor(females_pop$face_num)

females_pop_plot<- ggplot(females_pop, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_pop_plot+labs(title="Mean Rating Popular female Faces", x="female Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#that time I tried to do it all in base:
#males_pop <- data.frame(num_mturk$Q502_1, num_mturk$Q503_1, num_mturk$Q504_1, num_mturk$Q505_1)
#mface_pop_avgs <- colMeans(males_pop, na.rm = TRUE, dims = 1)
#names(mface_pop_avgs) <- c("male1","male2","male3","male4")
#bar_mface_pop <- barplot(mface_pop_avgs, main = "Males - Popular", ylab = "Average rating: 1=Strongly Disagree,5=Strongly Agree",ylim = c(0,5),las = 2)
#segments(bar_mface_pop, male1_pop - male1_pop_se * 2, bar_mface_pop, male2_pop + male2_pop_se * 2, lwd = 1.5)


#wealthy
male1_wea <- (mean(num_mturk$Q509_1, na.rm = TRUE))
male1_wea_se <- std.error(num_mturk$Q509_1)
male1_wea_sur <- (mean(num_mturk$Q509_2, na.rm = TRUE))
male2_wea <- (mean(num_mturk$Q510_1, na.rm = TRUE))
male2_wea_se <- std.error(num_mturk$Q510_1)
male2_wea_sur <- (mean(num_mturk$Q510_2, na.rm = TRUE))
male3_wea <- (mean(num_mturk$Q511_1, na.rm = TRUE))
male3_wea_se <- std.error(num_mturk$Q511_1)
male3_wea_sur <- (mean(num_mturk$Q511_2, na.rm = TRUE))
male4_wea <- (mean(num_mturk$Q512_1, na.rm = TRUE))
male4_wea_se <- std.error(num_mturk$Q512_1)
male4_wea_sur <- (mean(num_mturk$Q512_2, na.rm = TRUE))

males_wea <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_wea, male2_wea, male3_wea, male4_wea), 
                        "se" = c(male1_wea_se, male2_wea_se, male3_wea_se, male4_wea_se))
males_wea$face_num=as.factor(males_wea$face_num)

males_wea_plot<- ggplot(males_wea, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_wea_plot+labs(title="Mean Rating Wealthy Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

female1_wea <- (mean(num_mturk$Q489_1, na.rm = TRUE))
female1_wea_se <- std.error(num_mturk$Q489_1)
female1_wea_sur <- (mean(num_mturk$Q489_2, na.rm = TRUE))
female2_wea <- (mean(num_mturk$Q506_1, na.rm = TRUE))
female2_wea_se <- std.error(num_mturk$Q506_1)
female2_wea_sur <- (mean(num_mturk$Q506_2, na.rm = TRUE))
female3_wea <- (mean(num_mturk$Q507_1, na.rm = TRUE))
female3_wea_se <- std.error(num_mturk$Q507_1)
female3_wea_sur <- (mean(num_mturk$Q507_2, na.rm = TRUE))
female4_wea <- (mean(num_mturk$Q508_1, na.rm = TRUE))
female4_wea_se <- std.error(num_mturk$Q508_1)
female4_wea_sur <- (mean(num_mturk$Q508_2, na.rm = TRUE))

females_wea <- data.frame("face_num" = c(1:4), "mean_rating" = c(female1_wea, female2_wea, female3_wea, female4_wea), 
                        "se" = c(female1_wea_se, female2_wea_se, female3_wea_se, female4_wea_se))
females_wea$face_num=as.factor(females_wea$face_num)

females_wea_plot<- ggplot(females_wea, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_wea_plot+labs(title="Mean Rating Wealthy female Faces", x="female Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)



#avg popular and wealthy
male1_pop_wea <- mean(rbind(num_mturk$Q502_1, num_mturk$Q509_1), na.rm = TRUE)
male2_pop_wea <- mean(rbind(num_mturk$Q503_1, num_mturk$Q510_1), na.rm = TRUE)
male3_pop_wea <- mean(rbind(num_mturk$Q504_1, num_mturk$Q511_1), na.rm = TRUE)
male4_pop_wea <- mean(rbind(num_mturk$Q505_1, num_mturk$Q512_1), na.rm = TRUE)

males_pop_wea <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_pop_wea, male2_pop_wea, male3_pop_wea, male4_pop_wea)) 
                        #"se" = c(male1_wea_se, male2_wea_se, male3_wea_se, male4_wea_se))
males_pop_wea$face_num=as.factor(males_pop_wea$face_num)

males_pop_wea_plot<- ggplot(males_pop_wea, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 
  #geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
               # position=position_dodge(.9)) 

males_pop_wea_plot+labs(title="Mean Rating Popular & Wealthy Male Faces", x="Male Face Number", 
                        y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#flirty
male1_fli <- (mean(num_mturk$Q523_1, na.rm = TRUE))
male1_fli_se <- std.error(num_mturk$Q523_1)
male1_fli_sur <- (mean(num_mturk$Q523_2, na.rm = TRUE))
male2_fli <- (mean(num_mturk$Q524_1, na.rm = TRUE))
male2_fli_se <- std.error(num_mturk$Q524_1)
male2_fli_sur <- (mean(num_mturk$Q524_2, na.rm = TRUE))
male3_fli <- (mean(num_mturk$Q525_1, na.rm = TRUE))
male3_fli_se <- std.error(num_mturk$Q525_1)
male3_fli_sur <- (mean(num_mturk$Q525_2, na.rm = TRUE))
male4_fli <- (mean(num_mturk$Q526_1, na.rm = TRUE))
male4_fli_se <- std.error(num_mturk$Q526_1)
male4_fli_sur <- (mean(num_mturk$Q526_2, na.rm = TRUE))

males_fli <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_fli, male2_fli, male3_fli, male4_fli), 
                        "se" = c(male1_fli_se, male2_fli_se, male3_fli_se, male4_fli_se))
males_fli$face_num=as.factor(males_fli$face_num)

males_fli_plot<- ggplot(males_fli, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_fli_plot+labs(title="Mean Rating Flirty Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#single
male1_sin <- (mean(num_mturk$Q530_1, na.rm = TRUE))
male1_sin_se <- std.error(num_mturk$Q530_1)
male1_sin_sur <- (mean(num_mturk$Q530_2, na.rm = TRUE))
male2_sin <- (mean(num_mturk$Q531_1, na.rm = TRUE))
male2_sin_se <- std.error(num_mturk$Q531_1)
male2_sin_sur <- (mean(num_mturk$Q531_2, na.rm = TRUE))
male3_sin <- (mean(num_mturk$Q532_1, na.rm = TRUE))
male3_sin_se <- std.error(num_mturk$Q532_1)
male3_sin_sur <- (mean(num_mturk$Q532_2, na.rm = TRUE))
male4_sin <- (mean(num_mturk$Q533_1, na.rm = TRUE))
male4_sin_se <- std.error(num_mturk$Q533_1)
male4_sin_sur <- (mean(num_mturk$Q533_2, na.rm = TRUE))

males_sin <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_sin, male2_sin, male3_sin, male4_sin), 
                        "se" = c(male1_sin_se, male2_sin_se, male3_sin_se, male4_sin_se))
males_sin$face_num=as.factor(males_sin$face_num)

males_sin_plot<- ggplot(males_sin, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_sin_plot+labs(title="Mean Rating Single Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#avg flirty and single
male1_fli_sin <- mean(rbind(num_mturk$Q523_1, num_mturk$Q530_1), na.rm = TRUE)
male2_fli_sin <- mean(rbind(num_mturk$Q524_1, num_mturk$Q531_1), na.rm = TRUE)
male3_fli_sin <- mean(rbind(num_mturk$Q525_1, num_mturk$Q532_1), na.rm = TRUE)
male4_fli_sin <- mean(rbind(num_mturk$Q526_1, num_mturk$Q533_1), na.rm = TRUE)

males_fli_sin <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_fli_sin, male2_fli_sin, male3_fli_sin, male4_fli_sin)) 
males_fli_sin$face_num=as.factor(males_fli_sin$face_num)

males_fli_sin_plot<- ggplot(males_fli_sin, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_fli_sin_plot+labs(title="Mean Rating Flirty & Single Male Faces", x="Male Face Number", 
                        y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#swimming
male1_swi <- (mean(num_mturk$Q544_1, na.rm = TRUE))
male1_swi_se <- std.error(num_mturk$Q544_1)
male1_swi_sur <- (mean(num_mturk$Q544_2, na.rm = TRUE))
male2_swi <- (mean(num_mturk$Q544_1, na.rm = TRUE))
male2_swi_se <- std.error(num_mturk$Q544_1)
male2_swi_sur <- (mean(num_mturk$Q544_2, na.rm = TRUE))
male3_swi <- (mean(num_mturk$Q544_1, na.rm = TRUE))
male3_swi_se <- std.error(num_mturk$Q544_1)
male3_swi_sur <- (mean(num_mturk$Q544_2, na.rm = TRUE))
male4_swi <- (mean(num_mturk$Q544_1, na.rm = TRUE))
male4_swi_se <- std.error(num_mturk$Q544_1)
male4_swi_sur <- (mean(num_mturk$Q544_2, na.rm = TRUE))

males_swi <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_swi, male2_swi, male3_swi, male4_swi), 
                        "se" = c(male1_swi_se, male2_swi_se, male3_swi_se, male4_swi_se))
males_swi$face_num=as.factor(males_swi$face_num)

males_swi_plot<- ggplot(males_swi, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_swi_plot+labs(title="Mean Rating Swimming Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


#siblings
male1_sib <- (mean(num_mturk$Q551_1, na.rm = TRUE))
male1_sib_se <- std.error(num_mturk$Q551_1)
male1_sib_sur <- (mean(num_mturk$Q551_2, na.rm = TRUE))
male2_sib <- (mean(num_mturk$Q552_1, na.rm = TRUE))
male2_sib_se <- std.error(num_mturk$Q552_1)
male2_sib_sur <- (mean(num_mturk$Q552_2, na.rm = TRUE))
male3_sib <- (mean(num_mturk$Q553_1, na.rm = TRUE))
male3_sib_se <- std.error(num_mturk$Q553_1)
male3_sib_sur <- (mean(num_mturk$Q553_2, na.rm = TRUE))
male4_sib <- (mean(num_mturk$Q554_1, na.rm = TRUE))
male4_sib_se <- std.error(num_mturk$Q554_1)
male4_sib_sur <- (mean(num_mturk$Q554_2, na.rm = TRUE))

males_sib <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_sib, male2_sib, male3_sib, male4_sib), 
                        "se" = c(male1_sib_se, male2_sib_se, male3_sib_se, male4_sib_se))
males_sib$face_num=as.factor(males_sib$face_num)

males_sib_plot<- ggplot(males_sib, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_sib_plot+labs(title="Mean Rating Siblings Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

#milk
male1_mil <- (mean(num_mturk$Q558_1, na.rm = TRUE))
male1_mil_se <- std.error(num_mturk$Q558_1)
male1_mil_sur <- (mean(num_mturk$Q558_2, na.rm = TRUE))
male2_mil <- (mean(num_mturk$Q559_1, na.rm = TRUE))
male2_mil_se <- std.error(num_mturk$Q559_1)
male2_mil_sur <- (mean(num_mturk$Q559_2, na.rm = TRUE))
male3_mil <- (mean(num_mturk$Q560_1, na.rm = TRUE))
male3_mil_se <- std.error(num_mturk$Q560_1)
male3_mil_sur <- (mean(num_mturk$Q560_2, na.rm = TRUE))
male4_mil <- (mean(num_mturk$Q561_1, na.rm = TRUE))
male4_mil_se <- std.error(num_mturk$Q561_1)
male4_mil_sur <- (mean(num_mturk$Q561_2, na.rm = TRUE))

males_mil <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_mil, male2_mil, male3_mil, male4_mil), 
                        "se" = c(male1_mil_se, male2_mil_se, male3_mil_se, male4_mil_se))
males_mil$face_num=as.factor(males_mil$face_num)

males_mil_plot<- ggplot(males_mil, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_mil_plot+labs(title="Mean Rating Milk Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


#whistle
male1_whi <- (mean(num_mturk$Q565_1, na.rm = TRUE))
male1_whi_se <- std.error(num_mturk$Q565_1)
male1_whi_sur <- (mean(num_mturk$Q565_2, na.rm = TRUE))
male2_whi <- (mean(num_mturk$Q566_1, na.rm = TRUE))
male2_whi_se <- std.error(num_mturk$Q566_1)
male2_whi_sur <- (mean(num_mturk$Q566_2, na.rm = TRUE))
male3_whi <- (mean(num_mturk$Q567_1, na.rm = TRUE))
male3_whi_se <- std.error(num_mturk$Q567_1)
male3_whi_sur <- (mean(num_mturk$Q567_2, na.rm = TRUE))
male4_whi <- (mean(num_mturk$Q568_1, na.rm = TRUE))
male4_whi_se <- std.error(num_mturk$Q568_1)
male4_whi_sur <- (mean(num_mturk$Q568_2, na.rm = TRUE))

males_whi <- data.frame("face_num" = c(1:4), "mean_rating" = c(male1_whi, male2_whi, male3_whi, male4_whi), 
                        "se" = c(male1_whi_se, male2_whi_se, male3_whi_se, male4_whi_se))
males_whi$face_num=as.factor(males_whi$face_num)

males_whi_plot<- ggplot(males_whi, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_whi_plot+labs(title="Mean Rating Whistle Male Faces", x="Male Face Number", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


# how does that compare to the average rating for that word across the female (#1, #2, #3) faces?
females_pop <- mean(rbind(num_mturk$Q2_1, num_mturk$Q499_1, num_mturk$Q500_1), na.rm = TRUE)
females_wea <- mean(rbind(num_mturk$Q489_1, num_mturk$Q506_1, num_mturk$Q507_1), na.rm = TRUE)
females_fli <- mean(rbind(num_mturk$Q491_1, num_mturk$Q520_1, num_mturk$Q521_1), na.rm = TRUE)
females_sin <- mean(rbind(num_mturk$Q492_1, num_mturk$Q527_1, num_mturk$Q528_1), na.rm = TRUE)
females_mil <- mean(rbind(num_mturk$Q496_1, num_mturk$Q555_1, num_mturk$Q556_1), na.rm = TRUE)
females_sib <- mean(rbind(num_mturk$Q495_1, num_mturk$Q548_1, num_mturk$Q549_1), na.rm = TRUE)
females_swi <- mean(rbind(num_mturk$Q494_1, num_mturk$Q541_1, num_mturk$Q542_1), na.rm = TRUE)
females_whi <- mean(rbind(num_mturk$Q497_1, num_mturk$Q562_1, num_mturk$Q563_1), na.rm = TRUE)


females_allwords <- data.frame("word" = c("popular","wealthy","flirty","single","milk","siblings",
                                          "swimming","whistle"), 
                            "mean_rating" = c(females_pop, females_wea, females_fli, females_sin, 
                                              females_mil, females_sib, females_swi, females_whi)) 
females_allwords$word=as.factor(females_allwords$word)

females_allwords_plot<- ggplot(females_allwords, aes(x=word, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

females_allwords_plot+labs(title="Mean Rating Words All Female Faces", x="Word", 
                           y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


males_pop2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_pop, male2_pop, male3_pop, male4_pop, females_pop))
males_pop2$face_num=as.factor(males_pop2$face_num)

males_pop2_plot<- ggplot(males_pop2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_pop2_plot+labs(title="POPULAR Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

males_wea2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_wea, male2_wea, male3_wea, male4_wea, females_wea))
males_wea2$face_num=as.factor(males_wea2$face_num)

males_wea2_plot<- ggplot(males_wea2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_wea2_plot+labs(title="WEALTHY Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


males_fli2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_fli, male2_fli, male3_fli, male4_fli, females_fli))
males_fli2$face_num=as.factor(males_fli2$face_num)

males_fli2_plot<- ggplot(males_fli2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_fli2_plot+labs(title="FLIRTY Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

males_sin2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_sin, male2_sin, male3_sin, male4_sin, females_sin))
males_sin2$face_num=as.factor(males_sin2$face_num)

males_sin2_plot<- ggplot(males_sin2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_sin2_plot+labs(title="SINGLE Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

males_mil2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_mil, male2_mil, male3_mil, male4_mil, females_mil))
males_mil2$face_num=as.factor(males_mil2$face_num)

males_mil2_plot<- ggplot(males_mil2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_mil2_plot+labs(title="MILK Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

males_sib2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_sib, male2_sib, male3_sib, male4_sib, females_sib))
males_sib2$face_num=as.factor(males_sib2$face_num)

males_sib2_plot<- ggplot(males_sib2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_sib2_plot+labs(title="SIBLINGS Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


males_swi2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_swi, male2_swi, male3_swi, male4_swi, females_swi))
males_swi2$face_num=as.factor(males_swi2$face_num)

males_swi2_plot<- ggplot(males_swi2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

males_swi2_plot+labs(title="SWIMMING Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)


males_whi2 <- data.frame("face_num" = c("Male1","Male2","Male3","Male4","All 3 Females"), 
                         "mean_rating" = c(male1_whi, male2_whi, male3_whi, male4_whi, females_whi))
males_whi2$face_num=as.factor(males_whi2$face_num)

males_whi2_plot<- ggplot(males_whi2, aes(x=face_num, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 
  
males_whi2_plot+labs(title="WHISTLE Male Faces vs All Females", x="Male Face Number", 
                     y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

# So, it seems like we should choose Female Faces #1, #2, #3, and Male Faces #1, #2, #4. 
# What do those words look like across each of those averages (by gender)?

females_pop <- mean(rbind(num_mturk$Q2_1, num_mturk$Q499_1, num_mturk$Q500_1), na.rm = TRUE)
females_pop_sd <- sd(rbind(num_mturk$Q2_1, num_mturk$Q499_1, num_mturk$Q500_1), na.rm = TRUE)
females_pop_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q2_1, num_mturk$Q499_1, num_mturk$Q500_1)))
                     
females_wea <- mean(rbind(num_mturk$Q489_1, num_mturk$Q506_1, num_mturk$Q507_1), na.rm = TRUE)
females_wea_sd <- sd(rbind(num_mturk$Q489_1, num_mturk$Q506_1, num_mturk$Q507_1), na.rm = TRUE)
females_wea_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q489_1, num_mturk$Q506_1, num_mturk$Q507_1)))

females_fli <- mean(rbind(num_mturk$Q491_1, num_mturk$Q520_1, num_mturk$Q521_1), na.rm = TRUE)
females_fli_sd <- sd(rbind(num_mturk$Q491_1, num_mturk$Q520_1, num_mturk$Q521_1), na.rm = TRUE)
females_fli_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q491_1, num_mturk$Q520_1, num_mturk$Q521_1)))

females_sin <- mean(rbind(num_mturk$Q492_1, num_mturk$Q527_1, num_mturk$Q528_1), na.rm = TRUE)
females_sin_sd <- sd(rbind(num_mturk$Q492_1, num_mturk$Q527_1, num_mturk$Q528_1), na.rm = TRUE)
females_sin_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q492_1, num_mturk$Q527_1, num_mturk$Q528_1)))

females_mil <- mean(rbind(num_mturk$Q496_1, num_mturk$Q555_1, num_mturk$Q556_1), na.rm = TRUE)
females_mil_sd <- sd(rbind(num_mturk$Q496_1, num_mturk$Q555_1, num_mturk$Q556_1), na.rm = TRUE)
females_mil_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q496_1, num_mturk$Q555_1, num_mturk$Q556_1)))

females_sib <- mean(rbind(num_mturk$Q495_1, num_mturk$Q548_1, num_mturk$Q549_1), na.rm = TRUE)
females_sib_sd <- sd(rbind(num_mturk$Q495_1, num_mturk$Q548_1, num_mturk$Q549_1), na.rm = TRUE)
females_sib_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q495_1, num_mturk$Q548_1, num_mturk$Q549_1)))

females_swi <- mean(rbind(num_mturk$Q494_1, num_mturk$Q541_1, num_mturk$Q542_1), na.rm = TRUE)
females_swi_sd <- sd(rbind(num_mturk$Q494_1, num_mturk$Q541_1, num_mturk$Q542_1), na.rm = TRUE)
females_swi_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q494_1, num_mturk$Q541_1, num_mturk$Q542_1)))

females_whi <- mean(rbind(num_mturk$Q497_1, num_mturk$Q562_1, num_mturk$Q563_1), na.rm = TRUE)
females_whi_sd <- sd(rbind(num_mturk$Q497_1, num_mturk$Q562_1, num_mturk$Q563_1), na.rm = TRUE)
females_whi_se <- females_pop_sd/sum(!is.na(rbind(num_mturk$Q497_1, num_mturk$Q520_1, num_mturk$Q563_1)))

females_allwords <- data.frame("word" = c("1_popular","2_wealthy","3_flirty","4_single","5_milk","6_siblings",
                                        "7_swimming", "8_whistle"), 
                             "mean_rating" = c(females_pop, females_wea, females_fli, females_sin, 
                                               females_mil, females_sib, females_swi, females_whi), 
                             "se" = c(females_pop_se, females_wea_se, females_fli_se, females_sin_se, females_mil_se,
                                      females_sib_se, females_swi_se, females_whi_se))
females_allwords$word=as.factor(females_allwords$word)

females_allwords_plot<- ggplot(females_allwords, aes(x=word, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_allwords_plot+labs(title="Mean Rating by Word - Female Faces 1, 2, 3", x="Word", 
                         y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)




males_pop <- mean(rbind(num_mturk$Q502_1, num_mturk$Q503_1, num_mturk$Q505_1), na.rm = TRUE)
males_pop_sd <- sd(rbind(num_mturk$Q502_1, num_mturk$Q503_1, num_mturk$Q505_1), na.rm = TRUE)
males_pop_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q502_1, num_mturk$Q503_1, num_mturk$Q505_1)))

males_wea <- mean(rbind(num_mturk$Q509_1, num_mturk$Q510_1, num_mturk$Q512_1), na.rm = TRUE)
males_wea_sd <- sd(rbind(num_mturk$Q509_1, num_mturk$Q510_1, num_mturk$Q512_1), na.rm = TRUE)
males_wea_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q509_1, num_mturk$Q510_1, num_mturk$Q512_1)))

males_fli <- mean(rbind(num_mturk$Q523_1, num_mturk$Q524_1, num_mturk$Q526_1), na.rm = TRUE)
males_fli_sd <- sd(rbind(num_mturk$Q523_1, num_mturk$Q524_1, num_mturk$Q526_1), na.rm = TRUE)
males_fli_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q523_1, num_mturk$Q524_1, num_mturk$Q526_1)))

males_sin <- mean(rbind(num_mturk$Q530_1, num_mturk$Q531_1, num_mturk$Q533_1), na.rm = TRUE)
males_sin_sd <- sd(rbind(num_mturk$Q530_1, num_mturk$Q531_1, num_mturk$Q533_1), na.rm = TRUE)
males_sin_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q530_1, num_mturk$Q531_1, num_mturk$Q533_1)))

males_mil <- mean(rbind(num_mturk$Q558_1, num_mturk$Q559_1, num_mturk$Q561_1), na.rm = TRUE)
males_mil_sd <- sd(rbind(num_mturk$Q558_1, num_mturk$Q559_1, num_mturk$Q561_1), na.rm = TRUE)
males_mil_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q558_1, num_mturk$Q559_1, num_mturk$Q561_1)))

males_sib <- mean(rbind(num_mturk$Q551_1, num_mturk$Q552_1, num_mturk$Q554_1), na.rm = TRUE)
males_sib_sd <- sd(rbind(num_mturk$Q551_1, num_mturk$Q552_1, num_mturk$Q554_1), na.rm = TRUE)
males_sib_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q551_1, num_mturk$Q552_1, num_mturk$Q554_1)))

males_swi <- mean(rbind(num_mturk$Q544_1, num_mturk$Q545_1, num_mturk$Q547_1), na.rm = TRUE)
males_swi_sd <- sd(rbind(num_mturk$Q544_1, num_mturk$Q545_1, num_mturk$Q547_1), na.rm = TRUE)
males_swi_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q544_1, num_mturk$Q545_1, num_mturk$Q547_1)))

males_whi <- mean(rbind(num_mturk$Q565_1, num_mturk$Q566_1, num_mturk$Q568_1), na.rm = TRUE)
males_whi_sd <- sd(rbind(num_mturk$Q565_1, num_mturk$Q566_1, num_mturk$Q568_1), na.rm = TRUE)
males_whi_se <- males_pop_sd/sum(!is.na(rbind(num_mturk$Q565_1, num_mturk$Q566_1, num_mturk$Q568_1)))

males_allwords <- data.frame("word" = c("1_popular","2_wealthy","3_flirty","4_single","5_milk","6_siblings",
                                        "7_swimming", "8_whistle"), 
                             "mean_rating" = c(males_pop, males_wea, males_fli, males_sin, 
                                               males_mil, males_sib, males_swi, males_whi), 
                        "se" = c(males_pop_se, males_wea_se, males_fli_se, males_sin_se, males_mil_se,
                                 males_sib_se, males_swi_se, males_whi_se))
males_allwords$word=as.factor(males_allwords$word)

males_allwords_plot<- ggplot(males_allwords, aes(x=word, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

males_allwords_plot+labs(title="Mean Rating by Word - Male Faces 1, 2, 4", x="Word", 
                    y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

# Compare average females vs. males by word

# POPULAR

females_males_pop <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_pop, males_pop), 
                                "se" = c(females_pop_se, males_pop_se))
females_males_pop$gender=as.factor(females_males_pop$gender)

females_males_pop_plot<- ggplot(females_males_pop, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_pop_plot+labs(title="POPULAR - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_pop_ttest <- t.test((rbind(num_mturk$Q2_1, num_mturk$Q499_1, num_mturk$Q500_1)), 
       (rbind(num_mturk$Q502_1, num_mturk$Q503_1, num_mturk$Q505_1)))

# WEALTHY

females_males_wea <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_wea, males_wea), 
                                "se" = c(females_wea_se, males_wea_se))
females_males_wea$gender=as.factor(females_males_wea$gender)

females_males_wea_plot<- ggplot(females_males_wea, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_wea_plot+labs(title="WEALTHY - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_wea_ttest <- t.test((rbind(num_mturk$Q489_1, num_mturk$Q506_1, num_mturk$Q507_1)), 
       (rbind(num_mturk$Q509_1, num_mturk$Q510_1, num_mturk$Q512_1)))

# FLIRTY

females_males_fli <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_fli, males_fli), 
                                "se" = c(females_fli_se, males_fli_se))
females_males_fli$gender=as.factor(females_males_fli$gender)

females_males_fli_plot<- ggplot(females_males_fli, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_fli_plot+labs(title="FLIRTY - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_fli_ttest <- t.test((rbind(num_mturk$Q491_1, num_mturk$Q520_1, num_mturk$Q521_1)), 
                                  (rbind(num_mturk$Q523_1, num_mturk$Q524_1, num_mturk$Q526_1)))

# SINGLE

females_males_sin <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_sin, males_sin), 
                                "se" = c(females_sin_se, males_sin_se))
females_males_sin$gender=as.factor(females_males_sin$gender)

females_males_sin_plot<- ggplot(females_males_sin, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_sin_plot+labs(title="SINGLE - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_sin_ttest <- t.test((rbind(num_mturk$Q492_1, num_mturk$Q527_1, num_mturk$Q528_1)), 
                                  (rbind(num_mturk$Q530_1, num_mturk$Q531_1, num_mturk$Q533_1)))


# MILK

females_males_mil <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_mil, males_mil), 
                                "se" = c(females_mil_se, males_mil_se))
females_males_mil$gender=as.factor(females_males_mil$gender)

females_males_mil_plot<- ggplot(females_males_mil, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_mil_plot+labs(title="MILK - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_mil_ttest <- t.test((rbind(num_mturk$Q496_1, num_mturk$Q555_1, num_mturk$Q556_1)), 
                                  (rbind(num_mturk$Q558_1, num_mturk$Q559_1, num_mturk$Q561_1)))


# SIBLINGS

females_males_sib <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_sib, males_sib), 
                                "se" = c(females_sib_se, males_sib_se))
females_males_sib$gender=as.factor(females_males_sib$gender)

females_males_sib_plot<- ggplot(females_males_sib, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_sib_plot+labs(title="SIBLINGS - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_sib_ttest <- t.test((rbind(num_mturk$Q495_1, num_mturk$Q548_1, num_mturk$Q549_1)), 
                                  (rbind(num_mturk$Q551_1, num_mturk$Q552_1, num_mturk$Q554_1)))

# SWIMMING

females_males_swi <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_swi, males_swi), 
                                "se" = c(females_swi_se, males_swi_se))
females_males_swi$gender=as.factor(females_males_swi$gender)

females_males_swi_plot<- ggplot(females_males_swi, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_swi_plot+labs(title="SWIMMING - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_swi_ttest <- t.test((rbind(num_mturk$Q494_1, num_mturk$Q541_1, num_mturk$Q542_1)), 
                                  (rbind(num_mturk$Q544_1, num_mturk$Q545_1, num_mturk$Q547_1)))

# WHISTLE

females_males_whi <- data.frame("gender" = c("females","males"), 
                                "mean_rating" = c(females_whi, males_whi), 
                                "se" = c(females_whi_se, males_whi_se))
females_males_whi$gender=as.factor(females_males_whi$gender)

females_males_whi_plot<- ggplot(females_males_whi, aes(x=gender, y=mean_rating)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_rating-se, ymax=mean_rating+se), width=.2,
                position=position_dodge(.9)) 

females_males_whi_plot+labs(title="WHISTLE - Female Faces 1, 2, 3 (x); Male Faces 1, 2, 4 (y)", 
                            x="gender", y = "Mean Rating (1=Strongly Disagree, 5=Strongly Agree)")+
  theme_grey() +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0, 5)

females_males_whi_ttest <- t.test((rbind(num_mturk$Q497_1, num_mturk$Q562_1, num_mturk$Q563_1)), 
                                  (rbind(num_mturk$Q565_1, num_mturk$Q566_1, num_mturk$Q568_1)))


# 2 x 2 ANOVA across faces and words to be not significant.

# order of these three blocks randomized:

# POST SPLT - just have them answer this for each face ("In this task, which word did 
#this face go with most often?")

# Post-SPLT - each of the 6 faces ask "This person has high social status" (how much they agree).
# "This person is available for new romantic relationships"
# "I would like to hang out with this person"

# How interesting was it to learn if the people were [ each of the words ]