###############################################################################
# Molly McDermott
# Created 8/9/22
# Exploratory analysis of survey data
##############################################################################

#### SETUP ####

#load libraries
library(car)
library(tidyverse)
library(lme4)
library(lmerTest)
library(predictmeans)


#load data file
sv <- read.csv("RawDataExport_numeric_forR_all.csv")


#fix column formats
sv$EndDate <- as.Date(sv$EndDate, format = "%m/%d/%Y")
sv$Test <- as.factor(sv$Test)
sv$Test <- factor(sv$Test, levels=c("Pre","Post"))
sv$UniqueID <- as.factor(sv$UniqueID)
sv$ParticipantMentor <- as.factor(sv$ParticipantMentor)

#recode reverse-scored items
sv <- sv %>%
  mutate(
    CNS_4 = as.numeric(
      dplyr::recode(as.character(CNS_4), "1" = "5", "2" = "4", "4" = "2", "5" = "1")),
    CNS_12 = as.numeric(
      dplyr::recode(as.character(CNS_12), "1" = "5", "2" = "4", "4" = "2", "5" = "1")),
    CNS_14 = as.numeric(
      dplyr::recode(as.character(CNS_14), "1" = "5", "2" = "4", "4" = "2", "5" = "1")),
    CCHS_9 = as.numeric(
      dplyr::recode(as.character(CCHS_9), "1" = "5", "2" = "4", "4" = "2", "5" = "1")),
    CCHS_10 = as.numeric(
      dplyr::recode(as.character(CCHS_10), "1" = "5", "2" = "4", "4" = "2", "5" = "1")),
    CCHS_11 = as.numeric(
      dplyr::recode(as.character(CCHS_11), "1" = "5", "2" = "4", "4" = "2", "5" = "1"))
  )


#create summary variable of each survey instrument
sv_sum <- sv %>% 
  rowwise() %>%
  mutate(sci_sum = sum(c_across(starts_with("Sci"))),
         sci_comp_sum = sum(c_across(starts_with("SciComp"))),
         sci_int_sum = sum(c_across(starts_with("SciInt"))),
         sci_ID_sum = sum(c_across(starts_with("SciID"))),
         cns_sum = sum(c_across(starts_with("CNS")), na.rm = TRUE),
         cchs_sum = sum(c_across(starts_with("CCHS"))),
         cchs_eff_sum = sum(c_across(starts_with("CCHS_efficacy"))))

#### INDIVIDUAL QUESTIONS ####
#summarize question responses
vars <- c("sci_sum","sci_comp_sum", "sci_int_sum", "sci_ID_sum","cns_sum", "cchs_sum", "cchs_eff_sum")

sv_sum %>%
  group_by(Test) %>%
  summarise(across(starts_with("sci_"), ~ mean(.x, na.rm = TRUE)))

sv_sum %>%
  group_by(Test) %>%
  summarise(across(vars, ~ mean(.x, na.rm = TRUE)))

sv_sum %>%
  group_by(Test) %>%
  summarise(across(vars, ~ sd(.x, na.rm = TRUE)))


#### DATA ANALYSIS ####
#histograms
vars <- c("sci_sum","sci_comp_sum", "sci_int_sum", "sci_ID_sum","cns_sum", "cchs_sum", "cchs_eff_sum")

hist(sv_sum$sci_sum)
shapiro.test(sv_sum$sci_sum)

hist(sv_sum$sci_comp_sum)
hist(sv_sum$sci_int_sum)
hist(sv_sum$sci_ID_sum)

hist(sv_sum$cns_sum)
shapiro.test(sv_sum$cns_sum)

hist(sv_sum$cchs_sum)
shapiro.test(sv_sum$cchs_sum)

hist(sv_sum$cchs_eff_sum)

#linear models
scimod <- lmer(sci_sum ~ Test + (1|UniqueID), data = sv_sum)
residplot(scimod) # 2 observation2 skew normality

cnsmod <- lmer(cns_sum ~ Test + (1|UniqueID), data = sv_sum)
residplot(cnsmod) # looks fine
summary(cnsmod)

cchsmod <- lmer(cchs_sum ~ Test + (1|UniqueID), data = sv_sum)
residplot(cchsmod) #trend in residuals that may need investigation


#create data sets with complete cases only
ids <- c("143","155","268","305","351","375","611","615","881","950")
sci_ids <- c("143","155","268","305","351","611","615","881","950")

cc <- sv_sum %>%
  filter(UniqueID %in% ids)
sci <- sv_sum %>%
  filter(UniqueID %in% sci_ids)


#permutation test package
library(coin)

#independence test, permutation based, can handle  non-normal data

oneway_test(sci_sum ~ Test | UniqueID,
            data = sci,
            alternative = "less")
oneway_test(cns_sum ~ Test | UniqueID,
            data = cc,
            alternative = "less")
oneway_test(cchs_sum ~ Test | UniqueID,
            data = cc,
            alternative = "less")


#overall summary statistics
scc <- cc %>%
  group_by(Test) %>%
  summarize(mean.cns = mean(cns_sum), std.cns = sd(cns_sum),
            mean.cchs = mean(cchs_sum), std.cchs = sd(cchs_sum),
            mean.cchs.eff = mean(cchs_eff_sum), std.cchs.eff = sd(cchs_eff_sum))
write.csv(scc, "CNS_CCHS_mean_SD.csv")
sccpm <- cc %>%
  group_by(Test, ParticipantMentor) %>%
  summarize(mean.cns = mean(cns_sum), std.cns = sd(cns_sum),
            mean.cchs = mean(cchs_sum), std.cchs = sd(cchs_sum),
            mean.cchs.eff = mean(cchs_eff_sum), std.cchs.eff = sd(cchs_eff_sum))

ssci <- sci %>%
  group_by(Test) %>%
  summarize(mean.sci = mean(sci_sum), std.sci = sd(sci_sum),
            mean.comp = mean(sci_comp_sum), std.comp = sd(sci_comp_sum),
            mean.int = mean(sci_int_sum), std.int = sd(sci_int_sum),
            mean.id = mean(sci_ID_sum), std.id = sd(sci_ID_sum))
write.csv(ssci, "SSIS_mean_SD.csv")

sscipm <- sci %>%
  group_by(Test, ParticipantMentor) %>%
  summarize(mean.sci = mean(sci_sum), std.sci = sd(sci_sum),
            mean.comp = mean(sci_comp_sum), std.comp = sd(sci_comp_sum),
            mean.int = mean(sci_int_sum), std.int = sd(sci_int_sum),
            mean.id = mean(sci_ID_sum), std.id = sd(sci_ID_sum))

#summarize responses to individual questions
sci_resp <- sci %>%
  group_by(Test) %>%
  summarize(across(starts_with("Sci"), ~mean(.x, na.rm = TRUE)))

sci_resp <- as.data.frame(sci_resp)
rownames(sci_resp) <- sci_resp$Test
# subtracting only numerical col using row index
sci_diff <- rbind(sci_resp[,2:24], sci_resp["Post", 2: ncol(sci_resp)] - sci_resp["Pre", 2: ncol(sci_resp)])


cns_resp <- cc %>%
  group_by(Test) %>%
  summarize(across(starts_with("CNS"), ~mean(.x, na.rm = TRUE)))
cns_resp <- as.data.frame(cns_resp)
rownames(cns_resp) <- cns_resp$Test
# subtracting only numerical col using row index
cns_diff <- rbind(cns_resp[,2:16], cns_resp["Post", 2: ncol(cns_resp)] - cns_resp["Pre", 2: ncol(cns_resp)])

cchs_resp <- cc %>%
  group_by(Test) %>%
  summarize(across(starts_with("CCHS"), ~mean(.x, na.rm = TRUE)))
cchs_resp <- as.data.frame(cchs_resp)
rownames(cchs_resp) <- cchs_resp$Test
# subtracting only numerical col using row index
cchs_diff <- rbind(cchs_resp[,2:17], cchs_resp["Post", 2: ncol(cchs_resp)] - cchs_resp["Pre", 2: ncol(cchs_resp)])



#### GRAPHS ####
#visualize raw data with boxplots
sci_raw <- ggplot(sv_sum, aes(x = Test, y = sci_sum)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Science Identity") +
  xlab("") +
  ylab("Score") +
  theme_bw()

cns_raw <- ggplot(sv_sum, aes(x = Test, y = cns_sum)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Connectedness to Nature") +
  xlab("") +
  ylab("Score") +
  theme_bw()

cchs_raw <- ggplot(sv_sum, aes(x = Test, y = cchs_sum)) +
  geom_boxplot(fill = "gold") +
  labs(title = "Climate Change Hope") +
  xlab("") +
  ylab("Score") +
  theme_bw()

#combined figure showing raw data
pdf(file = "boxplot_SciID_CNS_CCHS_raw.pdf", width = 8, height = 3)

multiplot(sci_raw, cns_raw, cchs_raw,
          cols = 3)

dev.off()


#visualize complete cases data with boxplots
sci_pair <- ggplot(sci, aes(x = Test, y = sci_sum)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Science Identity") +
  xlab("") +
  ylab("Score") +
  theme_bw()

cns_pair <- ggplot(cc, aes(x = Test, y = cns_sum)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Connectedness to Nature") +
  xlab("") +
  ylab("Score") +
  theme_bw()

cchs_pair <- ggplot(cc, aes(x = Test, y = cchs_sum)) +
  geom_boxplot(fill = "gold") +
  labs(title = "Climate Change Hope") +
  xlab("") +
  ylab("Score") +
  theme_bw()

#combined figure showing paired data
pdf(file = "boxplot_SciID_CNS_CCHS_paired.pdf", width = 8, height = 3)

multiplot(sci_pair, cns_pair, cchs_pair,
          cols = 3)

dev.off()

#visualize complete cases stratified by participant vs mentor
sci_rxn <- ggplot(data = sci, 
                  aes(x = Test, y = sci_sum, group = UniqueID, color = ParticipantMentor)) +
  geom_line(size=1) +
  geom_point() +
  labs(title = "Science Identity") +
  xlab("") +
  ylab("Score") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("steelblue", "gray"),
                     name="")

cns_rxn <- ggplot(data = cc, 
                  aes(x = Test, y = cns_sum, group = UniqueID, color = ParticipantMentor)) +
  geom_line(size=1) +
  geom_point() +
  labs(title = "Connectedness to Nature") +
  xlab("") +
  ylab("Score") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("darkgreen", "gray"),
                     name="")

cchs_rxn <- ggplot(data = cc, 
                   aes(x = Test, y = cchs_sum, group = UniqueID, color = ParticipantMentor)) +
  geom_line(size=1) +
  geom_point() +
  labs(title = "Climate Change Hope") +
  xlab("") +
  ylab("Score") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("gold", "gray"),
                     name="")

pdf(file = "rxn_norm_SSIS_CNS_CCHS_paired.pdf", width = 8, height = 3)

multiplot(sci_rxn, cns_rxn, cchs_rxn,
          cols = 3)

dev.off()
