library(nnet)
library(car)
library(MASS)
library(FactoMineR)
library(missMDA)
library(tidyverse)
library(factoextra)
library(clValid)
library(Factoshiny)
library(dplyr)
library(VIM)
library(tableone)
library(openxlsx)
library(gridExtra)
library(fmsb)
library(ggplot2)
library(RColorBrewer)
library(Gmisc)
library(broom)
library(haven)
library(corrplot)
library(lme4)

#########Select cohort from NACC
data <- readRDS("~/NACCdata_Clean.rds") #Import clean NACC phenotype data file 

AD_NACCIDs <- data %>% #Get list of pts who didn't have AD at initial visit (NACCIDEM) and had an 
  filter(NACCIDEM == 1) %>% # AD diagnosis at most recent visit
  group_by(NACCID) %>%
  arrange(desc(NACCVNUM)) %>%
  distinct(NACCID, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(NACCETPR == 1) %>%
  distinct(NACCID)

data_ADpts <- left_join(AD_NACCIDs, data, by = "NACCID") #Add back all the data

data_ADYR <- data_ADpts %>%
  select(NACCID, BIRTHYR, VISITYR, NACCVNUM, NACCETPR)

data_ADpts <- data_ADpts %>% #Get initial visit data for AD pts
  filter(PACKET=="I")

rm(data)
rm(AD_NACCIDs)

#Add new variables for pts that get vascular dementia, dementia with lewy bodies, and FTD
data <- readRDS("~/Library/CloudStorage/Dropbox/RTraining/NACCData/NACCdata_Clean.rds")

AD_NACCIDs <- data %>% #Get list of pts who didn't have AD at initial visit (NACCIDEM) and had an 
  filter(NACCIDEM == 1) %>% # AD diangosis at most recent visit
  group_by(NACCID) %>%
  arrange(desc(NACCVNUM)) %>%
  distinct(NACCID, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(NACCETPR == 1) %>%
  distinct(NACCID)

data_ADpts <- left_join(AD_NACCIDs, data, by = "NACCID") #Add back all the data

data_ADYR <- data_ADpts %>%
  select(NACCID, BIRTHYR, VISITYR, NACCVNUM, NACCETPR)

data_ADpts <- data_ADpts %>% #Get initial visit data for AD pts
  filter(PACKET=="I")

rm(data)
rm(AD_NACCIDs)

#########Add new variables for pts that get vascular dementia, dementia with lewy bodies, and FTD

#Note that for both filters, subjects were included if they had any cognitive impairment (impaired not MCI, MCI, or demented). 
data <- readRDS("~/Library/CloudStorage/Dropbox/RTraining/NACCData/NACCdata_Clean.rds")

AD_NACCIDs <- data %>% #Get list of pts
  filter(NACCIDEM == 1) %>%
  group_by(NACCID) %>%
  arrange(desc(NACCVNUM)) %>%
  distinct(NACCID, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(NACCETPR == 1) %>%
  distinct(NACCID)

data <- left_join(AD_NACCIDs, data, by = "NACCID") #Add back all the data

# Vascular dementia (VASC) (n=371)
NACCID_vasc <- data %>% 
  filter(NACCUDSD != 1) %>% 
  filter(VASC==1 | CVD == 1) %>% 
  distinct(NACCID)

# Lewy Body Disease (n=199)
NACCID_lbd <- data %>% 
  filter(NACCUDSD != 1) %>% 
  filter(NACCLBDS==1) %>% 
  distinct(NACCID)

#Behavioral Variant Frontotemporal Dementia (n = 13)
NACCID_FTDbv <- data %>% 
  filter(NACCUDSD !=1) %>% 
  filter(NACCBVFT==1 | FTD==1) %>% 
  distinct(NACCID)

rm(data)
rm(AD_NACCIDs)

# Add vascular dementia, LBD, and FTD to data_ADpts data

data_ADpts$VascularDementia <- ifelse(data_ADpts$NACCID %in% NACCID_vasc$NACCID, "Present", "Absent")
data_ADpts$VascularDementia <- as.factor(data_ADpts$VascularDementia)

data_ADpts$LBD <- ifelse(data_ADpts$NACCID %in% NACCID_lbd$NACCID, "Present", "Absent")
data_ADpts$LBD <- as.factor(data_ADpts$LBD)

data_ADpts$FTD <- ifelse(data_ADpts$NACCID %in% NACCID_FTDbv$NACCID, "Present", "Absent")
data_ADpts$FTD <- as.factor(data_ADpts$FTD)

rm(NACCID_vasc)
rm(NACCID_lbd)
rm(NACCID_FTDbv)

############### Derive Age at AD Diagnosis and drop everyone with age of diagnosis less than 50
data_ADYR <- data_ADYR %>%
  filter(NACCETPR == 1) %>%
  group_by(NACCID) %>%
  arrange(NACCVNUM) %>%
  distinct(NACCID, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(AgeofAD = as.numeric(VISITYR - BIRTHYR))

data_ADYR <- data_ADYR %>%
  select(-BIRTHYR, -VISITYR, -NACCVNUM, -NACCETPR) 

data_ADpts <- left_join(data_ADpts, data_ADYR, by="NACCID") #Add AgeofAD 

data_ADpts <- data_ADpts %>% # Keep only late-onset AD diagnoses
  subset(AgeofAD >= 50)

rm(data_ADYR)

############Add family history data

data_FH <- data_ADpts

data_FH <- data_FH %>% 
  select(NACCID, NACCMOM, NACCDAD) %>% 
  mutate(NACCMOM = as.factor(NACCMOM), NACCID = as.factor(NACCID), NACCDAD = as.factor(NACCDAD)) %>% 
  group_by(NACCID) %>% 
  distinct(NACCID, .keep_all = TRUE) %>% 
  ungroup()

data_FH <- data_FH %>% 
  mutate(NACCMOM = as.character(NACCMOM), NACCDAD = as.character(NACCDAD)) # Change to character temporarily before next step

data_FH[data_FH =="-4"] = NA # Add in NA's where appropriate 
data_FH[data_FH == "9"] = NA

data_FH <- data_FH %>% 
  mutate(NACCMOM = as.factor(NACCMOM), NACCDAD = as.factor(NACCDAD)) # Change back to factor

data_ADpts <- left_join(data_ADpts, data_FH, by = "NACCID") #Add FH to dataset
rm(data_FH)

###########Clean ApoE variable
data_ADpts <- data_ADpts %>% # Convert "Missing/Unknown" (9) in NACCAPOE to "NA" 
  mutate(NACCAPOE = as.character(NACCAPOE)) %>%
  mutate(NACCAPOE = ifelse(NACCAPOE == "9", "NA", NACCAPOE)) %>%
  mutate(NACCAPOE = as.factor(NACCAPOE)) 

############Clean phenotype data
# Make changes to CVPACE and PACKSPER variables as appropriate (see methods)
data_ADpts <- data_ADpts %>% 
  mutate(
    FORMVER = as.character(FORMVER),         # Convert FORMVER factor to character
    CVPACDEF = as.character(CVPACDEF), # Convert CVPACDEF factor to character
    CVPACE = as.character(CVPACE), # Convert CVPACE factor to character
    CVPACE = ifelse(FORMVER == "3", CVPACDEF, CVPACE)) %>% # Mutate CVPACDEF value to CVPACE value for v3 patients) 
  mutate(
    FORMVER = as.factor(FORMVER),           # Convert columns back to factors
    CVPACDEF = as.factor(CVPACDEF),         
    CVPACE = as.factor(CVPACE),
    PACKSPER = as.factor(PACKSPER))

data_ADpts <- data_ADpts %>% # Convert "Not applicable" (8) in PACKSPER to "No cigarette use" (0)
  mutate(PACKSPER = as.character(PACKSPER)) %>%
  mutate(PACKSPER = ifelse(PACKSPER == "8", "0", PACKSPER)) %>%
  mutate(PACKSPER = as.factor(PACKSPER)) 

#Make new variables for early and late-onset AD
data_ADpts$Onset <- ifelse(data_ADpts$AgeofAD >= 50 & data_ADpts$AgeofAD <= 64, "EOAD", "LOAD")

data_ADpts$Onset <- as.factor(data_ADpts$Onset)

#Select variables of interest
ActiveVars <- c("TOBAC30", "TOBAC100", "PACKSPER", "CVHATT", "CVAFIB", "CVANGIO", "CVBYPASS", "CVPACE", "CVCHF", "CVOTHR", "CBSTROKE", "PD", "PDOTHR", "SEIZURES", "DIABETES","HYPERTEN", "HYPERCHO", "B12DEF", "THYROID", "INCONTU", "INCONTF", "ALCOHOL", "ABUSOTHR","DEP2YRS", "PSYCDIS", "NACCTBI")

QuantitSuppVars <- c("SMOKYRS", "EDUC", "NACCAGEB","NACCGDS", "AgeofAD")

QualiSuppVars <- c("SEX", "HISPANIC", "RACE","Onset","NACCMOM", "NACCAPOE", "VascularDementia", "LBD", "FTD", "NACCDAD")

#Change -4 and 9 to missing but only in the active variables
data_ADpts_NoMissing <- data_ADpts %>% #Select active variables that need to be edited
  select(NACCID, all_of(ActiveVars)) %>%
  mutate_if(is.factor, as.character) #Change to character temporarily 

data_ADpts <- data_ADpts %>% #Split data
  select(-all_of(ActiveVars))

data_ADpts_NoMissing[data_ADpts_NoMissing=="-4"] = NA #See data dictionary. -4 = "missing" and 9 = "unknown"

data_ADpts_NoMissing[data_ADpts_NoMissing=="9"] = NA

data_ADpts_NoMissing <- data_ADpts_NoMissing %>%
  mutate_if(is.character, as.factor) #Change data back to factors

data_ADpts <- left_join(data_ADpts_NoMissing, data_ADpts, by = "NACCID") #Put data back together
data_ADpts <- data_ADpts %>%
  select(NACCID, all_of(ActiveVars), all_of(QuantitSuppVars), all_of(QualiSuppVars))

print(nrow(data_ADpts))

#Collapse "present/active" and "remote/inactive" answers on the active variables
columns_to_edit <- c("TOBAC30", "TOBAC100", "CVHATT", "CVAFIB", "CVANGIO", "CVBYPASS", "CVPACE", "CVCHF", "CVOTHR", "CBSTROKE","PD", "PDOTHR", "SEIZURES", "DIABETES","HYPERTEN", "HYPERCHO", "B12DEF", "THYROID", "INCONTU", "INCONTF","ALCOHOL", "ABUSOTHR","DEP2YRS", "PSYCDIS", "NACCTBI")

data_ADpts <- data_ADpts %>% 
  mutate_at(columns_to_edit, ~recode(., `0` = "Absent", `1` = "Present", `2` = "Present"))

data_ADpts_WithCogStatus <- data_ADpts_WithCogStatus %>% 
  mutate_at(columns_to_edit, ~recode(., `0` = "Absent", `1` = "Present", `2` = "Present"))

#Drop everyone with data missing in the active variables
data_ADpts_Missing <- data_ADpts %>% 
  filter(rowSums(is.na(data_ADpts[,2:27])) > 0) #Create dataset of pts to be dropped

data_ADpts_WithCogStatus_Missing <- data_ADpts_WithCogStatus %>% 
  filter(rowSums(is.na(data_ADpts_WithCogStatus[,2:27])) > 0) 

data_ADpts <- subset(data_ADpts, complete.cases(data_ADpts[,2:27]))
print(nrow(data_ADpts))

data_ADpts_WithCogStatus <- subset(data_ADpts_WithCogStatus, complete.cases(data_ADpts_WithCogStatus[,2:27]))

#Summary table
data_summary <- data_ADpts %>%
  select(NACCAGEB, SEX, RACE, HISPANIC, EDUC, NACCMOM)


SummaryTable <- CreateTableOne(data = data_summary)
SummaryTable_csv <- print(SummaryTable, quote = TRUE, noSpaces = TRUE)
write.csv(SummaryTable_csv, file = "~/Library/CloudStorage/Dropbox/RTraining/NACCData/Results/SummaryTable.csv", quote = F)

# Create data summary of patients dropped because of missing data
data_summary_dropped <- data_ADpts_Missing %>% 
  select(NACCAGEB, SEX, RACE, HISPANIC, EDUC,)

SummaryTableDropped <- CreateTableOne(data = data_summary_dropped)
SummaryTableDropped_csv <- print(SummaryTableDropped, quote = TRUE, noSpaces = TRUE)

###############Multiple Correspondence Analysis
#First estimate number of components needed
numComponents <- estim_ncpMCA(data_ADpts[,1:26], ncp.max = 26, method = "Regularized", method.cv = "Kfold", nbsim=100, pNA=0.05)

#Run MCA and plug in value for ncp using above
res.mca <- MCA(data_ADpts[,2:42], ncp=5, graph = FALSE, quanti.sup = 27:31, quali.sup = 32:41)
res.mca.export <- MCA(data_ADpts[,2:27], ncp=5, graph = FALSE #Edit supplimentary column indices as needed
)
eig.val <- get_eigenvalue(res.mca)
head(eig.val)

data_export <- data_ADpts[,2:27] #Save original dataset prior to MCA
saveRDS(data_export, "OriginalData.rds")

#############Clustering
res.hcpc <- HCPC(res.mca, nb.clust = -1, method = "ward", consol=F)
hcpc_df <- res.hcpc$data.clust

NACCID_cluster <- cbind.data.frame(data_ADpts$NACCID, hcpc_df$clust)
NACCID_cluster <- NACCID_cluster %>% 
  rename(NACCID = 'data_ADpts$NACCID') %>% 
  rename(cluster = 'hcpc_df$clust')

#Make dendrogram
dend <- fviz_dend(res.hcpc, palette = c("forestgreen","tomato","royalblue2"), main = "")

#############Longitudinal analysis (see methods)

data <- readRDS("~/NACCdata_Clean.rds")
df_outcome <- left_join(NACCID_cluster, data, by = "NACCID")

#Select variables of interest and clean data
df_outcome <- df_outcome %>% 
  select(NACCID, cluster, NACCVNUM, NACCAGE, SEX, RACE, NACCAPOE, CDRSUM, NACCUDSD) %>% 
  mutate(NACCAPOE = as.factor(NACCAPOE), NACCVNUM = as.factor(NACCVNUM))

df_outcome$CDRSUM <- as.numeric(as.character(df_outcome$CDRSUM)) #Convert factor to numeric for downstream analyses
head(df_outcome)

#plot the raw mean CDRSUM values for each cluster at each NACC Visit 
# Calculate mean CDRSUM by cluster for each visit
mean_data <- df_outcome %>% 
  filter(as.integer(NACCVNUM) <= 10) %>% # Focus on just the first 10 visits 
  group_by(cluster, NACCVNUM) %>% 
  summarize(mean_CDRSUM = mean(CDRSUM, na.rm = T),
            se_change = sd(CDRSUM, na.rm = TRUE) / sqrt(n()))

ggplot(mean_data, aes(x = NACCVNUM, y = mean_CDRSUM, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_CDRSUM - se_change, ymax = mean_CDRSUM + se_change), width = 0.1) +
  scale_y_continuous(limits = c(0,8), breaks = seq(1,8, by = 1)) +
  ggtitle("Mean CDR-SB from baseline to the next 10 NACC Visits") +
  theme_minimal()

############Linear model and post-hoc tests
library(lme4)
# Derive baseline CDRSUM so it can be accounted for in the model
baseline_data <- df_outcome %>% 
  group_by(NACCID) %>% 
  filter(NACCVNUM == 1) %>% 
  select(NACCID, BaselineCDR = CDRSUM)
df_outcome <- left_join(df_outcome, baseline_data, by = "NACCID")

#Include only the first 3 visits
df_model <- df_outcome %>% 
  filter(as.integer(NACCVNUM) <= 3)

# Drop individuals who aren't MCI at initial visit (drops 814 individuals)
NACCID_MCIOnly <- df_model %>% 
  filter(NACCVNUM == 1 & NACCUDSD == 3) %>% 
  select(NACCID)

df_model <- left_join(NACCID_MCIOnly, df_model, by = "NACCID")

# Drop APOE missing individuals
df_model <- df_model %>% 
  filter(NACCAPOE != 9)

#Fit models

# Test for significant interaction terms
model <- lmer(CDRSUM ~ NACCAGE*cluster + (1|NACCID), REML = F, data = df_model)
model <- lmer(CDRSUM ~ SEX*cluster + (1|NACCID), REML = F, data = df_model)
# Just age interaction is significant

model <- lmer(CDRSUM ~ NACCAGE + SEX + NACCAPOE + BaselineCDR + NACCVNUM + cluster + (1|NACCID), REML = F, data = df_model) # fit final model
fixef(model)
confint(model)

#inverse probability weighting to account for non-random dropout
library(ipw)

df_model_ipw <- df_model %>% # Create dropout status variable 
  group_by(NACCID) %>%
  mutate(dropout_status = ifelse(all(NACCVNUM != 3), 1, 0)) %>%
  ungroup() %>% 
  mutate(dropout_status = as.factor(dropout_status))

dropout_model <- glm(dropout_status ~ NACCAGE + SEX + NACCAPOE + BaselineCDR + NACCVNUM + cluster, family = binomial, data = df_model_ipw) # Estimate the probability of dropout

# Calculate inverse probability weights
prob_dropout <- predict(dropout_model, type = "response")

df_model_ipw$weights <- ifelse(df_model_ipw$dropout_status == 1, 1 / prob_dropout, 1 / (1-prob_dropout))


# Fit the weighted model
weighted_model <- lmer(CDRSUM ~ NACCAGE + SEX + NACCAPOE + BaselineCDR + NACCVNUM + cluster + (1|NACCID), data = df_model_ipw, REML = F, weights = weights)

model_parameters(weighted_model)

fixed_effects <- fixef(weighted_model)
confint <- confint(weighted_model, method = "Wald", level = 0.95)

# Post-hoc tests
library(emmeans)
library(ggeffects)
library(parameters)

# Likelihood ratio test

weighted_model_reduced <- lmer(CDRSUM ~ NACCAGE + SEX + NACCAPOE + BaselineCDR + NACCVNUM + (1|NACCID), data = df_model_ipw
                               , REML = F, weights = weights) # Fit model without cluster

anova(weighted_model, weighted_model_reduced) #LR test

# Marginal Effects 

model_parameters(weighted_model)
my_predictions <- predict_response(weighted_model, "cluster")
my_predictions
dat <- test_predictions(my_predictions, pbkrtest.limit = 8000) #pairwise tests
print(dat)
test_predictions(my_predictions, pbkrtest.limit = 8000, test = "contrast") # Tests for difference between each cluster and the average across all the clusters
# Uses marginaleffects package to test for differences. This packages uses Wald tests to generate Z-scores and compares each estimated coefficient to its standard error

# Plotting
predictions <- ggpredict(weighted_model_reduced, term = c("NACCVNUM"))
predictions <- as.data.frame(predictions)
predictions <- predictions %>% 
  rename(Visit_Number = x)# %>% 
#rename(Cluster = group)

baseline_values <- predictions %>%
  filter(x == "1") %>%
  dplyr::select(x, predicted) %>%
  rename(baseline_predicted = predicted)

predictions <- predictions %>%
  left_join(baseline_values, by = "x") %>%
  mutate(change_from_baseline = predicted - baseline_predicted)

saveRDS(weighted_model, file = "~/weighted_model.rds")

############plot the adjusted mean change in CDRSUM values for each cluster and visit number

model_plot <- lmer(CDRSUM ~ NACCAGE + SEX + NACCAPOE + BaselineCDR + (1|NACCID), data = df_model_ipw, REML = F, weights = weights)

df <- df_model_ipw
df$predicted_no_cluster <- predict(model_plot)
df$adjusted_CDRSUM <- df$CDRSUM - df$predicted_no_cluster

# Calculate change from baseline 
df <- df %>%
  group_by(cluster) %>%
  mutate(change_from_baseline = adjusted_CDRSUM - adjusted_CDRSUM[NACCVNUM == 1])

# Calculate the standard error for each combination of Cluster and Visit_Number
df <- df %>%
  group_by(cluster, NACCVNUM) %>%
  summarise(
    mean_change = mean(change_from_baseline, na.rm = TRUE),
    se = sd(change_from_baseline, na.rm = TRUE) / sqrt(n())
  )


p <- ggplot(df, aes(x = NACCVNUM, y = mean_change, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_change - se, ymax = mean_change + se), width = 0.2) +
  scale_color_manual(values=c("forestgreen","royalblue2","tomato"),
                     labels = c("Minimal Problem History", "Substance Use History", "Cardiovascular Disease History")) +
  scale_y_continuous(labels = abs, breaks = seq(0, -max(df$mean_change), by = -0.2)) +
  scale_x_discrete(labels = c("Initial Visit", "Visit 2", "Visit 3")) +
  labs(x = "", y = "Adjusted Mean Change from Baseline", color = NULL) +
  ggtitle(label = "") +
  scale_y_reverse() +
  labs(x = "", color = "Cluster") +
  theme(panel.background = element_rect(fill = "white", colour = "white"), 
        panel.grid.major = element_blank(),      
        panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "white", colour = "black"),
        legend.position = c(0.95, 0.95),
        legend.title = element_blank(),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(size = 14),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(family = "Arial", size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(family = "Arial", size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14, color = "black", angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, color = "black"))
