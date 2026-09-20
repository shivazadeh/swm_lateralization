
# ============================================================
# Load data
# ============================================================
final_hcp <- read.csv("/path/to/data/hcp.csv")
final_abcd <- read.csv("/path/to/data/abcd.csv")


# ============================================================
# Packages
# ============================================================

library(lme4)
library(lmerTest)
library(parameters)
library(glmmTMB)
library(emmeans)
library(dplyr)
library(tidyr)
library(svglite)
library(yarrr)


# ============================================================
# Statistical Analysis
# ============================================================

##Assess SWM/FAT/AF lateralization 
##Medium_Cluster is SWM and Long_Cluster is FAT

###ABCD
# 1. FA SWM
m1 <- lmer(
  Lat_Index_FA_Medium_Cluster ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m1)

adj_LI1 <- predict(m1)
t.test(adj_LI1, mu = 0)


# 2. FA FAT
m2 <- lmer(
  Lat_Index_FA_Long_Cluster ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m2)

adj_LI2 <- predict(m2)
t.test(adj_LI2, mu = 0)


# 3. FA AF
m3 <- lmer(
  Lat_Index_FA_AF ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m3)

adj_LI3 <- predict(m3)
t.test(adj_LI3, mu = 0)


# 4. NoS SWM
m4 <- lmer(
  Lat_Index_NoS_Medium_Cluster ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m4)

adj_LI4 <- predict(m4)
t.test(adj_LI4, mu = 0)


# 5. NoS FAT
m5 <- lmer(
  Lat_Index_NoS_Long_Cluster ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m5)

adj_LI5 <- predict(m5)
t.test(adj_LI5, mu = 0)


# 6. NoS AF
m6 <- lmer(
  Lat_Index_NoS_AF ~ interview_age + sex + (1 | rel_family_id),
  data = final_abcd
)
summary(m6)

adj_LI6 <- predict(m6)
t.test(adj_LI6, mu = 0)

##correct p-values with fdr
p1 <- t.test(adj_LI1, mu = 0)$p.value
p2 <- t.test(adj_LI2, mu = 0)$p.value
p3 <- t.test(adj_LI3, mu = 0)$p.value
p4 <- t.test(adj_LI4, mu = 0)$p.value
p5 <- t.test(adj_LI5, mu = 0)$p.value
p6 <- t.test(adj_LI6, mu = 0)$p.value

pvals <- c(p1, p2, p3, p4, p5, p6)

pvals_fdr <- p.adjust(pvals, method = "fdr")
print(pvals_fdr)


###hcp
# 1. FA SWM
m1 <- lmer(
  Lat_Index_FA_Medium_Cluster ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m1)

adj_LI1 <- predict(m1)
t.test(adj_LI1, mu = 0)


# 2. FA FAT
m2 <- lmer(
  Lat_Index_FA_Long_Cluster ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m2)

adj_LI2 <- predict(m2)
t.test(adj_LI2, mu = 0)


# 3. FA AF
m3 <- lmer(
  Lat_Index_FA_Arcuate ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m3)

adj_LI3 <- predict(m3)
t.test(adj_LI3, mu = 0)


# 4. NoS SWM
m4 <- lmer(
  Lat_Index_NoS_Medium_Cluster ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m4)

adj_LI4 <- predict(m4)
t.test(adj_LI4, mu = 0)


# 5. NoS FAT
m5 <- lmer(
  Lat_Index_NoS_Long_Cluster ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m5)

adj_LI5 <- predict(m5)
t.test(adj_LI5, mu = 0)


# 6. NoS AF
m6 <- lmer(
  Lat_Index_NoS_Arcuate ~ Age_in_Yrs + Gender + (1 | Family_ID),
  data = final_hcp
)
summary(m6)

adj_LI6 <- predict(m6)
t.test(adj_LI6, mu = 0)

##correct p-values with fdr
p1 <- t.test(adj_LI1, mu = 0)$p.value
p2 <- t.test(adj_LI2, mu = 0)$p.value
p3 <- t.test(adj_LI3, mu = 0)$p.value
p4 <- t.test(adj_LI4, mu = 0)$p.value
p5 <- t.test(adj_LI5, mu = 0)$p.value
p6 <- t.test(adj_LI6, mu = 0)$p.value

pvals <- c(p1, p2, p3, p4, p5, p6)

pvals_fdr <- p.adjust(pvals, method = "fdr")
print(pvals_fdr)

##########################################
##HCP-YA mixed effects models examining FA and Language Performance, accounting for Family_ID

m1 <- lmer(left_medium_cluster.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m1)
##get standardized beta coefficients
model_parameters(m1, standardize = "refit")

m2 <- lmer(right_medium_cluster.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m2)
##get standardized beta coefficients
model_parameters(m2, standardize = "refit")

m3 <- lmer(left_long_cluster.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m3)
##get standardized beta coefficients
model_parameters(m3, standardize = "refit")

m4 <- lmer(right_long_cluster.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m4)
##get standardized beta coefficients
model_parameters(m4, standardize = "refit")

m5 <- lmer(left_AF.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m5)
##get standardized beta coefficients
model_parameters(m5, standardize = "refit")

m6 <- lmer(right_AF.FA1.Mean ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m6)
##get standardized beta coefficients
model_parameters(m6, standardize = "refit")


m7 <- lmer(left_medium_cluster.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m7)
##get standardized beta coefficients
model_parameters(m7, standardize = "refit")

m8 <- lmer(right_medium_cluster.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m8)
##get standardized beta coefficients
model_parameters(m8, standardize = "refit")

m9 <- lmer(left_long_cluster.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m9)
##get standardized beta coefficients
model_parameters(m9, standardize = "refit")

m10 <- lmer(right_long_cluster.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m10)
##get standardized beta coefficients
model_parameters(m10, standardize = "refit")

m11 <- lmer(left_AF.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m11)
##get standardized beta coefficients
model_parameters(m11, standardize = "refit")

m12 <- lmer(right_AF.FA1.Mean ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m12)
##get standardized beta coefficients
model_parameters(m12, standardize = "refit")

##Extract p-values for the language performance predictors
pvals <- c(
  summary(m1)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m2)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m3)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m4)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m5)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m6)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  
  summary(m7)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m8)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m9)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m10)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m11)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m12)$coefficients["ReadEng_Unadj", "Pr(>|t|)"]
)

##FDR correction across all 12 predictor tests
pvals_fdr <- p.adjust(pvals, method = "fdr")

##Create results table 
results <- data.frame(
  Model = paste0("m", 1:12),
  Raw_p = formatC(pvals, format = "f", digits = 8),
  FDR_p = formatC(pvals_fdr, format = "f", digits = 8)
)

results


##HCP mixed effects model examining laterality index and Language Performance, accounting for Family_ID

m1 <- lmer(Lat_Index_FA_Medium_Cluster ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m1)
##get standardized beta coefficients
model_parameters(m1, standardize = "refit")

m2 <- lmer(Lat_Index_FA_Long_Cluster ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m2)
##get standardized beta coefficients
model_parameters(m2, standardize = "refit")

m3 <- lmer(Lat_Index_FA_Arcuate ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m3)
##get standardized beta coefficients
model_parameters(m3, standardize = "refit")

m4 <- lmer(Lat_Index_FA_Medium_Cluster ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m4)
##get standardized beta coefficients
model_parameters(m4, standardize = "refit")

m5 <- lmer(Lat_Index_FA_Long_Cluster ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m5)
##get standardized beta coefficients
model_parameters(m5, standardize = "refit")

m6 <- lmer(Lat_Index_FA_Arcuate ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m6)
##get standardized beta coefficients
model_parameters(m6, standardize = "refit")

m7 <- lmer(Lat_Index_NoS_Medium_Cluster ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m7)
##get standardized beta coefficients
model_parameters(m7, standardize = "refit")

m8 <- lmer(Lat_Index_NoS_Long_Cluster ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m8)
##get standardized beta coefficients
model_parameters(m8, standardize = "refit")

m9 <- lmer(Lat_Index_NoS_Arcuate ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
           data = final_hcp)
summary(m9)
##get standardized beta coefficients
model_parameters(m9, standardize = "refit")

m10 <- lmer(Lat_Index_NoS_Medium_Cluster ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m10)
##get standardized beta coefficients
model_parameters(m10, standardize = "refit")

m11 <- lmer(Lat_Index_NoS_Long_Cluster ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m11)
##get standardized beta coefficients
model_parameters(m11, standardize = "refit")

m12 <- lmer(Lat_Index_NoS_Arcuate ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID), 
            data = final_hcp)
summary(m12)
##get standardized beta coefficients
model_parameters(m12, standardize = "refit")

# Extract p-values for the language performance predictors for FA
pvals <- c(
  summary(m1)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m2)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m3)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  
  summary(m4)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m5)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m6)$coefficients["ReadEng_Unadj", "Pr(>|t|)"]
)

# FDR correction across all 6 predictor tests
pvals_fdr <- p.adjust(pvals, method = "fdr")

# Create results table
results <- data.frame(
  Model = paste0("m", 1:6),
  Raw_p = pvals,
  FDR_p = pvals_fdr
)

results


# Extract p-values for the language performance predictors for NoS
pvals <- c(
  summary(m7)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m8)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  summary(m9)$coefficients["PicVocab_Unadj", "Pr(>|t|)"],
  
  summary(m10)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m11)$coefficients["ReadEng_Unadj", "Pr(>|t|)"],
  summary(m12)$coefficients["ReadEng_Unadj", "Pr(>|t|)"]
)

##FDR correction across all 6 predictor tests
pvals_fdr <- p.adjust(pvals, method = "fdr")

##Create results table
results <- data.frame(
  Model = paste0("m", 7:12),
  Raw_p = pvals,
  FDR_p = pvals_fdr
)

results


##ABCD mixed effects model examining FA and Language Performance, accounting for rel_family_id

m1 <- lmer(left_medium_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m1)
##get standardized beta coefficients
model_parameters(m1, standardize = "refit")

m2 <- lmer(right_medium_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m2)
##get standardized beta coefficients
model_parameters(m2, standardize = "refit")

m3 <- lmer(left_long_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m3)
##get standardized beta coefficients
model_parameters(m3, standardize = "refit")

m4 <- lmer(right_long_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m4)
##get standardized beta coefficients
model_parameters(m4, standardize = "refit")

m5 <- lmer(left_AF_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m5)
##get standardized beta coefficients
model_parameters(m5, standardize = "refit")

m6 <- lmer(right_AF_tensor1_FractionalAnisotropy_Mean ~ nihtbx_picvocab_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m6)
##get standardized beta coefficients
model_parameters(m6, standardize = "refit")


m7 <- lmer(left_medium_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m7)
##get standardized beta coefficients
model_parameters(m7, standardize = "refit")

m8 <- lmer(right_medium_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m8)
##get standardized beta coefficients
model_parameters(m8, standardize = "refit")

m9 <- lmer(left_long_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
             interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m9)
##get standardized beta coefficients
model_parameters(m9, standardize = "refit")

m10 <- lmer(right_long_cluster_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
              interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m10)
##get standardized beta coefficients
model_parameters(m10, standardize = "refit")

m11 <- lmer(left_AF_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
              interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m11)
##get standardized beta coefficients
model_parameters(m11, standardize = "refit")

m12 <- lmer(right_AF_tensor1_FractionalAnisotropy_Mean ~ nihtbx_reading_uncorrected + 
              interview_age + sex + (1|rel_family_id), data = final_abcd)
summary(m12)
##get standardized beta coefficients
model_parameters(m12, standardize = "refit")

# Extract p-values for the language performance predictors
pvals <- c(
  summary(m1)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m2)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m3)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m4)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m5)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m6)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  
  summary(m7)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m8)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m9)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m10)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m11)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m12)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"]
)

# FDR correction across all 12 tests
pvals_fdr <- p.adjust(pvals, method = "fdr")
print(pvals_fdr)


results <- data.frame(
  Model = paste0("m", 1:12),
  Raw_p = pvals,
  FDR_p = pvals_fdr
)

results

#ABCD mixed effects model examining laterality index and Language Performance, accounting for rel_family_id

m1 <- lmer(Lat_Index_FA_Medium_Cluster ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m1)
##get standardized beta coefficients
model_parameters(m1, standardize = "refit")

m2 <- lmer(Lat_Index_FA_Long_Cluster ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m2)
##get standardized beta coefficients
model_parameters(m2, standardize = "refit")

m3 <- lmer(Lat_Index_FA_AF ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m3)
##get standardized beta coefficients
model_parameters(m3, standardize = "refit")

m4 <- lmer(Lat_Index_FA_Medium_Cluster ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m4)
##get standardized beta coefficients
model_parameters(m4, standardize = "refit")

m5 <- lmer(Lat_Index_FA_Long_Cluster ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m5)
##get standardized beta coefficients
model_parameters(m5, standardize = "refit")

m6 <- lmer(Lat_Index_FA_AF ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m6)
##get standardized beta coefficients
model_parameters(m6, standardize = "refit")

m7 <- lmer(Lat_Index_NoS_Medium_Cluster ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m7)
##get standardized beta coefficients
model_parameters(m7, standardize = "refit")

m8 <- lmer(Lat_Index_NoS_Long_Cluster ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m8)
##get standardized beta coefficients
model_parameters(m8, standardize = "refit")

m9 <- lmer(Lat_Index_NoS_AF ~ nihtbx_picvocab_uncorrected + interview_age + sex + (1|rel_family_id), 
           data = final_abcd)
summary(m9)
##get standardized beta coefficients
model_parameters(m9, standardize = "refit")

m10 <- lmer(Lat_Index_NoS_Medium_Cluster ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
            data = final_abcd)
summary(m10)
##get standardized beta coefficients
model_parameters(m10, standardize = "refit")

m11 <- lmer(Lat_Index_NoS_Long_Cluster ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
            data = final_abcd)
summary(m11)
##get standardized beta coefficients
model_parameters(m11, standardize = "refit")

m12 <- lmer(Lat_Index_NoS_AF ~ nihtbx_reading_uncorrected + interview_age + sex + (1|rel_family_id), 
            data = final_abcd)
summary(m12)
##get standardized beta coefficients
model_parameters(m12, standardize = "refit")

# Extract p-values for the language performance predictors for FA
pvals_fa <- c(
  summary(m1)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m2)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m3)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  
  summary(m4)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m5)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m6)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"]
)

# FDR correction across all 6 FA predictor tests
pvals_fa_fdr <- p.adjust(pvals_fa, method = "fdr")

# Create results table 
results_fa <- data.frame(
  Model = paste0("m", 1:6),
  Raw_p = formatC(pvals_fa, format = "f", digits = 6),
  FDR_p = formatC(pvals_fa_fdr, format = "f", digits = 6)
)

results_fa

# Extract p-values for the language performance predictors for NoS
pvals_nos <- c(
  summary(m7)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m8)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  summary(m9)$coefficients["nihtbx_picvocab_uncorrected", "Pr(>|t|)"],
  
  summary(m10)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m11)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"],
  summary(m12)$coefficients["nihtbx_reading_uncorrected", "Pr(>|t|)"]
)

# FDR correction across all 6 NoS predictor tests
pvals_nos_fdr <- p.adjust(pvals_nos, method = "fdr")

# Create results table
results_nos <- data.frame(
  Model = paste0("m", 7:12),
  Raw_p = pvals_nos,
  FDR_p = pvals_nos_fdr
)

results_nos


##########################################

####HCP run glmmTMB for NoS (negative binomial regression with Family ID as a random effect)

### Calculate percent change for negative binomial regressions
### Percent change = (IRR - 1) * 100
### I obtained the IRR by exponentiating the model coefficient

m1 <- glmmTMB(left_medium_cluster.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m1)
IRR_PicVocab <- exp(fixef(m1)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange


m2 <- glmmTMB(right_medium_cluster.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m2)
IRR_PicVocab <- exp(fixef(m2)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m3 <- glmmTMB(left_long_cluster.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m3)
IRR_PicVocab <- exp(fixef(m3)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m4 <- glmmTMB(right_long_cluster.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m4)
IRR_PicVocab <- exp(fixef(m4)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange


m5 <- glmmTMB(left_AF.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m5)
IRR_PicVocab <- exp(fixef(m5)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m6 <- glmmTMB(right_AF.Num_Fibers ~ PicVocab_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m6)
IRR_PicVocab <- exp(fixef(m6)$cond["PicVocab_Unadj"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m7 <- glmmTMB(left_medium_cluster.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m7)
IRR_Reading <- exp(fixef(m7)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m8 <- glmmTMB(right_medium_cluster.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m8)
IRR_Reading <- exp(fixef(m8)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m9 <- glmmTMB(left_long_cluster.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
              family = nbinom2,
              data = final_hcp)

summary(m9)
IRR_Reading <- exp(fixef(m9)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m10 <- glmmTMB(right_long_cluster.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
               family = nbinom2,
               data = final_hcp)

summary(m10)
IRR_Reading <- exp(fixef(m10)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange


m11 <- glmmTMB(left_AF.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
               family = nbinom2,
               data = final_hcp)

summary(m11)
IRR_Reading <- exp(fixef(m11)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m12 <- glmmTMB(right_AF.Num_Fibers ~ ReadEng_Unadj + Age_in_Yrs + Gender + (1|Family_ID),
               family = nbinom2,
               data = final_hcp)

summary(m12)
IRR_Reading <- exp(fixef(m12)$cond["ReadEng_Unadj"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange


# Extract p-values for the language performance predictors
pvals <- c(
  summary(m1)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  summary(m2)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  summary(m3)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  summary(m4)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  summary(m5)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  summary(m6)$coefficients$cond["PicVocab_Unadj", "Pr(>|z|)"],
  
  summary(m7)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"],
  summary(m8)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"],
  summary(m9)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"],
  summary(m10)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"],
  summary(m11)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"],
  summary(m12)$coefficients$cond["ReadEng_Unadj", "Pr(>|z|)"]
)

# FDR correction across all 12 tests
pvals_fdr <- p.adjust(pvals, method = "fdr")

# Create results table
results <- data.frame(
  Model = paste0("m", 1:12),
  Raw_p = formatC(pvals, format = "f", digits = 6),
  FDR_p = formatC(pvals_fdr, format = "f", digits = 6)
)

results


####ABCD run glmmTMB for NoS (negative binomial regression with Family ID as a random effect)

m1 <- glmmTMB(
  left_medium_cluster_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m1)
IRR_PicVocab <- exp(fixef(m1)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m2 <- glmmTMB(
  right_medium_cluster_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m2)
IRR_PicVocab <- exp(fixef(m2)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m3 <- glmmTMB(
  left_long_cluster_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m3)
IRR_PicVocab <- exp(fixef(m3)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m4 <- glmmTMB(
  right_long_cluster_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m4)
IRR_PicVocab <- exp(fixef(m4)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m5 <- glmmTMB(
  left_AF_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m5)
IRR_PicVocab <- exp(fixef(m5)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange

m6 <- glmmTMB(
  right_AF_Num_Fibers ~ nihtbx_picvocab_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m6)
IRR_PicVocab <- exp(fixef(m6)$cond["nihtbx_picvocab_uncorrected"])
IRR_PicVocab
PercentChange <- (IRR_PicVocab - 1) * 100
PercentChange


m7 <- glmmTMB(
  left_medium_cluster_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m7)
IRR_Reading <- exp(fixef(m7)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m8 <- glmmTMB(
  right_medium_cluster_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m8)
IRR_Reading <- exp(fixef(m8)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m9 <- glmmTMB(
  left_long_cluster_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m9)
IRR_Reading <- exp(fixef(m9)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m10 <- glmmTMB(
  right_long_cluster_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m10)
IRR_Reading <- exp(fixef(m10)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m11 <- glmmTMB(
  left_AF_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m11)
IRR_Reading <- exp(fixef(m11)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

m12 <- glmmTMB(
  right_AF_Num_Fibers ~ nihtbx_reading_uncorrected + interview_age + sex +  (1 | rel_family_id),
  family = nbinom2,
  data   = final_abcd
)

summary(m12)
IRR_Reading <- exp(fixef(m12)$cond["nihtbx_reading_uncorrected"])
IRR_Reading
PercentChange <- (IRR_Reading - 1) * 100
PercentChange

# Extract p-values for the language performance predictors
pvals <- c(
  summary(m1)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  summary(m2)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  summary(m3)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  summary(m4)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  summary(m5)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  summary(m6)$coefficients$cond["nihtbx_picvocab_uncorrected", "Pr(>|z|)"],
  
  summary(m7)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"],
  summary(m8)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"],
  summary(m9)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"],
  summary(m10)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"],
  summary(m11)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"],
  summary(m12)$coefficients$cond["nihtbx_reading_uncorrected", "Pr(>|z|)"]
)

# FDR correction across all 12 tests
pvals_fdr <- p.adjust(pvals, method = "fdr")

# Create results table
results <- data.frame(
  Model = paste0("m", 1:12),
  Raw_p = formatC(pvals, format = "f", digits = 6),
  FDR_p = formatC(pvals_fdr, format = "f", digits = 6)
)

results


##########################################
####Comparing the tracts statistically within each dataset and accounting for family id

##hcp

##create long format data for hcp fa
hcp_long_fa <- final_hcp %>%
  select(
    Subject,
    Family_ID,
    Lat_Index_FA_Medium_Cluster,
    Lat_Index_FA_Long_Cluster,
    Lat_Index_FA_Arcuate
  ) %>%
  pivot_longer(
    cols = starts_with("Lat_Index_FA_"),
    names_to = "Tract",
    values_to = "Lat_Index_FA"
  )

hcp_long_fa$Tract <- factor(hcp_long_fa$Tract)

##fit the model
hcp_fa_model <- lmer(
  Lat_Index_FA ~ Tract +
    (1 | Family_ID) +
    (1 | Subject),
  data = hcp_long_fa
)

summary(hcp_fa_model)
emmeans(hcp_fa_model, pairwise ~ Tract, adjust = "fdr")

##95% CI
confint(
  pairs(emmeans(hcp_fa_model, ~ Tract)),
  adjust = "none"
)


####create long format data for hcp nos
hcp_long_nos <- final_hcp %>%
  select(
    Subject,
    Family_ID,
    Lat_Index_NoS_Medium_Cluster,
    Lat_Index_NoS_Long_Cluster,
    Lat_Index_NoS_Arcuate
  ) %>%
  pivot_longer(
    cols = starts_with("Lat_Index_NoS_"),
    names_to = "Tract",
    values_to = "Lat_Index_NoS"
  )

hcp_long_nos$Tract <- factor(hcp_long_nos$Tract)

##fit model
hcp_nos_model <- lmer(
  Lat_Index_NoS ~ Tract +
    (1 | Family_ID) +
    (1 | Subject),
  data = hcp_long_nos
)

summary(hcp_nos_model)
emmeans(
  hcp_nos_model,
  pairwise ~ Tract,
  adjust = "fdr"
)

##95% CI
confint(
  pairs(emmeans(hcp_nos_model, ~ Tract)),
  adjust = "none"
)


##abcd 

##create long format data for abcd fa
abcd_long_fa <- final_abcd %>%
  select(
    Subject,
    rel_family_id,
    Lat_Index_FA_Medium_Cluster,
    Lat_Index_FA_Long_Cluster,
    Lat_Index_FA_AF
  ) %>%
  pivot_longer(
    cols = starts_with("Lat_Index_FA_"),
    names_to = "Tract",
    values_to = "Lat_Index_FA"
  )

abcd_long_fa$Tract <- factor(abcd_long_fa$Tract)

##fit model
abcd_fa_model <- lmer(
  Lat_Index_FA ~ Tract +
    (1 | rel_family_id) +
    (1 | Subject),
  data = abcd_long_fa
)

summary(abcd_fa_model)
emmeans(
  abcd_fa_model,
  pairwise ~ Tract,
  adjust = "fdr"
)

##95% CI
confint(
  pairs(emmeans(abcd_fa_model, ~ Tract)),
  adjust = "none"
)


##create long format data for abcd nos
abcd_long_nos <- final_abcd %>%
  select(
    Subject,
    rel_family_id,
    Lat_Index_NoS_Medium_Cluster,
    Lat_Index_NoS_Long_Cluster,
    Lat_Index_NoS_AF
  ) %>%
  pivot_longer(
    cols = starts_with("Lat_Index_NoS_"),
    names_to = "Tract",
    values_to = "Lat_Index_NoS"
  )

abcd_long_nos$Tract <- factor(abcd_long_nos$Tract)

##fit model
abcd_nos_model <- lmer(
  Lat_Index_NoS ~ Tract +
    (1 | rel_family_id) +
    (1 | Subject),
  data = abcd_long_nos
)

summary(abcd_nos_model)

emmeans(
  abcd_nos_model,
  pairwise ~ Tract,
  adjust = "fdr"
)

##95% CI
confint(
  pairs(emmeans(abcd_nos_model, ~ Tract)),
  adjust = "none"
)



# ============================================================
# FA visualization
# ============================================================

FA_data_abcd <- data.frame(
  FA = c(
    final_abcd$left_medium_cluster_tensor1_FractionalAnisotropy_Mean,
    final_abcd$right_medium_cluster_tensor1_FractionalAnisotropy_Mean,
    final_abcd$left_long_cluster_tensor1_FractionalAnisotropy_Mean,
    final_abcd$right_long_cluster_tensor1_FractionalAnisotropy_Mean,
    final_abcd$left_AF_tensor1_FractionalAnisotropy_Mean,
    final_abcd$right_AF_tensor1_FractionalAnisotropy_Mean
  ),
  
  Tract = c(
    rep("Left SWM", nrow(final_abcd)),
    rep("Right SWM", nrow(final_abcd)),
    rep("Left FAT", nrow(final_abcd)),
    rep("Right FAT", nrow(final_abcd)),
    rep("Left Arcuate", nrow(final_abcd)),
    rep("Right Arcuate", nrow(final_abcd))
  ),
  
  Dataset = "ABCD"
)


FA_data_hcp <- data.frame(
  FA = c(
    final_hcp$left_medium_cluster.FA1.Mean,
    final_hcp$right_medium_cluster.FA1.Mean,
    final_hcp$left_long_cluster.FA1.Mean,
    final_hcp$right_long_cluster.FA1.Mean,
    final_hcp$left_AF.FA1.Mean,
    final_hcp$right_AF.FA1.Mean
  ),
  
  Tract = c(
    rep("Left SWM", nrow(final_hcp)),
    rep("Right SWM", nrow(final_hcp)),
    rep("Left FAT", nrow(final_hcp)),
    rep("Right FAT", nrow(final_hcp)),
    rep("Left Arcuate", nrow(final_hcp)),
    rep("Right Arcuate", nrow(final_hcp))
  ),
  
  Dataset = "HCP"
)


FA_data_combined <- rbind(
  FA_data_abcd,
  FA_data_hcp
)

FA_data_combined$Tract <- factor(
  FA_data_combined$Tract,
  levels = c(
    "Left Arcuate",
    "Right Arcuate",
    "Left FAT",
    "Right FAT",
    "Left SWM",
    "Right SWM"
  )
)

setwd("~/Documents/R")
svglite("FA_plot.svg", width = 17, height = 6)
pirateplot(formula = FA ~ Tract + Dataset, data = FA_data_combined, main = "FA of Pathways Across Datasets",
           point.o = 0, cap.beans = 1, 
           pal = c("darkorange1", "purple4", "cadetblue1", "deeppink", "chartreuse1", "firebrick1"),
           bean.f.o = 1, theme = 3, ylim  = c(0,0.9), ylab = "FA",
           back.col = "white",
           gl.col = "grey62", 
           avg.line.fun = mean,
           inf.disp = "bean",
           avg.line.col = "black",
           inf.b.o = 0,
           bean.b.col = "black",
           bean.b.o = 1)
dev.off()


# ============================================================
# NoS visualization
# ============================================================

NoS_data_abcd <- data.frame(
  NoS = c(
    final_abcd$left_medium_cluster_Num_Fibers,
    final_abcd$right_medium_cluster_Num_Fibers,
    final_abcd$left_long_cluster_Num_Fibers,
    final_abcd$right_long_cluster_Num_Fibers,
    final_abcd$left_AF_Num_Fibers,
    final_abcd$right_AF_Num_Fibers
  ),
  
  Tract = c(
    rep("Left SWM", nrow(final_abcd)),
    rep("Right SWM", nrow(final_abcd)),
    rep("Left FAT", nrow(final_abcd)),
    rep("Right FAT", nrow(final_abcd)),
    rep("Left Arcuate", nrow(final_abcd)),
    rep("Right Arcuate", nrow(final_abcd))
  ),
  
  Dataset = "ABCD"
)


NoS_data_hcp <- data.frame(
  NoS = c(
    final_hcp$left_medium_cluster.Num_Fibers,
    final_hcp$right_medium_cluster.Num_Fibers,
    final_hcp$left_long_cluster.Num_Fibers,
    final_hcp$right_long_cluster.Num_Fibers,
    final_hcp$left_AF.Num_Fibers,
    final_hcp$right_AF.Num_Fibers
  ),
  
  Tract = c(
    rep("Left SWM", nrow(final_hcp)),
    rep("Right SWM", nrow(final_hcp)),
    rep("Left FAT", nrow(final_hcp)),
    rep("Right FAT", nrow(final_hcp)),
    rep("Left Arcuate", nrow(final_hcp)),
    rep("Right Arcuate", nrow(final_hcp))
  ),
  
  Dataset = "HCP"
)


NoS_data_combined <- rbind(
  NoS_data_abcd,
  NoS_data_hcp
)

NoS_data_combined$Tract <- factor(
  NoS_data_combined$Tract,
  levels = c(
    "Left Arcuate",
    "Right Arcuate",
    "Left FAT",
    "Right FAT",
    "Left SWM",
    "Right SWM"
  )
)

setwd("~/Documents/R")
svglite("NoS_plot.svg", width = 17, height = 6)
pirateplot(formula = NoS ~ Tract + Dataset, data = NoS_data_combined, main = "NoS of Pathways Across Datasets",
           point.o = 0, cap.beans = 1, 
           pal = c("darkorange1", "purple4", "cadetblue1", "deeppink", "chartreuse1", "firebrick1"),
           bean.f.o = 1, theme = 3, ylim  = c(0,10000), ylab = "NoS",
           back.col = "white",
           gl.col = "grey62", 
           avg.line.fun = mean,
           inf.disp = "bean",
           avg.line.col = "black",
           inf.b.o = 0,
           bean.b.col = "black",
           bean.b.o = 1)
dev.off()


