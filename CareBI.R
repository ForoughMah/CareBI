rm(list = ls())
.libPaths()

# LIBRARIES -------------------------------------------- ----
library(lavaan)
library(dplyr)
library(tidyr)

# Data calling------------------------------------------ ----

#1. Read data: NHATS and NSOC FILES FOR ROUND 12
NSOC_12 <- read_sas("NSOC_R12_Crss_File.sas7bdat")
NHATS_12 <-read_sas("NHATS_Round_12_SP_File.sas7bdat")

#2. ENTER WHICH ROUND?
sp <- NHATS_12 %>% 
  mutate(rnd = 12) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
sp <- sp %>% 
  rename_all(~stringr::str_replace(.,"^r12","")) %>%
  rename_all(~stringr::str_replace(.,"^hc12","")) %>% 
  rename_all(~stringr::str_replace(.,"^is12","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp12","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg12",""))

#4. ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
sp <- sp %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#5. SUBSET NEEDED VARIABLES
df<-sp %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#6. FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))


NHATS_filtered <- df %>%
  filter(
    !(dresid %in% c(3, 5, 7)) &                # Exclude residential care, nursing home, deceased
      !(dresid == 4 & rnd == 1) &              # Exclude residential care in round 1
      !(dresid %in% c(6, 8))                   # Exclude nursing home residents, deceased
  )

#7. POPULATION of SUBSET
NHATS_caregivers_r12 <- NSOC_12[NSOC_12$spid %in% NHATS_filtered$spid, ]
new_NSOC <- NHATS_caregivers_r12

#8. Exclude those that their patients deceased and those did not help SP in the last month  
NHATS_caregivers_r12 <- NHATS_caregivers_r12 %>%
  filter(fl12helplstmth == 1)

#9. 
new_NSOC <- NHATS_caregivers_r12

# Filling----------------------------------------------- ----

#1. Replace -8, -7, -6 with NA for all selected variables
new_NSOC <- new_NSOC %>%
  #mutate(across(where(is.numeric), ~ na_if(., -1))) %>%
  mutate(across(where(is.numeric), ~ na_if(., -6))) %>%
  mutate(across(where(is.numeric), ~ na_if(., -7))) %>%
  mutate(across(where(is.numeric), ~ na_if(., -8)))

#2.  Modal fill function
fill_modal <- function(x, value) {
  x[is.na(x)] <- value
  return(x)
}

#3. Use the fill function to fill model variables

# Overload
# 1: very much	to 3: not so much 
nsoc_over <- c("cac12toomuch", "cac12exhaustd", "cac12notime")                   
new_NSOC[nsoc_over] <- lapply(new_NSOC[nsoc_over], fill_modal, value = 3)


# Difficulty
# 1: Yes, 2: No
diff_yesno <- c("cac12diffemo", "cac12diffphy", "cac12diffinc") 
new_NSOC[diff_yesno] <- lapply(new_NSOC[diff_yesno], fill_modal, value = 2)

# 1: every day difficult to	5: never difficult
diff_level <- c("cac12diffemlv", "cac12diffinlv", "cac12diffphlv")
new_NSOC[diff_level] <- lapply(new_NSOC[diff_level], fill_modal, value = 3) 

# Mood
# 1: every day to	5: never
mood_positive <- c("che12moodfull", "che12moodcher", "che12moodpcfl") 
new_NSOC[mood_positive] <- lapply(new_NSOC[mood_positive], fill_modal, value = 2)

# Health
new_NSOC$che12pain <- fill_modal(new_NSOC$che12pain, value = 1)  
new_NSOC$che12lowenrgy <- fill_modal(new_NSOC$che12lowenrgy, value = 2) 
health_limit <- c("che12enrgylmt", "che12painlmt")
new_NSOC[health_limit] <- lapply(new_NSOC[health_limit], fill_modal, value = 3) 
new_NSOC$che12sleeptrb <- fill_modal(new_NSOC$che12sleeptrb, value = 3)

# Social Participation
# 1: Yes, 2: No 
social_yesno <- c("cpp12hlpkptgo", "cpp12hlpkptvs", "cpp12hlpkptgr")
new_NSOC[social_yesno] <- lapply(new_NSOC[social_yesno], fill_modal, value = 2) 
# importance
new_NSOC$cpp12impgo <- fill_modal(new_NSOC$cpp12impgo, value = 2)  
new_NSOC$cpp12impgroup <- fill_modal(new_NSOC$cpp12impgroup, value = 3)  
new_NSOC$cpp12impvst <- fill_modal(new_NSOC$cpp12impvst, value = 1) 

# Relationship Quality
# 1: a lot	to 4: not at all 
partner_quality <- c("cac12spapprlv", "cac12joylevel")
new_NSOC[partner_quality] <- lapply(new_NSOC[partner_quality], fill_modal, value = 1) 
new_NSOC$cac12nerveslv <- fill_modal(new_NSOC$cac12nerveslv, 4) 

# Items recoding --------------------------------------- -------

#DIFFICULTY
# merge variables "cac12diffphy" and "cac12diffphlv" in "cac12diffphlvnew" 
new_NSOC <- new_NSOC %>%
  mutate(cac12diffphlvnew = ifelse(cac12diffphy == "2", 0, cac12diffphlv)) %>%
  mutate(cac12diffphlvnew = as.numeric(cac12diffphlvnew))
table(addNA(new_NSOC$cac12diffphlvnew))

# merge variables "cac12diffinc" and "cac12diffinlv" in "cac12diffinlvnew" 
new_NSOC <- new_NSOC %>%
  mutate(cac12diffinlvnew = ifelse(cac12diffinc == "2", 0, cac12diffinlv)) %>%
  mutate(cac12diffinlvnew = as.numeric(cac12diffinlvnew)) 
table(addNA(new_NSOC$cac12diffinlvnew))

# merge variables "cac12diffemo" and "cac12diffemlv" in "cac12diffemlvnew" 
new_NSOC <- new_NSOC %>%
  mutate(cac12diffemlvnew = ifelse(cac12diffemo == "2", 0, cac12diffemlv)) %>%
  mutate(cac12diffemlvnew = as.numeric(cac12diffemlvnew))
table(addNA(new_NSOC$cac12diffemlvnew)) 


# HEALTH
# merge variables "che12pain" and "che12painlmt" in "che12painlmtnew" 
new_NSOC <- new_NSOC %>%
  mutate(che12painlmtnew = ifelse(che12pain == "2", 6, che12painlmt)) %>%
  mutate(che12painlmtnew = as.numeric(che12painlmtnew))
table(addNA(new_NSOC$che12painlmtnew))

# merge variables "che12lowenrgy" and "che12enrgylmt" in "che12enrgylmtnew"
new_NSOC <- new_NSOC %>%
  mutate(che12enrgylmtnew = ifelse(che12lowenrgy == "2", 6, che12enrgylmt)) %>%
  mutate(che12enrgylmtnew = as.numeric(che12enrgylmtnew)) 
table(addNA(new_NSOC$che12enrgylmtnew))

# SOCIAL PARTICIPATION
# merge variables "cpp12impgo" and "cpp12hlpkptgo" in "cpp12hlpkptgonew" 
new_NSOC <- new_NSOC %>%
  mutate(cpp12hlpkptgonew = case_when(
    cpp12impgo == "1" & cpp12hlpkptgo == "1" ~ 4,
    cpp12impgo == "2" & cpp12hlpkptgo == "1" ~ 3,
    cpp12impgo == "3" & cpp12hlpkptgo == "1" ~ 2,
    cpp12impgo == "1" & cpp12hlpkptgo == "2" ~ 1,
    cpp12impgo == "2" & cpp12hlpkptgo == "2" ~ 1,
    cpp12impgo == "3" & cpp12hlpkptgo == "2" ~ 1,
    TRUE ~ as.numeric(cpp12hlpkptgo)
  ))

# merge variables "cpp12impgroup" and "cpp12hlpkptgr" in "cpp12hlpkptgrnew" 
new_NSOC <- new_NSOC %>%
  mutate(cpp12hlpkptgrnew = case_when(
    cpp12impgroup == "1" & cpp12hlpkptgr == "1" ~ 4,
    cpp12impgroup == "2" & cpp12hlpkptgr == "1" ~ 3,
    cpp12impgroup == "3" & cpp12hlpkptgr == "1" ~ 2,
    cpp12impgroup == "1" & cpp12hlpkptgr == "2" ~ 1,
    cpp12impgroup == "2" & cpp12hlpkptgr == "2" ~ 1,
    cpp12impgroup == "3" & cpp12hlpkptgr == "2" ~ 1,
    TRUE ~ as.numeric(cpp12hlpkptgr)
  ))
table(addNA(new_NSOC$cpp12hlpkptgrnew))

# merge variables "cpp12impvst" and "cpp12hlpkptvs" in "cpp12hlpkptvsnew" 
new_NSOC <- new_NSOC %>%
  mutate(cpp12hlpkptvsnew = case_when(
    cpp12impvst == "1" & cpp12hlpkptvs == "1" ~ 4,
    cpp12impvst == "2" & cpp12hlpkptvs == "1" ~ 3,
    cpp12impvst == "3" & cpp12hlpkptvs == "1" ~ 2,
    cpp12impvst == "1" & cpp12hlpkptvs == "2" ~ 1,
    cpp12impvst == "2" & cpp12hlpkptvs == "2" ~ 1,
    cpp12impvst == "3" & cpp12hlpkptvs == "2" ~ 1,
    TRUE ~ as.numeric(cpp12hlpkptvs) 
  ))
table(addNA(new_NSOC$cpp12hlpkptvsnew))


# Reverse coding --------------------------------------- -------
#1. Specify reverse and not-reverse variables
reverse_var <- c("cac12toomuch", "cac12nerveslv", "che12enrgylmtnew", 
                 "cac12exhaustd", "cac12notime", "che12sleeptrb", 
                 "che12painlmtnew") 
notreverse_var <- c("cac12spapprlv", "cac12joylevel", "cac12diffemlvnew", 
                    "cac12diffinlvnew", "cac12diffphlvnew", "che12moodcher", 
                    "che12moodpcfl", "che12moodfull", "cpp12hlpkptgonew", 
                    "cpp12hlpkptgrnew", "cpp12hlpkptvsnew")

#2. Filter "reverse_var" and "notreverse_var" variables
new_NSOC_12 <- new_NSOC %>%
  dplyr::select(all_of(c(reverse_var, notreverse_var, "spid", "opid")))

#3. Filter out rows where any column has negative values
new_NSOC_12 <- new_NSOC_12 %>%
  filter(if_all(everything(), ~ !is.na(.) & . >= 0))

#4. # Reverse the values of reverse_var columns based on their respective max values
for (vari in reverse_var) {
  # Convert the column to numeric, making sure to handle non-numeric data
  new_NSOC_12[[vari]] <- as.numeric(as.character(new_NSOC_12[[vari]]))
  
  # Check if conversion introduced NAs
  if (any(is.na(new_NSOC_12[[vari]]))) {
    warning(paste("NAs introduced in column", vari, "after numeric conversion."))
  }
  
  # Calculate the maximum value, ignoring NAs
  max_val <- max(new_NSOC_12[[vari]], na.rm = TRUE)
  
  # Check if max_val is numeric, otherwise skip the reversal for this column
  if (!is.numeric(max_val)) {
    warning(paste("Skipping reversal for column", vari, "due to non-numeric max_val."))
    next
  }
  
  # Perform the reversal if max_val is valid
  new_NSOC_12[[vari]] <- (max_val + 1) - new_NSOC_12[[vari]]
}

# CareBI estimation ------------------------------------ ------

#1. Develope the bifactor model
bifactor_syntax <- '
                    # Define the General Factor (g_factor) - it is measured by ALL items
  
                      g_factor =~ cac12toomuch + cac12notime + cac12exhaustd + 
                      cac12diffphlvnew + cac12diffemlvnew  + cac12diffinlvnew +
                      che12moodfull +  che12moodpcfl + che12moodcher +
                      che12enrgylmtnew + che12painlmtnew + che12sleeptrb +
                      cpp12hlpkptgonew + cpp12hlpkptgrnew + cpp12hlpkptvsnew +
                      cac12spapprlv + cac12joylevel + cac12nerveslv 
                      
                    # Define the Specific Factors based on the clean EFA
                      Overload =~ cac12toomuch + cac12notime + cac12exhaustd
                      TaskDiff =~ cac12diffphlvnew + cac12diffemlvnew  + cac12diffinlvnew
                      Mood =~  che12moodfull +  che12moodpcfl + che12moodcher
                      Health =~  che12enrgylmtnew + che12painlmtnew + che12sleeptrb
                      Social =~ cpp12hlpkptgonew + cpp12hlpkptgrnew + cpp12hlpkptvsnew
                      Relation =~  cac12spapprlv + cac12joylevel + cac12nerveslv
                      Relation ~~ cac12diffemlvnew
                  ' # End of model syntax string
              

# Bifactor
bifactor_fit <- cfa(
  model = bifactor_syntax,
  data = variables_to_efa,
  ordered = all_new,
  estimator = "WLSMV",      # Robust estimator for categorical (Likert) data "WLSMV",
  orthogonal = TRUE,        # This is essential for a bifactor model: Assumes g-factor and specific factors are uncorrelated
  std.lv = TRUE             # Standardizes latent factors for easier interpretation
)

#2. Extract the latent variable scores (factor scores)
factor_scores <- lavPredict(bifactor_fit, type = "lv")

#3. Cextract the column for the general factor
g_raw <- factor_scores[, "g_factor"]

#4. Re-Scale the scores to a 0–100 range
g_scaled <- (g_raw - min(g_raw, na.rm = TRUE)) /
  (max(g_raw, na.rm = TRUE) - min(g_raw, na.rm = TRUE)) * 100

#4. Round the scaled scores to the nearest integer
CareBI_scores <- round(g_scaled)

#5. Clip values to ensure they are strictly between 0 and 100
CareBI_scores <- pmin(pmax(CareBI_scores, 0), 100)
new_NSOC_12$CareBI <- CareBI_scores

# Plot CareBI distribution ----------------------------- ------
all_variables <- c(
  "cac12toomuch", "cac12exhaustd", "cac12notime", 
  
  "cac12diffphlvnew", "cac12diffinlvnew", "cac12diffemlvnew",
  
  "che12moodfull", "che12moodpcfl", "che12moodcher",
  
  "che12painlmtnew", "che12enrgylmtnew", "che12sleeptrb",
  
  "cpp12hlpkptgonew", "cpp12hlpkptgrnew", "cpp12hlpkptvsnew", 
  
  "cac12spapprlv",  "cac12joylevel",  "cac12nerveslv")

variables_observation <- new_NSOC_12[, all_variables] 

ggplot(variables_observation, aes(x = new_NSOC_12$CareBI)) +
  geom_histogram(binwidth = 5, color = "black", fill = "skyblue") +
  labs(title = "Distribution of Burden Score in NSOC-12",
       x = "Burden Score (Rescaled 0–100)",
       y = "Frequency") +
  theme_minimal()


# CareBI natural classification  ----------------------- ------
new_NSOC_12 <- new_NSOC_12 %>%
  mutate(CareBI_cat = case_when(
    CareBI >= 0 & CareBI < 30 ~ 1,
    CareBI >= 30 & CareBI < 50 ~ 2,
    CareBI >= 50 & CareBI <= 100 ~ 3,
    TRUE ~ NA_integer_
  ))


  
  
  
