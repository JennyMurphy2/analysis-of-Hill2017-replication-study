# Load packages 
library(tidyverse)
library(afex)
library(emmeans)
library(stats)
library(reshape2)
library(pastecs)
library(MOTE)
library(janitor)

# Replication data --------------------------------------------------------------------

# Load rep_data

rep_data <- read_csv("vo2_Data.csv") %>%
  clean_names()
head(rep_data)

# Prepare data

anova_rep_data <- rep_data %>%
  pivot_longer(cols = c(ibs:normal),
               names_to = "condition",
               values_to = "vo2") 

anova_rep_data$condition <-  as.factor(anova_rep_data$condition)
anova_rep_data$participant<-  as.factor(anova_rep_data$participant)

## Descriptives ---------------------

summary_rep_data <- anova_rep_data %>%
  group_by(condition) %>%
  summarise(count = n (),
            mean = mean(vo2),
            sd = sd(vo2))
summary_rep_data

### Plots ---------------------------------------------------------------------------

### Histogram 

# Prepare data
hist_dat <- anova_rep_data %>%
  select(vo2)

hist_dat$id <- 1:nrow(hist_dat)
hist_dat <- melt(hist_dat, id.vars = "id")

# Plot histogram
hist <- ggplot(data = hist_dat, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "10m Sprint Time (seconds)")
hist

### Q-Q plots 

ggplot(anova_rep_data, aes(sample = vo2)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_rep_data, aes(x = condition, y = vo2)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## Replication ANOVA ----------------------------------------------------------------------------

##afex::aov_4(continuous_var ~ condition_var + (RM_var|id_var)

anova_rep_data_afx <- afex::aov_4(
  vo2 ~ condition + (condition | participant),
  data = anova_rep_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_rep_data_afx

summary(anova_rep_data_afx)

### Assumption checking ---------

# Normality test

shapiro.test(anova_rep_data_afx$lm$residuals) # residuals are not normally distributed

anova_rep_data %>% 
  group_by(condition) %>% 
  rstatix::shapiro_test(vo2)

# Outliers

anova_rep_data %>% 
  group_by(condition) %>% 
  rstatix::identify_outliers(vo2)

## Replication effect size ----------------------------------------------------------------------------

pes_rep <- eta.F(
  dfm = 3,
  dfe = 132,
  Fvalue = anova_rep_data_afx$anova_table$F,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier
pes_rep

# Original data --------------------------------------------------------------------

# Load data

orig_data <- read_csv("original_data.csv") %>%
  clean_names() 
head(orig_data)

# Prepare rep_data

anova_orig_data <- orig_data %>%
  rename(internal = "vo2kg_internal_mean", 
         external = "vo2kg_external_mean", 
         movement = "vo2kg_movement_mean", 
         control = "vo2kg_control_mean") %>%
  select(id, internal, external, movement, control) %>%
  pivot_longer(cols = c(internal:control),
               names_to = "condition",
               values_to = "vo2") %>%
  drop_na() # remove participants with completely missing data

anova_orig_data$condition <-  as.factor(anova_orig_data$condition)
anova_orig_data$id <-  as.factor(anova_orig_data$id)

## Descriptives ---------------------

summary_orig_data <- anova_orig_data %>%
  group_by(condition) %>%
  summarise(count = n (),
            mean = mean(vo2),
            sd = sd(vo2))
summary_orig_data

### Plots ---------------------------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_dat <- anova_orig_data %>%
  select(vo2)

hist_dat$id <- 1:nrow(hist_dat)
hist_dat <- melt(hist_dat, id.vars = "id")

# Plot histogram
hist <- ggplot(data = hist_dat, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "10m Sprint Time (seconds)")
hist

### Q-Q plots 

ggplot(anova_orig_data, aes(sample = vo2)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_orig_data, aes(x = condition, y = vo2)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## Original ANOVA ----------------------------------------------------------------------------

##afex::aov_4(continuous_var ~ condition_var + (RM_var|id_var)

anova_orig_data_afx <- afex::aov_4(
  vo2 ~ condition + (condition | id),
  data = anova_orig_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_orig_data_afx

summary(anova_orig_data_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_orig_data_afx$lm$residuals) # residuals are not normally distributed

anova_orig_data %>% 
  group_by(condition) %>% 
  rstatix::shapiro_test(vo2)

# Outlier

anova_orig_data %>% 
  group_by(condition) %>% 
  rstatix::identify_outliers(vo2)

## Original effect size -----------------------------------------------

### Original data ------------------
## Values

orig_values <- data.frame(
  fval = 11.29,
  df1 = 3,
  df2 = 87
)

### Confirming reported eta --------

pes_ori <- eta.F(
  dfm = orig_values$df1,
  dfe = orig_values$df2,
  Fvalue = orig_values$fval,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Original study")) # add identifier
pes_ori

# Replication test -----

pes_rep_eta = anova_rep_data_afx$anova_table$pes
df_rep = 132
pes_orig_eta = pes_ori$eta
df_ori = orig_values$df2

rho_ori = 2 * sqrt(pes_orig_eta) - 1
rho_rep = 2 * sqrt(pes_rep_eta) - 1

rep_test = TOSTER::compare_cor(r1 = rho_ori,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test

# Forest plot ---------

## Labels for vo2 forest plot -------------
label_rep <- "0.04 [0.00, 0.10]"
label_ori <- "0.47 [0.09, 0.72]"

## Join rep_datasets -----------------
plot <-
  merge(
    pes_ori,
    pes_rep,
    by = c("eta", "etalow", "etahigh", "study_id"),
    all = TRUE
  )

## Plot -----------------------------
ggplot(plot,
       aes(
         y = study_id,
         x = eta,
         xmin = etalow,
         xmax = etahigh
       )) +
  ggtitle("Partial eta squared [95% CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-1, 2.2)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 1.8,
           y = 2,
           label = label_rep) +
  annotate("text",
           x = 1.8,
           y = 1,
           label = label_ori) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.94),
    panel.background = element_blank()
  )

