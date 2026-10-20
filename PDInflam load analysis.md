library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(mice)
library(tidyr)
library(Hmisc)
library(scales)
library(dunn.test)

data_all <- read_excel("PLL cohort full.xlsx", sheet = "case")
data <- data_all[data_all$Cohort == 1,]
data <- data_all[data_all$Cohort == 2,]
attach(data)

data_fpd <- data[data$Group == "HD (fail PD)",]
data_fail <- data[data$Fail == 1,]

epidata_all <- read_excel("PLL cohort full.xlsx", sheet = "perit_epi")
epidata <- epidata_all[epidata_all$Cohort == 1,]
attach(epidata)

summary(WCC1)
summary(WCC2)
summary(epidata$AUC_fok)

summary(data$AUC_cum)

epidata$WCC1_k <- epidata$WCC1/1000
epidata$AUC_k <- epidata$AUC/10000
epidata$WCC2_k <- epidata$WCC2/1000

hist_wcc <- ggplot(epidata, aes(x = WCC1_k)) +
  geom_histogram(aes(y = ..count..), binwidth = 5, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 5, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(8, 102), breaks = seq(10, 100, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  labs(x = "PDE WCC on presentation (×1000 cells per µL)", y = "No. of episode", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

ggsave("hist_wcc.jpg", plot = hist_wcc, width = 11.69, height = 8.27, dpi = 300)


hist_auc <- ggplot(epidata, aes(x = AUC_k)) +
  geom_histogram(aes(y = ..count..), binwidth = 2, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 2, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(-0.6, 80), breaks = seq(0, 80, by = 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50), expand = c(0, 0)) +
  labs(x = "PDE WCC AUC (×10E4 cell-day per µL)", y = "No. of episode", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

hist_auc <- ggplot(epidata, aes(x = AUC_fok)) +
  geom_histogram(aes(y = ..count..), binwidth = 2, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 2, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(-0.6, 120), breaks = seq(0, 120, by = 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 20), expand = c(0, 0)) +
  labs(x = "PDE WCC AUC (×10E3 cell-day per µL)", y = "No. of episode", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

hist_auc_trc <- ggplot(epidata, aes(x = AUC_trc)) +
  geom_histogram(aes(y = ..count..), binwidth = 2, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 2, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(-0.6, 100.6), breaks = seq(0, 100, by = 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50), expand = c(0, 0)) +
  labs(x = "PDE WCC AUC (×10E3 cell-day per µL)", y = "No. of episode", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

ggsave("hist_auc_trc.jpg", plot = hist_auc_trc, width = 11.69, height = 8.27, dpi = 300)

hist_wcc2 <- ggplot(epidata, aes(x = WCC2_k)) +
  geom_histogram(aes(y = ..count..), binwidth = 5, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 5, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(2, 72), breaks = seq(10, 70, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  labs(x = "PDE WCC on day 3 (×1000 cells per µL)", y = "No. of episode", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

ggsave("hist_wcc3.jpg", plot = hist_wcc2, width = 11.69, height = 8.27, dpi = 300)

#############################################
### Histogram: time from last peritonitis ###
#############################################

hist_lastp <- ggplot(data_fpd, aes(x = Lap_last)) +
  geom_histogram(aes(y = ..count..), binwidth = 3, boundary = 0, closed = "right",
                 colour = "black", fill = "white", na.rm = TRUE) +
  geom_text(
    stat = "bin",
    binwidth = 3, boundary = 0,
    aes(label = ..count.., y = ..count..),
    vjust = -0.25, size = 4.5
  ) +
  scale_x_continuous(limits = c(-0.2, 72), breaks = seq(0, 72, by = 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), expand = c(0, 0)) +
  labs(x = "time from last peritonitis episode (months)", y = "No. of patient", title = "") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 12)
  )

ggsave("hist_lastp.jpg", plot = hist_lastp, width = 11.69, height = 8.27, dpi = 300)



###############################
###       Box plot          ###
###############################

### linear y axis ###

box_fu <- ggplot(data, aes(x = Group, y = WithFU)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 50)) +
  labs(
    title = "",
    x = "patient group",
    y = "duration of follow up (months)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )

box_nop <- ggplot(data, aes(x = Group, y = NOP)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "",
    x = "patient group",
    y = "number of previous peritonitis"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )

box_FOK_cum <- ggplot(data, aes(x = Group, y = FOK_cum)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  labs(
    title = "",
    x = "patient group",
    y = "cumulative peritoneal leukocyte load \n (×10E3 cell-day per µL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )

box_TRC_cum <- ggplot(data, aes(x = Group, y = TRC_cum)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 50)) +
  labs(
    title = "",
    x = "patient group",
    y = "cumulative peritoneal leukocyte load \n (×10E3 cell-day per µL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )


ggsave("box_TRC_cum.jpg", plot = box_TRC_cum, width = 11.69, height = 8.27, dpi = 300)
ggsave("box_nop.jpg", plot = box_nop, width = 11.69, height = 8.27, dpi = 300)
ggsave("box_fu.jpg", plot = box_fu, width = 11.69, height = 8.27, dpi = 300)


box_FOK_back <-ggplot(data, aes(x = Group, y = FOK_back)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  labs(
    title = "",
    x = "patient group",
    y = "background peritoneal leukocyte load \n (×10E3 cell-day per µL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )

box_TRC_back <- ggplot(data, aes(x = Group, y = TRC_back)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "",
    x = "patient group",
    y = "background peritoneal leukocyte load \n (×10E3 cell-day per µL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )


ggsave("box_TRC_back.jpg", plot = box_TRC_back, width = 11.69, height = 8.27, dpi = 300)

box_auc_trc <- ggplot(data, aes(x = Group, y = AUC_trc)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.8) +
  geom_boxplot(fill = "white", color = "black", width = 0.7, size = 0.8, outlier.shape = 21, outlier.size = 4) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  labs(
    title = "",
    x = "patient group",
    y = " peritoneal leukocyte AUC of the \n last episode (×10E3 cell-day per µL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text = element_text(size = 20)
  )

ggsave("box_auc_trc.jpg", plot = box_auc_trc, width = 11.69, height = 8.27, dpi = 300)



###############################
###     Table 1 data        ###
###############################

summary(epidata$AUC_trc, na.rm=T)

table(Sex, Group)
table(Dx, Group)
table(DM, Group)
table(IHD, Group)
table(CVA, Group)
table(APD, Group)
table(BCompat, Group)

chisq.test(Sex, Group)
chisq.test(Dx, Group)
chisq.test(DM, Group)
chisq.test(IHD, Group)
chisq.test(CVA, Group)
chisq.test(APD, Group)
chisq.test(BCompat, Group)

tapply(Age, Group, mean, na.rm=T)
tapply(Charlson, Group, mean, na.rm=T)
tapply(BW, Group, mean, na.rm=T)
tapply(BH, Group, mean, na.rm=T)
tapply(BMI, Group, mean, na.rm=T)
tapply(DP4, Group, mean, na.rm=T)
tapply(MTAC, Group, mean, na.rm=T)

tapply(Age, Group, sd, na.rm=T)
tapply(Charlson, Group, sd, na.rm=T)
tapply(BW, Group, sd, na.rm=T)
tapply(BH, Group, sd, na.rm=T)
tapply(BMI, Group, sd, na.rm=T)
tapply(DP4, Group, sd, na.rm=T)
tapply(MTAC, Group, sd, na.rm=T)

tapply(data$AUC_cum, Group, summary, na.rm=T)
tapply(data$WithFU, Group, summary, na.rm=T)
tapply(data$WCC01_cum, Group, summary, na.rm=T)
tapply(data$WCC03_cum, Group, summary, na.rm=T)
tapply(data$WCC13_cum, Group, summary, na.rm=T)

anova(lm(Age ~ Group))
anova(lm(Charlson ~ Group))
anova(lm(BW ~ Group))
anova(lm(BH ~ Group))
anova(lm(BMI ~ Group))
anova(lm(DP4 ~ Group))
anova(lm(MTAC ~ Group))

mean(Age, na.rm=T)
mean(Charlson, na.rm=T)
mean(BW, na.rm=T)
mean(BH, na.rm=T)
mean(BMI, na.rm=T)
mean(DP4, na.rm=T)
mean(MTAC, na.rm=T)

sd(Age, na.rm=T)
sd(Charlson, na.rm=T)
sd(BW, na.rm=T)
sd(BH, na.rm=T)
sd(BMI, na.rm=T)
sd(DP4, na.rm=T)
sd(MTAC, na.rm=T)

sum(NOP)
sum(WithFU)
summary(TRC_cum)

summary(TPZ_cum)
summary(TPZ_back)
summary(FOK_cum)
summary(FOK_back)


###############################
## Prelim group calculation ###
###############################

tapply(NOP, Out_Gp, summary, na.rm=T)
tapply(AUC_cum, Out_Gp, summary, na.rm=T)
tapply(WCC01_cum, Out_Gp, summary, na.rm=T)
tapply(WCC03_cum, Out_Gp, summary, na.rm=T)
tapply(WCC13_cum, Out_Gp, summary, na.rm=T)
tapply(AUC_cum_l, Group, summary, na.rm=T)
tapply(WCC01_cum_l, Out_Gp, summary, na.rm=T)
tapply(WCC02_cum_l, Out_Gp, summary, na.rm=T)
tapply(WCC12_cum_l, Out_Gp, summary, na.rm=T)
tapply(WithFU, Out_Gp, summary, na.rm=T)

kruskal.test(AUC_trc ~ Group)
kruskal.test(TRC_cum ~ Group)
kruskal.test(TRC_back ~ Group)


kruskal.test(AUC_fok ~ Group)
kruskal.test(FOK_cum ~ Group)
kruskal.test(FOK_back ~ Group)
kruskal.test(NOP ~ Group)
kruskal.test(WithFU ~ Group)

kruskal.test(WCC01_cum ~ Out_Gp)
kruskal.test(WCC03_cum ~ Out_Gp)
kruskal.test(WCC13_cum ~ Out_Gp)
kruskal.test(AUC_cum_l ~ Out_Gp)
kruskal.test(WCC01_cum_l ~ Out_Gp)
kruskal.test(WCC02_cum_l ~ Out_Gp)
kruskal.test(WCC12_cum_l ~ Out_Gp)


tapply(WithFU24, Group24, summary, na.rm=T)
tapply(NOP_24, Group24, summary, na.rm=T)
tapply(AUC_c24, Group24, summary, na.rm=T)
tapply(WCC01_c24, Group24, summary, na.rm=T)
tapply(WCC03_c24, Group24, summary, na.rm=T)
tapply(WCC13_c24, Group24, summary, na.rm=T)
tapply(AUC_c24L, Group24, summary, na.rm=T)
tapply(WCC01_c24L, Group24, summary, na.rm=T)
tapply(WCC03_c24L, Group24, summary, na.rm=T)
tapply(WCC13_c24L, Group24, summary, na.rm=T)


tapply(WithFU36, Group36, summary, na.rm=T)
tapply(NOP_36, Group36, summary, na.rm=T)
tapply(AUC_c36, Group36, summary, na.rm=T)
tapply(WCC01_c36, Group36, summary, na.rm=T)
tapply(WCC03_c36, Group36, summary, na.rm=T)
tapply(WCC13_c36, Group36, summary, na.rm=T)
tapply(AUC_c36L, Group36, summary, na.rm=T)
tapply(WCC01_c36L, Group36, summary, na.rm=T)
tapply(WCC03_c36L, Group36, summary, na.rm=T)
tapply(WCC13_c36L, Group36, summary, na.rm=T)




continuous_vars <- data %>%
  select(NOP, AUC_cum, WCC01_cum, WCC03_cum, WCC13_cum, AUC_cum_l, WCC01_cum_l, WCC02_cum_l, WCC12_cum_l, WithFU)

correlation_matrix <- rcorr(as.matrix(continuous_vars), type = "pearson")

print("Correlation Coefficients:")
print(correlation_matrix$r)

print("P-values:")
print(correlation_matrix$P)

library(corrplot)
corrplot(correlation_matrix$r,
         method = "color",
         type = "upper",
         order = "hclust",
         p.mat = correlation_matrix$P,
         sig.level = 0.05,
         insig = "blank",
         title = "Correlation Matrix of Continuous Variables")

######################
## AUC calculation ###
######################

df_long <- read_excel("PLL cohort full.xlsx", sheet = "df_long", col_names = TRUE)

auc_quadratic <- function(x, y) {
  fit <- lm(y ~ poly(x, 2, raw = TRUE))
  coefs <- coef(fit)
  
  f <- function(x) {
    coefs[1] + coefs[2] * x + coefs[3] * x^2
  }
  
  integrate(f, lower = min(x), upper = max(x))$value
}

# Apply AUC per case in long format
auc_long <- df_long %>%
  group_by(case) %>%
  summarise(AUC = auc_quadratic(x = time, y = value)) %>%
  arrange(case)

write.csv(auc_long, "auc.csv", row.names = FALSE)

######################
## AUC by Bateman  ###
######################

df_long <- read_excel("PLL cohort full.xlsx", sheet = "df_long", col_names = TRUE)

bateman_func <- function(t, A0, ka, ke) {
  (A0 * ka / (ka - ke)) * (exp(-ke * t) - exp(-ka * t))
}

auc_quadratic <- function(x, y) {
  fit <- lm(y ~ poly(x, 2, raw = TRUE))
  coefs <- coef(fit)
  
  f <- function(x) {
    coefs[1] + coefs[2] * x + coefs[3] * x^2
  }
  
  integrate(f, lower = min(x), upper = max(x))$value
}

# Apply AUC per case in long format
auc_long <- df_long %>%
  group_by(case) %>%
  summarise(AUC = auc_quadratic(x = time, y = value)) %>%
  arrange(case)

write.csv(auc_long, "auc.csv", row.names = FALSE)


###############################
## Outcome analysis 15.4.26 ###
###############################

tapply(NOP, Group_6, summary, na.rm=T)
tapply(WithFU, Group_6, summary, na.rm=T)
tapply(AUC_cum, Group_6, summary, na.rm=T)
tapply(WCC01_cum, Group_6, summary, na.rm=T)
tapply(WCC02_cum, Group_6, summary, na.rm=T)
tapply(WCC12_cum, Group_6, summary, na.rm=T)
tapply(AUC_cum_l, Group_6, summary, na.rm=T)
tapply(WCC01_cum_l, Group_6, summary, na.rm=T)
tapply(WCC02_cum_l, Group_6, summary, na.rm=T)
tapply(WCC12_cum_l, Group_6, summary, na.rm=T)
tapply(AUC_last, Group_6, summary, na.rm=T)
tapply(WCC01_last, Group_6, summary, na.rm=T)
tapply(WCC02_last, Group_6, summary, na.rm=T)
tapply(WCC12_last, Group_6, summary, na.rm=T)

tapply(NOP, Group_5, summary, na.rm=T)
tapply(WithFU, Group_5, summary, na.rm=T)
tapply(AUC_cum, Group_5, summary, na.rm=T)
tapply(WCC01_cum, Group_5, summary, na.rm=T)
tapply(WCC02_cum, Group_5, summary, na.rm=T)
tapply(WCC12_cum, Group_5, summary, na.rm=T)
tapply(AUC_cum_l, Group_5, summary, na.rm=T)
tapply(WCC01_cum_l, Group_5, summary, na.rm=T)
tapply(WCC02_cum_l, Group_5, summary, na.rm=T)
tapply(WCC12_cum_l, Group_5, summary, na.rm=T)
tapply(AUC_last, Group_5, summary, na.rm=T)
tapply(WCC01_last, Group_5, summary, na.rm=T)
tapply(WCC02_last, Group_5, summary, na.rm=T)
tapply(WCC12_last, Group_5, summary, na.rm=T)

tapply(NOP, Group_m, summary, na.rm=T)
tapply(WithFU, Group_m, summary, na.rm=T)
tapply(AUC_cum, Group, summary, na.rm=T)
tapply(WCC01_cum, Group_m, summary, na.rm=T)
tapply(WCC02_cum, Group_m, summary, na.rm=T)
tapply(WCC12_cum, Group_m, summary, na.rm=T)
tapply(AUC_cum_l, Group, summary, na.rm=T)
tapply(WCC01_cum_l, Group_m, summary, na.rm=T)
tapply(WCC02_cum_l, Group_m, summary, na.rm=T)
tapply(WCC12_cum_l, Group_m, summary, na.rm=T)
tapply(AUC_last, Group_m, summary, na.rm=T)
tapply(WCC01_last, Group_m, summary, na.rm=T)
tapply(WCC02_last, Group_m, summary, na.rm=T)
tapply(WCC12_last, Group_m, summary, na.rm=T)

kruskal.test(NOP ~ Group)
kruskal.test(WithFU ~ Group)
kruskal.test(AUC_cum ~ Group)


kruskal.test(AUC_cum_l ~ Group)
kruskal.test(AUC_last ~ Group)

dunn.test(x = NOP, g = Group, method = "holm")
dunn.test(x = WithFU, g = Group, method = "holm")
dunn.test(x = AUC_fok, g = Group, method = "holm")
dunn.test(x = FOK_cum, g = Group, method = "holm")
dunn.test(x = FOK_back, g = Group, method = "holm")

dunn.test(x = AUC_trc, g = Group, method = "holm")
dunn.test(x = TRC_cum, g = Group, method = "holm")
dunn.test(x = TRC_back, g = Group, method = "holm")

dunn.test(x = WCC01_cum, g = Group_6, method = "holm")
dunn.test(x = WCC02_cum, g = Group_6, method = "holm")
dunn.test(x = WCC12_cum, g = Group_6, method = "holm")
dunn.test(x = AUC_cum_l, g = Group_6, method = "holm")
dunn.test(x = WCC01_cum_l, g = Group_6, method = "holm")
dunn.test(x = WCC02_cum_l, g = Group_6, method = "holm")
dunn.test(x = WCC12_cum_l, g = Group_6, method = "holm")
dunn.test(x = AUC_last, g = Group_6, method = "holm")
dunn.test(x = WCC01_last, g = Group_6, method = "holm")
dunn.test(x = WCC02_last, g = Group_6, method = "holm")
dunn.test(x = WCC12_last, g = Group_6, method = "holm")

dunn.test(x = NOP, g = Group_5, method = "holm")
dunn.test(x = WithFU, g = Group_5, method = "holm")
dunn.test(x = AUC_cum, g = Group_5, method = "holm")
dunn.test(x = WCC01_cum, g = Group_5, method = "holm")
dunn.test(x = WCC02_cum, g = Group_5, method = "holm")
dunn.test(x = WCC12_cum, g = Group_5, method = "holm")
dunn.test(x = AUC_cum_l, g = Group_5, method = "holm")
dunn.test(x = WCC01_cum_l, g = Group_5, method = "holm")
dunn.test(x = WCC02_cum_l, g = Group_5, method = "holm")
dunn.test(x = WCC12_cum_l, g = Group_5, method = "holm")
dunn.test(x = AUC_last, g = Group_5, method = "holm")
dunn.test(x = WCC01_last, g = Group_5, method = "holm")
dunn.test(x = WCC02_last, g = Group_5, method = "holm")
dunn.test(x = WCC12_last, g = Group_5, method = "holm")

dunn.test(x = NOP, g = Group_m, method = "holm")
dunn.test(x = WithFU, g = Group_m, method = "holm")
dunn.test(x = AUC_cum, g = Group_m, method = "holm")
dunn.test(x = WCC01_cum, g = Group_m, method = "holm")
dunn.test(x = WCC02_cum, g = Group_m, method = "holm")
dunn.test(x = WCC12_cum, g = Group_m, method = "holm")
dunn.test(x = AUC_cum_l, g = Group_m, method = "holm")
dunn.test(x = WCC01_cum_l, g = Group_m, method = "holm")
dunn.test(x = WCC02_cum_l, g = Group_m, method = "holm")
dunn.test(x = WCC12_cum_l, g = Group_m, method = "holm")
dunn.test(x = AUC_last, g = Group_m, method = "holm")
dunn.test(x = WCC01_last, g = Group_m, method = "holm")
dunn.test(x = WCC02_last, g = Group_m, method = "holm")
dunn.test(x = WCC12_last, g = Group_m, method = "holm")

summary(Group)
table(Group, Sex)

summary(AUC_cum, na.rm=T)
cor.test(AUC_cum_l, AUC_last, method = "spearman")
cor.test(AUC_cum_l, AUC_last, method = "pearson")

cor.test(data_fpd$AUC_cum_l, data_fpd$AUC_last, method = "spearman")

###############################
##      ROC calculation     ###
###############################

library(pROC)

roc_TRC_cum <- roc(response = data$Fail, 
               predictor = data$TRC_cum, 
               levels = c(0, 1), 
               direction = "<")

auc_value_TRC_cum <- auc(roc_TRC_cum)
auc_ci_TRC_cum <- ci.auc(roc_TRC_cum)

print(auc_value_TRC_cum)

wt_TRC_cum <- wilcox.test(TRC_cum ~ Fail, data = data)
p_TRC_cum <- wt_TRC_cum$p.value

print(auc_ci_TRC_cum)
print(paste("AUC value of TRC_cum is:", auc_value_TRC_cum))
print(paste("P-value:", p_TRC_cum))

roc_TRC_back <- roc(response = data$Fail, 
                   predictor = data$TRC_back, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_TRC_back <- auc(roc_TRC_back)
wt_TRC_back <- wilcox.test(TRC_back ~ Fail, data = data)
p_TRC_back <- wt_TRC_back$p.value

print(paste("AUC value of TRC_back is:", auc_value_TRC_back))
print(paste("P-value:", p_TRC_back))

roc_TPZ_cum <- roc(response = data$Fail, 
                   predictor = data$TPZ_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_TPZ_cum <- auc(roc_TPZ_cum)
auc_ci_TRC_cum <- ci.auc(roc_TRC_cum)
wt_TPZ_cum <- wilcox.test(TPZ_cum ~ Fail, data = data)
p_TPZ_cum <- wt_TPZ_cum$p.value

print(paste("AUC value of TPZ_cum is:", auc_value_TPZ_cum))
print(paste("P-value:", p_TPZ_cum))

roc_TPZ_back <- roc(response = data$Fail, 
                    predictor = data$TPZ_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_TPZ_back <- auc(roc_TPZ_back)
wt_TPZ_back <- wilcox.test(TPZ_back ~ Fail, data = data)
p_TPZ_back <- wt_TPZ_back$p.value

print(paste("AUC value of TPZ_back is:", auc_value_TPZ_back))
print(paste("P-value:", p_TPZ_back))

roc_TRC_cum <- roc(response = data$Fail, 
                   predictor = data$TRC_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_TRC_cum <- auc(roc_TRC_cum)
wt_TRC_cum <- wilcox.test(TRC_cum ~ Fail, data = data)
p_TRC_cum <- wt_TRC_cum$p.value

print(paste("AUC value of TRC_cum is:", auc_value_TRC_cum))
print(paste("P-value:", p_TRC_cum))

roc_TRC_back <- roc(response = data$Fail, 
                    predictor = data$TRC_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_TRC_back <- auc(roc_TRC_back)
wt_TRC_back <- wilcox.test(TRC_back ~ Fail, data = data)
p_TRC_back <- wt_TRC_back$p.value

print(paste("AUC value of TRC_back is:", auc_value_TRC_back))
print(paste("P-value:", p_TRC_back))

roc_FOK_cum <- roc(response = data$Fail, 
                   predictor = data$FOK_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_FOK_cum <- auc(roc_FOK_cum)
wt_FOK_cum <- wilcox.test(FOK_cum ~ Fail, data = data)
p_FOK_cum <- wt_FOK_cum$p.value

print(paste("AUC value of FOK_cum is:", auc_value_FOK_cum))
print(paste("P-value:", p_FOK_cum))

roc_FOK_back <- roc(response = data$Fail, 
                    predictor = data$FOK_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_FOK_back <- auc(roc_FOK_back)
wt_FOK_back <- wilcox.test(FOK_back ~ Fail, data = data)
p_FOK_back <- wt_FOK_back$p.value

print(paste("AUC value of FOK_back is:", auc_value_FOK_back))
print(paste("P-value:", p_FOK_back))



plot(roc_AUC_cum, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         # Use 1 - Specificity on the x-axis
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

plot(roc_AUC_cum_l, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         # Use 1 - Specificity on the x-axis
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

ggroc(list(background_PLL = roc_AUC_cum_l, cumulative_PLL = roc_AUC_cum), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

ggsave("roc_fig2.jpg", plot = roc_fig2, width = 8.27, height = 8.27, dpi = 300)


comparison_TRC <- roc.test(roc_TRC_cum, roc_TRC_back, method = "delong")
print(comparison_TRC)


#################################
##      ROC of NOP and FU     ###
#################################


roc_nop <- roc(response = data$Fail, 
                   predictor = data$NOP, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_nop <- auc(roc_nop)
wt_nop <- wilcox.test(NOP ~ Fail, data = data)
p_nop <- wt_nop$p.value

print(paste("AUC value of NOP is:", auc_value_nop))
print(paste("P-value:", p_nop))

plot(roc_nop, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

roc_fu <- roc(response = data$Fail, 
               predictor = data$WithFU, 
               levels = c(0, 1), 
               direction = "<")

auc_value_fu <- auc(roc_fu)
wt_fu <- wilcox.test(WithFU ~ Fail, data = data)
p_fu <- wt_fu$p.value

print(paste("AUC value of WIthFU is:", auc_value_fu))
print(paste("P-value:", p_fu))

plot(roc_fu, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

ggroc(list(AUC_nop = roc_nop, AUC_fu = roc_fu), 
                  size = 1.2, 
                  legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("AUC_nop" = "red", "AUC_fu" = "black")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

ggsave("roc_fig2.jpg", plot = roc_fig2, width = 8.27, height = 8.27, dpi = 300)


roc4 <- ggroc(list(background_PLL = roc_AUC_cum_l, cumulative_PLL = roc_AUC_cum, number_of_peritonitis = roc_nopm1, duration_of_dialysis = roc_fu), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red", "number_of_peritonitis" = "grey", "duration_of_dialysis" = "blue")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

roc4 <- ggroc(list(cumulative_PLL = roc_AUC_cum, background_PLL = roc_AUC_cum_l, number_of_peritonitis = roc_nopm1, duration_of_dialysis = roc_fu), 
              size = 1.2, 
              legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red", "number_of_peritonitis" = "grey", "duration_of_dialysis" = "blue")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "(A) Development cohort",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = "none")

roc4 <- ggroc(list(cumulative_PLL = roc_TRC_cum, background_PLL = roc_TRC_back, number_of_peritonitis = roc_nop, duration_of_dialysis = roc_fu), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red", "number_of_peritonitis" = "grey", "duration_of_dialysis" = "blue")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "(A) Development cohort",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = "none")

ggsave("roc4.jpg", plot = roc4, width = 8.27, height = 8.27, dpi = 300)

comparison_cum_nop <- roc.test(roc_TRC_cum, roc_nop, method = "delong")
print(comparison_cum_nop)

#############################################
##      Validation cohort demographic     ###
#############################################

data_v <- data_all[data_all$Cohort == 2,]
attach(data_v)

library(pROC)

roc_TRC_cum_v <- roc(response = data_v$Fail, 
                     predictor = data_v$TRC_cum, 
                     levels = c(0, 1), 
                     direction = "<")

auc_value_TRC_cum_v <- auc(roc_TRC_cum_v)
wt_TRC_cum_v <- wilcox.test(TRC_cum ~ Fail, data = data_v)
p_TRC_cum_v <- wt_TRC_cum_v $p.value

print(paste("AUC value of TRC_cum_v is:", auc_value_TRC_cum_v))
print(paste("P-value:", p_TRC_cum_v))

roc_TRC_back_v <- roc(response = data_v$Fail, 
                      predictor = data_v$TRC_back, 
                      levels = c(0, 1), 
                      direction = "<")

auc_value_TRC_back_v <- auc(roc_TRC_back_v)
wt_TRC_back_v <- wilcox.test(TRC_back ~ Fail, data = data_v)
p_TRC_back_v <- wt_TRC_back_v $p.value

print(paste("AUC value of TRC_back_v is:", auc_value_TRC_back_v))
print(paste("P-value:", p_TRC_back_v))



plot(roc_AUC_cum_v, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         # Use 1 - Specificity on the x-axis
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

plot(roc_AUC_cum_l_v, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         # Use 1 - Specificity on the x-axis
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

ggroc(list(background_PLL = roc_AUC_cum_l_v, cumulative_PLL = roc_AUC_cum_v), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

ggsave("roc_fig2.jpg", plot = roc_fig2, width = 8.27, height = 8.27, dpi = 300)


comparison_cum_cum_l_v <- roc.test(roc_AUC_cum_v, roc_AUC_cum_l_v, method = "delong")
print(comparison_cum_cum_l_v)



roc_nop_v <- roc(response = data_v$Fail, 
                 predictor = data_v$NOP, 
                 levels = c(0, 1), 
                 direction = "<")

auc_value_nop_v <- auc(roc_nop_v)
wt_nop_v <- wilcox.test(NOP ~ Fail, data = data_v)
p_nop_v <- wt_nop_v $p.value

print(paste("AUC value of NOP_v is:", auc_value_nop_v))
print(paste("P-value:", p_nop_v))

plot(roc_nop_v, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

roc_fu_v <- roc(response = data_v$Fail, 
                predictor = data_v$WithFU, 
                levels = c(0, 1), 
                direction = "<")

auc_value_fu_v <- auc(roc_fu_v)
wt_fu_v <- wilcox.test(WithFU ~ Fail, data = data_v)
p_fu_v <- wt_fu_v $p.value

print(paste("AUC value of WIthFU_v is:", auc_value_fu_v))
print(paste("P-value:", p_fu_v))

plot(roc_fu_v, 
     main = "", 
     col = "red", 
     lwd = 3, 
     print.auc = FALSE, 
     legacy.axes = TRUE,         
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")

ggroc(list(AUC_nop = roc_nop_v, AUC_fu = roc_fu_v), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("AUC_nop" = "red", "AUC_fu" = "black")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

ggsave("roc_fig2.jpg", plot = roc_fig2, width = 8.27, height = 8.27, dpi = 300)


ggroc(list(cumulative_PLL = roc_AUC_cum_v, background_PLL = roc_AUC_cum_l_v, number_of_peritonitis = roc_nop_v, duration_of_dialysis = roc_fu_v), 
                size = 1.2, 
                legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red", "number_of_peritonitis" = "grey", "duration_of_dialysis" = "blue")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "(B) Validation cohort",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = c(0.8, 0.2), # Place legend inside the plot
        legend.text = element_text(size = 11),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "black"))

roc4_v <- ggroc(list(cumulative_PLL = roc_TRC_cum_v, background_PLL = roc_TRC_back_v, number_of_peritonitis = roc_nop_v, duration_of_dialysis = roc_fu_v), 
      size = 1.2, 
      legacy.axes = TRUE) + # Use 1-Specificity for the x-axis
  scale_color_manual(values = c("cumulative_PLL" = "black", "background_PLL" = "red", "number_of_peritonitis" = "grey", "duration_of_dialysis" = "blue")) +
  # Set the axis ranges from 0.0 to 1.0
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  
  # Add the diagonal reference line (random classifier)
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour = "grey", linetype = "dashed") +
  
  # Labels and Theme
  labs(title = "(B) Validation cohort",
       x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Diagnostic Test") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10), hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 20, r = 10, b = 10, l = 10), hjust = 0.5, vjust = 0.5), 
        legend.position = "none")

ggsave("roc4_v.jpg", plot = roc4_v, width = 8.27, height = 8.27, dpi = 300)

comparison_cum_nop_v <- roc.test(roc_AUC_cum_v, roc_nop_v, method = "delong")
print(comparison_cum_nop_v)

#####################################
##      optimal ROC by Youden     ###
#####################################

library(pROC)

optimal_TRC_cum <- coords(
  roc_TRC_cum,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TRC_cum Cut-off (Youden Index) ===\n")
print(optimal_TRC_cum)

optimal_TRC_back <- coords(
  roc_TRC_back,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TRC_back Cut-off (Youden Index) ===\n")
print(optimal_TRC_back)

optimal_TPZ_cum <- coords(
  roc_TPZ_cum,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TPZ_cum Cut-off (Youden Index) ===\n")
print(optimal_TPZ_cum)

optimal_TPZ_back <- coords(
  roc_TPZ_back,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TPZ_back Cut-off (Youden Index) ===\n")
print(optimal_TPZ_back)

optimal_FOK_cum <- coords(
  roc_FOK_cum,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_FOK_cum Cut-off (Youden Index) ===\n")
print(optimal_FOK_cum)

optimal_FOK_back <- coords(
  roc_FOK_back,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_FOK_back Cut-off (Youden Index) ===\n")
print(optimal_FOK_back)

optimal_nop <- coords(
  roc_nop,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_nop Cut-off (Youden Index) ===\n")
print(optimal_nop)

optimal_TRC_cum_v <- coords(
  roc_TRC_cum_v,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TRC_cum_v Cut-off (Youden Index) ===\n")
print(optimal_TRC_cum_v)

optimal_TRC_back_v <- coords(
  roc_TRC_back_v,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_TRC_back_v Cut-off (Youden Index) ===\n")
print(optimal_TRC_back_v)

optimal_nop_v <- coords(
  roc_nop_v,  
  x = "best",
  best.method = "youden",   # maximise Sensitivity + Specificity - 1  
  ret = c("threshold", "sensitivity", "specificity", "youden")
)
cat("=== Optimal_nop_v Cut-off (Youden Index) ===\n")
print(optimal_nop_v)

###################################
### Net re classification index ###
###################################

data_co1 <- read_excel("PLL cut off analysis.xlsx", sheet = "co1_cal")

co1_nop <- glm(Fail ~ NOP, data = data_co1, family = binomial)
co1_auc <- glm(Fail ~ AUC_cum, data = data_co1, family = binomial)
co1_add <- glm(Fail ~ NOP + AUC_cum, data = data_co1, family = binomial)
co1_auc_l <- glm(Fail ~ AUC_cum_l, data = data_co1, family = binomial)
summary(co1_nop)
summary(co1_auc)
summary(co1_add)
summary(co1_auc)

data_co1_nr <- read_excel("PLL cut off analysis.xlsx", sheet = "co1_nr")
dat_nri <- data.frame(
  id      = 1:800,          # subject identifier
  outcome = c(0, 1, ...),   # 0/1 — 1 = event
  p_old   = fitted(m_old),  # probability 0–1, baseline model (test A)
  p_new   = fitted(m_new)   # probability 0–1, new model (test A + test B)
)

df_co1 <- read_excel("PLL cut off analysis.xlsx", sheet = "co1_nr")

nri_co1 <- improveProb(
  x1 = df_co1$p_nop,  # Traditional test probability
  x2 = df_co1$p_cum,  # New test probability
  y  = df_co1$Fail   # Event indicator
)

print(nri_co1)

nri_cum_l <- improveProb(
  x1 = df_co1$p_nop,  # Traditional test probability
  x2 = df_co1$p_cum_l,  # New test probability
  y  = df_co1$Fail   # Event indicator
)

print(nri_cum_l)

nri_co1_add <- improveProb(
  x1 = df_co1$p_nop,  # Traditional test probability
  x2 = df_co1$p_add,  # New test probability
  y  = df_co1$Fail   # Event indicator
)

print(nri_co1_add)

### COHORT 2 ###

data_co2 <- read_excel("PLL cut off analysis.xlsx", sheet = "co2_cal")

co2_nop <- glm(Fail ~ NOP, data = data_co2, family = binomial)
co2_auc <- glm(Fail ~ AUC_cum, data = data_co2, family = binomial)
co2_add <- glm(Fail ~ NOP + AUC_cum, data = data_co2, family = binomial)
co2_auc_l <- glm(Fail ~ AUC_cum_l, data = data_co2, family = binomial)
summary(co2_nop)
summary(co2_auc)
summary(co2_add)
summary(co2_auc)

df_co2 <- read_excel("PLL cut off analysis.xlsx", sheet = "co2_nr")

nri_co2_add <- improveProb(
  x1 = df_co2$p_nop,  # Traditional test probability
  x2 = df_co2$p_add,  # New test probability
  y  = df_co2$Fail   # Event indicator
)

print(nri_co2_add)

#################################
###    COX ANALYSIS OF PLL    ###
#################################

library(survival)
library(cmprsk)

data_all <- read_excel("PLL cohort full.xlsx", sheet = "case")
data_c1 <- data_all[data_all$Cohort == 1,]
data_c2 <- data_all[data_all$Cohort == 2,]
attach(data_c1)


cox_c1_nop <- coxph(Surv(Ev_FU, Fail==1) ~ NOP + GFR + DP4 + Charlson + DM,
                data = data_c1)
cox_c1 <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_cum + GFR + DP4 + Charlson + DM,
                      data = data_c1)
cox_c1b <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_back + GFR + DP4 + Charlson + DM,
                data = data_c1)
cox_c1_both <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_cum + NOP + GFR + DP4 + Charlson + DM,
                 data = data_c1)
cox_c1_bb <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_back + NOP + GFR + DP4 + Charlson + DM,
                     data = data_c1)

summary(cox_c1_nop)
summary(cox_c1)
summary(cox_c1b)
summary(cox_c1_both)
summary(cox_c1_bb)

cox_c2_both <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_cum + NOP + GFR + DP4 + Charlson + DM,
                     data = data_c2)
summary(cox_c2_both)


cox_c2_bb <- coxph(Surv(Ev_FU, Fail==1) ~ TPZ_back + NOP + GFR + DP4 + Charlson + DM,
                     data = data_c2)
summary(cox_c2_bb)

#################################
###          ROC 95CI         ###
#################################

roc_TPZ_cum <- roc(response = data$Fail, 
                   predictor = data$TPZ_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_TPZ_cum <- auc(roc_TPZ_cum)
auc_ci_TPZ_cum <- ci.auc(roc_TPZ_cum)
print(auc_value_TPZ_cum)
print(auc_ci_TPZ_cum)

roc_TPZ_back <- roc(response = data$Fail, 
                    predictor = data$TPZ_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_TPZ_back <- auc(roc_TPZ_back)
auc_ci_TPZ_back <- ci.auc(roc_TPZ_back)
print(auc_value_TPZ_back)
print(auc_ci_TPZ_back)

roc_nop <- roc(response = data$Fail, 
               predictor = data$NOP, 
               levels = c(0, 1), 
               direction = "<")

auc_value_nop <- auc(roc_nop)
auc_ci_nop <- ci.auc(roc_nop)
print(auc_value_nop)
print(auc_ci_nop)

roc_fu <- roc(response = data$Fail, 
              predictor = data$WithFU, 
              levels = c(0, 1), 
              direction = "<")

auc_value_fu <- auc(roc_fu)
auc_ci_fu <- ci.auc(roc_fu)
print(auc_value_fu)
print(auc_ci_fu)

roc_TRC_cum <- roc(response = data$Fail, 
                   predictor = data$TRC_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_TRC_cum <- auc(roc_TRC_cum)
auc_ci_TRC_cum <- ci.auc(roc_TRC_cum)
print(auc_value_TRC_cum)
print(auc_ci_TRC_cum)

roc_TRC_back <- roc(response = data$Fail, 
                    predictor = data$TRC_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_TRC_back <- auc(roc_TRC_back)
auc_ci_TRC_back <- ci.auc(roc_TRC_back)
print(auc_value_TRC_back)
print(auc_ci_TRC_back)

roc_FOK_cum <- roc(response = data$Fail, 
                   predictor = data$FOK_cum, 
                   levels = c(0, 1), 
                   direction = "<")

auc_value_FOK_cum <- auc(roc_FOK_cum)
auc_ci_FOK_cum <- ci.auc(roc_FOK_cum)
print(auc_value_FOK_cum)
print(auc_ci_FOK_cum)

roc_FOK_back <- roc(response = data$Fail, 
                    predictor = data$FOK_back, 
                    levels = c(0, 1), 
                    direction = "<")

auc_value_FOK_back <- auc(roc_FOK_back)
auc_ci_FOK_back <- ci.auc(roc_FOK_back)
print(auc_value_FOK_back)
print(auc_ci_FOK_back)
