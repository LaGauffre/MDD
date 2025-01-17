# exam_2025.R

# Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Importation données mortalité BELGIQUE
Death <- read_table("Deaths_1x1.txt", 
                            skip = 1)
Ec <- read_table("Exposures_1x1.txt", 
                            skip = 1)
# Etude année 2000
year <- 2000
Death_V1 <- Death  %>% filter((Year == year)) %>% 
  mutate (Age = as.numeric(Age)) %>%
  filter(Age >= 30 & Age < 105) %>% select(Age, Year, Total) %>% rename(Dx = Total)
Ec_V1 <- Ec  %>% filter((Year == 2000)) %>% 
  mutate (Age = as.numeric(Age)) %>%
  filter(Age >= 30 & Age < 105) %>% select(Age, Year, Total) %>% rename(ExC = Total)

df_Bel <- Ec_V1 %>% left_join(Death_V1) 
# Load the xtable library
library(xtable)
# Create a subset of the first few rows of the dataframe
df_Bel_head <- head(df_Bel)
# Convert the dataframe into a LaTeX table
latex_table <- xtable(df_Bel_head)
digits(latex_table) <- c(0, rep(0, ncol(df_Bel_head)))
# Print the LaTeX table code
print(latex_table, include.rownames = FALSE)

df_Bel_V1 <- df_Bel %>% mutate(mux = Dx / ExC, logx = log(Age), logmux = log(mux))
ggplot(data = df_Bel_V1) +
  geom_point(mapping = aes(x = Age, y = mux)) +
  labs(
    x = "x", 
    y = expression(mu[x]) # LaTeX-style y-axis label
  ) +
  theme_minimal() + # Apply minimal theme
  theme(
    axis.title = element_text(size = 32), # Increase axis label font size
    axis.text = element_text(size = 28)  # Increase tick labels font size
  )
ggsave(filename = "figures/mux_raw.pdf")

ggplot(data = df_Bel_V1) + geom_point(mapping = aes(x = logx, y = logmux)) +
  labs(
    x = "log(x)", 
    y = expression(log(mu[x]) # LaTeX-style y-axis label
  )) +
  theme_minimal() + # Apply minimal theme
  theme(
    axis.title = element_text(size = 32), # Increase axis label font size
    axis.text = element_text(size = 28)  # Increase tick labels font size
  )
ggsave(filename = "figures/log_mux_raw.pdf")

df_Bel_V1 <- df_Bel %>% mutate(mux = Dx / ExC, logx = log(Age), logmux = log(mux))
res <- lm(logmux ~ logx, data = df_Bel_V1)



inter <- as.numeric(res$coefficients[1]); slope <- as.numeric(res$coefficients[2])
alpha <- slope + 1; beta <- exp((log(alpha) - inter) / alpha)

df_Bel_smooth <- df_Bel_V1 %>% mutate( mux_smooth = ( alpha / beta) * (Age / beta)^(alpha - 1))
  

ggplot(data = df_Bel_smooth) + geom_line(mapping = aes(x = Age, y = mux_smooth)) + 
  geom_point(mapping = aes(x = Age, y = mux)) +
  labs(
    x = "x", 
    y = expression(mu[x]) # LaTeX-style y-axis label
  ) +
  theme_minimal() + # Apply minimal theme
  theme(
    axis.title = element_text(size = 32), # Increase axis label font size
    axis.text = element_text(size = 28)  # Increase tick labels font size
  )

ggsave(filename = "figures/mux_raw_VS_smoothed.pdf")

df_Bel_smooth <- df_Bel_V1 %>% mutate( mux_smooth = ( alpha / beta) * (Age / beta)^(alpha - 1))

res_binom_test <- binom.test(sum(df_Bel_smooth$mux > df_Bel_smooth$mux_smooth), 
                             nrow(df_Bel_smooth), p=0.5) 

# Exo 3 Survie de patients atteint d'un mélanome au Danemark
library(tidyverse)
library(finalfit)
melanoma <- boot::melanoma #F1 here for help page with data dictionary

latex_table <- xtable(head(melanoma))
digits(latex_table) <- c(0, c(0,0,0,0,0,2,0))
# Print the LaTeX table code
print(latex_table, include.rownames = FALSE)
melanoma <- melanoma %>%
  mutate(status_os = if_else(status == 2, 0, 1))

library(survival)
library(survminer)
#  Prt a)
fit <- survfit(Surv(time, status_os) ~ ulcer, data = melanoma)
ggsurvplot(
  fit, pval = FALSE, conf.int = TRUE, linetype = "strata", ggtheme = theme_minimal(), 
  font.main = c(32, "bold"), font.x = c(28), font.y = c(28), 
  font.tickslab = c(28),font.legend = c(28), font.text = c(28, "bold"), font.caption = c(28, "bold")
  )
ggsave(filename = "figures/KM_plot.pdf")

fit <- survfit(Surv(time, status_os) ~ ulcer, data = melanoma)
ggsurvplot(fit, pval = FALSE, conf.int = TRUE, linetype = "strata", ggtheme = theme_minimal())
library(xtable)

# Part b) 
surv_diff <- survdiff(Surv(time, status_os) ~ ulcer, data = melanoma)
surv_diff

# Part c)
res.cox <- coxph(Surv(time, status_os) ~ ulcer + year + age + sex + thickness, data = melanoma)
res.cox
summary(res.cox)



