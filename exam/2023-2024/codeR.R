# Code R exam MDD
# Exo 2
library(rstudioapi)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

Death_Iceland <- read_table("~/Dropbox/Enseignements/UNISTRA/MDD/exam/2023-2024/Deaths_1x1.txt", skip = 2)
Pop_Iceland <- read_table("~/Dropbox/Enseignements/UNISTRA/MDD/exam/2023-2024/Population.txt", skip = 2)

head(Death_Iceland)
head(Pop_Iceland)
# Année 2010  
pop2010 <- Pop_Iceland %>% mutate(Age  = as.numeric(Age)) %>% filter(Year == 2010, Age <= 100)
D2010 <- Death_Iceland %>% mutate(Age  = as.numeric(Age)) %>% filter(Year == 2010, Age <= 100)

df_Ice_2010 = rbind(
  data.frame(x = 0:100, 
             qx = D2010$Female / pop2010$Female, 
             gender = rep("F", 101), 
             Ex0 = pop2010$Female, 
             Dx = D2010$Female),
  data.frame(x = 0:100, 
             qx = D2010$Male / pop2010$Male, 
             gender = "H", 
             Ex0 = pop2010$Male, 
             Dx = D2010$Male), 
  data.frame(x = 0:100,
             qx = D2010$Total / pop2010$Total, 
             gender = "All", Ex0 = pop2010$Total, Dx = D2010$Total))


df_Ice_2010_V1 <- df_Ice_2010%>% mutate(
  qx_low =pmax( qx - qnorm(0.975) * sqrt( qx * (1-qx) / Ex0), 0),
  qx_up = pmin(qx + qnorm(0.975) * sqrt( qx * (1-qx) / Ex0), 1))

(qx_plot <- ggplot(data = df_Ice_2010_V1 %>% filter(gender == "All")) + geom_line(mapping = aes(x = x, y = qx)) + 
  geom_line(mapping = aes(x = x, y = qx_low), linetype ="dashed") + 
  geom_line(mapping = aes(x = x, y = qx_up), linetype ="dashed") + 
              theme_minimal() + theme(text = element_text(size = 20)))
ggsave("figures/qx_all.pdf")


df_Ice_2010_V2 <- df_Ice_2010_V1 %>% filter(qx > 0) %>% mutate(logit_qx = log(qx/(1-qx)))

ggplot(data = df_Ice_2010_V2 %>% filter(gender == "All" )) + geom_line(mapping = aes(x = x, y = logit_qx)) +
  theme_minimal() + theme(text = element_text(size = 20))

ggsave("figures/logit_qx_all.pdf")

res_lm <- lm(logit_qx ~ x, data = df_Ice_2010_V2 %>% filter(gender == "All" ))
summary(res_lm)

x = 0:100
eta <- res_lm$coefficients[1] + res_lm$coefficients[2] * x
qx_smooth <- exp(eta) / (1+exp(eta)) 
df_Ice_2010_All <- df_Ice_2010_V1 %>% filter(gender == "All")
df_Ice_2010_All["qx_smooth"] <- qx_smooth
ggplot(data = df_Ice_2010_All) + geom_line(mapping = aes(x = x, y = qx)) + 
  geom_line(mapping = aes(x = x, y = qx_smooth), linetype  = "dotted") + theme_minimal()+ theme(text = element_text(size = 20))       

ggsave("figures/qx_smoothed.pdf")

s <- df_Ice_2010_All$qx >0
res <- binom.test(sum(df_Ice_2010_All$qx[s] > df_Ice_2010_All$qx_smooth[s]), length(s), p = 0.5)
res

# Mortalité H/F
(qx_plot <- ggplot(data = df_Ice_2010_V1 %>% filter(gender != "All")) + 
    geom_line(mapping = aes(x = x, y = qx, linetype = gender)) +  
    theme_minimal() + theme(text = element_text(size = 20)))
ggsave("figures/qx_HF.pdf")

# Exo 3 Etude du maintien en incapacité en fonction de l'âge et du sexe
set.seed(1234)
age <- as.integer(pmin(pmax(rnorm(100, mean = 45,sd= 20), 25), 60))
gender <- rbinom(100, size = 1, p = 0.4)
status <- rbinom(100, size = 1, p = 0.9)
beta_0 <- -2  # Example value
beta_1 <- 0.05  # Linear effect of age
beta_2 <- 0  # Quadratic effect of age
rates <- exp(beta_0 + beta_1 * age + beta_2 * age^2)
df_incap <- data.frame(time = sapply(rates, function(x) 
  rexp(1, rate = x)),
          status = as.integer(status), 
          age = age, 
          gender = gender)
df_incap$gender <- as.factor(df_incap$gender)


library(survival)
library(survminer)
library(xtable)

xtable(head(df_incap))
help(xtable)
fit <- survfit(Surv(time, status) ~  gender, data = df_incap)
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           ggtheme = theme_bw(), 
           linetype = "strata"# Change ggplot2 theme
)

surv_diff <- survdiff(Surv(time, status) ~ gender, data = df_incap)
surv_diff


res.cox <- coxph(Surv(time, status) ~ age + gender, data = df_incap)
res.cox
summary(res.cox)
test.ph <- cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
