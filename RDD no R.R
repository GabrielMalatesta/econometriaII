#### RDD no R
# Prof.: Raphael Corbi / Monitor: Alan Leal

# Replicação de parte do artigo de Carpenter e Dobkin (2009)
# The Effect of Alcohol Consumption on Mortality: Regression Discontinuity Evidence from the Minimum Drinking Age

# Lendo os pacotes necessários:
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)} # ggplot2 basicamente
if(!require(stevedata)){install.packages("stevedata");require(stevedata)} # banco de dados
if(!require(estimatr)){install.packages("estimatr");require(estimatr)} # estimar o RDD
if(!require(rddtools)){install.packages("rddtools");require(rddtools)} # comparar com o próprio resultado do R

# Lendo a base de dados
data(mm_mlda)

# Vamos agora fazer um gráfico da taxa de mortalidade total em relação à idade:
p <- mm_mlda %>% filter(!is.na(all)) %>%
  select(agecell, all) %>%
  mutate(threshold = as.factor(ifelse(agecell >= 21, 1, 0))) %>%
  ggplot(aes(x = agecell, y = all)) +
  geom_point(aes(color = threshold),show.legend = FALSE) +
  geom_vline(xintercept = 21, color = "green",
             size = 1, linetype = "dashed") +
  labs(y = "Taxa de mortalidade (por 100.000)",
       x = "Idade (binned)")+theme_minimal()


# Primeiro Modelo: Mesmo intercepto:
mm_mlda %>%
  mutate(threshold=ifelse(agecell>=21,1,0)) %>%
  lm(log(all)~threshold+I(agecell-21),data=.)
# É necessário clusterizar os erros-padrão na running variable:
# Usaremos a função lm_robust para fazer isso.
mm_mlda %>%
  mutate(threshold=ifelse(agecell>=21,1,0)) %>%
  lm_robust(log(all)~threshold+I(agecell-21),data=.,clusters = agecell)

# Usando o rddtools:
rdd_data(y = log(mm_mlda$all),
         x = mm_mlda$agecell,
         cutpoint = 21) %>%
  rdd_reg_lm(slope = "same") %>%
  summary()


# 2) Flexibilizando agora a forma funcional da nossa regressão:
lm_quadratic <- mm_mlda %>%
  mutate(threshold = ifelse(agecell >= 21, 1, 0)) %>%
  lm_robust(log(all) ~ threshold + I(agecell - 21) + I((agecell -21)^2) + threshold:I(agecell - 21) +
              threshold:I((agecell - 21)^2),data=.,clusters=agecell)
summary(lm_quadratic)

mm_mlda %>% filter(!is.na(all)) %>%
  select(agecell, all) %>%
  mutate(threshold = as.factor(ifelse(agecell >= 21, 1, 0))) %>%
  ggplot(aes(x = agecell, y = all)) +
  geom_point(aes(color = threshold),show.legend = FALSE) +
  geom_smooth(aes(color=threshold),method = "lm",formula = y~poly(x,2,raw=TRUE), se = FALSE,show.legend=FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Taxa de mortalidade (por 100.00)")+theme_minimal()

# último gráfico:
p <- mm_mlda %>% filter(!is.na(all)) %>% mutate(threshold = ifelse(agecell >= 21, 1, 0)) %>%
  select(agecell,threshold,alcohol,suicide,homicide,drugs,mva) %>%
  pivot_longer(!c(agecell,threshold),names_to="cause_of_death",values_to="death_rate") %>%
  mutate(cause_thresh=as.factor(paste0(threshold,cause_of_death))) %>%
  ggplot(data=.) +
  geom_smooth(aes(x=agecell,y=death_rate,color=cause_thresh), method="lm",se=FALSE,show.legend = FALSE)+
  geom_point(aes(x=agecell,y=death_rate,shape=cause_of_death))+
  geom_vline(xintercept=21,color="red",linetype="dashed")+
  theme_minimal()+
  labs(x="Idade (binned)",y="Taxa de mortalidade por 100000",shape="Causa da Morte")
p