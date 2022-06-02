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

# Variaveis instrumentais no R I
# Prof. Raphael B. Corbi/ Monitor Alan Leal

# Pacotes utilizados neste código:
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(AER)){install.packages("AER");require(AER)}
if(!require(stargazer)){install.packages("stargazer");require(stargazer)}

# Lendo a base agora:
# Lendo a base:
card <- haven::read_stata("https://raw.github.com/scunning1975/mixtape/master/card.dta") %>%
  filter(if_all(c(lwage,educ,exper,black,south,married,smsa,nearc4),~!is.na(.)))

# Iremos criar agora as tres matrizes de interesse, X, Z e y:
y <- card$lwage
X <- cbind(1,card$educ,card$exper,card$black,card$south,card$married,card$smsa)
Z <- cbind(1,card$nearc4,card$exper,card$black,card$south,card$married,card$smsa)

# Vamos agora as estimativas de mqo e iv:
beta_ols <- solve(t(X)%*%X)%*%t(X)%*%y
beta_iv <- solve(t(Z)%*%X)%*%t(Z)%*%y
cbind(beta_ols,beta_iv)

# Faremos esse mesmo procedimento por dois estágios:
frst_stg <- lm(educ~nearc4+exper+black+south+married+smsa,data=card)
summary(frst_stg)
card$educ_hat <- predict(frst_stg)
# Segundo estágio:
scnd_stg <- lm(lwage~educ_hat+exper+black+south+married+smsa,data=card)
summary(scnd_stg)
# usando o comando próprio do R:
ivreg_card <- ivreg(lwage~educ+exper+black+south+married+smsa|nearc4+exper+black+south+married+smsa,data=card)
summary(ivreg_card)


# Vamos comparar o resultado do ivreg com o resultado de dois estágios:
stargazer(scnd_stg,ivreg_card,type="text")

# Usando o "diagnostics" do ivreg:
summary(ivreg_card,diagnostics = TRUE)

# Roteiro da monitoria prática 27-05
# Prof. Raphael B. Corbi/ Monitor: Alan Leal

# Lendo os pacotes necessários e lendo a base de dados:
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(AER)){install.packages("AER");require(AER)}
if(!require(plm)){install.packages("plm");require(plm)}
if(!require(stargazer)){install.packages("stargazer");require(stargazer)}
if(!require(estimatr)){install.packages("estimatr");require(estimatr)}


# Primeira Parte: Replicação de parte do Angrist e Kruger (1991), colunas 3,4,5 e 6 da Tabela 4:
angrist_krueger <- read_csv("/Users/alanleal/Library/Mobile Documents/com~apple~CloudDocs/PAE/2022I/Apresentações/Variáveis Instrumentais/angrist_krueger_1991.csv")
# Vamos criar algumas variáveis presentes na estimação de Angrist e Krueger (1991):
angrist_krueger <- angrist_krueger %>%
  mutate(COHORT=case_when(YOB<=39 & YOB>=30 ~30.39,#criando a dummy de coorte
                          YOB<=49 & YOB>=40 ~40.49,
                          TRUE~ 20.29)) %>%
  mutate(AGEQSQ=AGEQ^2) %>% #criando a variável de idade ao quadrado
  mutate(YR20=(YOB %in% c(1920,1930,1940)), #criando dummies do ano de nascimento do indivíduo
         YR21=(YOB %in% c(1921,1931,1941)),
         YR22=(YOB %in% c(1922,1932,1942)),
         YR23=(YOB %in% c(1923,1933,1943)),
         YR24=(YOB %in% c(1924,1934,1944)),
         YR25=(YOB %in% c(1925,1935,1945)),
         YR26=(YOB %in% c(1926,1936,1946)),
         YR27=(YOB %in% c(1927,1937,1947)),
         YR28=(YOB %in% c(1928,1938,1948)),
         YR29=(YOB %in% c(1929,1939,1949))) %>%
  mutate(QTR1=(QOB==1),#criando dummies do trimestre no qual o indivíduo nasceu
         QTR2=(QOB==2),
         QTR3=(QOB==3),
         QTR4=(QOB==4)) %>%
  mutate(QTR120= QTR1*YR20, #criando interaçoes entre o ano de nascimento do indivíduo e se trimestre de nascimento
         QTR121= QTR1*YR21,
         QTR122= QTR1*YR22,
         QTR123= QTR1*YR23,
         QTR124= QTR1*YR24,
         QTR125= QTR1*YR25,
         QTR126= QTR1*YR26,
         QTR127= QTR1*YR27,
         QTR128= QTR1*YR28,
         QTR129= QTR1*YR29,
         QTR220= QTR2*YR20,
         QTR221= QTR2*YR21,
         QTR222= QTR2*YR22,
         QTR223= QTR2*YR23,
         QTR224= QTR2*YR24,
         QTR225= QTR2*YR25,
         QTR226= QTR2*YR26,
         QTR227= QTR2*YR27,
         QTR228= QTR2*YR28,
         QTR229= QTR2*YR29,
         QTR320= QTR3*YR20,
         QTR321= QTR3*YR21,
         QTR322= QTR3*YR22,
         QTR323= QTR3*YR23,
         QTR324= QTR3*YR24,
         QTR325= QTR3*YR25,
         QTR326= QTR3*YR26,
         QTR327= QTR3*YR27,
         QTR328= QTR3*YR28,
         QTR329= QTR3*YR29) %>%
  filter(COHORT<20.30) #filtrando para aqueles que nasceram na década de 1920

# Agora, podemos rodar as regressões que desejamos implementar:
# Primeiramente, rodemos a coluna 3:
col_3 <- angrist_krueger %>%
  select(LWKLYWGE,EDUC,starts_with("YR"),-YR29,AGEQ,AGEQSQ) %>%
  lm(LWKLYWGE~.,data=.)

# Agora, rodemos a coluna 4:
col_4 <- ivreg(LWKLYWGE~YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28+
                 AGEQ+AGEQSQ+EDUC|YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28+
                 AGEQ+AGEQSQ+QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127+QTR128+QTR129+
                 QTR220+QTR221+QTR222+QTR223+QTR224+QTR225+QTR226+QTR227+QTR228+QTR229+
                 QTR320+QTR321+QTR322+QTR323+QTR324+QTR325+QTR326+QTR327+QTR328+QTR329,data=angrist_krueger)

col_5 <- angrist_krueger %>%
  select(LWKLYWGE,EDUC,RACE,MARRIED,SMSA,NEWENG,MIDATL,ENOCENT,WNOCENT,SOATL,ESOCENT,WSOCENT,MT,starts_with("YR"),-YR29) %>%
  lm(LWKLYWGE~.,data=.)
col_6 <- ivreg(LWKLYWGE~YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28+
                 EDUC+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+
                 ESOCENT+WSOCENT+MT|YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28+
                 AGEQ+AGEQSQ+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+
                 ESOCENT+WSOCENT+MT+QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127+QTR128+QTR129)
                 
                 
# Roteiro da aula de painel
# Prof. Raphael B. Conbi/ Monitor: Alan Leal

# Lendo os pacotes necessários:
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(estimatr)){install.packages("estimatr");require(estimatr)}
if(!require(plm)){install.packages("plm");require(plm)}
if(!require(stargazer)){install.packages("stargazer");require(stargazer)}

# Agora, lendo a base de dados usada aqui:
dairy <- read_csv("https://pages.stern.nyu.edu/~wgreene/Econometrics/dairy.csv") %>% arrange(FARM,YEAR)

# Parte I) MQO empilhado, efeitos fixos e demean.
# X1- log de qtd de vacas
# X2 - log de área de terra
# X3 - log do trabalho
# X4 - log da alimentação
mqo_a <- lm(MILK~X1+X2+X3+X4,data=dairy)
ef_a <- lm_robust(MILK~X1+X2+X3+X4,# usando estimatr
                  data = dairy,
                  fixed_effect = ~FARM,
                  se_type = "stata")
ef_b <- plm(MILK~X1+X2+X3+X4,# usando plm
            data=dairy,
            effect = "individual",
            model="within",
            index = c("FARM","YEAR"))
# Por fim, fazendo o MQO retirado de suas médias:
dairy_demean <- dairy %>%
  mutate(MILK=MILK-ave(MILK,FARM),
         X1=X1-ave(X1,FARM),
         X2=X2-ave(X2,FARM),
         X3=X3-ave(X3,FARM),
         X4=X4-ave(X4,FARM))
mqo_demean <- lm(MILK~-1+X1+X2+X3+X4,data=dairy_demean)
# O Cunningham não corrige os erros padrão dessa regressão retirada da média. Isso é errado!
# Por isso, é melhor trabalhar com a regressão de efeitos fixos programada no programa.
# Agora, vamos comparar todas essas estimações:
stargazer(mqo_a,ef_b,mqo_demean,type="text")

# Parte II) MQO de variáveis dummies equivale ao modelo de efeitos fixos:
mqo_dummies <- lm(MILK~X1+X2+X3+X4+as.factor(FARM),data=dairy)
stargazer(ef_b,mqo_dummies,type="text")

# Parte III) Primeiras diferenças para dois períodos equivale aos efeitos fixos para dois períodos (para 1997 e 1998)
dairy_fd <- dairy %>%
  filter(YEAR>=97) %>% group_by(FARM) %>%
  mutate(MILK=c(NA,diff(MILK)),
         X1=c(NA,diff(X1)),
         X2=c(NA,diff(X2)),
         X3=c(NA,diff(X3)),
         X4=c(NA,diff(X4))) %>% na.omit()

frst_dif <- lm(MILK~-1+X1+X2+X3+X4,data=dairy_fd)
# Agora, calculando o modelo de efeitos fixos apenas para 1997 e 1998:
dairy_9798 <- dairy %>% filter(YEAR>=97)
ef_b_1 <- plm(MILK~-1+X1+X2+X3+X4,# usando plm
              data=dairy_9798,
              effect = "individual",
              model="within",
              index = c("FARM","YEAR"))
stargazer(ef_b_1,frst_dif,type="text")                 