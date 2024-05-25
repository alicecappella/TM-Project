setwd("C:/Users/alice/OneDrive - Università degli Studi di Padova/Laurea Magistrale/Secondo Anno/Temi e Metodi di Popolazione e Società/Lavoro di Gruppo/Misture")
data = read.csv("insurance.csv")
str(data)

#Trasformiamo i char in fattori
library(dplyr)
data = data %>%
  mutate_if(is.character,as.factor) 

#Font
library(showtext)
font_path = "C:/Users/alice/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf"
font_add("CMUSerif",font_path)
showtext_auto()

#Analisi esplorativa  ----------------------------------------------------
#Grafico di charges all'aumentare dell'età
library(ggplot2)
library(paletteer)
ggplot(data,aes(x = age,y = charges)) + 
  stat_summary(fun = mean,geom = "line",
               col = paletteer_c("ggthemes::Classic Blue",6)[4],size = 1) +
  stat_summary(fun.data = mean_cl_normal,geom = "ribbon",
               fill = paletteer_c("ggthemes::Classic Blue",6)[2],alpha = 0.5) +
  labs(x = "Età",y = "Spese mediche") +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

#Visualizziamo la distribuzione della variabile charges
ggplot(data,aes(x = charges)) + 
  geom_histogram(aes(y = ..density..),bins = 30,
                 fill = paletteer_c("ggthemes::Classic Blue",6)[4],col = "white") + 
  geom_density(col = paletteer_c("ggthemes::Classic Blue",6)[2],
               fill = paletteer_c("ggthemes::Classic Blue",6)[2],
               alpha = 0.3,size = 0.8) +
  theme_minimal()

#Boxplot di charges divisa per smoker
ggplot(data,aes(y = charges,x = smoker)) + 
  geom_boxplot(fill = paletteer_c("ggthemes::Classic Blue",6)[4],
               col = paletteer_c("ggthemes::Classic Blue",6)[2]) +
  labs(x = "Sesso",y = "Spese mediche") +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

#Heatmap correlazione con le variabili quantitative in data
library(reshape2)
data_numeric = data[,sapply(data,is.numeric)]
corr = cor(data_numeric)
cor_melted = melt(corr)

#Heatmap delle correlazioni
cor_melted %>%
  ggplot(aes(Var1,Var2,fill = value)) +
  geom_tile() +
  scale_fill_paletteer_c(`"ggthemes::Classic Blue"`) +
  geom_text(aes(label = round(value,1)),family = "CMUSerif",size = 10) +
  theme_minimal() +
  labs(fill = "Correlazione") +
  xlab("") + 
  ylab("") +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        text = element_text(family = "CMUSerif"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm")) 

#Trasformazione variabili in fattori
data2 = data
#Riclassifichiamo in classi l'età 18-24, 25-34, 35-49, 50-64
data2$age = cut(data2$age,
                breaks = c(18,25,35,50,65),
                labels = c("18-24","25-34","35-49","50-64"),
                include.lowest = T,right = F)

#Riclassifichiamo in classi BMI (sottopeso, normopeso, sovrappeso, obeso, estremamente obeso)
data2$bmi = cut(data2$bmi,
                breaks = c(0,18.5,24.9,29.9,34.9,100),
                labels = c("Sottopeso","Normopeso","Sovrappeso","Obeso","Estremamente obeso"),
                right = F)
table(data2$bmi)

#Raggruppiamo le modalità 3, 4 e 5 di children in 3+
data2$children = cut(data2$children,
                     breaks = c(0,1,2,3,6),
                     labels = c("0","1","2","3+"),
                     include.lowest = T,right = F)
table(data2$children)

#Indice eta^2 di associazione tra variabili categoriali e charges
eta2 = function(x,y) {
  m = mean(x,na.rm = TRUE)
  sct = sum((x - m)^2,na.rm = TRUE)
  n = table(y)
  mk = tapply(x,y,mean,na.rm = TRUE)
  sce = sum(n * (mk - m)^2)
  return(ifelse(sct > 0,sce / sct,0))
}

var_qualitative = names(data2)[sapply(data2,is.factor)]

eta2_results = sapply(var_qualitative,function(var) {
  eta2(data2$charges,data2[[var]])
})
eta2_df = data.frame(variabile = names(eta2_results),eta2 = eta2_results)
eta2_df

library(latex2exp)
eta2_df %>%
  arrange(desc(eta2)) %>% 
  ggplot(aes(x = reorder(variabile,eta2),y = eta2,fill = eta2)) +
  geom_bar(stat = "identity") +
  scale_fill_paletteer_c(`"ggthemes::Classic Blue"`) +
  labs(x = "",y = TeX(sprintf("$\\eta^2$")),fill = TeX(sprintf("$\\eta^2$"))) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

#Regressione lineare -----------------------------------------------------
lm0 = lm(log(charges) ~ .,data = data)
summary(lm0)
#Tutte le variabili risultano significative

# Model-based clustering - mclust -----------------------------------------
library(mclust)
set.seed(123)
mbc = Mclust(data$charges)
summary(mbc)

mbc[["BIC"]]

AIC = c()
for(i in 1:5){
  AIC[i] = AIC(Mclust(data$charges,verbose = F,modelNames = "V",G = i))
}

#Grafico BIC (selezione del modello)
library(factoextra)
fviz_mclust_bic(mbc,legend = "right",shape = "model",size = 1,
                palette = paletteer_c("ggthemes::Classic Blue",6)[c(2,4)]) +
  labs(x = "Numero di componenti") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.5,'cm'),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        title = element_blank(),
        text = element_text(family = "CMUSerif"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA))

#Grafico distribuzione osservata e stimata
df = data.frame(charges = data$charges,
                density = dnorm(data$charges,mean = mbc[["parameters"]][["mean"]][["1"]],
                                sd = sqrt(mbc[["parameters"]][["variance"]]$sigmasq[1]))*mbc[["parameters"]][["pro"]][1] +
                  dnorm(data$charges,mean = mbc[["parameters"]][["mean"]][["2"]],
                       sd = sqrt(mbc[["parameters"]][["variance"]]$sigmasq[2]))*mbc[["parameters"]][["pro"]][2] +
                  dnorm(data$charges,mean = mbc[["parameters"]][["mean"]][["3"]],
                       sd = sqrt(mbc[["parameters"]][["variance"]]$sigmasq[3]))*mbc[["parameters"]][["pro"]][3] +
                  dnorm(data$charges,mean = mbc[["parameters"]][["mean"]][["4"]],
                       sd = sqrt(mbc[["parameters"]][["variance"]]$sigmasq[4]))*mbc[["parameters"]][["pro"]][4] +
                  dnorm(data$charges,mean = mbc[["parameters"]][["mean"]][["5"]],
                       sd = sqrt(mbc[["parameters"]][["variance"]]$sigmasq[5]))*mbc[["parameters"]][["pro"]][5])

ggplot(df,aes(x = charges,y = density)) + 
  geom_histogram(aes(y = ..density..),col = "white",bins = 30,
                 fill = paletteer_c("ggthemes::Classic Blue",6)[4]) +
  geom_line(col = paletteer_c("ggthemes::Classic Blue",6)[2],size = 0.8) +
  geom_area(fill = paletteer_c("ggthemes::Classic Blue",6)[2],alpha = 0.3) +
  labs(x = "Spese mediche",y = "Densità") +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

#Grafici charges contro età, bmi e numero di figli divisi per cluster
g1 = ggplot(data,aes(x = age,y = charges,col = factor(mbc[["classification"]]))) + 
  geom_point(size = 2) +
  labs(x = "Età",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Red-Blue",5),
                     label = paste("Cluster ",seq(1,5,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

g2 = ggplot(data,aes(x = bmi,y = charges,col = factor(mbc[["classification"]]))) +
  geom_point(size = 2) +
  labs(x = "BMI",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Red-Blue",5),
                     label = paste("Cluster ",seq(1,5,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

g3 = ggplot(data,aes(x = children,y = charges,col = factor(mbc[["classification"]]))) +
  geom_point(size = 2) +
  labs(x = "Numero di figli",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Red-Blue",5),
                     label = paste("Cluster ",seq(1,5,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

library(ggpubr)
ggarrange(g1,g2,g3,nrow = 3,common.legend = T,legend = "bottom")

#Primo e secondo cluster solo non fumatori (omettiamo la variabile nei modelli)
data$latent_class = mbc[["classification"]]
table(data$smoker,data$latent_class)

#Significatività delle variabili nei cluster -----------------------------
lm1 = lm(charges ~ age + sex + bmi + children + region,
        data = data %>%
          filter(latent_class == 1))
summary(lm1)

lm2 = lm(charges ~ age + sex + bmi + children + region,
         data = data %>%
           filter(latent_class == 2))
summary(lm2)

lm3 = lm(charges ~ age + sex + bmi + children + smoker + region,
         data = data %>%
           filter(latent_class == 3))
summary(lm3)

lm4 = lm(charges ~ age + sex + bmi + children + smoker + region,
         data = data %>%
           filter(latent_class == 4))
summary(lm4)

lm5 = lm(charges ~ age + sex + bmi + children + smoker + region,
         data = data %>%
           filter(latent_class == 5))
summary(lm5)

#Rappresentazione grafica significatività

#Lista in cui ogni elemento è coef.data contenete 
# - i coefficienti;
# - l'intervallo di confidenza;
# - la significatività 
#per i modelli da lm1 a lm5
coef.data_list = lapply(1:5,function(i){
  coef.data = data.frame(var = names(coef(get(paste0("lm",i)))[-1]),
                         coef = as.vector(coef(get(paste0("lm",i)))[-1]),
                         min_ci = as.vector(confint(get(paste0("lm",i)),level = 0.95)[-1,1]),
                         max_ci = as.vector(confint(get(paste0("lm",i)),level = 0.95)[-1,2]),
                         sign = as.vector(summary(get(paste0("lm",i)))$coefficients[-1,4]))
  coef.data$sign = round(coef.data$sign,4)
  return(coef.data)
})

#Modifichiamo il nome di alcune variabili
coef.data_list = lapply(coef.data_list,function(coef.data){
  coef.data$var = gsub("regionnorthwest","region [northwest]",coef.data$var)
  coef.data$var = gsub("regionsouthwest","region [southwest]",coef.data$var)
  coef.data$var = gsub("regionnortheast","region [northeast]",coef.data$var)
  coef.data$var = gsub("regionsoutheast","region [southeast]",coef.data$var)
  coef.data$var = gsub("sexmale","sex [male]",coef.data$var)
  coef.data$var = gsub("smokeryes","smoker [yes]",coef.data$var)
  return(coef.data)
})

plot_list = lapply(1:5,function(i) {
  coef.data = coef.data_list[[i]]
  if(i %in% 1:4){
    coef.data %>%
      ggplot(aes(y = fct_inorder(var),x = coef,xmin = min_ci,xmax = max_ci)) +
      geom_linerange() +
      geom_pointrange(size = 0.6,col = ifelse(coef.data$coef < 0,"#9C0824FF","#26456EFF"),
                      pch = ifelse(coef.data$sign < 0.01,16,8)) +
      geom_vline(xintercept = 0,col = "#9C0824FF",lty = "dashed") +
      labs(x = "Stima",y = "Variabile",title = paste("Cluster",i,sep = " ")) +
      theme_minimal() +
      theme(text = element_text(family = "CMUSerif"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 15))
  }
  else{
    coef.data %>%
      ggplot(aes(y = fct_inorder(var),x = coef,xmin = min_ci,xmax = max_ci,
                 shape = factor(sign < 0.01))) +
      geom_linerange() +
      geom_pointrange(size = 0.6,col = ifelse(coef.data$coef < 0,"#9C0824FF","#26456EFF")) +
      scale_shape_manual(values = ifelse(coef.data$sign < 0.01,16,8),
                         label = ifelse(coef.data$sign < 0.01,"Significativo","Non significativo")) +
      geom_vline(xintercept = 0,col = "#9C0824FF",lty = "dashed") +
      labs(x = "Stima",y = "Variabile",title = paste("Cluster",i,sep = " "),shape = "") +
      theme_minimal() +
      theme(text = element_text(family = "CMUSerif"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 15))
  }
})

ggarrange(plotlist = plot_list,ncol = 2,nrow = 3,
          common.legend = T,legend = "bottom")

#Caratteristiche dei cluster ---------------------------------------------
table(data2$age,data2$latent_class)
table(data2$bmi,data2$latent_class)
table(data2$children,data2$latent_class)
table(data2$smoker,data2$latent_class)
table(data2$region,data2$latent_class)
table(data2$sex,data2$latent_class)

#Approfondimento ---------------------------------------------------------
#Selezioniamo il modello con l'ICL
set.seed(123)
icl = mclustICL(data$charges)

icl.df = data.frame(G = rep(1:9,2),
                    ICL = c(icl[,"E"],icl[,"V"]),
                    modelNames = c(rep("E",9),
                                   rep("V",9)))

ggplot(icl.df,aes(x = G,y = ICL,col = modelNames)) +
  geom_line(aes(group = modelNames),size = 1) +
  geom_point(aes(shape = modelNames),size = 3) +
  geom_vline(xintercept = 5,col = 2,lty = 2,size = 1) +
  scale_shape_manual(values = c(19,17),
                     labels = c("E","V")) +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Blue",6)[c(2,4)]) +
  labs(x = "Numero di componenti",y = "ICL",shape = "",col = "") +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.5,"cm"))

#Stima mistura con due componenti e diverse varianze 
set.seed(123)
mbc2 = Mclust(data$charges,G = 2,modelName = "V")
summary(mbc2)

#Grafico distribuzione osservata e stimata
df2 = data.frame(charges = data$charges,
                 density = 
                   dnorm(data$charges,mean = mbc2[["parameters"]][["mean"]][["1"]],
                         sd = sqrt(mbc2[["parameters"]][["variance"]]$sigmasq[1]))*mbc2[["parameters"]][["pro"]][1] +
                   dnorm(data$charges,mean = mbc2[["parameters"]][["mean"]][["2"]],
                         sd = sqrt(mbc2[["parameters"]][["variance"]]$sigmasq[2]))*mbc2[["parameters"]][["pro"]][2])

ggplot(df2,aes(x = charges,y = density)) + 
  geom_histogram(aes(y = ..density..),col = "white",bins = 30,
                 fill = paletteer_c("ggthemes::Classic Blue",6)[4]) +
  geom_line(col = paletteer_c("ggthemes::Classic Blue",6)[2],size = 0.8) +
  geom_area(fill = paletteer_c("ggthemes::Classic Blue",6)[2],alpha = 0.3) +
  labs(x = "Spese mediche",y = "Densità") +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

#Grafici charges contro età, bmi e numero di figli divisi per cluster
g1 = ggplot(data,aes(x = age,y = charges,col = factor(mbc2[["classification"]]))) + 
  geom_point(size = 2) +
  labs(x = "Età",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Blue",6)[c(2,4)],
                     label = paste("Cluster ",seq(1,2,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

g2 = ggplot(data,aes(x = bmi,y = charges,col = factor(mbc2[["classification"]]))) +
  geom_point(size = 2) +
  labs(x = "BMI",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Blue",6)[c(2,4)],
                     label = paste("Cluster ",seq(1,2,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

g3 = ggplot(data,aes(x = children,y = charges,col = factor(mbc2[["classification"]]))) +
  geom_point(size = 2) +
  labs(x = "Numero di figli",y = "Spese mediche") +
  scale_color_manual(values = paletteer_c("ggthemes::Classic Blue",6)[c(2,4)],
                     label = paste("Cluster ",seq(1,2,1),sep = "")) +
  labs(col = " ") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() + 
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1,"cm"))

ggarrange(g1,g2,g3,nrow = 3,common.legend = T,legend = "bottom")

#Stimiamo i modelli nei cluster 
lm1.2 = lm(charges ~ .,data = data %>%
             filter(mbc2[["classification"]] == 1))
summary(lm1.2)

lm2.2 = lm(charges ~ .,data = data %>%
             filter(mbc2[["classification"]] == 2))
summary(lm2.2)

data2$latent_class2 = mbc2[["classification"]]

#Caratteristiche dei cluster
table(data2$age,data2$latent_class2)
table(data2$bmi,data2$latent_class2)
table(data2$children,data2$latent_class2)
table(data2$smoker,data2$latent_class2)
table(data2$region,data2$latent_class2)
table(data2$sex,data2$latent_class2)