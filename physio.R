#Planche 14, en premier lieu c’est la difference en fonction de la profondeur que tu veux voir, donc le graphe du bas…et si tu veux regarder les différences entre espèces, moi je pense que je ferais une analyse multivariées avec tout tes paramètres physio + croissance pr voir si tes espèces se repartissent selon un gradient physiologique avec la profondeur
#Planche 16. Pas pertinent, comme la 14? Car la question est la profondeur
#Planche 18 Acuta c’est marrant il a plus de Chloé C a 30 m et moins de a, interessant…..a creuser je pense
#Planche 20. Pas nécessaire car pas de distinction profondeur 
#Planche 21. Pas de différence intéressant, c’est vraiment un ajustement des pigments…
#garder que graph aevc distinction avec la prof



#packages
library(tidyr)
library(plyr)
library(dplyr)
#########################################################################

##données physio des 2 pep, 7 especes (december 2023)
physio<-data_physio_R

shapiro.test(physio$zoox_cm2)
#Pas de normalité

#TEST SUR zoox_cm2
kruskal.test(physio$zoox_cm2,physio$pep)
kruskal.test(physio$zoox_cm2,physio$esp)
kruskal.test(physio$zoox_cm2,physio$age)
kruskal.test(physio$zoox_cm2,physio$genre)
kruskal.test(physio$zoox_cm2,physio$colonie)
#rien de significatif

meanzooxpep<-ddply(physio,c("pep"),summarise,mean=mean(zoox_cm2),sd=sd(zoox_cm2),se=se(zoox_cm2),CI=qnorm(0.975)*sd(zoox_cm2)/sqrt(length(zoox_cm2)))


#TEST SUR content_chl/zoox
kruskal.test(physio$Chl_content,physio$pep)
#Kruskal-Wallis chi-squared = 5.3642, df = 1, p-value = 0.02055
#--> SIGNIFICATIF
kruskal.test(physio$Chl_content,physio$esp)
#Kruskal-Wallis chi-squared = 42.704, df = 6, p-value =1.335e-07
#--> SIGNIFICATIF
kruskal.test(physio$Chl_content,physio$age)
kruskal.test(physio$Chl_content,physio$genre)
#Kruskal-Wallis chi-squared = 42.704, df = 6, p-value = 1.335e-07
#--> SIGNIFICATIF

#TEST SUR ratio a:c
kruskal.test(physio$ratio_ac,physio$pep)
#Kruskal-Wallis chi-squared = 4.0053, df = 1, p-value = 0.04536
#--> SIGNIFICATIF
kruskal.test(physio$ratio_ac,physio$esp)
#Kruskal-Wallis chi-squared = 18.754, df = 6, p-value = 0.004599
#--> SIGNIFICATIF
kruskal.test(physio$ratio_ac,physio$age)
#Kruskal-Wallis chi-squared = 40.399, df = 26, p-value = 0.03563
#--> SIGNIFICATIF
kruskal.test(physio$ratio_ac,physio$genre)
#Kruskal-Wallis chi-squared = 18.754, df = 6, p-value = 0.004599
#--> SIGNIFICATIF



###TEST SUR µg_chla_zoox
kruskal.test(physio$µg_chla_zoox,physio$pep)
#Kruskal-Wallis chi-squared = 16.986, df = 1, p-value = 3.765e-05
#--> SIGNIFICATIF
kruskal.test(physio$µg_chla_zoox,physio$esp)
#Kruskal-Wallis chi-squared = 32.598, df = 6, p-value = 1.253e-05
#--> SIGNIFICATIF
kruskal.test(physio$µg_chla_zoox,physio$age)
#Kruskal-Wallis chi-squared = 39.949, df = 26, p-value = 0.03947
#--> SIGNIFICATIF

###TEST SUR µg_chlc_zoox
kruskal.test(physio$µg_chlc_zoox,physio$pep)
#Kruskal-Wallis chi-squared = 1.2465, df = 1, p-value = 0.2642
#--> non SIGNIFICATIF
kruskal.test(physio$µg_chlc_zoox,physio$esp)
#Kruskal-Wallis chi-squared = 31.467, df = 6, p-value = 2.064e-05
#--> SIGNIFICATIF
kruskal.test(physio$µg_chlc_zoox,physio$age)
#Kruskal-Wallis chi-squared = 39.233, df = 26, p-value = 0.04631
#--> SIGNIFICATIF

###TEST SUR µg_chlc_cm2
kruskal.test(physio$µg_chlc_cm2,physio$pep)
#Kruskal-Wallis chi-squared = 0.25481, df = 1, p-value = 0.6137
#--> non SIGNIFICATIF
kruskal.test(physio$µg_chlc_cm2,physio$esp)
#Kruskal-Wallis chi-squared = 24.364, df = 6, p-value = 0.0004476
#--> SIGNIFICATIF
kruskal.test(physio$µg_chlc_cm2,physio$age)
#Kruskal-Wallis chi-squared = 29.182, df = 26, p-value = 0.3029
#--> non SIGNIFICATIF

###TEST SUR µg_chla_cm2
kruskal.test(physio$µg_chla_cm2,physio$pep)
#Kruskal-Wallis chi-squared = 3.4994, df = 1, p-value = 0.06139
#--> non SIGNIFICATIF
kruskal.test(physio$µg_chla_cm2,physio$esp)
#Kruskal-Wallis chi-squared = 38.433, df = 6, p-value = 9.243e-07
#--> SIGNIFICATIF
kruskal.test(physio$µg_chla_cm2,physio$age)
#Kruskal-Wallis chi-squared = 27.453, df = 26, p-value = 0.3859
#--> non SIGNIFICATIF

###test sur genotype C129
C129<-physio  %>% filter(colonie=="C129")
kruskal.test(C129$µg_chla_cm2,C129$pep)
kruskal.test(C129$µg_chla_cm2,C129$age)

kruskal.test(C129$µg_chla_cm2,C129$pep)
kruskal.test(C129$µg_chla_cm2,C129$age)

kruskal.test(C129$µg_chlc_cm2,C129$pep)
kruskal.test(C129$µg_chlc_cm2,C129$age)

kruskal.test(C129$µg_chlc_zoox,C129$pep)
kruskal.test(C129$µg_chlc_zoox,C129$age)

kruskal.test(C129$µg_chlc_zoox,C129$pep)
kruskal.test(C129$µg_chlc_zoox,C129$age)
#rien de significatif

##############################  illustrations   ##########################
##############################  illustrations   ##########################
##############################  illustrations   ##########################
##############################  illustrations   ##########################

library(ggplot2)

##############################  µg_chla_zoox   ##########################

###graph µg_chla_zoox selon esp##

ggplot(physio, aes(x=esp,y=µg_chla_zoox, fill=factor(esp))) +
  geom_violin()+
  geom_jitter()+
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()

ggplot(physio, aes(x=esp,y=µg_chla_zoox, fill=factor(esp))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()


#selon la pep
ggplot(physio, aes(x=pep,y=µg_chla_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()


#graph de la Chla par zoox par esp dans les 2peps
ggplot(physio, aes(x=esp, y=µg_chla_zoox, colour=pep, fill=pep))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("Espèces")+
  labs(fill="Pépinières")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

meanchlazooxpep<-ddply(physio,c("pep"),summarise,mean=mean(µg_chla_zoox),sd=sd(µg_chla_zoox),se=se(µg_chla_zoox),CI=qnorm(0.975)*sd(µg_chla_zoox)/sqrt(length(µg_chla_zoox)))
meanchlczooxpep<-ddply(physio,c("pep"),summarise,mean=mean(µg_chlc_zoox),sd=sd(µg_chlc_zoox),se=se(µg_chlc_zoox),CI=qnorm(0.975)*sd(µg_chlc_zoox)/sqrt(length(µg_chlc_zoox)))


##### les 2 pep par especes sur meme graph####
meanchlazoox<-ddply(physio,c("esp","pep"),summarise,mean=mean(µg_chla_zoox),sd=sd(µg_chla_zoox),se=se(µg_chla_zoox),CI=qnorm(0.975)*sd(µg_chla_zoox)/sqrt(length(µg_chla_zoox)))
ggplot(meanchlazoox, aes(x = esp, y = mean, fill = esp)) +
  geom_bar(stat = "identity")+
  facet_grid(rows=vars(pep),scales="free_x")+
  xlab("Espèces")+
  ylab("µg de chlorophylle a / zooxanthelle")+
  geom_errorbar(aes(ymin = mean-CI, ymax = mean + CI), width = 0.2)+
  theme_classic()+ theme(legend.position = "none")+theme(axis.text.x = element_text(angle=50, hjust=1, vjust=1))

####NAP IRR
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
ggplot(napirr, aes(x=pep,y=µg_chla_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("Napopra irregularis")+
  labs(fill="Pépinières")+
  theme_classic()



####PHY CUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
ggplot(napirr, aes(x=pep,y=µg_chla_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle a / zooxanthelle")+
  xlab("Phymastrea curta")+
  labs(fill="Pépinières")+
  theme_classic()

##############################  µg_chlcontent_zoox   ##########################

#selon la pep
ggplot(physio, aes(x=pep,y=Chl_content, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("ratio de chl a:c / zooxanthelle")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()



##############################  µg_chlc_zoox   ##########################

###graph µg_chlc_zoox selon esp##

ggplot(physio, aes(x=esp,y=µg_chlc_zoox, fill=factor(esp))) +
  geom_violin()+
  geom_jitter()+
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()

ggplot(physio, aes(x=esp,y=µg_chlc_zoox, fill=factor(esp))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("")+
  labs(fill="Espèces")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))



#selon la pep
ggplot(physio, aes(x=pep,y=µg_chlc_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()


#graph de la Chla par zoox par esp dans les 2peps
ggplot(physio, aes(x=esp, y=µg_chlc_zoox, colour=pep, fill=pep))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("Espèces")+
  labs(fill="Pépinières")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


##### les 2 pep par especes sur meme graph####
meanchlczoox<-ddply(physio,c("esp","pep"),summarise,mean=mean(µg_chlc_zoox),sd=sd(µg_chlc_zoox),se=se(µg_chlc_zoox),CI=qnorm(0.975)*sd(µg_chlc_zoox)/sqrt(length(µg_chlc_zoox)))
ggplot(meanchlczoox, aes(x = esp, y = mean, fill = esp)) +
  geom_bar(stat = "identity")+
  facet_grid(rows=vars(pep),scales="free_x")+
  xlab("Espèces")+
  ylab("µg de chlorophylle c / zooxanthelle")+
  geom_errorbar(aes(ymin = mean-CI, ymax = mean + CI), width = 0.2)+
  theme_classic()+ theme(legend.position = "none")+theme(axis.text.x = element_text(angle=50, hjust=1, vjust=1))

#ACR HYA
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
ggplot(acrhya, aes(x=pep,y=µg_chlc_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("Acropora hyacinthus")+
  labs(fill="Pépinières")+
  theme_classic()

#POC ACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
ggplot(pocacu, aes(x=pep,y=µg_chlc_zoox, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle c / zooxanthelle")+
  xlab("Pocillopora acuta")+
  labs(fill="Pépinières")+
  theme_classic()

##############################  zoox_cm²   ##########################

###graph selon esp##

ggplot(physio, aes(x=esp,y=zoox_cm2, fill=factor(esp))) +
  geom_violin()+
  geom_jitter()+
  ylab("Nombre de zooxanthelles / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()

ggplot(physio, aes(x=esp,y=zoox_cm2, fill=factor(esp))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  ylab("Nombre de zooxanthelles / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()


#selon la pep
ggplot(physio, aes(x=pep,y=zoox_cm2, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("Nombre de zooxanthelles / cm²")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()


#graph de la Chla par zoox par esp dans les 2peps
ggplot(physio, aes(x=esp, y=zoox_cm2, colour=pep, fill=pep))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("Nombre de zooxanthelles / cm²")+
  xlab("Espèces")+
  labs(fill="Pépinières")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


##### les 2 pep par especes sur meme graph####
meanzooxcm<-ddply(physio,c("esp","pep"),summarise,mean=mean(zoox_cm2),sd=sd(zoox_cm2),se=se(zoox_cm2),CI=qnorm(0.975)*sd(zoox_cm2)/sqrt(length(zoox_cm2)))
ggplot(meanzooxcm, aes(x = esp, y = mean, fill = esp)) +
  geom_bar(stat = "identity")+
  facet_grid(rows=vars(pep),scales="free_x")+
  xlab("Espèces")+
  ylab("Nombre de zooxanthelles / cm²")+
  geom_errorbar(aes(ymin = mean-CI, ymax = mean + CI), width = 0.2)+
  theme_classic()+ theme(legend.position = "none")+theme(axis.text.x = element_text(angle=50, hjust=1, vjust=1))



##############################  µg_chla_cm   ##########################


###graph µg_chla_cm selon esp##

ggplot(physio, aes(x=esp,y=µg_chla_cm2, fill=factor(esp))) +
  geom_violin()+
  geom_jitter()+
  ylab("µg de chlorophylle a / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()

ggplot(physio, aes(x=esp,y=µg_chla_cm2, fill=factor(esp))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  ylab("µg de chlorophylle a / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()


#selon la pep
ggplot(physio, aes(x=pep,y=µg_chla_cm2, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle a / cm²")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()


#graph de la Chla par zoox par esp dans les 2peps
ggplot(physio, aes(x=esp, y=µg_chla_cm2, colour=pep, fill=pep))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("µg de chlorophylle a / cm²")+
  xlab("Espèces")+
  labs(fill="Pépinières")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


##### les 2 pep par especes sur meme graph####
meanchlacm<-ddply(physio,c("esp","pep"),summarise,mean=mean(µg_chla_cm2),sd=sd(µg_chla_cm2),se=se(µg_chla_cm2),CI=qnorm(0.975)*sd(µg_chla_cm2)/sqrt(length(µg_chla_cm2)))
ggplot(meanchlacm, aes(x = esp, y = mean, fill = esp)) +
  geom_bar(stat = "identity")+
  facet_grid(rows=vars(pep),scales="free_x")+
  xlab("Espèces")+
  ylab("µg de chlorophylle a / cm²")+
  geom_errorbar(aes(ymin = mean-CI, ymax = mean + CI), width = 0.2)+
  theme_classic()+ theme(legend.position = "none")+theme(axis.text.x = element_text(angle=50, hjust=1, vjust=1))

##############################  µg_chlc_cm   ##########################



###graph µg_chlC_cm selon esp##

ggplot(physio, aes(x=esp,y=µg_chlc_cm2, fill=factor(esp))) +
  geom_violin()+
  geom_jitter()+
  ylab("µg de chlorophylle c / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()

ggplot(physio, aes(x=esp,y=µg_chlc_cm2, fill=factor(esp))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  ylab("µg de chlorophylle c / cm²")+
  xlab("")+
  labs(fill="Espèces")+
  theme_bw()


#selon la pep
ggplot(physio, aes(x=pep,y=µg_chlc_cm2, fill=factor(pep))) +
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+  
  ylab("µg de chlorophylle c / cm²")+
  xlab("Pépinières")+
  labs(fill="Pépinières")+
  theme_classic()


#graph par esp dans les 2peps
ggplot(physio, aes(x=esp, y=µg_chlc_cm2, colour=pep, fill=pep))+
  geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) + 
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8), fatten=NULL)+ 
  scale_colour_manual(values=c(c("#66CCFF", "#006699", "#FFFFFF")))+ scale_fill_manual(values=c("#66CCFF", "#006699", "#FFFFFF"))+ 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width=0.65,size = 1.5, linetype = "solid",position = position_dodge(width=0.7))+ 
  ylab("µg de chlorophylle c / cm²")+
  xlab("Espèces")+
  labs(fill="Pépinières")+
  theme_classic()+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


##### les 2 pep par especes sur meme graph####
meanchlccm<-ddply(physio,c("esp","pep"),summarise,mean=mean(µg_chlc_cm2),sd=sd(µg_chlc_cm2),se=se(µg_chlc_cm2),CI=qnorm(0.975)*sd(µg_chlc_cm2)/sqrt(length(µg_chlc_cm2)))
ggplot(meanchlccm, aes(x = esp, y = mean, fill = esp)) +
  geom_bar(stat = "identity")+
  facet_grid(rows=vars(pep),scales="free_x")+
  xlab("Espèces")+
  ylab("µg de chlorophylle a / cm²")+
  geom_errorbar(aes(ymin = mean-CI, ymax = mean + CI), width = 0.2)+
  theme_classic()+ theme(legend.position = "none")+theme(axis.text.x = element_text(angle=50, hjust=1, vjust=1))


#############################   graph age   ##################################

##zoox_cm2##
ggplot(physio, aes(x=age, y=zoox_cm2)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  ylab("Nombre de zoox/cm²")+
  xlab("age (j)")+
  theme_bw()

#µg chloro a/cm²
ggplot(physio, aes(x=age, y=µg_chla_cm2)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  ylab("µg chloro a/cm²")+
  xlab("age (j)")+
  theme_bw()

###graph µg_chlc_CM²##
ggplot(physio, aes(x=age, y=µg_chlc_cm2)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  ylab("µg chloro c/cm²")+
  xlab("age (j)")+
  theme_bw()


#####################################################################
###########Comparaison chlo a/zoox selon les especes 2 à 2############
#####################################################################



##############################   ACR HYA    ##############################
#filtre acrhya garpla
acrhya_garpla<-acrhya_garpla 
montest=wilcox.test(acrhya_garpla$µg_chla_zoox~acrhya_garpla$esp)
montest
#W = 116, p-value = 0.5018
#--> NS
kruskal.test(acrhya_garpla$µg_chla_zoox~acrhya_garpla$esp)
#Kruskal-Wallis chi-squared = 0.49548, df = 1, p-value = 0.4815


#filtre acrhya NAPIRR
acrhya_napirr<-acrhya_napirr 
montest=wilcox.test(acrhya_napirr$µg_chla_zoox~acrhya_napirr$esp)
montest
#W = 94, p-value = 0.675
#--> NS
kruskal.test(acrhya_napirr$µg_chla_zoox~acrhya_napirr$esp)
#Kruskal-Wallis chi-squared = 0.2042, df = 1, p-value = 0.6514

#filtre acrhya PAV CAC
acrhya_pavcac<-acrhya_pavcac 
montest=wilcox.test(acrhya_pavcac$µg_chla_zoox~acrhya_pavcac$esp)
montest
#W = 96, p-value = 0.0001299
#--> SIGNIFICATIF
kruskal.test(acrhya_pavcac$µg_chla_zoox~acrhya_pavcac$esp)
#Kruskal-Wallis chi-squared = 12.091, df = 1, p-value = 0.0005065

#filtre acrhya PHYCUR
acrhya_phycur<-acrhya_phycur
montest=wilcox.test(acrhya_phycur$µg_chla_zoox~acrhya_phycur$esp)
montest
#W  = 73, p-value = 0.1048
#--> NS
kruskal.test(acrhya_phycur$µg_chla_zoox~acrhya_phycur$esp)
#Kruskal-Wallis chi-squared = 2.7069, df = 1, p-value = 0.09991

#filtre acrhya POCACU
acrhya_pocacu<-acrhya_pocacu
montest=wilcox.test(acrhya_pocacu$µg_chla_zoox~acrhya_pocacu$esp)
montest
#W = 66, p-value = 0.4679
#--> NS
kruskal.test(acrhya_pocacu$µg_chla_zoox~acrhya_pocacu$esp)
#Kruskal-Wallis chi-squared = 0.6, df = 1, p-value = 0.4386

#filtre acrhya PORRUS
acrhya_porrus<-acrhya_porrus
montest=wilcox.test(acrhya_porrus$µg_chla_zoox~acrhya_porrus$esp)
montest
#W = 75, p-value = 0.2865
#--> NS
kruskal.test(acrhya_porrus$µg_chla_zoox~acrhya_porrus$esp)
#Kruskal-Wallis chi-squared = 1.2097, df = 1, p-value = 0.2714

##############################   PAV CAC    ##############################

#filtre pavcac napirr
pavcac_napirr<-data_physio_R %>% filter(esp%in% c("PAV CAC", "NAP IRR"))
montest=wilcox.test(pavcac_napirr$µg_chla_zoox~pavcac_napirr$esp)
montest
#W = 150, p-value = 0.0005744
#--> SIGNIFICATIF
kruskal.test(pavcac_napirr$µg_chla_zoox~pavcac_napirr$esp)
#Kruskal-Wallis chi-squared = 10.651, df = 1, p-value = 0.0011

#filtre pavcac GAR PLA
pavcac_garpla<-data_physio_R %>% filter(esp%in% c("PAV CAC", "GAR PLA"))
montest=wilcox.test(pavcac_garpla$µg_chla_zoox~pavcac_garpla$esp)
montest
#W = 179, p-value = 0.0002052
#--> SIGNIFICATIF
kruskal.test(pavcac_garpla$µg_chla_zoox~pavcac_garpla$esp)
#Kruskal-Wallis chi-squared = 12.079, df = 1, p-value = 0.0005098

#filtre pavcac phycur
pavcac_phycur<-data_physio_R %>% filter(esp%in% c("PAV CAC", "PHY CUR"))
montest=wilcox.test(pavcac_phycur$µg_chla_zoox~pavcac_phycur$esp)
montest
#W = 1, p-value = 4.321e-08
#--> SIGNIFICATIF
kruskal.test(pavcac_phycur$µg_chla_zoox~pavcac_phycur$esp)
#Kruskal-Wallis chi-squared = 19.943, df = 1, p-value = 7.98e-06

#filtre pavcac pocacu
pavcac_pocacu<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu$µg_chla_zoox~pavcac_pocacu$esp)
montest
#W = 25, p-value = 0.0357
#--> SIGNIFICATIF
kruskal.test(pavcac_pocacu$µg_chla_zoox~pavcac_pocacu$esp)
#Kruskal-Wallis chi-squared = 4.4628, df = 1, p-value = 0.03464

#filtre pavcac porrus
pavcac_porrus<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POR RUS"))
montest=wilcox.test(pavcac_porrus$µg_chla_zoox~pavcac_porrus$esp)
montest
#W = 0, p-value = 6.657e-08
#--> SIGNIFICATIF
kruskal.test(pavcac_porrus$µg_chla_zoox~pavcac_porrus$esp)
#Kruskal-Wallis chi-squared = 19.355, df = 1, p-value = 1.085e-05


##############################   NAP IRR    ##############################

#filtre NAPIRR garpla
napirr_garpla<-data_physio_R %>% filter(esp%in% c("NAP IRR", "GAR PLA"))
montest=wilcox.test(napirr_garpla$µg_chla_zoox~napirr_garpla$esp)
montest
#W = 157, p-value = 0.7066
#--> NS
kruskal.test(napirr_garpla$µg_chla_zoox~napirr_garpla$esp)
#Kruskal-Wallis chi-squared = 0.15697, df = 1, p-value = 0.692

#filtre NAPIRR PHYCUR
napirr_phycur<-data_physio_R %>% filter(esp%in% c("NAP IRR", "PHY CUR"))
montest=wilcox.test(napirr_phycur$µg_chla_zoox~napirr_phycur$esp)
montest
#W = 106, p-value = 0.01368
#--> SIGNIFICATIF
kruskal.test(napirr_phycur$µg_chla_zoox~napirr_phycur$esp)
#Kruskal-Wallis chi-squared = 5.9961, df = 1, p-value = 0.01434

#filtre NAPIRR POCACU
napirr_pocacu<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu$µg_chla_zoox~napirr_pocacu$esp)
montest
#W = 109, p-value = 0.4869
#--> NS
kruskal.test(napirr_pocacu$µg_chla_zoox~napirr_pocacu$esp)
#Kruskal-Wallis chi-squared = 0.53162, df = 1, p-value = 0.4659


#filtre NAPIRR PORRUS
napirr_porrus<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POR RUS"))
montest=wilcox.test(napirr_porrus$µg_chla_zoox~napirr_porrus$esp)
montest
#W = 115, p-value = 0.09707
#--> NS
kruskal.test(napirr_porrus$µg_chla_zoox~napirr_porrus$esp)
#Kruskal-Wallis chi-squared = 2.8096, df = 1, p-value = 0.0937


##############################   PHY CUR    ##############################


#filtre PHYCUR POCACU
phycur_pocacu<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu$µg_chla_zoox~phycur_pocacu$esp)
montest
#W = 188, p-value = 0.02304
#--> SIGNIFICATIF
kruskal.test(phycur_pocacu$µg_chla_zoox~phycur_pocacu$esp)
#Kruskal-Wallis chi-squared = 5.1256, df = 1, p-value = 0.02358

#filtre PHYCUR GARPLA
phycur_garpla<-data_physio_R %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla$µg_chla_zoox~phycur_garpla$esp)
montest
#W = 105, p-value = 0.001885
#--> SIGNIFICATIF
kruskal.test(phycur_garpla$µg_chla_zoox~phycur_garpla$esp)
#Kruskal-Wallis chi-squared = 9.2638, df = 1, p-value = 0.002337

#filtre PHYCUR PORRUS
phycur_porrus<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus$µg_chla_zoox~phycur_porrus$esp)
montest
#W = 272, p-value = 0.3157
#--> NS
kruskal.test(phycur_porrus$µg_chla_zoox~phycur_porrus$esp)
#Kruskal-Wallis chi-squared = 1.0458, df = 1, p-value = 0.3065

##############################   GAR PLA    ##############################

#filtre GARPLA POC ACU
garpla_pocacu<-data_physio_R %>% filter(esp%in% c("GAR PLA","POC ACU"))
montest=wilcox.test(garpla_pocacu$µg_chla_zoox~garpla_pocacu$esp)
montest
#W = 117, p-value = 0.7918
#--> NS
kruskal.test(garpla_pocacu$µg_chla_zoox~garpla_pocacu$esp)
#Kruskal-Wallis chi-squared = 0.083523, df = 1, p-value = 0.7726

#filtre GARPLA POR RUS
garpla_porrus<-data_physio_R %>% filter(esp%in% c("GAR PLA","POR RUS"))
montest=wilcox.test(garpla_porrus$µg_chla_zoox~garpla_porrus$esp)
montest
#W = 128, p-value = 0.0524
#--> LIMITE SIGNIFICATIF
kruskal.test(garpla_porrus$µg_chla_zoox~garpla_porrus$esp)
#Kruskal-Wallis chi-squared = 3.7932, df = 1, p-value = 0.05146

##############################   POC ACU    ##############################


#filtre POC ACU POR RUS
pocacu_porrus<-data_physio_R %>% filter(esp%in% c("POC ACU","POR RUS"))
montest=wilcox.test(pocacu_porrus$µg_chla_zoox~pocacu_porrus$esp)
montest
#W = 74, p-value = 0.1447
#--> NS
kruskal.test(pocacu_porrus$µg_chla_zoox~pocacu_porrus$esp)
#Kruskal-Wallis chi-squared = 2.2091, df = 1, p-value = 0.1372

#####################################################################
###########Comparaison chlo a/zoox selon les pep ############
#####################################################################

#filtre acrhya
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
montest=wilcox.test(acrhya$µg_chla_zoox~acrhya$pep)
montest
#W = 7, p-value = 0.3095
#--> NS
kruskal.test(acrhya$µg_chla_zoox~acrhya$pep)
#Kruskal-Wallis chi-squared = 1.32, df = 1, p-value = 0.2506

#filtre garpla
garpla<-data_physio_R  %>% filter(esp=="GAR PLA")
montest=wilcox.test(garpla$µg_chla_zoox~garpla$pep)
montest
#W = 29, p-value = 0.1569
#--> NS
kruskal.test(garpla$µg_chla_zoox~garpla$pep)
#Kruskal-Wallis chi-squared = 2.1488, df = 1, p-value = 0.1427

#filtre napirr
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
montest=wilcox.test(napirr$µg_chla_zoox~napirr$pep)
montest
#W = 13, p-value = 0.0274
#--> SIGNIFICATIF
kruskal.test(napirr$µg_chla_zoox~napirr$pep)
#Kruskal-Wallis chi-squared = 4.8981, df = 1, p-value = 0.02689

#filtre pavcac
pavcac<-data_physio_R  %>% filter(esp=="PAV CAC")
montest=wilcox.test(pavcac$µg_chla_zoox~pavcac$pep)
montest
#W = 13, p-value = 1
#--> NS
kruskal.test(pavcac$µg_chla_zoox~pavcac$pep)
#Kruskal-Wallis chi-squared = 0.010909, df = 1, p-value = 0.9168

#filtre PHYCUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
montest=wilcox.test(phycur$µg_chla_zoox~phycur$pep)
montest
#W = 32, p-value = 0.04216
#--> SIGNIFICATIF
kruskal.test(phycur$µg_chla_zoox~phycur$pep)
#Kruskal-Wallis chi-squared = 4.1885, df = 1, p-value = 0.0407

#filtre POCACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
montest=wilcox.test(pocacu$µg_chla_zoox~pocacu$pep)
montest
#W = 5, p-value = 0.1091
#--> NS
kruskal.test(pocacu$µg_chla_zoox~pocacu$pep)
#Kruskal-Wallis chi-squared = 2.8929, df = 1, p-value = 0.08897

#filtre POR RUS
porrus<-data_physio_R  %>% filter(esp=="POR RUS")
montest=wilcox.test(porrus$µg_chla_zoox~porrus$pep)
montest
#W = 20, p-value = 0.07575
#--> NS
kruskal.test(porrus$µg_chla_zoox~porrus$pep)
#Kruskal-Wallis chi-squared = 3.2925, df = 1, p-value = 0.0696


#####################################################################
###########Comparaison chlo c/zoox selon les esp ############
#####################################################################

##############################   ACR HYA    ##############################

#filtre acrhya garpla
acrhya_garpla<-data_physio_R %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla$µg_chlc_zoox~acrhya_garpla$esp)
montest
#W  = 109, p-value = 0.7132
#--> NS
kruskal.test(acrhya_garpla$µg_chlc_zoox~acrhya_garpla$esp)
#Kruskal-Wallis chi-squared = 0.15677, df = 1, p-value = 0.6921


#filtre acrhya NAPIRR
acrhya_napirr<-data_physio_R %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr$µg_chlc_zoox~acrhya_napirr$esp)
montest
#W  = 53, p-value = 0.1149
#--> NS
kruskal.test(acrhya_napirr$µg_chlc_zoox~acrhya_napirr$esp)
#Kruskal-Wallis chi-squared = 2.5815, df = 1, p-value = 0.1081

#filtre acrhya PAV CAC
acrhya_pavcac<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac$µg_chlc_zoox~acrhya_pavcac$esp)
montest
#W= 63, p-value = 0.3527
#--> ns
kruskal.test(acrhya_pavcac$µg_chlc_zoox~acrhya_pavcac$esp)
#Kruskal-Wallis chi-squared = 0.96571, df = 1, p-value = 0.3258


#filtre acrhya PHYCUR
acrhya_phycur<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur$µg_chlc_zoox~acrhya_phycur$esp)
montest
#W = 66, p-value = 0.05651
#--> NS quasi S
kruskal.test(acrhya_phycur$µg_chlc_zoox~acrhya_phycur$esp)
#Kruskal-Wallis chi-squared = 3.6844, df = 1, p-value = 0.05492

#filtre acrhya POCACU
acrhya_pocacu<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu$µg_chlc_zoox~acrhya_pocacu$esp)
montest
#= 65, p-value = 0.5116
#--> NS
kruskal.test(acrhya_pocacu$µg_chlc_zoox~acrhya_pocacu$esp)
#Kruskal-Wallis chi-squared = 0.49587, df = 1, p-value = 0.4813

#filtre acrhya PORRUS
acrhya_porrus<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus$µg_chlc_zoox~acrhya_porrus$esp)
montest
#W = 22, p-value = 0.0002587
#--> SIGNIFICATIF
kruskal.test(acrhya_porrus$µg_chlc_zoox~acrhya_porrus$esp)
#Kruskal-Wallis chi-squared = 11.775, df = 1, p-value = 0.0006002


##############################   PAV CAC    ##############################

#filtre pavcac napirr
pavcac_napirr<-data_physio_R %>% filter(esp%in% c("PAV CAC", "NAP IRR"))
montest=wilcox.test(pavcac_napirr$µg_chlc_zoox~pavcac_napirr$esp)
montest
#W = 129, p-value = 0.02702
#--> SIGNIFICATIF
kruskal.test(pavcac_napirr$µg_chlc_zoox~pavcac_napirr$esp)
#Kruskal-Wallis chi-squared = 4.8807, df = 1, p-value = 0.02716

#filtre pavcac GAR PLA
pavcac_garpla<-data_physio_R %>% filter(esp%in% c("PAV CAC", "GAR PLA"))
montest=wilcox.test(pavcac_garpla$µg_chlc_zoox~pavcac_garpla$esp)
montest
##--> NS
kruskal.test(pavcac_garpla$µg_chlc_zoox~pavcac_garpla$esp)
##--> NS

#filtre pavcac phycur
pavcac_phycur<-data_physio_R %>% filter(esp%in% c("PAV CAC", "PHY CUR"))
montest=wilcox.test(pavcac_phycur$µg_chlc_zoox~pavcac_phycur$esp)
montest
#W = 46, p-value = 0.005773
#--> SIGNIFICATIF
kruskal.test(pavcac_phycur$µg_chlc_zoox~pavcac_phycur$esp)
#Kruskal-Wallis chi-squared = 7.3059, df = 1, p-value = 0.006873

#filtre pavcac pocacu
pavcac_pocacu<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu$µg_chlc_zoox~pavcac_pocacu$esp)
montest
#--> NS
kruskal.test(pavcac_pocacu$µg_chlc_zoox~pavcac_pocacu$esp)
#--> NS

#filtre pavcac porrus
pavcac_porrus<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POR RUS"))
montest=wilcox.test(pavcac_porrus$µg_chlc_zoox~pavcac_porrus$esp)
montest
#W = 13, p-value = 2.436e-05
#--> SIGNIFICATIF
kruskal.test(pavcac_porrus$µg_chlc_zoox~pavcac_porrus$esp)
#Kruskal-Wallis chi-squared = 14.65, df = 1, p-value = 0.0001295

##############################   NAP IRR    ##############################

#filtre NAPIRR garpla
napirr_garpla<-data_physio_R %>% filter(esp%in% c("NAP IRR", "GAR PLA"))
montest=wilcox.test(napirr_garpla$µg_chlc_zoox~napirr_garpla$esp)
montest
#W = 104, p-value = 0.04498
#--> SIGNIFICATIFF
kruskal.test(napirr_garpla$µg_chlc_zoox~napirr_garpla$esp)
#Kruskal-Wallis chi-squared = 4.0458, df = 1, p-value = 0.04428


#filtre NAPIRR PHYCUR
napirr_phycur<-data_physio_R %>% filter(esp%in% c("NAP IRR", "PHY CUR"))
montest=wilcox.test(napirr_phycur$µg_chlc_zoox~napirr_phycur$esp)
montest
#--> NS
kruskal.test(napirr_phycur$µg_chlc_zoox~napirr_phycur$esp)
#--> NS

#filtre NAPIRR POCACU
napirr_pocacu<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu$µg_chlc_zoox~napirr_pocacu$esp)
montest
#W = 137, p-value = 0.04167
#--> SIGNIFICATIF
kruskal.test(napirr_pocacu$µg_chlc_zoox~napirr_pocacu$esp)
#Kruskal-Wallis chi-squared = 4.1872, df = 1, p-value = 0.04073


#filtre NAPIRR PORRUS
napirr_porrus<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POR RUS"))
montest=wilcox.test(napirr_porrus$µg_chlc_zoox~napirr_porrus$esp)
montest
#--> NS
kruskal.test(napirr_porrus$µg_chlc_zoox~napirr_porrus$esp)
#--> NS


##############################   PHY CUR    ##############################


#filtre PHYCUR POCACU
phycur_pocacu<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu$µg_chlc_zoox~phycur_pocacu$esp)
montest
#W = 187, p-value = 0.02553
#--> SIGNIFICATIF
kruskal.test(phycur_pocacu$µg_chlc_zoox~phycur_pocacu$esp)
#Kruskal-Wallis chi-squared = 4.9602, df = 1, p-value = 0.02594


#filtre PHYCUR GARPLA
phycur_garpla<-data_physio_R %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla$µg_chlc_zoox~phycur_garpla$esp)
montest
#W = 128, p-value = 0.01238
#--> SIGNIFICATIF
kruskal.test(phycur_garpla$µg_chlc_zoox~phycur_garpla$esp)
#Kruskal-Wallis chi-squared = 6.1684, df = 1, p-value = 0.01301


#filtre PHYCUR PORRUS
phycur_porrus<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus$µg_chlc_zoox~phycur_porrus$esp)
montest
#W = 118, p-value = 0.005749
#--> SIGNIFICATIF
kruskal.test(phycur_porrus$µg_chlc_zoox~phycur_porrus$esp)
#Kruskal-Wallis chi-squared = 7.4372, df = 1, p-value = 0.006389


##############################   GAR PLA    ##############################

#filtre GARPLA POC ACU
garpla_pocacu<-data_physio_R %>% filter(esp%in% c("GAR PLA","POC ACU"))
montest=wilcox.test(garpla_pocacu$µg_chlc_zoox~garpla_pocacu$esp)
montest
#--> NS
kruskal.test(garpla_pocacu$µg_chlc_zoox~garpla_pocacu$esp)
#--> NS

#filtre GARPLA POR RUS
garpla_porrus<-data_physio_R %>% filter(esp%in% c("GAR PLA","POR RUS"))
montest=wilcox.test(garpla_porrus$µg_chlc_zoox~garpla_porrus$esp)
montest
#W = 58, p-value = 5.208e-05
#-->  SIGNIFICATIF
kruskal.test(garpla_porrus$µg_chlc_zoox~garpla_porrus$esp)
#Kruskal-Wallis chi-squared = 14.754, df = 1, p-value = 0.0001225

##############################   POC ACU    ##############################


#filtre POC ACU POR RUS
pocacu_porrus<-data_physio_R %>% filter(esp%in% c("POC ACU","POR RUS"))
montest=wilcox.test(pocacu_porrus$µg_chlc_zoox~pocacu_porrus$esp)
montest
#W = 30, p-value = 0.000525
#--> SIGNIFICATIF
kruskal.test(pocacu_porrus$µg_chlc_zoox~pocacu_porrus$esp)
#Kruskal-Wallis chi-squared = 10.909, df = 1, p-value = 0.0009569


####################################################################
###########Comparaison chlo c/cm² selon les esp ############
#####################################################################

##############################   ACR HYA    ##############################

#filtre acrhya garpla
acrhya_garpla<-data_physio_R %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla$µg_chlc_cm2~acrhya_garpla$esp)
montest
#--> NS
kruskal.test(acrhya_garpla$µg_chlc_cm2~acrhya_garpla$esp)
#--> NS


#filtre acrhya NAPIRR
acrhya_napirr<-data_physio_R %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr$µg_chlc_cm2~acrhya_napirr$esp)
montest
#--> NS
kruskal.test(acrhya_napirr$µg_chlc_cm2~acrhya_napirr$esp)
#--> NS

#filtre acrhya PAV CAC
acrhya_pavcac<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac$µg_chlc_cm2~acrhya_pavcac$esp)
montest
#--> ns
kruskal.test(acrhya_pavcac$µg_chlc_cm2~acrhya_pavcac$esp)
#--> NS


#filtre acrhya PHYCUR
acrhya_phycur<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur$µg_chlc_cm2~acrhya_phycur$esp)
montest
#W = 34, p-value = 0.0009114
#--> SIGNIFICATIF
kruskal.test(acrhya_phycur$µg_chlc_cm2~acrhya_phycur$esp)
#Kruskal-Wallis chi-squared = 10.068, df = 1, p-value = 0.001509


#filtre acrhya POCACU
acrhya_pocacu<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu$µg_chlc_cm2~acrhya_pocacu$esp)
montest
#--> NS
kruskal.test(acrhya_pocacu$µg_chlc_cm2~acrhya_pocacu$esp)
#--> NS

#filtre acrhya PORRUS
acrhya_porrus<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus$µg_chlc_cm2~acrhya_porrus$esp)
montest
#W = 48, p-value = 0.02156
#--> SIGNIFICATIF
kruskal.test(acrhya_porrus$µg_chlc_cm2~acrhya_porrus$esp)
#Kruskal-Wallis chi-squared = 5.2335, df = 1, p-value = 0.02216


##############################   PAV CAC    ##############################

#filtre pavcac napirr
pavcac_napirr<-data_physio_R %>% filter(esp%in% c("PAV CAC", "NAP IRR"))
montest=wilcox.test(pavcac_napirr$µg_chlc_cm2~pavcac_napirr$esp)
montest
#--> NS
kruskal.test(pavcac_napirr$µg_chlc_cm2~pavcac_napirr$esp)
#--> NS

#filtre pavcac GAR PLA
pavcac_garpla<-data_physio_R %>% filter(esp%in% c("PAV CAC", "GAR PLA"))
montest=wilcox.test(pavcac_garpla$µg_chlc_cm2~pavcac_garpla$esp)
montest
#W = 146, p-value = 0.04392
# SIGNIFICATIF
kruskal.test(pavcac_garpla$µg_chlc_cm2~pavcac_garpla$esp)
# SIGNIFICATIF

#filtre pavcac phycur
pavcac_phycur<-data_physio_R %>% filter(esp%in% c("PAV CAC", "PHY CUR"))
montest=wilcox.test(pavcac_phycur$µg_chlc_cm2~pavcac_phycur$esp)
montest
#W = 23, p-value = 0.0001053
#--> SIGNIFICATIF
kruskal.test(pavcac_phycur$µg_chlc_cm2~pavcac_phycur$esp)
#Kruskal-Wallis chi-squared = 12.988, df = 1, p-value = 0.0003135


#filtre pavcac pocacu
pavcac_pocacu<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu$µg_chlc_cm2~pavcac_pocacu$esp)
montest
#--> NS
kruskal.test(pavcac_pocacu$µg_chlc_cm2~pavcac_pocacu$esp)
#--> NS

#filtre pavcac porrus
pavcac_porrus<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POR RUS"))
montest=wilcox.test(pavcac_porrus$µg_chlc_cm2~pavcac_porrus$esp)
montest
#W = 33, p-value = 0.002317
#--> SIGNIFICATIF
kruskal.test(pavcac_porrus$µg_chlc_cm2~pavcac_porrus$esp)
#Kruskal-Wallis chi-squared = 8.6884, df = 1, p-value = 0.003202

##############################   NAP IRR    ##############################

#filtre NAPIRR garpla
napirr_garpla<-data_physio_R %>% filter(esp%in% c("NAP IRR", "GAR PLA"))
montest=wilcox.test(napirr_garpla$µg_chlc_cm2~napirr_garpla$esp)
montest
#W = 33, p-value = 0.002317
#--> SIGNIFICATIFF
kruskal.test(napirr_garpla$µg_chlc_cm2~napirr_garpla$esp)
#Kruskal-Wallis chi-squared = 0.44954, df = 1, p-value = 0.5026


#filtre NAPIRR PHYCUR
napirr_phycur<-data_physio_R %>% filter(esp%in% c("NAP IRR", "PHY CUR"))
montest=wilcox.test(napirr_phycur$µg_chlc_cm2~napirr_phycur$esp)
montest
#W = 105, p-value = 0.0126
#--> S
kruskal.test(napirr_phycur$µg_chlc_cm2~napirr_phycur$esp)
#Kruskal-Wallis chi-squared = 6.1308, df = 1, p-value = 0.01328
#--> NS

#filtre NAPIRR POCACU
napirr_pocacu<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu$µg_chlc_cm2~napirr_pocacu$esp)
montest
#--> ns
kruskal.test(napirr_pocacu$µg_chlc_cm2~napirr_pocacu$esp)
#--> ns


#filtre NAPIRR PORRUS
napirr_porrus<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POR RUS"))
montest=wilcox.test(napirr_porrus$µg_chlc_cm2~napirr_porrus$esp)
montest
#--> NS
kruskal.test(napirr_porrus$µg_chlc_cm2~napirr_porrus$esp)
#--> NS


##############################   PHY CUR    ##############################


#filtre PHYCUR POCACU
phycur_pocacu<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu$µg_chlc_cm2~phycur_pocacu$esp)
montest
#W = 210, p-value = 0.001454
#--> SIGNIFICATIF
kruskal.test(phycur_pocacu$µg_chlc_cm2~phycur_pocacu$esp)
#Kruskal-Wallis chi-squared = 9.4486, df = 1, p-value = 0.002113



#filtre PHYCUR GARPLA
phycur_garpla<-data_physio_R %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla$µg_chlc_cm2~phycur_garpla$esp)
montest
#W = 129, p-value = 0.01331
#--> SIGNIFICATIF
kruskal.test(phycur_garpla$µg_chlc_cm2~phycur_garpla$esp)
#Kruskal-Wallis chi-squared = 6.048, df = 1, p-value = 0.01392


#filtre PHYCUR PORRUS
phycur_porrus<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus$µg_chlc_cm2~phycur_porrus$esp)
montest
#W = 118, p-value = 0.005749
#--> SIGNIFICATIF
kruskal.test(phycur_porrus$µg_chlc_cm2~phycur_porrus$esp)
#Kruskal-Wallis chi-squared = 7.4372, df = 1, p-value = 0.006389


##############################   GAR PLA    ##############################

#filtre GARPLA POC ACU
garpla_pocacu<-data_physio_R %>% filter(esp%in% c("GAR PLA","POC ACU"))
montest=wilcox.test(garpla_pocacu$µg_chlc_cm2~garpla_pocacu$esp)
montest
#--> NS
kruskal.test(garpla_pocacu$µg_chlc_cm2~garpla_pocacu$esp)
#--> NS

#filtre GARPLA POR RUS
garpla_porrus<-data_physio_R %>% filter(esp%in% c("GAR PLA","POR RUS"))
montest=wilcox.test(garpla_porrus$µg_chlc_cm2~garpla_porrus$esp)
montest
#W = 149, p-value = 0.1738
#-->  SIGNIFICATIF
kruskal.test(garpla_porrus$µg_chlc_cm2~garpla_porrus$esp)
#Kruskal-Wallis chi-squared = 1.9032, df = 1, p-value = 0.1677

##############################   POC ACU    ##############################


#filtre POC ACU POR RUS
pocacu_porrus<-data_physio_R %>% filter(esp%in% c("POC ACU","POR RUS"))
montest=wilcox.test(pocacu_porrus$µg_chlc_cm2~pocacu_porrus$esp)
montest
#W = 56, p-value = 0.02546
#--> SIGNIFICATIF
kruskal.test(pocacu_porrus$µg_chla_cm2~pocacu_porrus$esp)
#Kruskal-Wallis chi-squared = 4.9705, df = 1, p-value = 0.02578


####################################################################
###########Comparaison chlo a/cm² selon les esp ############
#####################################################################

##############################   ACR HYA    ##############################

#filtre acrhya garpla
acrhya_garpla<-data_physio_R %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla$µg_chla_cm2~acrhya_garpla$esp)
montest
#--> NS
kruskal.test(acrhya_garpla$µg_chla_cm2~acrhya_garpla$esp)
#--> NS


#filtre acrhya NAPIRR
acrhya_napirr<-data_physio_R %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr$µg_chla_cm2~acrhya_napirr$esp)
montest
#--> NS
kruskal.test(acrhya_napirr$µg_chla_cm2~acrhya_napirr$esp)
#--> NS

#filtre acrhya PAV CAC
acrhya_pavcac<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac$µg_chla_cm2~acrhya_pavcac$esp)
montest
#W = 84, p-value = 0.008931
#--> SIGNIFICATIF
kruskal.test(acrhya_pavcac$µg_chla_cm2~acrhya_pavcac$esp)


#filtre acrhya PHYCUR
acrhya_phycur<-data_physio_R %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur$µg_chla_cm2~acrhya_phycur$esp)
montest
#W = 53, p-value = 0.01414
#--> SIGNIFICATIF
kruskal.test(acrhya_phycur$µg_chla_cm2~acrhya_phycur$esp)


#filtre acrhya POCACU
acrhya_pocacu<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu$µg_chla_cm2~acrhya_pocacu$esp)
montest
#--> NS
kruskal.test(acrhya_pocacu$µg_chla_cm2~acrhya_pocacu$esp)
#--> NS

#filtre acrhya PORRUS
acrhya_porrus<-data_physio_R %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus$µg_chla_cm2~acrhya_porrus$esp)
montest
#--> NS
kruskal.test(acrhya_porrus$µg_chla_cm2~acrhya_porrus$esp)
#--> NS


##############################   PAV CAC    ##############################

#filtre pavcac napirr
pavcac_napirr<-data_physio_R %>% filter(esp%in% c("PAV CAC", "NAP IRR"))
montest=wilcox.test(pavcac_napirr$µg_chla_cm2~pavcac_napirr$esp)
montest
#--> NS
kruskal.test(pavcac_napirr$µg_chla_cm2~pavcac_napirr$esp)
#--> NS

#filtre pavcac GAR PLA
pavcac_garpla<-data_physio_R %>% filter(esp%in% c("PAV CAC", "GAR PLA"))
montest=wilcox.test(pavcac_garpla$µg_chla_cm2~pavcac_garpla$esp)
montest
#W = 184, p-value = 5.791e-05
# SIGNIFICATIF
kruskal.test(pavcac_garpla$µg_chla_cm2~pavcac_garpla$esp)
# SIGNIFICATIF

#filtre pavcac phycur
pavcac_phycur<-data_physio_R %>% filter(esp%in% c("PAV CAC", "PHY CUR"))
montest=wilcox.test(pavcac_phycur$µg_chla_cm2~pavcac_phycur$esp)
montest
#W = 9, p-value = 2.096e-06
#--> SIGNIFICATIF
kruskal.test(pavcac_phycur$µg_chla_cm2~pavcac_phycur$esp)


#filtre pavcac pocacu
pavcac_pocacu<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu$µg_chla_cm2~pavcac_pocacu$esp)
montest
#W = 16, p-value = 0.004786
#--> SIGNIFICATIF
kruskal.test(pavcac_pocacu$µg_chla_cm2~pavcac_pocacu$esp)

#filtre pavcac porrus
pavcac_porrus<-data_physio_R %>% filter(esp%in% c("PAV CAC", "POR RUS"))
montest=wilcox.test(pavcac_porrus$µg_chla_cm2~pavcac_porrus$esp)
montest
#W = 46, p-value = 0.01666
#--> SIGNIFICATIF
kruskal.test(pavcac_porrus$µg_chla_cm2~pavcac_porrus$esp)

##############################   NAP IRR    ##############################

#filtre NAPIRR garpla
napirr_garpla<-data_physio_R %>% filter(esp%in% c("NAP IRR", "GAR PLA"))
montest=wilcox.test(napirr_garpla$µg_chla_cm2~napirr_garpla$esp)
montest
#W = 267, p-value = 0.00252
#--> SIGNIFICATIFF
kruskal.test(napirr_garpla$µg_chla_cm2~napirr_garpla$esp)


#filtre NAPIRR PHYCUR
napirr_phycur<-data_physio_R %>% filter(esp%in% c("NAP IRR", "PHY CUR"))
montest=wilcox.test(napirr_phycur$µg_chla_cm2~napirr_phycur$esp)
montest
#W = 43, p-value = 7.366e-06
#--> S
kruskal.test(napirr_phycur$µg_chla_cm2~napirr_phycur$esp)

#filtre NAPIRR POCACU
napirr_pocacu<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu$µg_chla_cm2~napirr_pocacu$esp)
montest
#--> ns
kruskal.test(napirr_pocacu$µg_chla_cm2~napirr_pocacu$esp)
#--> ns


#filtre NAPIRR PORRUS
napirr_porrus<-data_physio_R %>% filter(esp%in% c("NAP IRR", "POR RUS"))
montest=wilcox.test(napirr_porrus$µg_chla_cm2~napirr_porrus$esp)
montest
#--> NS
kruskal.test(napirr_porrus$µg_chla_cm2~napirr_porrus$esp)
#--> NS


##############################   PHY CUR    ##############################


#filtre PHYCUR POCACU
phycur_pocacu<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu$µg_chla_cm2~phycur_pocacu$esp)
montest
#W = 201, p-value = 0.005113
#--> SIGNIFICATIF
kruskal.test(phycur_pocacu$µg_chla_cm2~phycur_pocacu$esp)



#filtre PHYCUR GARPLA
phycur_garpla<-data_physio_R %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla$µg_chla_cm2~phycur_garpla$esp)
montest
#W = 126, p-value = 0.01068
#--> SIGNIFICATIF
kruskal.test(phycur_garpla$µg_chla_cm2~phycur_garpla$esp)


#filtre PHYCUR PORRUS
phycur_porrus<-data_physio_R %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus$µg_chla_cm2~phycur_porrus$esp)
montest
#W = 380, p-value = 0.0001428
#--> SIGNIFICATIF
kruskal.test(phycur_porrus$µg_chla_cm2~phycur_porrus$esp)


##############################   GAR PLA    ##############################

#filtre GARPLA POC ACU
garpla_pocacu<-data_physio_R %>% filter(esp%in% c("GAR PLA","POC ACU"))
montest=wilcox.test(garpla_pocacu$µg_chla_cm2~garpla_pocacu$esp)
montest
#--> NS
kruskal.test(garpla_pocacu$µg_chla_cm2~garpla_pocacu$esp)
#--> NS

#filtre GARPLA POR RUS
garpla_porrus<-data_physio_R %>% filter(esp%in% c("GAR PLA","POR RUS"))
montest=wilcox.test(garpla_porrus$µg_chla_cm2~garpla_porrus$esp)
montest
#W = 284, p-value = 0.02272
#-->  SIGNIFICATIF
kruskal.test(garpla_porrus$µg_chla_cm2~garpla_porrus$esp)

##############################   POC ACU    ##############################


#filtre POC ACU POR RUS
pocacu_porrus<-data_physio_R %>% filter(esp%in% c("POC ACU","POR RUS"))
montest=wilcox.test(pocacu_porrus$µg_chla_cm2~pocacu_porrus$esp)
montest
#--> NS
kruskal.test(pocacu_porrus$µg_chla_cm2~pocacu_porrus$esp)

############### COMPARAISON ENTRE ESP AU SEIN DE CHAQUE PEP #########################
###############################################################
###filtre
phytrentem<-physio  %>% filter(pep=="30m")
phytreizem<-physio  %>% filter(pep=="13m")

###############chlc par cm²#########################
#####################ACR HYA##################################

#GAR PLA a 13m 
acrhya_garpla13<-phytreizem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
acrhya_garpla30<-phytrentem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla30$µg_chlc_cm2~acrhya_garpla30$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
acrhya_napirr13<-phytreizem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr13$µg_chlc_cm2~acrhya_napirr13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
acrhya_napirr30<-phytrentem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr30$µg_chlc_cm2~acrhya_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
acrhya_pavcac13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac13$µg_chlc_cm2~acrhya_pavcac13$esp)
montest
#-->ns

#--> ns 

#PAV CAC a 30m 
acrhya_pavcac30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac30$µg_chlc_cm2~acrhya_pavcac30$esp)
montest
#-->ns

#PHY CUR a 13m 
acrhya_phycur13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur13$µg_chlc_cm2~acrhya_phycur13$esp)
montest
#-->W = 9, p-value = 0.05528
#LIMITE SIGN
kruskal.test(acrhya_phycur13$µg_chlc_cm2~acrhya_phycur13$esp)
#--> Kruskal-Wallis chi-squared = 3.84, df = 1, p-value = 0.05004
#significatif

#PHY CUR a 30m 
acrhya_phycur30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur30$µg_chlc_cm2~acrhya_phycur30$esp)
montest
#-->W = 9, p-value = 0.01937
#significatif
kruskal.test(acrhya_phycur13$µg_chlc_cm2~acrhya_phycur13$esp)
#--> ns 

#Poc acu a 13m 
acrhya_pocacu13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu13$µg_chlc_cm2~acrhya_pocacu13$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chlc_cm2~acrhya_pocacu13$esp)
#--> ns

#POC ACU a 30m 
acrhya_pocacu30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu30$µg_chlc_cm2~acrhya_pocacu30$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chlc_cm2~acrhya_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
acrhya_porrus13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus13$µg_chlc_cm2~acrhya_porrus13$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$µg_chlc_cm2~acrhya_porrus13$esp)
#--> ns

#POR RUS a 30m 
acrhya_porrus30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus30$µg_chlc_cm2~acrhya_porrus30$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$µg_chlc_cm2~acrhya_porrus13$esp)
#--> ns 

#############################Phy cur########################################


#GAR PLA a 13m 
phycur_garpla13<-phytreizem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla13$µg_chlc_cm2~phycur_garpla13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chlc_cm2~phycur_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
phycur_garpla30<-phytrentem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla30$µg_chlc_cm2~phycur_garpla30$esp)
montest
#-->W = 30, p-value = 0.00798
#SIGNIFICATIF
kruskal.test(phycur_garpla13$µg_chlc_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
phycur_napirr13<-phytreizem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr13$µg_chlc_cm2~phycur_napirr13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chlc_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
phycur_napirr30<-phytrentem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr30$µg_chlc_cm2~phycur_napirr30$esp)
montest
#-->W = 22, p-value = 0.01381
#significatif

#PAV CAC a 13m 
phycur_pavcac13<-phytreizem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac13$µg_chlc_cm2~phycur_pavcac13$esp)
montest
#-->W = 7, p-value = 0.02797
#significatif

#PAV CAC a 30m 
phycur_pavcac30<-phytrentem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac30$µg_chlc_cm2~phycur_pavcac30$esp)
montest
#-->W = 6, p-value = 0.006769
#significatif

#Poc acu a 13m 
phycur_pocacu13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu13$µg_chlc_cm2~phycur_pocacu13$esp)
montest
#-->ns
kruskal.test(phycur_pocacu13$µg_chlc_cm2~phycur_pocacu13$esp)
#--> ns

#POC ACU a 30m 
phycur_pocacu30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu30$µg_chlc_cm2~phycur_pocacu30$esp)
montest
#-->W = 50, p-value = 0.003361
#significatif
kruskal.test(phycur_pocacu13$µg_chlc_cm2~phycur_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
phycur_porrus13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus13$µg_chlc_cm2~phycur_porrus13$esp)
montest
#-->ns
kruskal.test(phycur_porrus13$µg_chlc_cm2~phycur_porrus13$esp)
#--> ns

#POR RUS a 30m 
phycur_porrus30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus30$µg_chlc_cm2~phycur_porrus30$esp)
montest
#-->ns
kruskal.test(phycur_porrus13$µg_chlc_cm2~phycur_porrus13$esp)
#--> ns 

#############################POR RUS########################################


#GAR PLA a 13m 
porrus_garpla13<-phytreizem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla13$µg_chlc_cm2~porrus_garpla13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chlc_cm2~porrus_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
porrus_garpla30<-phytrentem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla30$µg_chlc_cm2~porrus_garpla30$esp)
montest
#-->W = 41, p-value = 0.02698
#SIGNIFICATIF
kruskal.test(porrus_garpla30$µg_chlc_cm2~porrus_garpla30$esp)
#--> ns 

#NAP IRR a 13m 
porrus_napirr13<-phytreizem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr13$µg_chlc_cm2~porrus_napirr13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chlc_cm2~porrus_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
porrus_napirr30<-phytrentem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr30$µg_chlc_cm2~porrus_napirr30$esp)
montest
#-->W = 31, p-value = 0.04559
#significatif

#PAV CAC a 13m 
porrus_pavcac13<-phytreizem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac13$µg_chlc_cm2~porrus_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
porrus_pavcac30<-phytrentem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac30$µg_chlc_cm2~porrus_pavcac30$esp)
montest
#-->W = 9, p-value = 0.01428
#significatif

#Poc acu a 13m 
porrus_pocacu13<-phytreizem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu13$µg_chlc_cm2~porrus_pocacu13$esp)
montest
#-->ns
kruskal.test(porrus_pocacu13$µg_chlc_cm2~porrus_pocacu13$esp)
#--> ns

#POC ACU a 30m 
porrus_pocacu30<-phytrentem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu30$µg_chlc_cm2~porrus_pocacu30$esp)
montest
#-->W = 3, p-value = 0.004575
#significatif
kruskal.test(porrus_pocacu13$µg_chlc_cm2~porrus_pocacu13$esp)
#--> ns 

#############################GAR PLA########################################

#NAP IRR a 13m 
garpla_napirr13<-phytreizem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr13$µg_chlc_cm2~garpla_napirr13$esp)
montest
#-->ns


#NAP IRR a 30m 
garpla_napirr30<-phytrentem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr30$µg_chlc_cm2~garpla_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
garpla_pavcac13<-phytreizem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac13$µg_chlc_cm2~garpla_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
garpla_pavcac30<-phytrentem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac30$µg_chlc_cm2~garpla_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
garpla_pocacu13<-phytreizem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu13$µg_chlc_cm2~garpla_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
garpla_pocacu30<-phytrentem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu30$µg_chlc_cm2~garpla_pocacu30$esp)
montest
#-->ns 


#############################NAP IRR########################################


#PAV CAC a 13m 
napirr_pavcac13<-phytreizem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac13$µg_chlc_cm2~napirr_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
napirr_pavcac30<-phytrentem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac30$µg_chlc_cm2~napirr_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
napirr_pocacu13<-phytreizem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu13$µg_chlc_cm2~napirr_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
napirr_pocacu30<-phytrentem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu30$µg_chlc_cm2~napirr_pocacu30$esp)
montest
#-->ns

###################################pav cac#########################
#Poc acu a 13m 
pavcac_pocacu13<-phytreizem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu13$µg_chlc_cm2~pavcac_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
pavcac_pocacu30<-phytrentem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu30$µg_chlc_cm2~pavcac_pocacu30$esp)
montest
#-->ns

####################################################################
###########Comparaison chlo c/zoox selon les pep ############
#####################################################################

#filtre acrhya
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
montest=wilcox.test(acrhya$µg_chlc_zoox~acrhya$pep)
montest
#W = 3, p-value = 0.05556
#--> quasi S
kruskal.test(acrhya$µg_chlc_zoox~acrhya$pep)
#Kruskal-Wallis chi-squared = 3.9382, df = 1, p-value = 0.0472
#significatif

#filtre garpla
garpla<-data_physio_R  %>% filter(esp=="GAR PLA")
montest=wilcox.test(garpla$µg_chlc_zoox~garpla$pep)
montest
#--> NS
kruskal.test(garpla$µg_chlc_zoox~garpla$pep)

#filtre napirr
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
montest=wilcox.test(napirr$µg_chlc_zoox~napirr$pep)
montest
#--> ns
kruskal.test(napirr$µg_chlc_zoox~napirr$pep)

#filtre pavcac
pavcac<-data_physio_R  %>% filter(esp=="PAV CAC")
montest=wilcox.test(pavcac$µg_chlc_zoox~pavcac$pep)
montest
#--> NS
kruskal.test(pavcac$µg_chlc_zoox~pavcac$pep)

#filtre PHYCUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
montest=wilcox.test(phycur$µg_chlc_zoox~phycur$pep)
montest
#--> ns
kruskal.test(phycur$µg_chlc_zoox~phycur$pep)

#filtre POCACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
montest=wilcox.test(pocacu$µg_chlc_zoox~pocacu$pep)
montest
#W = 28, p-value = 0.006061
#--> SIGNIFICATIF
kruskal.test(pocacu$µg_chlc_zoox~pocacu$pep)
#Kruskal-Wallis chi-squared = 7, df = 1, p-value = 0.008151


#filtre POR RUS
porrus<-data_physio_R  %>% filter(esp=="POR RUS")
montest=wilcox.test(porrus$µg_chlc_zoox~porrus$pep)
montest
#--> NS
kruskal.test(porrus$µg_chlc_zoox~porrus$pep)

############### COMPARAISON ENTRE ESP AU SEIN DE CHAQUE PEP #########################
###############################################################
###filtre
phytrentem<-physio  %>% filter(pep=="30m")
phytreizem<-physio  %>% filter(pep=="13m")

###############chla par zoox#########################
#####################ACR HYA##################################

#GAR PLA a 13m 
acrhya_garpla13<-phytreizem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla13$µg_chla_zoox~acrhya_garpla13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
acrhya_garpla30<-phytrentem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla30$µg_chla_zoox~acrhya_garpla30$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chla_zoox~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
acrhya_napirr13<-phytreizem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr13$µg_chla_zoox~acrhya_napirr13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chla_zoox~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
acrhya_napirr30<-phytrentem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr30$µg_chla_zoox~acrhya_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
acrhya_pavcac13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac13$µg_chla_zoox~acrhya_pavcac13$esp)
montest
#-->W = 25, p-value = 0.007937
#SIGNIFICATIF
 

#PAV CAC a 30m 
acrhya_pavcac30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac30$µg_chla_zoox~acrhya_pavcac30$esp)
montest
#-->ns
kruskal.test(acrhya_pavcac30$µg_chla_zoox~acrhya_pavcac30$esp)
#Kruskal-Wallis chi-squared = 3.9382, df = 1, p-value = 0.0472
#SIGNIFICATIF

#PHY CUR a 13m 
acrhya_phycur13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur13$µg_chla_zoox~acrhya_phycur13$esp)
montest
#-->NS


#PHY CUR a 30m 
acrhya_phycur30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur30$µg_chla_zoox~acrhya_phycur30$esp)
montest
#-->NS
kruskal.test(acrhya_phycur13$µg_chla_zoox~acrhya_phycur13$esp)
#--> ns 

#Poc acu a 13m 
acrhya_pocacu13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu13$µg_chla_zoox~acrhya_pocacu13$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chla_zoox~acrhya_pocacu13$esp)
#--> ns

#POC ACU a 30m 
acrhya_pocacu30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu30$µg_chla_zoox~acrhya_pocacu30$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chla_zoox~acrhya_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
acrhya_porrus13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus13$µg_chla_zoox~acrhya_porrus13$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$µg_chla_zoox~acrhya_porrus13$esp)
#--> ns

#POR RUS a 30m 
acrhya_porrus30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus30$µg_chla_zoox~acrhya_porrus30$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$µg_chla_zoox~acrhya_porrus13$esp)
#--> ns 

#############################Phy cur########################################


#GAR PLA a 13m 
phycur_garpla13<-phytreizem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla13$µg_chla_zoox~phycur_garpla13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chla_zoox~phycur_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
phycur_garpla30<-phytrentem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla30$µg_chla_zoox~phycur_garpla30$esp)
montest
#-->W = 24, p-value = 0.002422
#SIGNIFICATIF
kruskal.test(phycur_garpla13$µg_chla_zoox~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
phycur_napirr13<-phytreizem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr13$µg_chla_zoox~phycur_napirr13$esp)
montest
#-->W = 13, p-value = 0.01554
#SIGNIFICATIF
kruskal.test(phycur_garpla13$µg_chla_zoox~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
phycur_napirr30<-phytrentem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr30$µg_chla_zoox~phycur_napirr30$esp)
montest
#-->NS

#PAV CAC a 13m 
phycur_pavcac13<-phytreizem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac13$µg_chla_zoox~phycur_pavcac13$esp)
montest
#-->W = 1, p-value = 0.001332
#significatif

#PAV CAC a 30m 
phycur_pavcac30<-phytrentem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac30$µg_chla_zoox~phycur_pavcac30$esp)
montest
#-->W = 0, p-value = 0.0002334
#significatif

#Poc acu a 13m 
phycur_pocacu13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu13$µg_chla_zoox~phycur_pocacu13$esp)
montest
#-->W = 56, p-value = 0.04309
#significatif
kruskal.test(phycur_pocacu13$µg_chla_zoox~phycur_pocacu13$esp)


#POC ACU a 30m 
phycur_pocacu30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu30$µg_chla_zoox~phycur_pocacu30$esp)
montest
#-->NS
kruskal.test(phycur_pocacu30$µg_chla_zoox~phycur_pocacu30$esp)
#--> ns 

#POR RUS a 13m 
phycur_porrus13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus13$µg_chla_zoox~phycur_porrus13$esp)
montest
#-->ns
kruskal.test(phycur_porrus13$µg_chla_zoox~phycur_porrus13$esp)
#--> ns

#POR RUS a 30m 
phycur_porrus30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus30$µg_chla_zoox~phycur_porrus30$esp)
montest
#-->ns
#--> ns 

#############################POR RUS########################################


#GAR PLA a 13m 
porrus_garpla13<-phytreizem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla13$µg_chla_zoox~porrus_garpla13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chla_zoox~porrus_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
porrus_garpla30<-phytrentem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla30$µg_chla_zoox~porrus_garpla30$esp)
montest
#-->NS
kruskal.test(porrus_garpla30$µg_chla_zoox~porrus_garpla30$esp)
#--> ns 

#NAP IRR a 13m 
porrus_napirr13<-phytreizem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr13$µg_chla_zoox~porrus_napirr13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chla_zoox~porrus_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
porrus_napirr30<-phytrentem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr30$µg_chla_zoox~porrus_napirr30$esp)
montest
#-->NS

#PAV CAC a 13m 
porrus_pavcac13<-phytreizem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac13$µg_chla_zoox~porrus_pavcac13$esp)
montest
#-->W = 0, p-value = 0.004329
#SIGNIFICATIF

#PAV CAC a 30m 
porrus_pavcac30<-phytrentem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac30$µg_chla_zoox~porrus_pavcac30$esp)
montest
#-->W = 0, p-value = 0.000172
#significatif

#Poc acu a 13m 
porrus_pocacu13<-phytreizem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu13$µg_chla_zoox~porrus_pocacu13$esp)
montest
#-->ns
kruskal.test(porrus_pocacu13$µg_chla_zoox~porrus_pocacu13$esp)
#--> ns

#POC ACU a 30m 
porrus_pocacu30<-phytrentem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu30$µg_chla_zoox~porrus_pocacu30$esp)
montest
#-->NS
kruskal.test(porrus_pocacu13$µg_chla_zoox~porrus_pocacu13$esp)
#--> ns 

#############################GAR PLA########################################

#NAP IRR a 13m 
garpla_napirr13<-phytreizem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr13$µg_chla_zoox~garpla_napirr13$esp)
montest
#-->ns


#NAP IRR a 30m 
garpla_napirr30<-phytrentem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr30$µg_chla_zoox~garpla_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
garpla_pavcac13<-phytreizem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac13$µg_chla_zoox~garpla_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
garpla_pavcac30<-phytrentem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac30$µg_chla_zoox~garpla_pavcac30$esp)
montest
#-->W = 57, p-value = 0.002262
#SIGNIFICATIF

#Poc acu a 13m 
garpla_pocacu13<-phytreizem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu13$µg_chla_zoox~garpla_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
garpla_pocacu30<-phytrentem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu30$µg_chla_zoox~garpla_pocacu30$esp)
montest
#-->ns 


#############################NAP IRR########################################


#PAV CAC a 13m 
napirr_pavcac13<-phytreizem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac13$µg_chla_zoox~napirr_pavcac13$esp)
montest
#-->W = 37, p-value = 0.01088
#SIGNIFICATIF

#PAV CAC a 30m 
napirr_pavcac30<-phytrentem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac30$µg_chla_zoox~napirr_pavcac30$esp)
montest
#-->W = 40, p-value = 0.01898
#SIGNIFICATIF

#Poc acu a 13m 
napirr_pocacu13<-phytreizem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu13$µg_chla_zoox~napirr_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
napirr_pocacu30<-phytrentem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu30$µg_chla_zoox~napirr_pocacu30$esp)
montest
#-->ns

###################################pav cac#########################
#Poc acu a 13m 
pavcac_pocacu13<-phytreizem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu13$µg_chla_zoox~pavcac_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
pavcac_pocacu30<-phytrentem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu30$µg_chla_zoox~pavcac_pocacu30$esp)
montest
#-->W = 0, p-value = 0.01587
#SIGNIFICATIF

############### COMPARAISON ENTRE ESP AU SEIN DE CHAQUE PEP #########################
###############################################################
###filtre
phytrentem<-physio  %>% filter(pep=="30m")
phytreizem<-physio  %>% filter(pep=="13m")

###############chlc par zoox#########################
#####################ACR HYA##################################

#GAR PLA a 13m 
acrhya_garpla13<-phytreizem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla13$µg_chlc_zoox~acrhya_garpla13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
acrhya_garpla30<-phytrentem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla30$µg_chlc_zoox~acrhya_garpla30$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_zoox~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
acrhya_napirr13<-phytreizem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr13$µg_chlc_zoox~acrhya_napirr13$esp)
montest
#-->W = 2, p-value = 0.006216
#signif
kruskal.test(acrhya_garpla13$µg_chlc_zoox~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
acrhya_napirr30<-phytrentem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr30$µg_chlc_zoox~acrhya_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
acrhya_pavcac13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac13$µg_chlc_zoox~acrhya_pavcac13$esp)
montest
#-->ns


#PAV CAC a 30m 
acrhya_pavcac30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac30$µg_chlc_zoox~acrhya_pavcac30$esp)
montest
#-->ns
kruskal.test(acrhya_pavcac30$µg_chlc_zoox~acrhya_pavcac30$esp)
#Kruskal-Wallis chi-squared = 3.9382, df = 1, p-value = 0.0472
#SIGNIFICATIF

#PHY CUR a 13m 
acrhya_phycur13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur13$µg_chlc_zoox~acrhya_phycur13$esp)
montest
#-->W = 4, p-value = 0.007992
#SIGNIFICATIF


#PHY CUR a 30m 
acrhya_phycur30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur30$µg_chlc_zoox~acrhya_phycur30$esp)
montest
#-->NS
kruskal.test(acrhya_phycur13$µg_chlc_zoox~acrhya_phycur13$esp)
#--> ns 

#Poc acu a 13m 
acrhya_pocacu13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu13$µg_chlc_zoox~acrhya_pocacu13$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chlc_zoox~acrhya_pocacu13$esp)
#--> ns

#POC ACU a 30m 
acrhya_pocacu30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu30$µg_chlc_zoox~acrhya_pocacu30$esp)
montest
#-->W = 20, p-value = 0.01587
#SIGNIFICATIF
kruskal.test(acrhya_pocacu13$µg_chlc_zoox~acrhya_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
acrhya_porrus13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus13$µg_chlc_zoox~acrhya_porrus13$esp)
montest
#-->W = 0, p-value = 0.004329
#signif
kruskal.test(acrhya_porrus13$µg_chlc_zoox~acrhya_porrus13$esp)
#--> ns

#POR RUS a 30m 
acrhya_porrus30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus30$µg_chlc_zoox~acrhya_porrus30$esp)
montest
#-->ns
kruskal.test(acrhya_porrus30$µg_chlc_zoox~acrhya_porrus30$esp)
#--> ns 

#############################Phy cur########################################


#GAR PLA a 13m 
phycur_garpla13<-phytreizem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla13$µg_chlc_zoox~phycur_garpla13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chlc_zoox~phycur_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
phycur_garpla30<-phytrentem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla30$µg_chlc_zoox~phycur_garpla30$esp)
montest
#-->W = 41, p-value = 0.04571
#SIGNIFICATIF
kruskal.test(phycur_garpla13$µg_chlc_zoox~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
phycur_napirr13<-phytreizem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr13$µg_chlc_zoox~phycur_napirr13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chlc_zoox~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
phycur_napirr30<-phytrentem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr30$µg_chlc_zoox~phycur_napirr30$esp)
montest
#-->NS

#PAV CAC a 13m 
phycur_pavcac13<-phytreizem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac13$µg_chlc_zoox~phycur_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
phycur_pavcac30<-phytrentem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac30$µg_chlc_zoox~phycur_pavcac30$esp)
montest
#-->W = 12, p-value = 0.04599
#significatif

#Poc acu a 13m 
phycur_pocacu13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu13$µg_chlc_zoox~phycur_pocacu13$esp)
montest
#-->ns
kruskal.test(phycur_pocacu13$µg_chlc_zoox~phycur_pocacu13$esp)


#POC ACU a 30m 
phycur_pocacu30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu30$µg_chlc_zoox~phycur_pocacu30$esp)
montest
#-->W = 49, p-value = 0.005882
#significatif
kruskal.test(phycur_pocacu30$µg_chlc_zoox~phycur_pocacu30$esp)
#--> ns 

#POR RUS a 13m 
phycur_porrus13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus13$µg_chlc_zoox~phycur_porrus13$esp)
montest
#-->ns
kruskal.test(phycur_porrus13$µg_chlc_zoox~phycur_porrus13$esp)
#--> ns

#POR RUS a 30m 
phycur_porrus30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus30$µg_chlc_zoox~phycur_porrus30$esp)
montest
#-->W = 48, p-value = 0.03766
#--> signif 

#############################POR RUS########################################


#GAR PLA a 13m 
porrus_garpla13<-phytreizem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla13$µg_chlc_zoox~porrus_garpla13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chlc_zoox~porrus_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
porrus_garpla30<-phytrentem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla30$µg_chlc_zoox~porrus_garpla30$esp)
montest
#-->W = 13, p-value = 7.704e-05
#signif
kruskal.test(porrus_garpla30$µg_chlc_zoox~porrus_garpla30$esp)
#--> ns 

#NAP IRR a 13m 
porrus_napirr13<-phytreizem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr13$µg_chlc_zoox~porrus_napirr13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chlc_zoox~porrus_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
porrus_napirr30<-phytrentem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr30$µg_chlc_zoox~porrus_napirr30$esp)
montest
#-->W = 29, p-value = 0.03275
#signif

#PAV CAC a 13m 
porrus_pavcac13<-phytreizem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac13$µg_chlc_zoox~porrus_pavcac13$esp)
montest
#-->W = 3, p-value = 0.0303
#SIGNIFICATIF

#PAV CAC a 30m 
porrus_pavcac30<-phytrentem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac30$µg_chlc_zoox~porrus_pavcac30$esp)
montest
#-->W = 2, p-value = 0.000688
#significatif

#Poc acu a 13m 
porrus_pocacu13<-phytreizem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu13$µg_chlc_zoox~porrus_pocacu13$esp)
montest
#-->ns
kruskal.test(porrus_pocacu13$µg_chlc_zoox~porrus_pocacu13$esp)
#--> ns

#POC ACU a 30m 
porrus_pocacu30<-phytrentem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu30$µg_chlc_zoox~porrus_pocacu30$esp)
montest
#-->W = 0, p-value = 0.0006536
#signif
kruskal.test(porrus_pocacu13$µg_chlc_zoox~porrus_pocacu13$esp)
#--> ns 

#############################GAR PLA########################################

#NAP IRR a 13m 
garpla_napirr13<-phytreizem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr13$µg_chlc_zoox~garpla_napirr13$esp)
montest
#-->ns


#NAP IRR a 30m 
garpla_napirr30<-phytrentem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr30$µg_chlc_zoox~garpla_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
garpla_pavcac13<-phytreizem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac13$µg_chlc_zoox~garpla_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
garpla_pavcac30<-phytrentem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac30$µg_chlc_zoox~garpla_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
garpla_pocacu13<-phytreizem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu13$µg_chlc_zoox~garpla_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
garpla_pocacu30<-phytrentem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu30$µg_chlc_zoox~garpla_pocacu30$esp)
montest
#-->ns 
kruskal.test(garpla_pocacu30$µg_chlc_zoox~garpla_pocacu30$esp)
#-->ns 


#############################NAP IRR########################################


#PAV CAC a 13m 
napirr_pavcac13<-phytreizem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac13$µg_chlc_zoox~napirr_pavcac13$esp)
montest
#-->W = 34, p-value = 0.04507
#SIGNIFICATIF

#PAV CAC a 30m 
napirr_pavcac30<-phytrentem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac30$µg_chlc_zoox~napirr_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
napirr_pocacu13<-phytreizem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu13$µg_chlc_zoox~napirr_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
napirr_pocacu30<-phytrentem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu30$µg_chlc_zoox~napirr_pocacu30$esp)
montest
#-->W = 32, p-value = 0.03357
#signif

###################################pav cac#########################
#Poc acu a 13m 
pavcac_pocacu13<-phytreizem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu13$µg_chlc_zoox~pavcac_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
pavcac_pocacu30<-phytrentem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu30$µg_chlc_zoox~pavcac_pocacu30$esp)
montest
#-->ns


############### COMPARAISON ENTRE ESP AU SEIN DE CHAQUE PEP #########################
###############################################################
###filtre
phytrentem<-physio  %>% filter(pep=="30m")
phytreizem<-physio  %>% filter(pep=="13m")

###############zoox par cm²#########################
#####################ACR HYA##################################

#GAR PLA a 13m 
acrhya_garpla13<-phytreizem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla13$zoox_cm2~acrhya_garpla13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chlc_cm2~acrhya_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
acrhya_garpla30<-phytrentem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla30$zoox_cm2~acrhya_garpla30$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$zoox_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
acrhya_napirr13<-phytreizem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr13$zoox_cm2~acrhya_napirr13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$zoox_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
acrhya_napirr30<-phytrentem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr30$zoox_cm2~acrhya_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
acrhya_pavcac13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac13$zoox_cm2~acrhya_pavcac13$esp)
montest
#-->ns


#PAV CAC a 30m 
acrhya_pavcac30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac30$zoox_cm2~acrhya_pavcac30$esp)
montest
#-->ns
kruskal.test(acrhya_pavcac30$zoox_cm2~acrhya_pavcac30$esp)
#ns

#PHY CUR a 13m 
acrhya_phycur13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur13$zoox_cm2~acrhya_phycur13$esp)
montest
#-->ns


#PHY CUR a 30m 
acrhya_phycur30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur30$zoox_cm2~acrhya_phycur30$esp)
montest
#-->NS
kruskal.test(acrhya_phycur13$zoox_cm2~acrhya_phycur13$esp)
#--> ns 

#Poc acu a 13m 
acrhya_pocacu13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu13$zoox_cm2~acrhya_pocacu13$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$zoox_cm2~acrhya_pocacu13$esp)
#--> ns

#POC ACU a 30m 
acrhya_pocacu30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu30$zoox_cm2~acrhya_pocacu30$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$zoox_cm2~acrhya_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
acrhya_porrus13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus13$zoox_cm2~acrhya_porrus13$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$zoox_cm2~acrhya_porrus13$esp)
#--> ns

#POR RUS a 30m 
acrhya_porrus30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus30$zoox_cm2~acrhya_porrus30$esp)
montest
#-->ns
kruskal.test(acrhya_porrus30$zoox_cm2~acrhya_porrus30$esp)
#--> ns 

#############################Phy cur########################################


#GAR PLA a 13m 
phycur_garpla13<-phytreizem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla13$zoox_cm2~phycur_garpla13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$zoox_cm2~phycur_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
phycur_garpla30<-phytrentem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla30$zoox_cm2~phycur_garpla30$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$zoox_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
phycur_napirr13<-phytreizem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr13$zoox_cm2~phycur_napirr13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$zoox_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
phycur_napirr30<-phytrentem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr30$zoox_cm2~phycur_napirr30$esp)
montest
#-->NS

#PAV CAC a 13m 
phycur_pavcac13<-phytreizem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac13$zoox_cm2~phycur_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
phycur_pavcac30<-phytrentem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac30$zoox_cm2~phycur_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
phycur_pocacu13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu13$zoox_cm2~phycur_pocacu13$esp)
montest
#-->ns
kruskal.test(phycur_pocacu13$zoox_cm2~phycur_pocacu13$esp)


#POC ACU a 30m 
phycur_pocacu30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu30$zoox_cm2~phycur_pocacu30$esp)
montest
#-->ns
kruskal.test(phycur_pocacu30$zoox_cm2~phycur_pocacu30$esp)
#--> ns 

#POR RUS a 13m 
phycur_porrus13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus13$zoox_cm2~phycur_porrus13$esp)
montest
#-->W = 8, p-value = 0.01598
#signif
kruskal.test(phycur_porrus13$zoox_cm2~phycur_porrus13$esp)
#--> ns

#POR RUS a 30m 
phycur_porrus30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus30$zoox_cm2~phycur_porrus30$esp)
montest
#-->ns 

#############################POR RUS########################################


#GAR PLA a 13m 
porrus_garpla13<-phytreizem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla13$zoox_cm2~porrus_garpla13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$zoox_cm2~porrus_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
porrus_garpla30<-phytrentem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla30$zoox_cm2~porrus_garpla30$esp)
montest
#-->ns
kruskal.test(porrus_garpla30$zoox_cm2~porrus_garpla30$esp)
#--> ns 

#NAP IRR a 13m 
porrus_napirr13<-phytreizem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr13$zoox_cm2~porrus_napirr13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$zoox_cm2~porrus_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
porrus_napirr30<-phytrentem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr30$zoox_cm2~porrus_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
porrus_pavcac13<-phytreizem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac13$zoox_cm2~porrus_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
porrus_pavcac30<-phytrentem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac30$zoox_cm2~porrus_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
porrus_pocacu13<-phytreizem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu13$zoox_cm2~porrus_pocacu13$esp)
montest
#-->ns
kruskal.test(porrus_pocacu13$zoox_cm2~porrus_pocacu13$esp)
#--> ns

#POC ACU a 30m 
porrus_pocacu30<-phytrentem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu30$zoox_cm2~porrus_pocacu30$esp)
montest
#-->W = 0, p-value = 0.0006536
#signif
kruskal.test(porrus_pocacu13$zoox_cm2~porrus_pocacu13$esp)
#--> ns 

#############################GAR PLA########################################

#NAP IRR a 13m 
garpla_napirr13<-phytreizem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr13$zoox_cm2~garpla_napirr13$esp)
montest
#-->ns


#NAP IRR a 30m 
garpla_napirr30<-phytrentem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr30$zoox_cm2~garpla_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
garpla_pavcac13<-phytreizem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac13$zoox_cm2~garpla_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
garpla_pavcac30<-phytrentem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac30$zoox_cm2~garpla_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
garpla_pocacu13<-phytreizem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu13$zoox_cm2~garpla_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
garpla_pocacu30<-phytrentem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu30$zoox_cm2~garpla_pocacu30$esp)
montest
#-->ns 
kruskal.test(garpla_pocacu30$zoox_cm2~garpla_pocacu30$esp)
#-->ns 


#############################NAP IRR########################################


#PAV CAC a 13m 
napirr_pavcac13<-phytreizem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac13$zoox_cm2~napirr_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
napirr_pavcac30<-phytrentem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac30$zoox_cm2~napirr_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
napirr_pocacu13<-phytreizem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu13$zoox_cm2~napirr_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
napirr_pocacu30<-phytrentem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu30$zoox_cm2~napirr_pocacu30$esp)
montest
#-->ns

###################################pav cac#########################
#Poc acu a 13m 
pavcac_pocacu13<-phytreizem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu13$zoox_cm2~pavcac_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
pavcac_pocacu30<-phytrentem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu30$zoox_cm2~pavcac_pocacu30$esp)
montest
#-->ns

####################################################################
###########Comparaison zoox par cm² selon les pep ############
#####################################################################

#filtre acrhya
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
montest=wilcox.test(acrhya$zoox_cm2~acrhya$pep)
montest
#ns
kruskal.test(acrhya$zoox_cm2~acrhya$pep)


#filtre garpla
garpla<-data_physio_R  %>% filter(esp=="GAR PLA")
montest=wilcox.test(garpla$zoox_cm2~garpla$pep)
montest
#--> NS
kruskal.test(garpla$zoox_cm2~garpla$pep)

#filtre napirr
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
montest=wilcox.test(napirr$zoox_cm2~napirr$pep)
montest
#--> ns
kruskal.test(napirr$zoox_cm2~napirr$pep)

#filtre pavcac
pavcac<-data_physio_R  %>% filter(esp=="PAV CAC")
montest=wilcox.test(pavcac$zoox_cm2~pavcac$pep)
montest
#--> NS
kruskal.test(pavcac$zoox_cm2~pavcac$pep)

#filtre PHYCUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
montest=wilcox.test(phycur$zoox_cm2~phycur$pep)
montest
#--> quasi significatif
kruskal.test(phycur$zoox_cm2~phycur$pep)
#--> quasi significatif

#filtre POCACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
montest=wilcox.test(pocacu$zoox_cm2~pocacu$pep)
montest
#ns
kruskal.test(pocacu$zoox_cm2~pocacu$pep)
#Kruskal-Wallis chi-squared = 7, df = 1, p-value = 0.008151


#filtre POR RUS
porrus<-data_physio_R  %>% filter(esp=="POR RUS")
montest=wilcox.test(porrus$zoox_cm2~porrus$pep)
montest
#--> NS
kruskal.test(porrus$zoox_cm2~porrus$pep)

############### COMPARAISON ENTRE ESP AU SEIN DE CHAQUE PEP #########################
###############################################################
###filtre
phytrentem<-physio  %>% filter(pep=="30m")
phytreizem<-physio  %>% filter(pep=="13m")

###############chla cm²#########################
#####################ACR HYA##################################

#GAR PLA a 13m 
acrhya_garpla13<-phytreizem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla13$µg_chla_cm2~acrhya_garpla13$esp)
montest
#-->ns
#--> ns 

#GAR PLA a 30m 
acrhya_garpla30<-phytrentem %>% filter(esp%in% c("ACR HYA", "GAR PLA"))
montest=wilcox.test(acrhya_garpla30$µg_chla_cm2~acrhya_garpla30$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chla_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
acrhya_napirr13<-phytreizem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr13$µg_chla_cm2~acrhya_napirr13$esp)
montest
#-->ns
kruskal.test(acrhya_garpla13$µg_chla_cm2~acrhya_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
acrhya_napirr30<-phytrentem %>% filter(esp%in% c("ACR HYA", "NAP IRR"))
montest=wilcox.test(acrhya_napirr30$µg_chla_cm2~acrhya_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
acrhya_pavcac13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac13$µg_chla_cm2~acrhya_pavcac13$esp)
montest
#-->ns


#PAV CAC a 30m 
acrhya_pavcac30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PAV CAC"))
montest=wilcox.test(acrhya_pavcac30$µg_chla_cm2~acrhya_pavcac30$esp)
montest
#-->ns
kruskal.test(acrhya_pavcac30$µg_chla_cm2~acrhya_pavcac30$esp)
#ns

#PHY CUR a 13m 
acrhya_phycur13<-phytreizem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur13$µg_chla_cm2~acrhya_phycur13$esp)
montest
#-->ns


#PHY CUR a 30m 
acrhya_phycur30<-phytrentem %>% filter(esp%in% c("ACR HYA", "PHY CUR"))
montest=wilcox.test(acrhya_phycur30$µg_chla_cm2~acrhya_phycur30$esp)
montest
#-->NS
#--> ns 

#Poc acu a 13m 
acrhya_pocacu13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu13$µg_chla_cm2~acrhya_pocacu13$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chla_cm2~acrhya_pocacu13$esp)
#--> ns

#POC ACU a 30m 
acrhya_pocacu30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POC ACU"))
montest=wilcox.test(acrhya_pocacu30$µg_chla_cm2~acrhya_pocacu30$esp)
montest
#-->ns
kruskal.test(acrhya_pocacu13$µg_chla_cm2~acrhya_pocacu13$esp)
#--> ns 

#POR RUS a 13m 
acrhya_porrus13<-phytreizem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus13$µg_chla_cm2~acrhya_porrus13$esp)
montest
#-->ns
kruskal.test(acrhya_porrus13$µg_chla_cm2~acrhya_porrus13$esp)
#--> ns

#POR RUS a 30m 
acrhya_porrus30<-phytrentem %>% filter(esp%in% c("ACR HYA", "POR RUS"))
montest=wilcox.test(acrhya_porrus30$µg_chla_cm2~acrhya_porrus30$esp)
montest
#-->ns
kruskal.test(acrhya_porrus30$µg_chla_cm2~acrhya_porrus30$esp)
#--> ns 

#############################Phy cur########################################


#GAR PLA a 13m 
phycur_garpla13<-phytreizem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla13$µg_chla_cm2~phycur_garpla13$esp)
montest
#-->ns
kruskal.test(phycur_garpla13$µg_chla_cm2~phycur_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
phycur_garpla30<-phytrentem %>% filter(esp%in% c("PHY CUR", "GAR PLA"))
montest=wilcox.test(phycur_garpla30$µg_chla_cm2~phycur_garpla30$esp)
montest
#-->W = 38, p-value = 0.02982
#significatif
kruskal.test(phycur_garpla13$µg_chla_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 13m 
phycur_napirr13<-phytreizem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr13$µg_chla_cm2~phycur_napirr13$esp)
montest
#-->W = 12, p-value = 0.01166
#significatif
kruskal.test(phycur_garpla13$µg_chla_cm2~phycur_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
phycur_napirr30<-phytrentem %>% filter(esp%in% c("PHY CUR", "NAP IRR"))
montest=wilcox.test(phycur_napirr30$µg_chla_cm2~phycur_napirr30$esp)
montest
#-->W = 9, p-value = 0.00039
#signifi

#PAV CAC a 13m 
phycur_pavcac13<-phytreizem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac13$µg_chla_cm2~phycur_pavcac13$esp)
montest
#-->W = 2, p-value = 0.002664
#signif

#PAV CAC a 30m 
phycur_pavcac30<-phytrentem %>% filter(esp%in% c("PHY CUR", "PAV CAC"))
montest=wilcox.test(phycur_pavcac30$µg_chla_cm2~phycur_pavcac30$esp)
montest
#-->W = 2, p-value = 0.0009337
#signi

#Poc acu a 13m 
phycur_pocacu13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu13$µg_chla_cm2~phycur_pocacu13$esp)
montest
#-->W = 55, p-value = 0.05533
#quasi S
kruskal.test(phycur_pocacu13$µg_chla_cm2~phycur_pocacu13$esp)
#Kruskal-Wallis chi-squared = 3.8095, df = 1, p-value = 0.05096
#sign

#POC ACU a 30m 
phycur_pocacu30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POC ACU"))
montest=wilcox.test(phycur_pocacu30$µg_chla_cm2~phycur_pocacu30$esp)
montest
#-->ns
kruskal.test(phycur_pocacu30$µg_chla_cm2~phycur_pocacu30$esp)
#--> ns 

#POR RUS a 13m 
phycur_porrus13<-phytreizem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus13$µg_chla_cm2~phycur_porrus13$esp)
montest
#-->W = 49, p-value = 0.04196
#signif
kruskal.test(phycur_porrus13$µg_chla_cm2~phycur_porrus13$esp)
#--> ns

#POR RUS a 30m 
phycur_porrus30<-phytrentem %>% filter(esp%in% c("PHY CUR", "POR RUS"))
montest=wilcox.test(phycur_porrus30$µg_chla_cm2~phycur_porrus30$esp)
montest
#-->W = 158, p-value = 0.0006612
#signif

#############################POR RUS########################################


#GAR PLA a 13m 
porrus_garpla13<-phytreizem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla13$µg_chla_cm2~porrus_garpla13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chla_cm2~porrus_garpla13$esp)
#--> ns 

#GAR PLA a 30m 
porrus_garpla30<-phytrentem %>% filter(esp%in% c("POR RUS", "GAR PLA"))
montest=wilcox.test(porrus_garpla30$µg_chla_cm2~porrus_garpla30$esp)
montest
#-->W = 123, p-value = 0.04635
#signif
kruskal.test(porrus_garpla30$µg_chla_cm2~porrus_garpla30$esp)
#--> ns 

#NAP IRR a 13m 
porrus_napirr13<-phytreizem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr13$µg_chla_cm2~porrus_napirr13$esp)
montest
#-->ns
kruskal.test(porrus_garpla13$µg_chla_cm2~porrus_garpla13$esp)
#--> ns 

#NAP IRR a 30m 
porrus_napirr30<-phytrentem %>% filter(esp%in% c("POR RUS", "NAP IRR"))
montest=wilcox.test(porrus_napirr30$µg_chla_cm2~porrus_napirr30$esp)
montest
#-->ns

#PAV CAC a 13m 
porrus_pavcac13<-phytreizem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac13$µg_chla_cm2~porrus_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
porrus_pavcac30<-phytrentem %>% filter(esp%in% c("POR RUS", "PAV CAC"))
montest=wilcox.test(porrus_pavcac30$µg_chla_cm2~porrus_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
porrus_pocacu13<-phytreizem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu13$µg_chla_cm2~porrus_pocacu13$esp)
montest
#-->ns
kruskal.test(porrus_pocacu13$µg_chla_cm2~porrus_pocacu13$esp)
#--> ns

#POC ACU a 30m 
porrus_pocacu30<-phytrentem %>% filter(esp%in% c("POR RUS", "POC ACU"))
montest=wilcox.test(porrus_pocacu30$µg_chla_cm2~porrus_pocacu30$esp)
montest
#-->W = 0, p-value = 0.0006536
#signif
kruskal.test(porrus_pocacu13$µg_chla_cm2~porrus_pocacu13$esp)
#--> ns 

#############################GAR PLA########################################

#NAP IRR a 13m 
garpla_napirr13<-phytreizem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr13$µg_chla_cm2~garpla_napirr13$esp)
montest
#-->ns


#NAP IRR a 30m 
garpla_napirr30<-phytrentem %>% filter(esp%in% c("GAR PLA", "NAP IRR"))
montest=wilcox.test(garpla_napirr30$µg_chla_cm2~garpla_napirr30$esp)
montest
#-->W = 87, p-value = 0.01839
#signi

#PAV CAC a 13m 
garpla_pavcac13<-phytreizem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac13$µg_chla_cm2~garpla_pavcac13$esp)
montest
#-->W = 38, p-value = 0.006216
#signi

#PAV CAC a 30m 
garpla_pavcac30<-phytrentem %>% filter(esp%in% c("GAR PLA", "PAV CAC"))
montest=wilcox.test(garpla_pavcac30$µg_chla_cm2~garpla_pavcac30$esp)
montest
#-->W = 54, p-value = 0.009373
#signi

#Poc acu a 13m 
garpla_pocacu13<-phytreizem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu13$µg_chla_cm2~garpla_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
garpla_pocacu30<-phytrentem %>% filter(esp%in% c("GAR PLA", "POC ACU"))
montest=wilcox.test(garpla_pocacu30$µg_chla_cm2~garpla_pocacu30$esp)
montest
#-->ns 
kruskal.test(garpla_pocacu30$µg_chla_cm2~garpla_pocacu30$esp)
#-->ns 


#############################NAP IRR########################################


#PAV CAC a 13m 
napirr_pavcac13<-phytreizem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac13$µg_chla_cm2~napirr_pavcac13$esp)
montest
#-->ns

#PAV CAC a 30m 
napirr_pavcac30<-phytrentem %>% filter(esp%in% c("NAP IRR", "PAV CAC"))
montest=wilcox.test(napirr_pavcac30$µg_chla_cm2~napirr_pavcac30$esp)
montest
#-->ns

#Poc acu a 13m 
napirr_pocacu13<-phytreizem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu13$µg_chla_cm2~napirr_pocacu13$esp)
montest
#-->ns


#POC ACU a 30m 
napirr_pocacu30<-phytrentem %>% filter(esp%in% c("NAP IRR", "POC ACU"))
montest=wilcox.test(napirr_pocacu30$µg_chla_cm2~napirr_pocacu30$esp)
montest
#-->ns

###################################pav cac#########################
#Poc acu a 13m 
pavcac_pocacu13<-phytreizem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu13$µg_chla_cm2~pavcac_pocacu13$esp)
montest
#-->W = 4, p-value = 0.0303
#signi


#POC ACU a 30m 
pavcac_pocacu30<-phytrentem %>% filter(esp%in% c("PAV CAC", "POC ACU"))
montest=wilcox.test(pavcac_pocacu30$µg_chla_cm2~pavcac_pocacu30$esp)
montest
#-->ns


####################################################################
###########Comparaison chla par cm² selon les pep ############
#####################################################################

#filtre acrhya
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
montest=wilcox.test(acrhya$µg_chla_cm2~acrhya$pep)
montest
#ns
kruskal.test(acrhya$µg_chla_cm2~acrhya$pep)


#filtre garpla
garpla<-data_physio_R  %>% filter(esp=="GAR PLA")
montest=wilcox.test(garpla$µg_chla_cm2~garpla$pep)
montest
#--> NS
kruskal.test(garpla$µg_chla_cm2~garpla$pep)

#filtre napirr
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
montest=wilcox.test(napirr$µg_chla_cm2~napirr$pep)
montest
#--> ns
kruskal.test(napirr$µg_chla_cm2~napirr$pep)

#filtre pavcac
pavcac<-data_physio_R  %>% filter(esp=="PAV CAC")
montest=wilcox.test(pavcac$µg_chla_cm2~pavcac$pep)
montest
#--> NS
kruskal.test(pavcac$µg_chla_cm2~pavcac$pep)

#filtre PHYCUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
montest=wilcox.test(phycur$µg_chla_cm2~phycur$pep)
montest
#--> ns
kruskal.test(phycur$µg_chla_cm2~phycur$pep)
#--> ns

#filtre POCACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
montest=wilcox.test(pocacu$µg_chla_cm2~pocacu$pep)
montest
#ns
kruskal.test(pocacu$µg_chla_cm2~pocacu$pep)


#filtre POR RUS
porrus<-data_physio_R  %>% filter(esp=="POR RUS")
montest=wilcox.test(porrus$µg_chla_cm2~porrus$pep)
montest
#--> NS
kruskal.test(porrus$µg_chla_cm2~porrus$pep)

####################################################################
###########Comparaison chlc par cm² selon les pep ############
#####################################################################

#filtre acrhya
acrhya<-data_physio_R  %>% filter(esp=="ACR HYA")
montest=wilcox.test(acrhya$µg_chlc_cm2~acrhya$pep)
montest
#ns
kruskal.test(acrhya$µg_chlc_cm2~acrhya$pep)


#filtre garpla
garpla<-data_physio_R  %>% filter(esp=="GAR PLA")
montest=wilcox.test(garpla$µg_chlc_cm2~garpla$pep)
montest
#--> NS
kruskal.test(garpla$µg_chlc_cm2~garpla$pep)

#filtre napirr
napirr<-data_physio_R  %>% filter(esp=="NAP IRR")
montest=wilcox.test(napirr$µg_chlc_cm2~napirr$pep)
montest
#--> ns
kruskal.test(napirr$µg_chlc_cm2~napirr$pep)

#filtre pavcac
pavcac<-data_physio_R  %>% filter(esp=="PAV CAC")
montest=wilcox.test(pavcac$µg_chlc_cm2~pavcac$pep)
montest
#--> NS
kruskal.test(pavcac$µg_chlc_cm2~pavcac$pep)

#filtre PHYCUR
phycur<-data_physio_R  %>% filter(esp=="PHY CUR")
montest=wilcox.test(phycur$µg_chlc_cm2~phycur$pep)
montest
#--> ns
kruskal.test(phycur$µg_chlc_cm2~phycur$pep)
#--> ns

#filtre POCACU
pocacu<-data_physio_R  %>% filter(esp=="POC ACU")
montest=wilcox.test(pocacu$µg_chlc_cm2~pocacu$pep)
montest
#ns
kruskal.test(pocacu$µg_chlc_cm2~pocacu$pep)


#filtre POR RUS
porrus<-data_physio_R  %>% filter(esp=="POR RUS")
montest=wilcox.test(porrus$µg_chlc_cm2~porrus$pep)
montest
#--> NS
kruskal.test(porrus$µg_chlc_cm2~porrus$pep)