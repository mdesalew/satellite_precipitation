
# Application of satellite and reanalysis precipitation products for 
# hydrological modeling in the data-scarce Porij˜ogi catchment, Estonia
# streamflow comparison

######################  1  ##################
# SWAT calibration results  - time series hydrographs
library(openxlsx) 
library(ggplot2)
library(dplyr)
# library(extrafont)
# library(remotes)

discharge<-read.xlsx("./streamflow_scatterplot.xlsx", sheet = "Sheet1", startRow = 1, colNames = TRUE)

ggplot(discharge, aes(x=Ref, y=Observed))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+ ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge"~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge"~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.75, se = F, col = "red")+
  annotate("text", x = 2, y = 10, label= "a) Gauged", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8, 
           label= '
           r = 0.77
           RMSE = 0.8 
           RB = - 16',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

ggplot(discharge, aes(x=Ref, y=SM2RASC))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+  ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge"~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge"~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.75, se = F, col = "red")+
  annotate("text", x = 2, y = 10, label= "b) SM2RASC", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8, 
           label= '
           r = 0.5
           RMSE = 1.14 
           RB = 19.5',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

ggplot(discharge, aes(x=Ref, y=IMERG))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+  ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge"~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge"~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red")+
  annotate("text", x = 2, y = 10, label= "c) IMERG", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8,
           label= '
           r = 0.91
           RMSE = 0.52 
           RB = -1.64',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

ggplot(discharge, aes(x=Ref, y=PERSIANN))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+  ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge"~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge"~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red")+
  annotate("text", x = 3, y = 10, label= "d) PERSIANN-CDR", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8,
           label= '
           r = 0.61
           RMSE = 1.2 
           RB = - 14.75',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

ggplot(discharge, aes(x=Ref, y=ERA))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+  ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge"~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge"~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red")+
  annotate("text", x = 2, y = 10, label= "e) ERA5", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8, 
           label= '
           r = 0.75
           RMSE = 0.89
           RB = -11.42',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

ggplot(discharge, aes(x=Ref, y=CMORPH))+
  geom_point(alpha = 0.75,shape=18, size = 2, col = "blue")+
  theme_bw()+  ylim(0,10)+ xlim(0,10)+
  theme(text= element_text(family = "Times New Roman"))+
  xlab(bquote("Gauged discharge" ~(m^3~s^-1)))+
  ylab(bquote("Simulated discharge" ~(m^3~s^-1)))+
  theme(axis.title.x = element_text(size = 12, face = 2))+
  theme(axis.title.y = element_text(size = 12, face = 2))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.80, 0.15))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(0.5, "cm"))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red")+
  annotate("text", x = 3, y = 10, label= "f) CMORPH-CRT", size = 5, color = "gray19", 
           fontface = 2, family="Times New Roman")+
  annotate("text", x = 0, y = 8,
           label= '
           r = 0.62
           RMSE = 1.07 
           RB = 32.66',
           size = 5,hjust = 0, fontface = 3, col = "black", family="Times New Roman")

###########################  2   ############################
# SWAT parameter sensitivity analysis during calibration
library(openxlsx)
library(ggplot2)
# library(matrixStats) 
# library(extrafont)
# loadfonts(device = "win")

sensitivity_cal=read.xlsx("./streamflow_ParameterSensitivity.xlsx", sheet = "Cal", 
                          startRow = 1, colNames = TRUE)

sensitivity_cal1 <- sensitivity_cal   
sensitivity_cal1$Model <- factor(sensitivity_cal1$Model, levels = c('Gauge', "SM2RASC","IMERG",
                                                                    "PERSIANN-CDR","ERA5","CMORPH-CRT"))

ggplot(sensitivity_cal1, aes(Model, CN2))+ 
  stat_boxplot(geom="errorbar", width=0.25)+
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1"))+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, ESCO))+ 
  stat_boxplot(geom="errorbar", width=0.25)+
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, ALPHA_BNK))+ 
  stat_boxplot(geom="errorbar", width=0.25)+
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, SOL_BD))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, GWQMN))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, RCHRG_DP))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))


ggplot(sensitivity_cal1, aes(Model, SOL_AWC))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, SOL_Z))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))

ggplot(sensitivity_cal1, aes(Model, CH_N2))+ 
  stat_boxplot(geom="errorbar", width=0.25)+ 
  geom_boxplot(aes(fill= factor(Model)), show.legend = F)+
  labs(x ="", y = "Parameter range")+ theme_bw()+
  scale_fill_manual(values = c('dodgerblue3', "firebrick", "gray41",
                                            "darkmagenta", "darkgreen","goldenrod1") )+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, angle = 35, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 12))







