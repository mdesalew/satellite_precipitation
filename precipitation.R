
# Application of satellite and reanalysis precipitation products for 
# hydrological modeling in the data-scarce Porij˜ogi catchment, Estonia
  # Precipitation comparisons

###### 1. Daily precipitation comparison using PDF

library(readxl) 
library(ggplot2) 
library(tidyverse)

daily_pdf<-read_excel("./daily_pdf.xlsx", sheet = "sheet1")

daily_pdf1 <- daily_pdf %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

daily_pdf2 <- daily_pdf1   # duplicating the original data
daily_pdf2$Class <- factor(daily_pdf2$Class, levels = c("<1","1-5","6-10","11-15","16-20",">20"))

ggplot() +
  geom_bar(data=filter(daily_pdf2, product %in% c("Gauge")), aes(x = Class, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+
  geom_point(data=filter(daily_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = Class, y = value,colour=product), size = 2) +
  geom_line(data=filter(daily_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = Class, y = value,colour=product, group=product), size = 1)+
  ylab("Probability")+ 
  xlab("Precipitation (mm/day)")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkmagenta",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.7, 0.5))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 10))+
  theme(legend.key.size = unit(0.5, "cm"))


## 2. Heatmaps for daily precipitation comparison
library(readxl) 
library(pheatmap)
library(viridis)

pmap<-read.xlsx("./dailyPcp_heatmap.xlsx", sheet = "pod")
fmap<-read.xlsx("./dailyPcp_heatmap.xlsx", sheet = "far")
cmap<-read.xlsx("./dailyPcp_heatmap.xlsx", sheet = "csi")

# converting the data into matrix
hpod=as.matrix(pmap[,-6])
hfar=as.matrix(fmap[,-6])
hcsi=as.matrix(cmap[,-6])

#compute heatmap
pheatmap(hpod,
         show_rownames = F,
         cluster_cols = F,
         cluster_rows = T,
         show_colnames = T,
         display_numbers = T,
         drop_levels = TRUE,
         number_color = "gray85", 
         fontsize_number = 6,
         color= inferno(10),
         border_color = NA,
         cellwidth = 40, 
         cellheight = 15,
         fontsize = 8,
         angle_col = 45,
         )

pheatmap(hfar, 
         show_rownames = F,
         cluster_cols = F,
         cluster_rows = T,
         show_colnames = T,
         display_numbers = T,
         number_color = "gray85", 
         fontsize_number = 6,
         color= inferno(25),
         border_color = NA,
         cellwidth = 40, 
         cellheight = 15,
         fontsize = 8,
         angle_col = 45,
         )

pheatmap(hcsi, 
         show_rownames = F,
         cluster_cols = F,
         cluster_rows = T,
         show_colnames = T,
         display_numbers = T,
         number_color = "gray85", 
         fontsize_number = 6,
         color= inferno(25),
         border_color = NA,
         cellwidth = 40, 
         cellheight = 15,
         fontsize = 8,
         angle_col = 45,
         )

##### 3. Categorical metrics for evaluating the detection capability of daily SRP products
library(readxl)
library(ggplot2)

low_pcp <- read_excel("./dailyPcp_catigorical.xlsx", sheet = "low")
moderate_pcp<- read_excel("./dailyPcp_catigorical", sheet = 'moderate')
high_pcp<- read_excel("./dailyPcp_catigorical", sheet = "high")

# changing the order of products
low_pcp2 <- low_pcp 
low_pcp2$Product <- factor(low_pcp2$Product, levels = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

moderate_pcp2 <- moderate_pcp   # duplicating the original data
moderate_pcp2$Product <- factor(moderate_pcp2$Product, levels = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

high_pcp2 <- high_pcp   # duplicating the original data
high_pcp2$Product <- factor(high_pcp2$Product, levels = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

ggplot(low_pcp2, aes(x=Product, y=POD, fill= Product)) +
  geom_violin(trim=FALSE, alpha=0.5, color = "black")+ ylim(0.0,0.5)+
  geom_boxplot(width = 0.07,aes(fill=Product))+ 
  labs(x ="", y = "POD")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.48,label = "", size=5)

ggplot(low_pcp2, aes(x=Product, y=FAR, fill=Product)) +
  geom_violin(trim=FALSE, color = "black", alpha=0.5)+ ylim(0.0,1.0)+
  geom_boxplot(width = 0.07,aes(fill=Product))+
  labs(x ="", y = "FAR")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.99,label = "", size=5)

ggplot(low_pcp2, aes(x=Product, y=CSI, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0.0,0.4)+
  geom_boxplot(width = 0.07,aes(fill=Product))+ 
  labs(x ="", y = "CSI")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.34,label = "", size=5)

ggplot(moderate_pcp2, aes(x=Product, y=POD, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0,0.35)+ ylim(0.0,0.5)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "POD")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.48,label = "", size=5)

ggplot(moderate_pcp2, aes(x=Product, y=FAR, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0.0,1.0)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "FAR")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.99,label = "", size=5)

ggplot(moderate_pcp2, aes(x=Product, y=CSI, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0.0,0.4)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "CSI")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.34,label = "", size=5) 

ggplot(high_pcp2, aes(x=Product, y=POD, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0.0,0.5)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "POD")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.48,label = "", size=5)

ggplot(high_pcp2, aes(x=Product, y=FAR, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0.0,1.0)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "FAR")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.99, label = "", size=5)

ggplot(high_pcp2, aes(x=Product, y=CSI, fill=Product)) +
  geom_violin(trim=FALSE,color = "black", alpha=0.5)+ ylim(0,0.35)+ ylim(0.0,0.4)+
  geom_boxplot(width = 0.07,aes(fill=Product)) + 
  labs(x ="", y = "CSI")+ theme_bw()+
  scale_fill_manual(values = c("firebrick", "gray41","darkmagenta", "darkgreen","goldenrod1"))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10, angle = 25, vjust = 0.5))+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  annotate("text", x = 0.75, y = 0.34, label = "", size=5)

##### 4.Scatterplots of daily SRP products evaluation
library(openxlsx) 
library(ggplot2)  

dpcp<-read.xlsx("./dailyPcp_scatterPlot.xlsx", sheet = "Org", startRow = 1, colNames = TRUE)

ggplot(dpcp, aes(x=Obs, y=SMR))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+
  xlim(0, 60)+ ylim(0,15)+
  labs(x ="Gauge (mm/day)", y = "SM2RASC (mm/day)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 23, y = 13.5, 
           label= "
           r = 0.32
           MAE = 2.04
           RMSE = 4.62
           RB = -28.4%", 
           size = 5,hjust = 0, fontface = 3)

ggplot(dpcp, aes(x=Obs, y=GPM))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+
  labs(x ="Gauge (mm/day)", y = "IMERG (mm/day)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 25, y = 50, 
           label= "
           r = 0.39
           MAE = 2.43 
           RMSE = 3.58 
           RB = 17.8%", 
           size = 5,hjust = 0, fontface = 3)

ggplot(dpcp, aes(x=Obs, y=ERA))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+
  labs(x ="Gauge (mm/day)", y = "ERA5 (mm/day)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 15, y = 26, 
           label= "
           r = 0.36
           MAE = 2.2 
           RMSE = 4.06 
           RB = 28.1%", 
           size = 5,hjust = 0, fontface = 3)

ggplot(dpcp, aes(x=Obs, y=CDR))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+
  labs(x ="Gauge (mm/day)", y = "PERSIANN-CDR (mm/day)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 25, y = 43, 
           label= "
           r = 0.29
           MAE = 2.46 
           RMSE = 4.84 
           RB = 20.8%", 
           size = 5,hjust = 0, fontface = 3)

ggplot(dpcp, aes(x=Obs, y=CRT))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+
  labs(x ="Gauge (mm/day)", y = "CMORPH-CRT (mm/day)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 25, y = 55, 
           label= "
           r = 0.22
           MAE = 1.91 
           RMSE = 4.09 
           RB = -72.9%", 
           size = 5,hjust = 0, fontface = 3)

##### 5. Monthly average precipitation
library(openxlsx)
library(ggplot2)  
require(tidyverse)

mavg <- read.xlsx("./monthlyPcp_barPlots.xlsx", sheet = "Sheet1", startRow = 1, colNames = TRUE)

mavg1<- mavg %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

mavg2 <- mavg1   # duplicating the original data
mavg2$month <- factor(mavg2$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", 
                                              "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot() +
  geom_bar(data=filter(mavg2, product %in% c("Gauge")), aes(x = month, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+
  ylim(0, 100)+
  geom_point(data=filter(mavg2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = month, y = value,colour=product), size = 2) +
  geom_line(data=filter(mavg2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = month, y = value,colour=product, group=product), size = 2)+
  ylab("Precipitation (mm/month)")+ xlab("Month")+
  theme_bw()+
  theme(text= element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkviolet",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"),
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"),
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 16))+
  theme(legend.position = c(0.27, 0.82))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size=16))

### 6. Monthly precipitation comparison using scatter plots
library(openxlsx) 
library(ggplot2)
# library(dplyr)
# library(extrafont)
# library(remotes)

mpcp<-read.xlsx("./monthly_pcp.xlsx", sheet = "Org", startRow = 1, colNames = TRUE)

ggplot(mpcp, aes(x=Obs, y=SMR))+
  geom_point(alpha = 0.75,shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+ ylim(0, 200)+
  labs(x ="Gauge (mm/month)", y = "SM2RASC (mm/month)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 80, y = 35, 
           label= "
           r = 0.63
           MAE = 22.67
           RMSE = 28.57", 
           size = 5,hjust = 0, fontface = 3)

ggplot(mpcp, aes(x=Obs, y=GPM))+
  geom_point(alpha = 0.75, shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+ ylim(0, 200)+
  labs(x ="Gauge (mm/month)", y = "IMERG (mm/month)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 80, y = 35, 
           label= "
           r = 0.84
           MAE = 16.35
           RMSE = 21.04", 
           size = 5,hjust = 0, fontface = 3)

ggplot(mpcp, aes(x=Obs, y=ERA))+
  geom_point(alpha = 0.75, shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+ ylim(0, 200)+
  labs(x ="Gauge (mm/month)", y = "ERA5 (mm/month)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 80, y = 35, 
           label= "
           r = 0.74
           MAE = 17.4
           RMSE = 24.59", 
           size = 5,hjust = 0, fontface = 3)

ggplot(mpcp, aes(x=Obs, y=CDR))+
  geom_point(alpha = 0.75, shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+ ylim(0, 200)+
  labs(x ="Gauge (mm/month)", y = "PERSIANN-CDR (mm/month)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 80, y = 35, 
           label= "
           r = 0.70
           MAE = 18.2
           RMSE = 25.24", 
           size = 5,hjust = 0, fontface = 3)

ggplot(mpcp, aes(x=Obs, y=CRT))+
  geom_point(alpha = 0.75, shape=19, size = 1, col = "dodgerblue2")+
  theme_bw()+ ylim(0, 200)+
  labs(x ="Gauge (mm/month)", y = "CMORPH-CRT (mm/month)")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  geom_smooth(method = lm, size = 0.5, alpha = 0.5, se = F, col = "red3")+
  annotate("text", x = 80, y = 35, 
           label= "
           r = 0.61
           MAE = 28.25
           RMSE = 35.14", 
           size = 5,hjust = 0, fontface = 3)

##### 7. Seasonal precipitation comparison using PDFs
library(openxlsx) 
library(ggplot2)  
# library(matrixStats)  
# require(dplyr)
# library(tidyr)
# library(extrafont)

winter_pdf<-read.xlsx("./seasonalPcp_pdf.xlsx", sheet = "winter", startRow = 1, colNames = TRUE)
spring_pdf<-read.xlsx("./seasonalPcp_pdf.xlsx", sheet = "spring", startRow = 1, colNames = TRUE)
summer_pdf<-read.xlsx("./seasonalPcp_pdf.xlsx", sheet = "summer", startRow = 1, colNames = TRUE)
autumn_pdf<-read.xlsx("./seasonalPcp_pdf.xlsx", sheet = "autumn", startRow = 1, colNames = TRUE)

winter_pdf1 <- winter_pdf  %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

spring_pdf1 <- spring_pdf  %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

summer_pdf1 <- summer_pdf  %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

autumn_pdf1 <- autumn_pdf  %>%
  gather(product, value, "Gauge":"CMORPH-CRT")

winter_pdf2 <- winter_pdf1   
winter_pdf2$Class <- factor(winter_pdf2$Class, levels = c("<1","1-5","6-10","11-15","16-20",">20"))

spring_pdf2 <- spring_pdf1   
spring_pdf2$Class <- factor(spring_pdf2$Class, levels = c("<1","1-5","6-10","11-15","16-20",">20"))

summer_pdf2 <- summer_pdf1   
summer_pdf2$Class <- factor(summer_pdf2$Class, levels = c("<1","1-5","6-10","11-15","16-20",">20"))

autumn_pdf2 <- autumn_pdf1   
autumn_pdf2$Class <- factor(autumn_pdf2$Class, levels = c("<1","1-5","6-10","11-15","16-20",">20"))

ggplot() +
  geom_bar(data=filter(winter_pdf2, product %in% c("Gauge")), aes(x = Class, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+ ylim(0, 1)+
  geom_point(data=filter(winter_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = Class, y = value,colour=product), size = 2) +
  geom_line(data=filter(winter_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = Class, y = value,colour=product, group=product), size = 1)+
  ylab("Probability")+ 
  xlab("Precipitation (mm/day)")+
  theme_bw()+
  theme(text= element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkmagenta",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.7, 0.5))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 10))
theme(legend.key.size = unit(0.6, "cm"))

ggplot() +
  geom_bar(data=filter(spring_pdf2, product %in% c("Gauge")), aes(x = Class, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+ ylim(0, 1)+
  geom_point(data=filter(spring_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = Class, y = value,colour=product), size = 2) +
  geom_line(data=filter(spring_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = Class, y = value,colour=product, group=product), size = 1)+
  ylab("Probability")+ 
  xlab("Precipitation (mm/day)")+
  theme_bw()+
  theme(text= element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkmagenta",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.7, 0.5))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 10))+
  theme(legend.key.size = unit(0.6, "cm"))

ggplot() +
  geom_bar(data=filter(summer_pdf2, product %in% c("Gauge")), aes(x = Class, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+
  geom_point(data=filter(summer_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = Class, y = value,colour=product), size = 2) + ylim(0, 1)+
  geom_line(data=filter(summer_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = Class, y = value,colour=product, group=product), size = 1)+
  ylab("Probability")+ 
  xlab("Precipitation (mm/day)")+
  theme_bw()+
  theme(text= element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkmagenta",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.7, 0.5))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 10))+
  theme(legend.key.size = unit(0.6, "cm"))

ggplot() +
  geom_bar(data=filter(autumn_pdf2, product %in% c("Gauge")), aes(x = Class, y = value, fill=product) , 
           stat ="identity", position="dodge", width = 0.5)+ ylim(0, 1)+
  geom_point(data=filter(autumn_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")),
             aes(x = Class, y = value,colour=product), size = 2) +
  geom_line(data=filter(autumn_pdf2, product %in% c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")), 
            aes(x = Class, y = value,colour=product, group=product), size = 1)+
  ylab("Probability")+ 
  xlab("Precipitation (mm/day)")+
  theme_bw()+
  theme(text= element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(vjust = 1),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  scale_color_manual(values =c("SM2RASC" = "firebrick","IMERG" = "black","PERSIANN-CDR" = "darkmagenta",
                               "ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  scale_fill_manual(values = c("Gauge" = "gray50"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(legend.position = c(0.7, 0.5))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 10))+
  theme(legend.key.size = unit(0.6, "cm"))

##### 8. Seasonal precipitation comparison using CDFs
winter_cdf<-read.xlsx("./seasonalPcp_cdf.xlsx", sheet = "winter", startRow = 1, colNames = TRUE)
spring_cdf<-read.xlsx("./seasonalPcp_cdf.xlsx", sheet = "spring", startRow = 1, colNames = TRUE)
summer_cdf<-read.xlsx("./seasonalPcp_cdf.xlsx", sheet = "summer", startRow = 1, colNames = TRUE)
autumn_cdf<-read.xlsx("./seasonalPcp_cdf.xlsx", sheet = "autumn", startRow = 1, colNames = TRUE)

winter_cdf1 <- winter_cdf  %>%
  gather(product, value, "Observed":"CMORPH-CRT")

spring_cdf1 <- spring_cdf  %>%
  gather(product, value, "Observed":"CMORPH-CRT")

summer_cdf1 <- summer_cdf  %>%
  gather(product, value, "Observed":"CMORPH-CRT")

autumn_cdf1 <- autumn_cdf  %>%
  gather(product, value, "Observed":"CMORPH-CRT")

winter_cdf2 <- winter_cdf1   
winter_cdf2$product <- factor(winter_cdf2$product, levels = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

spring_cdf2 <- spring_cdf1   
spring_cdf2$product <- factor(spring_cdf2$product, levels = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

summer_cdf2 <- summer_cdf1   
summer_cdf2$product <- factor(summer_cdf2$product, levels = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))

autumn_cdf2 <- autumn_cdf1   
autumn_cdf2$product <- factor(autumn_cdf2$product, levels = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))


ggplot(winter_cdf2, aes(value, color = product))+
  labs(x ="Precipitation (mm/day)", y = "Probability")+
  stat_ecdf(size = 1.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0,20)+
  scale_color_manual(values =c("Observed" = "blue3", "SM2RASC" = "firebrick","IMERG" = "black",
                               "PERSIANN-CDR" = "darkmagenta","ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position = c(0.7, 0.25))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.key.size = unit(0.5, "cm"))

ggplot(spring_cdf2, aes(value, color = product))+
  labs(x ="Precipitation (mm/day)", y = "Probability")+
  stat_ecdf(size = 1.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0,20)+
  scale_color_manual(values =c("Observed" = "blue3", "SM2RASC" = "firebrick","IMERG" = "black",
                               "PERSIANN-CDR" = "darkmagenta","ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position = c(0.7, 0.25))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.key.size = unit(0.5, "cm"))

ggplot(summer_cdf2, aes(value, color = product))+
  labs(x ="Precipitation (mm/day)", y = "Probability")+
  stat_ecdf(size = 1.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0,20)+
  scale_color_manual(values =c("Observed" = "blue3", "SM2RASC" = "firebrick","IMERG" = "black",
                               "PERSIANN-CDR" = "darkmagenta","ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position = c(0.7, 0.25))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.key.size = unit(0.5, "cm"))

ggplot(autumn_cdf2, aes(value, color = product))+
  labs(x ="Precipitation (mm/day)", y = "Probability")+
  stat_ecdf(size = 1.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0,20)+
  scale_color_manual(values =c("Observed" = "blue3", "SM2RASC" = "firebrick","IMERG" = "black",
                               "PERSIANN-CDR" = "darkmagenta","ERA5" = "darkgreen","CMORPH-CRT" = "goldenrod1"), 
                     labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT")) +
  scale_linetype_manual(breaks = c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"), 
                        values=c("dashed", "solid","solid","solid","solid","solid"), 
                        labels=c("Observed", "SM2RASC","IMERG","PERSIANN-CDR","ERA5","CMORPH-CRT"))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position = c(0.7, 0.25))+
  theme(legend.title = element_text(size = 0))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.key.size = unit(0.5, "cm"))

#################################  END  ################################

