
library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")
library(ggpattern)
require(tidyverse)
library("lubridate")
require("ggalt")



#################################################
###### READING ALL DATA  ########################
#################################################
data2<-read_excel('all_SA_data_7May.xlsx')

BA4_data<-read_excel('BA.4_14April_n120.xlsx')
BA5_data<-read_excel('BA.5_14April_n51.xlsx')

BA4_data_SA<-subset(BA4_data,country=='South Africa')

remove <- BA4_data$strain
data2<-data2[!data2$strain %in% remove, ]

data2<-rbind(data2,BA4_data)

remove <- BA5_data$strain
data2<-data2[!data2$strain %in% remove, ]

data2<-rbind(data2,BA5_data)

data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Others","20H (Beta, V2)","20I (Alpha, V1)","Delta",'BA.1','BA.1.1','BA.2','BA.3', 'BA.4','BA.5'))

data2$days<-as.Date(cut(data2$date,breaks = "day",start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date,breaks = "week",start.on.monday = FALSE))
data2$date2<-as.Date(cut(data2$date,breaks = "2 week",start.on.monday = FALSE))
data2$date4<-as.Date(cut(data2$date,breaks = "1 month",start.on.monday = FALSE))

data2<- data2 %>% filter(division!="South Africa")



EC_df<-subset(data2,division=='Eastern Cape')
KZN_df<-subset(data2,division=='KwaZulu-Natal')
WC_df<-subset(data2,division=='Western Cape')
GP_df<-subset(data2, division=='Gauteng')
FS_df<-subset(data2, division=='Free State')
LP_df<-subset(data2, division=='Limpopo')
MP_df<-subset(data2, division=='Mpumalanga')
NC_df<-subset(data2, division=='Northern Cape')
NW_df<-subset(data2, division=='North West')




library (readr)

urlfile="https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"

mydata<-read_csv(url(urlfile))
provincial_cases<-mydata
provincial_cases$days<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "day",
                                   start.on.monday = FALSE))

provincial_cases$date<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "week",
                                   start.on.monday = FALSE))

provincial_cases <- within(provincial_cases,GP_daily <- ave(GP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,KZN_daily <- ave(KZN,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,WC_daily <- ave(WC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,EC_daily <- ave(EC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,LP_daily <- ave(LP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,MP_daily <- ave(MP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NC_daily <- ave(NC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NW_daily <- ave(NW,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,FS_daily <- ave(FS,FUN = function(x) c(x[1],diff(x))))

provincial_cases <- within(provincial_cases,total_daily <- ave(total,FUN = function(x) c(x[1],diff(x))))


###Fixing artefact in case numbers for 23rd Nov 2021
provincial_cases[provincial_cases$days=="2021-11-23", "GP_daily"] <- 1823
provincial_cases[provincial_cases$days=="2021-11-23", "EC_daily"] <- 22
provincial_cases[provincial_cases$days=="2021-11-23", "FS_daily"] <- 24
provincial_cases[provincial_cases$days=="2021-11-23", "KZN_daily"] <- 62
provincial_cases[provincial_cases$days=="2021-11-23", "LP_daily"] <- 44
provincial_cases[provincial_cases$days=="2021-11-23", "MP_daily"] <- 57
provincial_cases[provincial_cases$days=="2021-11-23", "NW_daily"] <- 98
provincial_cases[provincial_cases$days=="2021-11-23", "NC_daily"] <- 26
provincial_cases[provincial_cases$days=="2021-11-23", "WC_daily"] <- 74
provincial_cases[provincial_cases$days=="2021-11-23", "total_daily"] <- 2230


provincial_cases[provincial_cases$days=="2021-12-12", "total_daily"] <- 15000


library(tidyverse)
library(zoo)

rollspan <- 7 # span of rolling average, in days.

provincial_cases <- provincial_cases %>% 
  dplyr::mutate(GP_daily_7day = zoo::rollmean(GP_daily, k = 7, fill = 'extend'),
                KZN_daily_7day = zoo::rollmean(KZN_daily, k = 7, fill = 'extend'),
                WC_daily_7day = zoo::rollmean(WC_daily, k = 7, fill = 'extend'),
                EC_daily_7day = zoo::rollmean(EC_daily, k = 7, fill = 'extend'),
                LP_daily_7day = zoo::rollmean(LP_daily, k = 7, fill = 'extend'),
                MP_daily_7day = zoo::rollmean(MP_daily, k = 7, fill = 'extend'),
                NC_daily_7day = zoo::rollmean(NC_daily, k = 7, fill = 'extend'),
                total_daily_7day = zoo::rollmean(total_daily, k = 7, fill = 'extend')
                
  ) %>% 
  dplyr::ungroup()




prop.table(table(data2$days, data2$Nextstrain_variants))

P <- prop.table(table(data2$days, data2$Nextstrain_variants), margin=1)

temp<-as.data.frame(P)
names(temp)[1] <- 'days'

 head(temp)
 temp$days<-as.Date(cut(as.Date(temp$days,format='%Y-%m-%d'),
                         breaks = "day",
                         start.on.monday = FALSE))
 # # Create empty data frame
extra.data2 <- data.frame()
#
# # Populate the data frame using a for loop
for (i in seq(as.Date("2022/05/01"), by = "day", length.out = 7)) {
  #   # Get the row data
  days <- as.Date(i)
  days

 # Populate the rows

    new.row1 <- data.frame(days = days, Var2 = "Others", Freq=0)
    new.row2 <- data.frame(days = days, Var2 = "Delta",  Freq=0)
    new.row3 <- data.frame(days = days, Var2 = "20I (Alpha, V1)",  Freq=0)
    new.row4 <- data.frame(days = days, Var2 = "20H (Beta, V2)",  Freq=0)
    new.row5 <- data.frame(days = days, Var2 = "BA.1",  Freq=0)
    new.row6 <- data.frame(days = days, Var2 = "BA.2",  Freq=0.05)
    new.row7 <- data.frame(days = days, Var2 = "BA.3",  Freq=0)
    new.row8 <- data.frame(days = days, Var2 = "BA.1.1",  Freq=0)
    new.row9 <- data.frame(days = days, Var2 = "BA.4",  Freq=0.3)
    new.row10 <- data.frame(days = days, Var2 = "BA.5",  Freq=0.65)

  #   # Add the row
    extra.data2 <- rbind(extra.data2, new.row1)
    extra.data2 <- rbind(extra.data2, new.row2)
    extra.data2 <- rbind(extra.data2, new.row3)
    extra.data2 <- rbind(extra.data2, new.row4)
    extra.data2 <- rbind(extra.data2, new.row5)
    extra.data2 <- rbind(extra.data2, new.row6)
    extra.data2 <- rbind(extra.data2, new.row7)
    extra.data2 <- rbind(extra.data2, new.row8)
    extra.data2 <- rbind(extra.data2, new.row9)
    extra.data2 <- rbind(extra.data2, new.row10)
  #
  #
}

# Print the data frame
extra.data2

temp <- rbind(temp, extra.data2)

temp2<-provincial_cases[c("days","total_daily")]
head(temp2)

library(plyr)
temp3<-join(temp, temp2,
            type = "left")

tail(temp3)

temp3$days<-as.Date(cut(as.Date(temp3$days,format='%Y-%m-%d'),
                               breaks = "day",
                               start.on.monday = FALSE))

temp3$cases_per_variant=temp3$total_daily*temp3$Freq


temp3 <- temp3 %>%
  group_by(Var2) %>%
  dplyr::mutate(cases_per_variant_7day = zoo::rollmean(cases_per_variant, k = 7, fill='extend')) %>%
  dplyr::ungroup()
head(temp3)




p_Epi_SA<-ggplot() + 
  theme_minimal()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month", limits=as.Date(c("2020/03/10","2022/05/08")))+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants',
                    labels=c('Others','Beta','Alpha','Delta','BA.1','BA.1.1','BA.2','BA.3','BA.4','BA.5'))+
  geom_area(data=temp3,aes(x = days, y = cases_per_variant_7day, fill = Var2),size=0.2,color='black', position='stack')+
  geom_hline(yintercept=0, colour="white", size=1)+
   ylab('Daily Cases\n(7-day Moving Average)')+
  xlab('')+
  theme(axis.line = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.direction="horizontal",legend.position="bottom",legend.text= element_text(size=8, family="Helvetica")) +
  ggtitle('Epidemic and Variant Dynamics in South Africa')+
  labs(fill='Variants')+
  guides(fill = guide_legend(nrow = 1))

p_Epi_SA

sgtf_data<-read_excel("TaqPath_SGTF.xlsx")
sgtf_data$date<-as.Date(cut(sgtf_data$date,breaks = "day",start.on.monday = FALSE))

P_sgtf<-ggplot()+
  geom_line(data=sgtf_data,aes(x=date,y=TaqPath_pos/10000,color='TaqPath Positive'),linetype='dashed',size=1)+
  
  geom_line(data=sgtf_data,aes(x=date,y=sgtf_30_perc,color='SGTF Proportion'),size=1)+
  
  theme_bw()+
  theme(axis.text.x = element_text(color="black", size=16))+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/04/25")))+
  
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=8, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=8))+
  scale_color_manual(values=c('grey','grey30'),name="", labels=c("SGTF Proportion","TaqPath\nPositives(x10^4)"))+
 theme(legend.text = element_text(size=8))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.6,0.7))+
  theme(legend.background=element_blank(),legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  theme(legend.key.size = unit(0.2, "cm"))+
  ylim(0,1)+
  xlab('')+
  ylab('Proportion of\nTaqpath SGTF qPCR')+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_y_continuous(

  # Features of the first axis
    name = "", limits=c(0,1), breaks = c(0,1,0.5),

   )
P_sgtf

P<-ggplot()+
   geom_bar(data=subset(data2, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black', position='fill',size=0.2)+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
   theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/05/10")))+
  
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('white'),name="")+
   theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.position = "none")+
   theme(legend.key.size = unit(0.2, "cm"))+
  xlab('')+
  ylab('Genomic\nPrevalence')

P


Parea<-ggplot()+
  geom_area(data=subset(data2, !is.na(Nextstrain_variants)),mapping=aes(x = days,fill=Nextstrain_variants),stat='bin',color='black',binwidth=10, position='fill',size=0.2)+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('white'),name="")+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
 # theme(legend.position = "none")+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('')+
  ylab('Genomic\nPrevalence')

Parea


P_KZN<-ggplot()+
   geom_bar(data=subset(KZN_df, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black', position='fill',size=0.2)+
  scale_fill_manual(values=c('grey90','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/04/25")))+
  
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('white'),name="")+
   theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.position = "none")+
  #theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('')+
  ylab('Genomic\nPrevalence')+
  scale_y_continuous(limits=c(0,1), breaks = c(0,1,0.5))+
  geom_text(label="KwaZulu-Natal", aes(x=as.Date("2021/12/20"),y=0.8), color='white')

P_KZN



P_GP<-ggplot()+
  geom_bar(data=subset(GP_df, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black', position='fill',size=0.2)+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/04/25")))+
    theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('white'),name="")+
   theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.position = "none")+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('')+
  ylab('Genomic\nPrevalence')+
  scale_y_continuous(limits=c(0,1), breaks = c(0,1,0.5))+
  geom_text(label="Gauteng", aes(x=as.Date("2021/12/01"),y=0.8), color='white')

P_GP

panelB_D<-plot_grid(P,P_sgtf,align='v',ncol=1,labels=c("B","D"),rel_heights = c(0.6,0.4))
panelB_D


P_count<-ggplot()+
  geom_bar(data=subset(data2, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black',size=0.2)+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
    theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/05/10")))+
    theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=12))+
  scale_color_manual(values=c('black'),name="")+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('')+
  ylab('Genome Count')


P_count


P_provinces<-ggplot()+
 geom_bar(data=subset(data2, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black', position='fill')+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
   theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90, hjust=1,vjust=0.5))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%d-%b-%Y",date_breaks = "2 week", limits=as.Date(c("2021/11/01","2022/05/10")))+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=12))+
  scale_color_manual(values=c('black'),name="")+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab('Genomic Prevalence')+
  facet_wrap(division~.)

P_provinces


P_provinces_count<-ggplot()+
  geom_bar(data=subset(data2, !is.na(Nextstrain_variants)),aes(x = date,fill=Nextstrain_variants),width=7,color='black')+
  scale_fill_manual(values=c('grey90','bisque2','grey40','mediumseagreen','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','grey90','grey90'), name='Variants')+
   theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90, hjust=1,vjust=0.5))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%d-%b-%Y",date_breaks = "2 week", limits=as.Date(c("2021/11/01","2022/05/10")))+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=12))+
  scale_color_manual(values=c('black'),name="")+
   theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=12))+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab('Genome Count')+
  facet_wrap(division~.)

P_provinces_count

plot_grid(P,P_count)
plot_grid(P_provinces,P_provinces_count)



panelC<-ggplot(data=data2) +
  theme_classic()+
  geom_segment(aes(x=min(data2$days), y = division, xend=max(data2$days), yend=division, group=division), colour="grey88", size=10) +
  geom_point(data=subset(data2,Nextstrain_variants=='Others'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='20H (Beta, V2)'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='20I (Alpha, V1)'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='Delta'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.1'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.1.1'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.2'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.3'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.4'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  geom_point(data=subset(data2,Nextstrain_variants=='BA.5'),aes(x=days, y=division, fill=Nextstrain_variants,color=Nextstrain_variants), position = position_jitter(width=0.3, height=0.3), shape=21, size=2.2, stroke=0.2)+
  ylab('')+ xlab('')+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/11/01","2022/05/15")))+
  scale_color_manual(values=c('bisque3','grey20','deeppink4','orchid4','dodgerblue4','purple3','gold3','darkorange3','darkgreen','grey70','grey90'), name='Variants')+
  scale_fill_manual(values=c('bisque2','grey40','deeppink2','orchid3','dodgerblue3','purple3','gold2','darkorange2','mediumseagreen','grey90','grey90'), name='Variants')+
  
  theme(legend.position="none") 

panelC

c_e<-plot_grid(panelC,"",ncol=1,labels=c("C","E"),rel_heights = c(0.6,0.4))
c_e

b_c_d_e<-plot_grid(panelB_D,c_e,labels=c("",""),rel_widths = c(0.45,0.55))
b_c_d_e


plot_grid(p_Epi_SA,b_c_d_e, labels=c("A",""),ncol=1)

