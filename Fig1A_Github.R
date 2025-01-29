# load pacakges
library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)
library("readxl")
library(lubridate)
library(cowplot)
library(svglite)




tree <- read.beast("Full_Final_Rename_NewSTEXP10MLTaxon_combined_V2.tree")
#df <- read_excel('Phylo_Data1.xlsx')

tree <- groupClade(tree,.node=c(224,274,391))

p <- ggtree(tree, mrsd="2022-04-07", as.Date=TRUE,aes(color=group),size=0.8) + theme_tree2() +
  scale_color_manual(values=c('deeppink2','darkorange3','gold2','dodgerblue3'), labels=c("Omicron\nAncestor","BA.5","BA.4","BA.2"), name='')+
  theme(legend.position = "top")+
  theme(legend.direction = "horizontal")+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank() # get rid of minor grid
    #legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+
  #theme(axis.text.x = element_text(angle=90,hjust = 0.5,vjust=0.5))+
  
  #geom_segment(x=as.Date("2021-11-29"),xend=as.Date("2021-11-29"),y=80,yend=120, color='gold2',linetype='dashed')+
  #geom_ribbon(aes(x=as.Date(date_decimal(2021.82)),xmin=as.Date(date_decimal(2021.75)),xmax=as.Date(date_decimal(2021.9))), fill='dodgerblue2',alpha=0.2, color=NA)+
  
  #geom_ribbon(aes(x=as.Date("2021-11-29"),xmin=as.Date(date_decimal(2021.83)),xmax=as.Date(date_decimal(2022))), fill='gold2',alpha=0.2, color=NA)+
  #geom_ribbon(aes(x=as.Date("2022-01-21"),xmin=as.Date(date_decimal(2021.94)),xmax=as.Date(date_decimal(2022.07))), fill='darkorange2',alpha=0.2, color=NA)+
  
  #geom_vline(xintercept=as.Date("2021-11-29"), color='gold2',linetype='dashed')+
  #geom_vline(xintercept=as.Date("2022-01-21"), color='darkorange2',linetype='dashed')+
  #geom_vline(xintercept=as.Date(date_decimal(2021.82)), color='dodgerblue2',linetype='dashed')+
  
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month", limits=as.Date(c("2021/09/10","2022/04/10")))
  #geom_text(aes(label=node), hjust=-.3, size=2)
  

p

#yellow_colors_dark1<-randomcoloR::randomColor(count=3, hue="yellow", luminosity = "random")
#yellow_colors_dark<-randomcoloR::randomColor(count=3, hue="yellow", luminosity = "random")
#yellow_colors_dark<-randomcoloR::randomColor(count=3, hue="yellow", luminßosity = "random")

my_date_transform <- function(x) {format(date_decimal(x), "%m/%d/%y")}


age_densityBA2<-read_excel('age_density_BA2_V2.xlsx')
age_densityBA2$date2<-as.Date(date_decimal(age_densityBA2$date))
age_densityBA2$days<-as.Date(cut(age_densityBA2$date,breaks = "day",start.on.monday = FALSE))


age_densityBA5<-read_excel('age_density_BA5_V2.xlsx')
age_densityBA5$date2<-as.Date(date_decimal(age_densityBA5$date))

age_densityBA4<-read_excel('age_density_BA4_V2.xlsx')
age_densityBA4$date2<-as.Date(date_decimal(age_densityBA4$date))


tmrca<-ggplot()+
  theme_classic()+
  #geom_area(data=age_densityroot,aes(x=date,y=density), fill='deeppink2',alpha=0.5)+
  
  geom_area(data=age_densityBA2,aes(x=date,y=density), fill='dodgerblue2',alpha=0.5)+
  geom_area(data=age_densityBA4,aes(x=date,y=density), fill='gold2',alpha=0.5)+
  
  geom_area(data=age_densityBA5,aes(x=date,y=density), fill='darkorange2',alpha=0.5)+
  #geom_area(aes(x=date,y=ageBA4), fill='gold2',alpha=0.5)+
  scale_x_continuous(labels = my_date_transform, limits=c(decimal_date(as.Date("2021/09/10")),decimal_date(as.Date("2022/04/10"))))


  
tmrca


plot_grid(p,tmrca,ncol=1,align='v',rel_heights = c(0.8,0.2))

ggsave("tree_ages_V2.svg", width = 12, height = 18, 
       units = "cm", device = "svg", limitsize = FALSE,bg = "transparent")

tempest_df<-read_excel('Tempest_tree_V2.xlsx')
tempest_df$date2<-as.Date(date_decimal(tempest_df$date))

ggplot(tempest_df,aes(date2,distance))+
  theme_classic()+
  scale_fill_manual(values=c('dodgerblue3','gold2','darkorange3'), name='Lineage')+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month")+
  xlab('Sampling Date')+
  ylab("Average per site genetic\ndivergence from root")+
  geom_point(shape=21,size=3,aes(fill=lineage))+
  geom_smooth(method='lm', color='black',size=0.6)+
  annotate(geom = 'text',label='Correlation coefficient = 0.6', x=as.Date("2022/01/30"),y=0.00080)+
  annotate(geom = 'text',label='R squared = 0.4', x=as.Date("2022/01/30"),y=0.00070)+
  ylim(0,0.001)

