library(ggplot2)
library(grid) # to use "unit" in ggplot

setwd('..')

washoff_frac=as.data.frame(read.csv("./1_Data/Results_all.csv"))

washoff_frac$Load=c(rep(c(rep(c("50 g","100 g", "200 g"),each=10, times=3),rep("200 g",times=10)),times=2),
                    rep(c(rep(c("50 g","100 g", "200 g"),each=12, times=3),rep("200 g",times=12)),times=3))
washoff_frac$Slope=c(rep(rep(c("16%","8%","4%","2%"),times=c(30,30,30,10)),times=2),
                     rep(rep(c("16%","8%","4%","2%"),times=c(36,36,36,12)),times=3))
washoff_frac$Intensity=rep(c("33 mm/hr","47 mm/hr","75 mm/hr","110 mm/hr","155 mm/hr"), 
                         times=c(100,100,120,120,120))

washoff_frac$Load = factor(washoff_frac$Load, levels=unique(washoff_frac$Load)) #to adjust the order of facet_wrap in ggplot
washoff_frac$Slope = factor(washoff_frac$Slope, levels=rev(unique(washoff_frac$Slope))) #to adjust the order of facet_wrap in ggplot
washoff_frac$Intensity = factor(washoff_frac$Intensity, levels=unique(washoff_frac$Intensity)) #to adjust the order of facet_wrap in ggplot


head(washoff_frac)

theme_set(theme_bw())
ggplot(washoff_frac, aes(x = Duration, y = Fraction, shape= Load, colour = Load)) + 
  geom_point(size = 2)+
  facet_grid(Intensity~Slope)+
  scale_y_continuous(limits = c(0, 1))+
  labs(x="Duration [min]", y="Wash-off fraction [%]")+
  theme(legend.position="bottom",legend.title=element_blank(),
        legend.text=element_text(face="bold",size=10),legend.key=element_blank(),
        legend.key.width=unit(1,'cm'),legend.margin = unit(0.05, "cm"),legend.key.height=unit(0.05, "cm"))+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(size=8,face="bold"),
        axis.title=element_text(size=10,face="bold"))+
  theme(strip.text.x = element_text(size = 10))

ggsave(
  "plot1.png",
  plot1,
  width = 4,
  height = 4,
  dpi = 360
)