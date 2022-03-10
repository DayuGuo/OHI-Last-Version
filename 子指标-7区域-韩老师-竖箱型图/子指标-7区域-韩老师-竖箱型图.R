

ohi$region = as.factor(ohi$region)
ohi$region = ordered(ohi$region,levels = c('East Asia and Pacific','Europe and Central Asia',"Latin America and The Caribbean",
                                           'Middle East and North Africa','North America','South Asia','Sub-Saharan Arica'))

e.II = ohi[,c('region','E.1','E.2','E.3','E.4','E.5')]
e.II.long = melt(e.II, id.vars = 'region')

pe = ggplot(e.II.long, aes(x=region, y=value, fill=region)) +
  geom_boxplot()+ylab("Scores")+xlab("")+
  geom_jitter(shape=16, size=1, position=position_jitter(0.2), show.legend = FALSE)+
  facet_grid(cols = vars(variable))+
  scale_fill_manual(values = c("#FB9A99","#FDBF6F","#FFFF99","#B2DF8A","#80CDC1","#A6CEE3","#CAB2D6"))+
  theme_bw()+theme(panel.border = element_rect(linetype = "dotted", fill = NA))+
  theme(legend.title = element_blank(), axis.text.x = element_blank(),legend.text = element_text(size = 10),
        strip.text.x = element_text(size=15),axis.title = element_text(size = 15),plot.title = element_text(size = 15),
        strip.background = element_rect(color="grey", fill="grey", size=15, linetype="solid"))

ggsave("Fig eII.jpg",plot = pe, width = 13.5, height = 7.5,dpi = 600)