library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

a = read.csv("mating_ROHLength.csv")
b = read.csv("mating_ROHProportions.csv")
c = read.csv("ROH_shared.csv")

a$ROH = a$ROH/1000000
a$sd = a$sd/1000000
c$length = c$length/100000

a$cil = a$ROH-1.96*a$sd
a$cir = a$ROH+1.96*a$sd

b$cil = b$prop-1.96*b$sd
b$cir = b$prop+1.96*b$sd

a$cil[a$cil<0] = 0
b$cil[b$cil<0] = 0



a$pairs = factor(a$pairs, levels = c("RTR-RTR","WAY-WAY","KTR-KTR","ZOO-ZOO","KAZ-KAZ","COR-COR"))
b$pairs = factor(b$pairs, levels = c("RTR-RTR","WAY-WAY","KTR-KTR","ZOO-ZOO","KAZ-KAZ","COR-COR"))
c$pop = factor(c$pop, levels = c("OTHERS","KTR","WAY","RTR"))

ggp = ggplot(data = a, aes(x = pairs, y = ROH)) +
  #geom_hline(yintercept = 0.5) +
  geom_bar(stat = "identity", fill = cols[3], col = cols[3]) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.1, col = "black") +
  xlab("mating pair") +
  ylab("length of >1Mb stretches in offspring (Mb)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500))

pd = position_dodge(0.5)
ggp = ggplot(data = b, aes(x = pairs, y = prop, col = type, fill = type)) +
  #geom_hline(yintercept = 0.5) +
  geom_bar(stat = "identity", width=.5, position = "dodge") +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.1, col = "black", position = pd) +
  xlab("mating pair") +
  ylab("proportion of ROH runs in offspring")+
  theme_tufte_revised()


ggp2 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c(">0.1Mb",">1Mb"), values = cols[c(2,3)]) +
  scale_fill_manual(breaks = c(">0.1Mb",">1Mb"), values = cols[c(2,3)]) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4))

library(cowplot)
g1 = plot_grid(ggp1,ggp2,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))
  

  
png('roh4.png', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()



c$type = factor(c$type, levels = c("0.01-0.1Mb","0.1-1Mb",">1Mb"))

pd = position_dodge(0.5)
ggp = ggplot(data = c, aes(x = pop, y = length, col = type, fill = type)) +
  #geom_hline(yintercept = 0.5) +
  geom_bar(stat = "identity", width=.5, position = "dodge") +
  xlab("population") +
  ylab("ROH shared across individuals (Mb)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("0.01-0.1Mb","0.1-1Mb",">1Mb"), values = cols[c(1,2,3)]) +
  scale_fill_manual(breaks = c("0.01-0.1Mb","0.1-1Mb",">1Mb"), values = cols[c(1,2,3)]) +
  scale_y_continuous(breaks = c(0,200,400,600,800,1000))


png('roh5.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()