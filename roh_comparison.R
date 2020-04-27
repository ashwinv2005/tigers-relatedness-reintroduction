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

a = read.csv("prop_homozygozity.csv")

inb0.1mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb0.1mb$mean = inb0.1mb$cil = inb0.1mb$cir = 0

inb1mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb1mb$mean = inb1mb$cil = inb1mb$cir = 0

inb5mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb5mb$mean = inb5mb$cil = inb5mb$cir = 0

inb10mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb10mb$mean = inb10mb$cil = inb10mb$cir = 0

names(a) = c("ind","site","region","mb0.1","mb1","mb5","mb10","mb20","total")

a$mb0.1 = a$mb0.1/a$total
a$mb1 = a$mb1/a$total
a$mb5 = a$mb5/a$total
a$mb10 = a$mb10/a$total


###################################### 0.1mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[1] = median(b1)
inb0.1mb$cil[1] = quantile(b1,0.025)
inb0.1mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[2] = median(b1)
inb0.1mb$cil[2] = quantile(b1,0.025)
inb0.1mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[3] = median(b1)
inb0.1mb$cil[3] = quantile(b1,0.025)
inb0.1mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[4] = median(b1)
inb0.1mb$cil[4] = quantile(b1,0.025)
inb0.1mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[5] = median(b1)
inb0.1mb$cil[5] = quantile(b1,0.025)
inb0.1mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb0.1mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)





################################################## 1mb


## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[1] = median(b1)
inb1mb$cil[1] = quantile(b1,0.025)
inb1mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[2] = median(b1)
inb1mb$cil[2] = quantile(b1,0.025)
inb1mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[3] = median(b1)
inb1mb$cil[3] = quantile(b1,0.025)
inb1mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[4] = median(b1)
inb1mb$cil[4] = quantile(b1,0.025)
inb1mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[5] = median(b1)
inb1mb$cil[5] = quantile(b1,0.025)
inb1mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb1mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 5mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[1] = median(b1)
inb5mb$cil[1] = quantile(b1,0.025)
inb5mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[2] = median(b1)
inb5mb$cil[2] = quantile(b1,0.025)
inb5mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[3] = median(b1)
inb5mb$cil[3] = quantile(b1,0.025)
inb5mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[4] = median(b1)
inb5mb$cil[4] = quantile(b1,0.025)
inb5mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[5] = median(b1)
inb5mb$cil[5] = quantile(b1,0.025)
inb5mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb5mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 10mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[1] = median(b1)
inb10mb$cil[1] = quantile(b1,0.025)
inb10mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[2] = median(b1)
inb10mb$cil[2] = quantile(b1,0.025)
inb10mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[3] = median(b1)
inb10mb$cil[3] = quantile(b1,0.025)
inb10mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[4] = median(b1)
inb10mb$cil[4] = quantile(b1,0.025)
inb10mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[5] = median(b1)
inb10mb$cil[5] = quantile(b1,0.025)
inb10mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb10mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)






inb = rbind(inb0.1mb,inb1mb,inb5mb,inb10mb)
inb$roh = rep(c(">0.1Mb",">1Mb",">5Mb",">10Mb"), each = 5)
inb$roh = factor(inb$roh, levels = c(">0.1Mb",">1Mb",">5Mb",">10Mb"))
inb$site = factor(inb$site, levels = c("ranthambore","wayanad","Kanha","zoo","panindia"))
inb$panel = "L"
inb[inb$roh %in% c(">0.1Mb",">1Mb"),]$panel = "S"
inb$panel = factor(inb$panel, levels = c("S","L"))

pd = position_dodge(0.2)
ggp = ggplot(data = inb, aes(x = site, y = mean*100, col = roh)) +
  #geom_hline(yintercept = 0.5) +
  facet_wrap(.~panel, nrow = 2, ncol = 1, scales = "free_y") +
  geom_point(size = 3, position = pd) +
  geom_errorbar(aes(ymin = cil*100, ymax = cir*100), size = 0.6, width = 0.2, position = pd) +
  xlab("geographic region") +
  ylab("genome in ROH (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_blank()) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_colour_manual(breaks = c(">0.1Mb",">1Mb",">5Mb",">10Mb"), 
                      labels = c("455 generations","45 generations","9 generations","4 generations"), 
                      values = cols[c(3,10,6,4)]) +
  scale_x_discrete(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                   labels = c("Ranthambore","Wayanad","Kanha","Zoo","All India")) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = "bottom")

png('Fig. 1.png', units="in", width=7, height=7, res=1000)
ggp1
dev.off()




ggp = ggplot(data = inb, aes(x = site, y = mean*100)) +
  facet_wrap(. ~ roh, scale="free", ncol = 2) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil*100, ymax = cir*100), size = 0.5, width = 0.2, position = pd) +
  xlab("geographic region") +
  ylab("genome in ROH (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                   labels = c("Ranthambore","Wayanad","Kanha","Zoo","All India")) +
  theme(strip.text.x = element_text(size = 15))


png('S1.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()





##################################################################


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

a = read.csv("inbreeding_history.csv")

inb0.1mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb0.1mb$mean = inb0.1mb$cil = inb0.1mb$cir = 0

inb1mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb1mb$mean = inb1mb$cil = inb1mb$cir = 0

inb5mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb5mb$mean = inb5mb$cil = inb5mb$cir = 0

inb10mb = data.frame(site = c("Kanha","ranthambore","wayanad","zoo","panindia"))
inb10mb$mean = inb10mb$cil = inb10mb$cir = 0

names(a) = c("ind","site","region","mb0.1","mb1","mb5","mb10")
a = a %>% filter(!is.na(mb0.1))


###################################### 0.1mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[1] = median(b1)
inb0.1mb$cil[1] = quantile(b1,0.025)
inb0.1mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[2] = median(b1)
inb0.1mb$cil[2] = quantile(b1,0.025)
inb0.1mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[3] = median(b1)
inb0.1mb$cil[3] = quantile(b1,0.025)
inb0.1mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[4] = median(b1)
inb0.1mb$cil[4] = quantile(b1,0.025)
inb0.1mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb0.1,length(a1$mb0.1),replace = T)
  b1[i] = mean(b)
}
inb0.1mb$mean[5] = median(b1)
inb0.1mb$cil[5] = quantile(b1,0.025)
inb0.1mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb0.1mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)





################################################## 1mb


## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[1] = median(b1)
inb1mb$cil[1] = quantile(b1,0.025)
inb1mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[2] = median(b1)
inb1mb$cil[2] = quantile(b1,0.025)
inb1mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[3] = median(b1)
inb1mb$cil[3] = quantile(b1,0.025)
inb1mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[4] = median(b1)
inb1mb$cil[4] = quantile(b1,0.025)
inb1mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[5] = median(b1)
inb1mb$cil[5] = quantile(b1,0.025)
inb1mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb1mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 5mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[1] = median(b1)
inb5mb$cil[1] = quantile(b1,0.025)
inb5mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[2] = median(b1)
inb5mb$cil[2] = quantile(b1,0.025)
inb5mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[3] = median(b1)
inb5mb$cil[3] = quantile(b1,0.025)
inb5mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[4] = median(b1)
inb5mb$cil[4] = quantile(b1,0.025)
inb5mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[5] = median(b1)
inb5mb$cil[5] = quantile(b1,0.025)
inb5mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb5mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 10mb

## north

a1 = a %>% filter(site == "Kanha")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[1] = median(b1)
inb10mb$cil[1] = quantile(b1,0.025)
inb10mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(site == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[2] = median(b1)
inb10mb$cil[2] = quantile(b1,0.025)
inb10mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(site == "wayanad")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[3] = median(b1)
inb10mb$cil[3] = quantile(b1,0.025)
inb10mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(site == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[4] = median(b1)
inb10mb$cil[4] = quantile(b1,0.025)
inb10mb$cir[4] = quantile(b1,0.975)


## panindia

a1 = a %>% filter(site != "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[5] = median(b1)
inb10mb$cil[5] = quantile(b1,0.025)
inb10mb$cir[5] = quantile(b1,0.975)



ggplot(data = inb10mb, aes(x = site, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)






inb = rbind(inb0.1mb,inb1mb,inb5mb,inb10mb)
inb$roh = rep(c(">0.1Mb",">1Mb",">5Mb",">10Mb"), each = 5)
inb$roh = factor(inb$roh, levels = c(">0.1Mb",">1Mb",">5Mb",">10Mb"))
inb$site = factor(inb$site, levels = c("ranthambore","wayanad","Kanha","zoo","panindia"))
inb$panel = "L"
inb[inb$roh %in% c(">0.1Mb",">1Mb"),]$panel = "S"
inb$panel = factor(inb$panel, levels = c("S","L"))

pd = position_dodge(0.3)
ggp = ggplot(data = inb, aes(x = roh, y = mean, col = site)) +
  #geom_hline(yintercept = 0.5) +
  #facet_wrap(.~panel, nrow = 2, ncol = 1, scales = "free_y") +
  geom_point(size = 3, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.6, width = 0.2, position = pd) +
  xlab("time") +
  #ylab(expression(paste("F"["ROH"]," (%)")))+
  ylab(expression("F"["ROH"]))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_blank()) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c(">0.1Mb",">1Mb",">5Mb",">10Mb"), 
                      labels = c("2300ya\n(before divergence)","2300-225ya\n(before bounty\nhunting)",
                                 "225-45ya\n(before protection)","45ya-present\n(after protection)")) +
  scale_colour_manual(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                   labels = c("Ranthambore","Wayanad","Kanha","Zoo","All India"), 
                   values = cols[c(2,5,9,7,1)]) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = "bottom")

png('Fig. 2.png', units="in", width=7, height=7, res=1000)
ggp1
dev.off()







###########################################################################


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

a = read.csv("mutations.csv")
a$site = factor(a$site,levels = c("ranthambore","wayanad","Kanha","zoo","panindia"))
mt1 = read.csv("miss_inROH.csv")
mt2 = read.csv("lof_inROH.csv")

mat = rbind(c(1,0,0,0,-1),c(0,1,0,0,-1),c(0,0,1,0,-1),c(0,0,0,1,-1))
library(MASS)
cMat = ginv(mat)

fit = glm(miss ~ site, weights = totalcount_m, data = a, contrasts = list(site = cMat), 
          family = binomial(link = 'logit'))
summary(fit)

newdata = data.frame(site = c("ranthambore","wayanad","Kanha","zoo","panindia"))

pred1 = predict(fit, newdata, se.fit = T, type = "response")

fit = glm(loss ~ site, weights = totalcount_l, data = a, contrasts = list(site = cMat), 
          family = binomial(link = 'logit'))
summary(fit)

pred2 = predict(fit, newdata, se.fit = T, type = "response")

mut = rbind(newdata,newdata)
mut$type = c(rep("missense",5),rep("loss-of-function",5))
mut$type = factor(mut$type, levels = c("missense","loss-of-function"))
mut$site = factor(mut$site,levels = c("ranthambore","wayanad","Kanha","zoo","panindia"))
mut$mean = c(pred1$fit,pred2$fit)
mut$ci = c(pred1$se.fit*1.96,pred2$se.fit*1.96)
mut$met = ""
mut$met[7] = "*"


ggp = ggplot(data = mut, aes(x = site, y = mean*100)) +
  facet_wrap(.~type, nrow = 2, ncol = 1, scales = "free_y") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = (mean-ci)*100, ymax = (mean+ci)*100), size = 0.6, width = 0.1) +
  xlab("geographic region") +
  ylab("deleterious mutations (%)")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  scale_x_discrete(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                   labels = c("Ranthambore","Wayanad","Kanha","Zoo","All India")) +
  geom_text(aes(label=met),hjust=-0.5,vjust=-0.5,size=20)+
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = "bottom")

png('Fig. S2.png', units="in", width=7, height=7, res=1000)
ggp1
dev.off()

d1 = a[a$site != "panindia",]$individual
d2 = a[a$site == "panindia",]$individual
diff = setdiff(d2,d1)
bx = a[a$site != "panindia",]
by = a[a$individual %in% diff,]
b = rbind(bx,by)
b1 = b2 = b
b1$type = "missense"
b2$type = "loss-of-function"
b1$mut = b1$miss
b2$mut = b2$loss
b1 = left_join(b1,mt1)
b2 = left_join(b2,mt2)
b = rbind(b1,b2)

#summary(with(b1,lm(mut_inROH~froh)))
#summary(with(b2,lm(mut_inROH~froh)))

b$type = factor(b$type, levels = c("missense","loss-of-function"))

ggp = ggplot(data = b, aes(x = froh, y = mut*100)) +
  facet_wrap(.~type, nrow = 2, ncol = 1, scales = "free_y") +
  geom_point(size = 1) +
  ylab("deleterious mutations (%)") +
  #xlab(expression(paste("F"["ROH"]," (%)")))+
  xlab(expression("F"["ROH"]))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = "bottom")

png('Fig. S3.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


################################## check for difference in relationship


b$comp = b$site
b$comp = as.character(b$comp)
b[b$comp != "ranthambore",]$comp = "all others"

mat = rbind(c(-1,1))
library(MASS)
cMat = ginv(mat)

b$comp = factor(b$comp, levels = c("all others","ranthambore"))

fit = lm(mut_inROH ~ froh*comp*type, data = b, contrasts = list(comp = cMat))
summary(fit)

newdata = data.frame(comp = rep(c("all others","ranthambore"),each = 13), type = rep("missense",26), 
                     froh = rep(seq(0,0.6,0.05),2))

pred1 = predict(fit, newdata, se.fit = F, type = "response")

int1 = pred1[1]
int2 = pred1[14]

slope1 = (pred1[11] - pred1[1])*2
slope2 = (pred1[24] - pred1[14])*2


##########################################




#b = b %>% filter(site != "zoo")
ggp = ggplot() +
  geom_point(data = b, aes(x = froh, y = mut_inROH, col = site, shape = type), size = 3) +
  #geom_smooth(data = data.frame(y = pred1[3:11], x = seq(0.1,0.6,0.05)), aes(x = x,y = y), 
  #            se = F, col = "black", size = 0.5) +
  #geom_smooth(data = data.frame(y = pred1[21:26], x = seq(0.35,0.6,0.05)), aes(x = x,y = y), 
  #            se = F, col = "black", size = 0.5) +
  geom_abline(intercept = int1, slope = slope1, col = "black") +
  geom_abline(intercept = int2, slope = slope2, col = "black") +
  ylab("deleterious mutations in ROH (%)") +
  #xlab(expression(paste("F"["ROH"]," (%)")))+
  xlab(expression("F"["ROH"]))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  scale_colour_manual(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                      labels = c("Ranthambore","Wayanad","Kanha","Zoo","others"), 
                      values = cols[c(2,5,9,7,1)]) +
  scale_shape_manual(values = c(16,1)) +
  theme(legend.position = "bottom")

png('Fig. 4.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


#b = b %>% filter(site != "zoo")
ggp = ggplot() +
  geom_point(data = b, aes(x = froh, y = mut_inROH, col = site, shape = type), size = 3) +
  #geom_smooth(data = data.frame(y = pred1[3:11], x = seq(0.1,0.6,0.05)), aes(x = x,y = y), 
  #            se = F, col = "black", size = 0.5) +
  #geom_smooth(data = data.frame(y = pred1[21:26], x = seq(0.35,0.6,0.05)), aes(x = x,y = y), 
  #            se = F, col = "black", size = 0.5) +
  #geom_abline(intercept = int1, slope = slope1, col = "black") +
  #geom_abline(intercept = int2, slope = slope2, col = "black") +
  ylab("deleterious mutations in ROH (%)") +
  #xlab(expression(paste("F"["ROH"]," (%)")))+
  xlab(expression("F"["ROH"]))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  scale_colour_manual(breaks = c("ranthambore","wayanad","Kanha","zoo","panindia"),
                      labels = c("Ranthambore","Wayanad","Kanha","Zoo","others"), 
                      values = cols[c(2,5,9,7,1)]) +
  scale_shape_manual(values = c(16,1)) +
  theme(legend.position = "bottom")

png('Fig. 4b.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()




###########################################################################


library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())

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

a = read.csv("R_file_loads_lof_jacknife.csv")
b = read.csv("R_file_loads_missense_jacknife.csv")

c = data.frame(comp = c(rep(names(a), each = length(a[,1])),rep(names(b), each = length(b[,1]))))
c$type = rep(c("missense","loss-of-function"), each = 300)
c$val = c(a[,1],a[,2],a[,3],b[,1],b[,2],b[,3])

fit = lm((val - 1) ~ 0 + comp*type, data = c)
summary(fit)

newdata = data.frame(comp = rep(names(a),2), type = rep(c("missense","loss-of-function"), each = 3))

pred1 = predict(fit, newdata, se.fit = T, type = "response")

newdata$pred = pred1$fit + 1
newdata$cil = newdata$pred - pred1$se.fit*1.96
newdata$cir = newdata$pred + pred1$se.fit*1.96

#pd = position_dodge(0.1)
ggp = ggplot(data = newdata[newdata$type == "missense",], aes(x = comp, y = pred)) +
  #facet_wrap(.~type, nrow = 2, ncol = 1, scales = "free_y") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.6, width = 0.04) +
  xlab("pair-wise comparisons") +
  ylab("relative deleterious loads")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  scale_x_discrete(breaks = c("RRTR.KTR","RWAY.KTR","RRTR.WAY"),
                   labels = c("Ranthambore-Kanha","Wayanad-Kanha","Ranthambore-Wayanad")) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.5) +
  #theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = "bottom")

png('Fig. 3.png', units="in", width=7, height=7, res=1000)
ggp1
dev.off()
