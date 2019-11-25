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

a = read.csv("prop_homozygozity.csv")

inb1mb = data.frame(region = c("north","ranthambore","south","zoo"))
inb1mb$mean = inb1mb$cil = inb1mb$cir = 0

inb5mb = data.frame(region = c("north","ranthambore","south","zoo"))
inb5mb$mean = inb5mb$cil = inb5mb$cir = 0

inb10mb = data.frame(region = c("north","ranthambore","south","zoo"))
inb10mb$mean = inb10mb$cil = inb10mb$cir = 0

inb20mb = data.frame(region = c("north","ranthambore","south","zoo"))
inb20mb$mean = inb20mb$cil = inb20mb$cir = 0

names(a) = c("ind","site","region","mb1","mb5","mb10","mb20","total")

a$mb1 = a$mb1/a$total
a$mb5 = a$mb5/a$total
a$mb10 = a$mb10/a$total
a$mb20 = a$mb20/a$total

unique(a$region)

## north

a1 = a %>% filter(region == "north")

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

a1 = a %>% filter(region == "ranthambore")

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

a1 = a %>% filter(region == "south")

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

a1 = a %>% filter(region == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb1,length(a1$mb1),replace = T)
  b1[i] = mean(b)
}
inb1mb$mean[4] = median(b1)
inb1mb$cil[4] = quantile(b1,0.025)
inb1mb$cir[4] = quantile(b1,0.975)



ggplot(data = inb1mb, aes(x = region, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 5mb


## north

a1 = a %>% filter(region == "north")

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

a1 = a %>% filter(region == "ranthambore")

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

a1 = a %>% filter(region == "south")

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

a1 = a %>% filter(region == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb5,length(a1$mb5),replace = T)
  b1[i] = mean(b)
}
inb5mb$mean[4] = median(b1)
inb5mb$cil[4] = quantile(b1,0.025)
inb5mb$cir[4] = quantile(b1,0.975)



ggplot(data = inb5mb, aes(x = region, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 10mb


## north

a1 = a %>% filter(region == "north")

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

a1 = a %>% filter(region == "ranthambore")

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

a1 = a %>% filter(region == "south")

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

a1 = a %>% filter(region == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb10,length(a1$mb10),replace = T)
  b1[i] = mean(b)
}
inb10mb$mean[4] = median(b1)
inb10mb$cil[4] = quantile(b1,0.025)
inb10mb$cir[4] = quantile(b1,0.975)



ggplot(data = inb10mb, aes(x = region, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)




###################################### 20mb


## north

a1 = a %>% filter(region == "north")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb20,length(a1$mb20),replace = T)
  b1[i] = mean(b)
}
inb20mb$mean[1] = median(b1)
inb20mb$cil[1] = quantile(b1,0.025)
inb20mb$cir[1] = quantile(b1,0.975)


## ranth

a1 = a %>% filter(region == "ranthambore")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb20,length(a1$mb20),replace = T)
  b1[i] = mean(b)
}
inb20mb$mean[2] = median(b1)
inb20mb$cil[2] = quantile(b1,0.025)
inb20mb$cir[2] = quantile(b1,0.975)


## south

a1 = a %>% filter(region == "south")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb20,length(a1$mb20),replace = T)
  b1[i] = mean(b)
}
inb20mb$mean[3] = median(b1)
inb20mb$cil[3] = quantile(b1,0.025)
inb20mb$cir[3] = quantile(b1,0.975)


## zoo

a1 = a %>% filter(region == "zoo")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$mb20,length(a1$mb20),replace = T)
  b1[i] = mean(b)
}
inb20mb$mean[4] = median(b1)
inb20mb$cil[4] = quantile(b1,0.025)
inb20mb$cir[4] = quantile(b1,0.975)



ggplot(data = inb20mb, aes(x = region, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.2)



inb = rbind(inb1mb,inb5mb,inb10mb,inb20mb)
inb$roh = rep(c("1mb","5mb","10mb","20mb"), each = 4)
inb$roh = factor(inb$roh, levels = c("1mb","5mb","10mb","20mb"))
inb$region = factor(inb$region, levels = c("ranthambore","south","north","zoo"))

pd = position_dodge(0.3)
ggp = ggplot(data = inb, aes(x = region, y = mean, col = roh)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.2, position = pd) +
  xlab("geographic region") +
  ylab("proportion of homozygosity runs")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("ranthambore","south","north","zoo"),
                   labels = c("North-West\nIndia","South\nIndia","Central\nCluster","Zoo")) 

png('roh1.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = inb, aes(x = region, y = mean)) +
  facet_wrap(. ~ roh, scale="free", ncol = 2) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.2, position = pd) +
  xlab("geographic region") +
  ylab("proportion of homozygosity runs")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("ranthambore","south","north","zoo"),
                   labels = c("North-West\nIndia","South\nIndia","Central\nCluster","Zoo")) +
  theme(strip.text.x = element_text(size = 15))


png('roh2.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

