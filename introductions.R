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
require(lme4)

a = read.csv("introducedroh.csv")
a[is.na(a)] = 0


#a$ind1roh1 = scale(a$ind1roh1,center = F)

mat = rbind(c(-1,1,0,0,0,0,0,0,0),c(-1,0,1,0,0,0,0,0,0),c(-1,0,0,1,0,0,0,0,0),c(-1,0,0,0,1,0,0,0,0),
            c(-1,0,0,0,0,1,0,0,0),c(-1,0,0,0,0,0,1,0,0),c(-1,0,0,0,0,0,0,1,0),c(-1,0,0,0,0,0,0,0,1))
library(MASS)
cMat = ginv(mat)

a$prop1 = a$roh1/a$ind1roh1

data = a[a$res1 == "Ranthambore",]
data$pair = factor(data$pair, levels = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                         "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                         "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                         "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"))

fit1 = glm(data = data, prop1 ~ pair, weights = ind1roh1, contrasts = list(pair = cMat), 
           family = "binomial")

pred1 = predict(fit1, data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                  "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                  "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                  "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo")), 
        se.fit = T, type = "response")

ranthambore1 = data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                   "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                   "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                   "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"),
                          proprem = pred1$fit, se = pred1$se.fit)

ranthambore1 = ranthambore1 %>% arrange(desc(proprem))
ranthambore1$length = "above10k"

##########################

a$prop2 = a$roh2/a$ind1roh2

data = a[a$res1 == "Ranthambore",]
data$pair = factor(data$pair, levels = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                         "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                         "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                         "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"))

fit2 = glm(data = data, prop2 ~ pair, weights = ind1roh2, contrasts = list(pair = cMat), 
           family = "binomial")

pred2 = predict(fit2, data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                          "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                          "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                          "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo")), 
                se.fit = T, type = "response")

ranthambore2 = data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                   "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                   "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                   "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"),
                          proprem = pred2$fit, se = pred2$se.fit)

ranthambore2 = ranthambore2 %>% arrange(desc(proprem))
ranthambore2$length = "above100k"

###########################

a$prop3 = a$roh3/a$ind1roh3

data = a[a$res1 == "Ranthambore",]
data$pair = factor(data$pair, levels = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                         "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                         "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                         "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"))

fit3 = glm(data = data, prop3 ~ pair, weights = ind1roh3, contrasts = list(pair = cMat), 
           family = "binomial")

pred3 = predict(fit3, data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                          "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                          "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                          "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo")), 
                se.fit = T, type = "response")

ranthambore3 = data.frame(pair = c("Ranthambore-Ranthambore","Ranthambore-Corbett",
                                   "Ranthambore-Chandrapur","Ranthambore-Kanha",
                                   "Ranthambore-Kaziranga","Ranthambore-Lalgarh",
                                   "Ranthambore-Wayanad","Ranthambore-Periyar","Ranthambore-Zoo"),
                          proprem = pred3$fit, se = pred3$se.fit)

ranthambore3 = ranthambore3 %>% arrange(desc(proprem))
ranthambore3$length = "above1000k"

ranthambore = rbind(ranthambore1,ranthambore2,ranthambore3)




######## Wayanad

a$prop1 = a$roh1/a$ind1roh1

data = a[a$res1 == "Wayanad",]
data$pair = factor(data$pair, levels = c("Wayanad-Wayanad","Wayanad-Corbett",
                                         "Wayanad-Chandrapur","Wayanad-Kanha",
                                         "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                         "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"))

fit1 = glm(data = data, prop1 ~ pair, weights = ind1roh1, contrasts = list(pair = cMat), 
           family = "binomial")

pred1 = predict(fit1, data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                          "Wayanad-Chandrapur","Wayanad-Kanha",
                                          "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                          "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo")), 
                se.fit = T, type = "response")

wayanad1 = data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                   "Wayanad-Chandrapur","Wayanad-Kanha",
                                   "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                   "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"),
                          proprem = pred1$fit, se = pred1$se.fit)

wayanad1 = wayanad1 %>% arrange(desc(proprem))
wayanad1$length = "above10k"

##########################

a$prop2 = a$roh2/a$ind1roh2

data = a[a$res1 == "Wayanad",]
data$pair = factor(data$pair, levels = c("Wayanad-Wayanad","Wayanad-Corbett",
                                         "Wayanad-Chandrapur","Wayanad-Kanha",
                                         "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                         "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"))

fit2 = glm(data = data, prop2 ~ pair, weights = ind1roh2, contrasts = list(pair = cMat), 
           family = "binomial")

pred2 = predict(fit2, data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                          "Wayanad-Chandrapur","Wayanad-Kanha",
                                          "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                          "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo")), 
                se.fit = T, type = "response")

wayanad2 = data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                   "Wayanad-Chandrapur","Wayanad-Kanha",
                                   "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                   "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"),
                          proprem = pred2$fit, se = pred2$se.fit)

wayanad2 = wayanad2 %>% arrange(desc(proprem))
wayanad2$length = "above100k"

###########################

a$prop3 = a$roh3/a$ind1roh3

data = a[a$res1 == "Wayanad",]
data$pair = factor(data$pair, levels = c("Wayanad-Wayanad","Wayanad-Corbett",
                                         "Wayanad-Chandrapur","Wayanad-Kanha",
                                         "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                         "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"))

fit3 = glm(data = data, prop3 ~ pair, weights = ind1roh3, contrasts = list(pair = cMat), 
           family = "binomial")

pred3 = predict(fit3, data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                          "Wayanad-Chandrapur","Wayanad-Kanha",
                                          "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                          "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo")), 
                se.fit = T, type = "response")

wayanad3 = data.frame(pair = c("Wayanad-Wayanad","Wayanad-Corbett",
                                   "Wayanad-Chandrapur","Wayanad-Kanha",
                                   "Wayanad-Kaziranga","Wayanad-Lalgarh",
                                   "Wayanad-Ranthambore","Wayanad-Periyar","Wayanad-Zoo"),
                          proprem = pred3$fit, se = pred3$se.fit)

wayanad3 = wayanad3 %>% arrange(desc(proprem))
wayanad3$length = "above1000k"

wayanad = rbind(wayanad1,wayanad2,wayanad3)

intro = rbind(ranthambore,wayanad)
intro = intro %>% separate(col = pair, into = c("left", "right"), sep = "-") %>%
  group_by(left,length) %>% arrange(desc(proprem), .by_group = T) %>% ungroup
intro$right = factor(intro$right, levels = unique(intro$right))
intro$cil = intro$proprem - 1.96*intro$se
intro$cir = intro$proprem + 1.96*intro$se
intro$cil[intro$cil<0] = 0
intro$cir[intro$cir>1] = 0

pd = position_dodge(1)

ggp = ggplot(data = intro, aes(x = right, y = proprem, col = length)) +
  facet_wrap(. ~ left, ncol = 1) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.8, position = pd) +
  xlab("population to be introduced") +
  ylab("proportion of homozygous runs remaining")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15))


png('intro.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



