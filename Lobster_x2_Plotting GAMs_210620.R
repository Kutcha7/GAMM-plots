### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# Script to plot individual GAM models and also plot any interactions

### hello tim

# librarys----
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(mgcv)
library(png)
library(jpeg)
library(gridExtra)
library(grid)
library(googlesheets)


rm(list=ls())
study<-"lobster.density"

# Where the data sits----
# Set work directory----
work.dir=("~/Google Drive/Analysis/Analysis_Irob_Rottnest WRL") #for Tim's Mac
# work.dir=("/Users/Kutcha/Google Drive/Analysis_Irob_Rottnest WR") #change for Katja

tidy.data=paste(work.dir,"Data/Tidy data",sep="/")
plots=paste(work.dir,"Plots",sep="/")
model.out=paste(work.dir,"ModelOut",sep="/")


# Themes and functions for plotting----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# functions for summarising data on plots---
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)



# Bring in the data----

setwd(tidy.data)
dir()

# From googlesheet---
gs_ls() #to enable google sheet import

import <- gs_title("lobster.density1805.xlsx")%>%
  gs_read_csv(ws = "lobster.density")


# Format and mutate import----

dat<-import%>%
  mutate(legal.size=(x75+x85+x95+x105+x115+x125+x135+x145))%>% #removed x75 and added to sublegal, did not change much - tjl put this back
  mutate(sub.legal=(x25+x35+x55+x65))%>%
  mutate(sanctuary=ifelse(sanctuary=="Armstrong Bay","Armstrong",sanctuary))%>% #rationalise the names of sanctuaries
  mutate(sanctuary=ifelse(sanctuary=="armstrong","Armstrong",sanctuary))%>%
  mutate(sanctuary=ifelse(sanctuary=="green","Green Island",sanctuary))%>%
  mutate(sanctuary=ifelse(sanctuary=="parker","Parker Point",sanctuary))%>%
  select(year,sanctuary,status,site,complexity,algal.cover,legal.size,sub.legal)%>%
  gather(key=Taxa, value = response, (match("algal.cover",names(.))+1):ncol(.))%>%
  mutate(sanctuary=as.factor(sanctuary))%>%
  mutate(status=as.factor(status))%>%
  mutate(site=as.factor(site))%>%
  mutate(dummy=runif(3912))%>% #we need a dummy!
  na.omit()%>%
  mutate(status=ifelse(status=="inside","No-take",ifelse(status=="outside","Fished",status)))

tail(dat,2)
unique(dat$complexity)
unique(dat$status)

# Fix a format bug in dplyr()--
write.csv(dat,"dat.csv") 
dat<-read.csv("dat.csv")

# Manually make the most parsimonious GAM model  ----

# MODEL sub-Legal Complexity + Year by Status +Status----

gamm=gam(response~s(complexity,k=3,bs='cr')+s(year,k=3,bs='cr',by=status)+s(sanctuary,site,bs="re")+status, family=tw(),data=dat%>%filter(Taxa=="sub.legal"))
summary(gamm)

# predicts.sub.legal.status----
mod<-gamm
testdata <- crossing(complexity=mean(mod$model$complexity),
                     year=mean(mod$model$year),
                     sanctuary=(mod$model$sanctuary), #random factor does not need mean()
                     site=(mod$model$site),  #random factor does not need mean()
                     status = c("No-take","Fished")) 

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.sub.legal.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.sub.legal.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.sub.legal.status<-read.csv("predicts.csv")
head(predicts.sub.legal.status,5)


# predicts.sub.legal.status.by.year----
mod<-gamm
testdata <- crossing(year=(seq(min(dat$year),max(dat$year),length.out = 20)),
                     complexity=mean(mod$model$complexity),
                     sanctuary=(mod$model$sanctuary),
                     site=(mod$model$site),
                     status = c("No-take","Fished")) 

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.sub.legal.status.by.year = testdata%>%data.frame(fits)%>%
  group_by(status,year)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
head(predicts.sub.legal.status.by.year)
write.csv(predicts.sub.legal.status.by.year,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.sub.legal.status.by.year<-read.csv("predicts.csv")
head(predicts.sub.legal.status.by.year,5)



# predicts.sub.legal.complexity----
mod<-gamm
testdata <- crossing(complexity=(seq(min(dat$complexity),max(dat$complexity),length.out = 20)),
                     year=mean(mod$model$year),
                     sanctuary=(mod$model$sanctuary),
                     site=(mod$model$site),
                     status = c("No-take","Fished")) 

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.sub.legal.complexity = testdata%>%data.frame(fits)%>%
  group_by(complexity)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
head(predicts.sub.legal.complexity)
write.csv(predicts.sub.legal.complexity,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.sub.legal.complexity<-read.csv("predicts.csv")
head(predicts.sub.legal.complexity,5)



# MODEL Legal FOR KATJA TO FILL IN----
# complexity+year+status

# gamm2=gam(response~s(complexity,year, bs='cr')+status, family=tw(),data=dat%>%filter(Taxa=="legal.size")) #TJL  this is wrong

gamm2=gam(response~s(complexity,k=3,bs='cr')+s(year,k=3,bs='cr')+s(sanctuary,site,bs="re")+status, family=tw(),data=dat%>%filter(Taxa=="legal.size"))

# summary(gamm) #wrong model
summary(gamm2)


# predicts.legal.status----
mod2<-gamm2
testdata <- crossing(complexity=mean(mod2$model$complexity),
                     year=mean(mod2$model$year),
                     sanctuary=(mod2$model$sanctuary), #random factor does not need mean()
                     site=(mod2$model$site),  #random factor does not need mean()
                     status = c("No-take","Fished")) 

fits2 <- predict.gam(mod2, newdata=testdata, type='response', se.fit=T)
predicts.legal.status = testdata%>%data.frame(fits2)%>%
  group_by(status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.legal.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.legal.status<-read.csv("predicts.csv")
head(predicts.legal.status,5)


#####
# predicts.legal.complexity----
mod2<-gamm2
testdata <- crossing(complexity=(seq(min(dat$complexity),max(dat$complexity),length.out = 20)),
                     year=mean(mod2$model$year),
                     sanctuary=(mod2$model$sanctuary),
                     site=(mod2$model$site),
                     status = c("No-take","Fished")) 

fits2 <- predict.gam(mod2, newdata=testdata, type='response', se.fit=T)
predicts.legal.complexity = testdata%>%data.frame(fits2)%>%
  group_by(complexity)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
head(predicts.legal.complexity)
write.csv(predicts.legal.complexity,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.legal.complexity<-read.csv("predicts.csv")
head(predicts.legal.complexity,5)

# predicts.legal.year
mod2<-gamm2
testdata <- crossing(year=(seq(min(dat$year),max(dat$year),length.out = 20)),
                     complexity=mean(mod2$model$complexity),
                     sanctuary=(mod2$model$sanctuary),
                     site=(mod2$model$site),
                     status = c("No-take","Fished")) 

fits2 <- predict.gam(mod2, newdata=testdata, type='response', se.fit=T)
predicts.legal.year = testdata%>%data.frame(fits2)%>%
  group_by(year)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
head(predicts.legal.year)
write.csv(predicts.legal.year,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.legal.year<-read.csv("predicts.csv")
head(predicts.legal.year,5)



# PLOTS----
setwd(plots)

# PLOTS  sub-Legal Complexity + Year by Status +Status----
head(predicts.sub.legal.status,2)

ggmod.sub.Legal.status<- ggplot(aes(x=status,y=response,fill=status,colour=status), data=predicts.sub.legal.status) +
  ylab("Abundance")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c( "red","black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c( "red","black"))+
  scale_x_discrete(limits = (levels(predicts.sub.legal.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Sub-legal",vjust = 1, hjust = -.1,size=5)
ggmod.sub.Legal.status


###different colour scheme
ggmod.sub.Legal.status2<- ggplot(aes(x=status,y=response,fill=status,colour=status), data=predicts.sub.legal.status) +
  ylab("Abundance")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c( "gray","black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c( "gray","black"))+
  scale_x_discrete(limits = (levels(predicts.sub.legal.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "   Sub-legal",vjust = 1, hjust = -.1,size=5)
ggmod.sub.Legal.status2

##### status*year
head(predicts.sub.legal.status.by.year,2)
ggmod.sub.Legal.status.x.year<- ggplot(aes(x=year,y=response,colour=status), data=dat%>%filter(Taxa=="sub.legal")) +
  ylab("Abundance (sqrt)")+
  xlab('Year')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.sub.legal.status.by.year,show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.status.by.year,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.status.by.year,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
  # geom_blank(data=dat.bds,aes(x=distance,y=response*1.05))#to nudge data off annotations
ggmod.sub.Legal.status.x.year

###2

head(predicts.sub.legal.status.by.year,2)
ggmod.sub.Legal.status.x.year2<- ggplot(aes(x=year,y=response,colour=status), data=dat%>%filter(Taxa=="sub.legal")) +
  ylab("Abundance (sqrt)")+
  xlab('Year')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("gray","black"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.sub.legal.status.by.year,show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.status.by.year,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.status.by.year,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=distance,y=response*1.05))#to nudge data off annotations
ggmod.sub.Legal.status.x.year2


####complexity

head(predicts.sub.legal.complexity,2)
ggmod.sub.Legal.complexity<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Habitat complexity')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="sub.legal"),aes(x=complexity,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response),alpha=0.5)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.sub.Legal.complexity

#### 2
head(predicts.sub.legal.complexity,2)
ggmod.sub.Legal.complexity2<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Habitat complexity')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("gray","black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="sub.legal"),aes(x=complexity,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response),alpha=0.5)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.legal.complexity,aes(x=complexity,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.sub.Legal.complexity2




########
# PLOTS  Legal Complexity + Year  +Status----
head(predicts.legal.status,2)

ggmod.Legal.status<- ggplot(aes(x=status,y=response,fill=status,colour=status), data=predicts.legal.status) +
  ylab("Abundance")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c( "red","black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c( "red","black"))+
  scale_x_discrete(limits = (levels(predicts.legal.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = " Legal",vjust = 1, hjust = -.5,size=5)
ggmod.Legal.status


###b&g
ggmod.Legal.status2<- ggplot(aes(x=status,y=response,fill=status,colour=status), data=predicts.legal.status) +
  ylab("Abundance")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c( "gray","black"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c( "gray","black"))+
  scale_x_discrete(limits = (levels(predicts.legal.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = " Legal",vjust = 1, hjust = -.5,size=5)
ggmod.Legal.status2


###year
head(predicts.legal.year,2)
ggmod.Legal.year<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Year')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="legal.size"),aes(x=year,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response),alpha=0.5)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.Legal.year


###b&g
head(predicts.legal.year,2)
ggmod.Legal.year2<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Year')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("gray","black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="legal.size"),aes(x=year,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response),alpha=0.5)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.year,aes(x=year,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(e)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.Legal.year2



####complexity

head(predicts.legal.complexity,2)
ggmod.Legal.complexity<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Habitat complexity')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="legal.size"),aes(x=complexity,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response),alpha=0.5)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.Legal.complexity


###b&g
ggmod.Legal.complexity2<- ggplot() +
  ylab("Abundance (sqrt)")+
  xlab('Habitat complexity')+
  #   ggtitle(substitute(italic(name)))+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("gray","black"))+
  #   geom_jitter(width = 0.25,height = 0)+
  geom_jitter(width = 0.25,height = 0,data=dat%>%filter(Taxa=="legal.size"),aes(x=complexity,y=response,colour=status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response),alpha=0.5)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.complexity,aes(x=complexity,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_sqrt()+
  annotate("text", x = -Inf, y=Inf, label = "(f)",vjust = 1, hjust = -.1,size=5)
# geom_blank(data=dat.bds,aes(x=sqrt.X500um,y=response*1.05))#to nudge data off annotations
ggmod.Legal.complexity2




# Combined  plots----
setwd(plots)
dir()
# TargetLoc.plot------
# To see what they will look like use grid.arrange()

###sublegal
blank <- grid.rect(gp=gpar(col="white")) #demonstration of blank
grid.arrange(ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,ggmod.sub.Legal.complexity,
             ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,blank,
             nrow=2,ncol=3,
             left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
             right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))


####black&gray
grid.arrange(ggmod.sub.Legal.status2,ggmod.sub.Legal.status.x.year2,ggmod.sub.Legal.complexity2,
             nrow=1,ncol=3,
             left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
             right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))


# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
all.plot<-arrangeGrob(ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,ggmod.sub.Legal.complexity,
                            ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,blank,
                            nrow=2,ncol=3,
                            left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
                            right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))
ggsave(all.plot,file="all.plot.png", width = 30, height = 20,units = "cm")



####legal size

blank <- grid.rect(gp=gpar(col="white")) #demonstration of blank
grid.arrange(ggmod.Legal.status,ggmod.Legal.year,ggmod.Legal.complexity,
             ggmod.Legal.status,ggmod.Legal.year,blank,
             nrow=2,ncol=3,
             left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
             right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))

# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
all.plot<-arrangeGrob(ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,ggmod.sub.Legal.complexity,
                      ggmod.sub.Legal.status,ggmod.sub.Legal.status.x.year,blank,
                      nrow=2,ncol=3,
                      left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
                      right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))
ggsave(all.plot,file="all.plot.png", width = 30, height = 20,units = "cm")



####legal + sublegal

blank <- grid.rect(gp=gpar(col="white")) #demonstration of blank
grid.arrange(ggmod.sub.Legal.status2,ggmod.sub.Legal.status.x.year2,ggmod.sub.Legal.complexity2,
             ggmod.Legal.status2,ggmod.Legal.year2,ggmod.Legal.complexity2,
             nrow=2,ncol=3,
             left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
             right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))


all.plot2<-arrangeGrob(ggmod.sub.Legal.status2,ggmod.sub.Legal.status.x.year2,ggmod.sub.Legal.complexity2,
                       ggmod.Legal.status2,ggmod.Legal.year2,ggmod.Legal.complexity2,
                       nrow=2,ncol=3,
                       left=textGrob(" ", rot=90,hjust=0.5, x = unit(0.5, "npc")),
                       right=textGrob("  ", rot=90,hjust=0.5, x = unit(0.5, "npc")))
ggsave(all.plot2,file="all.plot2.png", width = 30, height = 20,units = "cm")

