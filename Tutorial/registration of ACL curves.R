library(reshape2) # CRAN v1.4.4
library(tidyverse) # CRAN v1.3.0
library(readxl) # CRAN v1.3.1
library(fda) # CRAN v5.1.4
library(ggthemes)
library(ggpubr)
library(ggdark)

############ Lets get warping ...
###### read in the data
###### Left Angle ACLD
S2_Data_imp1 <- read_excel("~/Desktop/rStuff/bfPCA-ACL-Project/S2 Data.xlsx", sheet = 3)
S2_Data_imp2 <- read_excel("~/Desktop/rStuff/bfPCA-ACL-Project/S2 Data.xlsx", sheet = 4)
S2_Data_imp = bind_cols(S2_Data_imp1, S2_Data_imp2)
######## GGPLOT

#View(S2_Data)
S2_Data = as.data.frame(S2_Data_imp)
rownames(S2_Data)=1:100
colnames(S2_Data) = paste("subject", 1:46)
S2_Data %>% rownames_to_column(var = "time") -> S2_Data
S2_plt <- S2_Data %>% melt(id.vars=c("time"), variable.name="subject", value.name = "angle")

ggplot(data=S2_plt)+
  aes(x=as.numeric(time), y=angle, group=subject, color=subject)+
  geom_line()+
  theme(legend.position = "none")
######


#### set up smooting 
time = 1:100
# basis
s2_basis <- create.bspline.basis(rangeval = range(time), norder = 5, nbasis = 103)
# smoothing par
my_spar <- fdPar(fdobj = s2_basis, Lfdobj = int2Lfd(3), lambda = 1e-3)

# and smooth
s2_unreg_smooth <- smooth.basis(argvals = time, y = as.matrix(S2_Data_imp), fdParobj = my_spar)

## plot
plot(s2_unreg_smooth)
lines(mean.fd(s2_unreg_smooth$fd), lwd=3, col="black") # see mean

##### Identify the landmarks
veloctiyfun <- deriv.fd(s2_unreg_smooth$fd,1)
nsubjects <- ncol(S2_Data_imp)
my_landmarks <- matrix(0, nsubjects, 2)
par(mfrow=c(1,1), ask=T)
icase=1
for(icase in 1:nsubjects){
  velveci <- predict(veloctiyfun[icase], time)
  plot(time, velveci, "l", main=paste("case", icase),
       xlab="time", ylab="angle", ylim=c(-1.2,2.2))
  lines(c(1,100), c(0,0), lty=2, lwd=1.2)
  my_loc = locator(n=2)
  my_landmarks[icase, 1]=my_loc$x[1]
  my_landmarks[icase, 2]=my_loc$x[2]
}


write.csv(my_landmarks, file="landmarks.csv")

my_landmarks_mean = apply(my_landmarks, MARGIN = 2, FUN = mean)


#### set up basis for the warping functions
wbasisLM <- create.bspline.basis(c(1,100), nbasis=5, norder = 3, breaks = c(1,my_landmarks_mean,100))

WfdLM = fd(matrix(0,5,1), wbasisLM) # are these initial guesses
WfdParLM <- fdPar(WfdLM, 1, 1e-10)

regListLM <- landmarkreg(veloctiyfun, my_landmarks, my_landmarks_mean, WfdParLM, TRUE)
###### lets have a look
par(mfrow=c(1,2), ask=F)
plot(veloctiyfun, main="Un Registered")
plot(regListLM$regfd)

### lets save some stuff
reg_velocity_fd <- regListLM$regfd
warpfd_lm <- regListLM$warpfd
WfdLM <- regListLM$Wfd

###
par(mfrow=c(1,1))
plot(warpfd_lm)

#### predict a new function

s2_reg_smooth <- register.newfd(yfd = s2_unreg_smooth$fd, warpfd_lm, type = "direct")

par(mfrow=c(1,2), ask=F)
plot(s2_unreg_smooth)
plot(s2_reg_smooth)


###############################
# Extract info for plots
###############################

s2_unreg_df = as.data.frame(eval.fd(evalarg = time, fdobj = s2_unreg_smooth$fd))
rownames(s2_unreg_df)=1:100
colnames(s2_unreg_df) = paste("subject", 1:46)
s2_unreg_df %>% rownames_to_column(var = "time") -> s2_unreg_df
S2_unreg_plt <- s2_unreg_df %>% melt(id.vars=c("time"), variable.name="subject", value.name = "unreg")

s2_reg_df = as.data.frame(eval.fd(evalarg = time, fdobj = s2_reg_smooth))
rownames(s2_reg_df)=1:100
colnames(s2_reg_df) = paste("subject", 1:46)
s2_reg_df %>% rownames_to_column(var = "time") -> s2_reg_df
S2_reg_plt <- s2_reg_df %>% melt(id.vars=c("time"), variable.name="subject", value.name = "reg")

##### lets animate a few of them pick subs between 5&15
library(extrafont)
library(gganimate)
df <- data.frame(x1 = 45, x2 = 50, y1 = -30, y2 = -18)
df2 <- data.frame(x1 = 75, x2 = 85, y1 = -50, y2 = -40)
an <- inner_join(S2_reg_plt, S2_unreg_plt, by = c("time", "subject")) %>%
  mutate(health=case_when(subject %in% paste("subject", 1:23) ~ "healthy",
                          TRUE ~ "injured")) %>%
  #filter(subject %in% paste("subject", 11:20) | subject %in% paste("subject", 31:40) ) %>%
  melt(id.vars=c("time", "subject", "health"), variable.name="registration", value.name = "angle") %>%
  mutate(registration=factor(registration, levels=c("unreg", "reg"), labels=c("unregistered", "registered"))) %>%
  ggplot()+
  #facet_wrap(~registration)+
  geom_line(aes(color=health, x=as.numeric(time), y=angle, group=subject), size=0.9, alpha=0.6)+
  #dark_theme_classic()+
  dark_theme_bw()+
  labs(x="time", y="left knee joint angle", title="Landmark Registration",
       subtitle="{previous_state} curves")+
  transition_states(states = registration)+
  scale_color_manual(values=c("darkgoldenrod1", "cornflowerblue"), name="Group")+
  ease_aes()+
  theme(legend.position = "bottom",
        axis.title = element_text(size=16, family="Comic Sans MS"),
        legend.text = element_text(size=16, family="Comic Sans MS"),
        legend.title = element_text(size=22, family="Comic Sans MS"),
        axis.text = element_text(size=16, family="Comic Sans MS"),
        plot.title = element_text(size=20, hjust=0.5, family="Comic Sans MS"),
        plot.subtitle = element_text(size=16, hjust = 0.5, family="Comic Sans MS", face="italic"))+
  geom_curve(data=df, aes(x=x1, y=y1, xend=x2, yend=y2), curvature = 0.3, arrow = arrow())+
  annotate(geom = "text", x = 37, y=-34, label="Landmark 1", family="Comic Sans MS", size=6, color="white")+
  geom_curve(data=df2, aes(x=x1, y=y1, xend=x2, yend=y2), curvature = 0.3, arrow = arrow())+
  annotate(geom = "text", x = 70, y=-55, label="Landmark 2", family="Comic Sans MS", size=6, color="white")



animate(an, width=600, height=550, renderer = gifski_renderer(file="registration.gif",loop='F'))



