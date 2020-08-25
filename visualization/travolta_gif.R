#LOAD Packages first:====
#to produce and process plot with Travolta GIF:
# install.packages("ggplot2")#this package contains ggplot functions. Install only once
library(ggplot2)#Once installed you need to call it up to the console.
# install.packages("magick")# Install only once if not installed on your comp yet
library(magick)#call it out to RStudio
# install.packages("here")#For making the script run without a working directory
library(here) ##call it out to RStudio
# install.packages(("magrittr"))# Install only once if not installed on your comp yet
library(magrittr) # For piping the logo

#Draw Travolta Boxplot=======
#These data is based on responses from TURF members (50% turnout !!!)
#Let us create three vectors with names, time spent and experience with R:
#[A vector is a sequence of data elements of the same basic type.]

N<-c("Anonymous","Fabiola", "Sarah","Emily","Stephen","Renata", "Steve","Christina")
T <- c(0.38, 5.34, 0.50, 2.41, 1.07, 0.52, 5.56, 1.00)
E<- c(">5" , "1to5","1to5",">5","<1", "<1","1to5",">5")

#Use these three vectors to create a data frame that holds all together:
TURF_Survey<- data.frame(Name = N, Time_Spent = T, R_Experience_Years = E)
str(TURF_Survey)#str stands for structure, see what is looking like

#Produce boxplot and save to your working directory:
ggplot(TURF_Survey, aes(x= "Survey", y=Time_Spent) )+ 
  geom_boxplot()+
  geom_jitter(position=position_jitter(),aes(color=Name, size=Name))+
  theme_bw() +ggtitle("TURF_Survey_2017_With_Travolta") +ylab("Minutes")+ xlab("")+
  theme(axis.text.y=element_text(size=22),
        axis.title.x=element_text(size=22),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=24),
        legend.position = "right",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face="bold"),
        plot.title = element_text(lineheight=1.2, face="bold",hjust = 0.5))+
  ggsave(filename ="TURF_Survey_2017.png",
         width = 5, height = 4, dpi = 300)

#Now plot is saved in your working directory. If not sure where it is go:
getwd()

#ADD TRAVOLTA GIF=========
#Now call back the plot
background <- image_read(paste0(here("/"), "TURF_Survey_2017.png"))
background
# And bring in a logo
logo_raw <- image_read("https://i.imgur.com/e1IneGq.jpg") 
logo_raw #Check him out

animation <- image_animate(image_join(frames))

frames <- lapply(logo_raw, function(frame) {
  image_composite(background, frame, offset = "+70+800")
})

image_write(animation, "~/TURF_Travolta.gif")#It may take quite a lot of computation ~10-30 minuntes.

#Code sourced from:
http://danielphadley.com/ggplot-Logo/