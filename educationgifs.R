library(ggplot2)
library(dplyr)
library(gganimate)
library(animation)


#Loading datasets
load("fifty_states.rda")
load("latlongstates.Rda")
policy <- read.csv("statepolicies.csv", header = TRUE)

#Basic Map of US
ggplot() + geom_polygon( data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey10" )

#Examining where data for educational spending is missing
educational_spending <-policy %>%
  group_by(state, year) %>%
  count(Aedpi)

view(educational_spending)

#Dataset of only complete cases for educational spending, don't want empty frames in the animation
educational_spendingDF <- educational_spending[complete.cases(educational_spending), ]

#Adding latitude and longitude points in the middle of every state to the dataframe
educational_spending_complete <- merge(landl,educational_spendingDF,by="state")


#Creating animation showing how spending on education has varied by state from the 1950s-
plot <- (ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
           geom_point(data=educational_spending_complete, aes(x=lon, y=lat, color = Aedpi, size = Aedpi)) + 
           scale_size(name="", range = c(2, 15)) + 
           guides(colour = guide_legend("Education Spending/ \nPercent of Income"), size = guide_legend("Education Spending/ \nPercent of Income"))+
           theme_void() )

animation <- (plot + transition_time(year) + 
                labs(title = "Year: {frame_time}"))

#To play animation of education spending
animation

#To save animation of education spending as multiple pngs and a gif that can be opened in a web browser
anim_save(filename="animation.png", animation = last_animation())
system("convert -delay 40 *.png educationspending.gif")

################################################################################################################################
# Examining data on hs diploma


#Examining where data for educational spending is missing
diploma_percentage <-policy %>%
  group_by(state, year) %>%
  count(hsdiploma)

view(diploma_percentage)

#Dataset of only complete cases for educational spending, don't want empty frames in the animation
diploma_percentageDF <- diploma_percentage[complete.cases(diploma_percentage), ]
#DC is recorded as having no hs graduates, removing from dataset
diploma_percentageDF <- diploma_percentageDF[which(diploma_percentageDF$state!="District of Columbia"),]



#Adding latitude and longitude points in the middle of every state to the dataframe
diploma_percentage_complete <- merge(landl,diploma_percentageDF,by="state")


#Creating animation showing how percentage of high school graduates has varied by state from the 1950s-
plot2 <- (ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
           geom_point(data=diploma_percentage_complete, aes(x=lon, y=lat, color = hsdiploma, size = hsdiploma)) + 
           scale_size(name="", range = c(2, 15)) + 
           guides(colour = guide_legend("Percentage of \nHS graduates"), size = guide_legend("Percentage of \nHS graduates"))+
           theme_void() )

animation2 <- (plot2 + transition_time(year) + 
                labs(title = "Year: {frame_time}"))

#To play animation of hs graduates as percentage of population
animation2

#To save animation of hs graduates as percentage of population as multiple pngs and a gif that can be opened in a web browser
anim_save(filename="animation2.png", animation = last_animation())
system("convert -delay 40 *animation2.png hsdiplomas.gif")
