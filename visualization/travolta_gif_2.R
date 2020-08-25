library(ggplot2)
library(magick)
library(here) # For making the script run without a wd
library(magrittr) # For piping the logo

# Make a simple plot and save it
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + 
  ggtitle("Cars") +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 5, height = 4, dpi = 300)

# Call back the plot
plot <- image_read(paste0(here("/"), "Cars.png"))
# And bring in a logo
logo_raw <- image_read("http://hexb.in/hexagons/ggplot2.png") 

# Scale down the logo and give it a border and annotation
# This is the cool part because you can do a lot to the image/logo before adding it
logo <- logo_raw %>%
  image_scale("100") %>% 
  image_background("grey", flatten = TRUE) %>%
  image_border("grey", "600x10") %>%
  image_annotate("Powered By R", color = "white", size = 30, 
                 location = "+10+50", gravity = "northeast")

# Stack them on top of each other
final_plot <- image_append(image_scale(c(plot, logo), "500"), stack = TRUE)
# And overwrite the plot without a logo
image_write(final_plot, paste0(here("/"), last_plot()$labels$title, ".png"))



# Now call back the plot
background <- image_read(paste0(here("/"), "Cars.png"))
# And bring in a logo
logo_raw <- image_read("https://i.imgur.com/e1IneGq.jpg") 

logo_raw <- image_transparent(logo_raw, 'white')


frames <- lapply(logo_raw, function(frame) {
  image_composite(frame,background, offset = "+70+800")
})


image_composite(logo_raw[50],background, offset = "+70+800")


animation <- image_animate(image_join(frames))


image_write(animation, "~/Cars_Travolta.gif")
