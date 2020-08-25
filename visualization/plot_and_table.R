# install.packages('tinytex')
# tinytex::install_tinytex()
# 
# # writeLines(c(
# #   '\\documentclass{article}',
# #   '\\begin{document}', 'Hello world!', '\\end{document}'
# # ), 'test.tex')
# tinytex::pdflatex('test.tex')


library(gtable)
library(gridExtra)
library(grid)
library(hrbrthemes)
library(ggplot2)
library(Cairo)
library(extrafont)
library(ggplot2)
library(cowplot)


fig1 <- ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(gear))) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_discrete(name = "gear") +
  geom_point()

fig2 <- ggplot(mtcars, aes(x = gear, y = disp, fill = as.factor(gear))) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_discrete(name = "gear") +
  geom_violin()

g_table <- tableGrob(iris[1:4, 1:3], rows = NULL)

# plot_grid(fig1, fig2, labels = "AUTO")

grid.arrange(fig1, g_table,
             ncol=2,
             as.table=TRUE,
             heights=c(3,1))








library(ggpmisc)
library(ggpubr)
library(dplyr)
library(tibble)
mtcars %>%
  group_by(cyl) %>%
  summarize(wt = mean(wt), mpg = mean(mpg)) %>%
  ungroup() %>%
  mutate(wt = sprintf("%.2f", wt),
         mpg = sprintf("%.1f", mpg)) -> tb
df <- tibble(x = 0.95, y = 0.95, tb = list(tb))
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  geom_table_npc(data = df, aes(npcx = x, npcy = y, label = tb),
                 hjust = 1, vjust = 1)


df <- head(iris)

# Default table
# Remove row names using rows = NULL
ggtexttable(df, rows = NULL)
