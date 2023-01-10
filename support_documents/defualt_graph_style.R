# this uses the built-in data set mtcars
library(tidyverse)
library(here)
library(akima)

# for discrete color scales
cbPalette <- c("#999999", "#0072B2", "#D55E00", 
                "#F0E442", "#56B4E9", "#E69F00",
                "#0072B2", "#009E73", "#CC79A7")
# for continuous scales use viridis
library(viridis)

data <- mtcars

#line graphs, scatter plots, the labels are fake and do npot match the data

line_plot = ggplot(arrange(data, hp), aes(x = hp, y = mpg)) +
  theme_classic(base_size = 20) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  # if you need to trim axis
  #coord_cartesian(ylim = c(0,1.0))+
  geom_path(color = "black", linewidth = 1) 
X11()
print(line_plot)
ggsave(filename = here("support_documents", "example_line_plot.png"),
       plot = line_plot,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

scatter_plot = ggplot(data, aes(x = hp, y = qsec, color = factor(cyl))) +
  theme_classic(base_size = 20) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  geom_point(shape = 1, size = 3, stroke = 2) +
  scale_color_manual(values = cbPalette, name = "Number of\nLifecycles") 
X11()
print(scatter_plot)

ggsave(filename = here("support_documents", "example_scatter_plot.png"),
       plot = scatter_plot,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

# if the legend can fit on white space on the graph put it there
scatter_plot_shift = ggplot(data, aes(x = hp, y = qsec, color = factor(cyl))) +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0.8)) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  geom_point(shape = 1, size = 3, stroke = 2) +
  scale_color_manual(values = cbPalette, name = "Number of\nLifecycles") 
X11()
print(scatter_plot_shift)

ggsave(filename = here("support_documents", "example_scatter_plot_shift.png"),
       plot = scatter_plot_shift,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

# a heat map

# A bunch of stuff first just to make som eheat map data 
heatmap_data <- data %>% 
  group_by(hp, wt) %>% 
  summarize(mpg = mean(mpg))

resolution = 100
x_points = seq(min(heatmap_data$hp), max(heatmap_data$hp), length.out = resolution)
y_points = seq(min(heatmap_data$wt), max(heatmap_data$wt), length.out = resolution)

i = interp(x = heatmap_data$hp,
                      y = heatmap_data$wt,
                      z = heatmap_data$mpg,
                      xo = x_points,
                      yo = y_points,
                      linear = TRUE,
                      extrap = FALSE)
heatmap_int_data = cbind(expand.grid(i$x, i$y), c(i$z)) %>% 
  rename(x = 1,
         y = 2, 
         z = 3) %>% 
  na.omit()
  
# now the graph part
heatmap_plot = ggplot(heatmap_int_data, aes(x = x, y = y, z = z, fill = z)) +
  theme_classic(base_size = 20) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  geom_tile(interpolate = TRUE) +
  scale_fill_viridis(name = "mpg") 
X11()
print(heatmap_plot)

ggsave(filename = here("support_documents", "heatmap_plot.png"),
       plot = heatmap_plot,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

# now the graph part
heatmap_plot_shift = ggplot(heatmap_int_data, aes(x = x, y = y, z = z, fill = z)) +
  theme_classic(base_size = 25) +
  theme(legend.position = c(0.15, 0.8),
        legend.background = element_rect(fill='transparent')) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  geom_tile(interpolate = TRUE) +
  scale_fill_viridis(name = "mpg") 
X11()
print(heatmap_plot_shift)

ggsave(filename = here("support_documents", "heatmap_plot_shift.png"),
       plot = heatmap_plot_shift,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)
