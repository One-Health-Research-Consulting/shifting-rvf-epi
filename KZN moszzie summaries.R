#####KZN Mosquito summaries

#libaries
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(ggtext)

#Data
kznmoz <- read.csv("./Data/Data for KZN nonAedes RVFV vector plot in KZN.csv")

#ggplot parameters
#Set plot theme:
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        plot.title = element_text(size=16))

###Set factor order for x axis and colors
kznmoz$month <- factor(kznmoz$month, levels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug" ))

group.colors <- c(Aedes_other="peachpuff1", `Culex Vectors`= "orange3", Other= "lightskyblue2", Neomelaniconion="indianred3")

#plot
kznmoz_sp_plot <- ggplot(kznmoz, aes(x = month, y = month_mean, fill = Plot_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = stacked_mean - se, ymax = stacked_mean + se), width = .2) +
  scale_fill_manual(values=group.colors, labels = c("Other" = "Other species", "Culex Vectors" = "*Culex* RVFV vectors", "Aedes_other"= "Other *Aedes* spp.", "Neomelaniconion" = "*Neomelaniconion*")) +
  labs(x = "Month",
       y = "Average number \nof mosquitoes",
       fill = "Mosquito group")  +
  plot_theme +
  theme(legend.text = element_markdown())

kznmoz_sp_plot

ggsave(kznmoz_sp_plot, filename = "output/Plot of the average number of mosquitoes collected in KZN by month 2019-2023 - with regard to RVFV vectors.pdf", width=7, height=2.75)
ggsave(plot = kznmoz_sp_plot, filename = "output/Plot of the average number of mosquitoes collected in KZN by month 2019-2023 - with regard to RVFV vectors.svg", width=7, height=2.75)

