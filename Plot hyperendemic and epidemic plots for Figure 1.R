#' Diagrams for Figure 1.
#' Run model at DOI: 10.5281/zenodo.15632526.
#' For epidemic run the scenario: muC.25_lower <-  TRUE. Then adjust the host plot on the main script to only show recovered, susceptible and infected.
#' For hyperedenmic run the vaccinate scenario (Vaccinate <- TRUE) and set tvax.proportion <- .16. Then use the code below.

final.populations_vax <- final.populations%>%
  mutate(RL = replace(RL, RL < 1, 0))%>%
  mutate(RS = replace(RS, RS < 1, 0))%>%
  mutate(IL = replace(IL, IL < 1, 0))%>%
  mutate(IS = replace(IS, IS < 1, 0))%>%
  mutate(VL = replace(VL, VL < 1, 0))%>%
  mutate(VS = replace(VS, VS < 1, 0))%>%
  mutate(NS = replace(NS, NS < 1, 0))%>%
  mutate(NL = replace(NL, NL < 1, 0))%>%
  mutate(RN = RL+VL+AL+VS+RS)%>% #Representing the seropositive/immune population consisting of recovered and vaccinated
  mutate(SN = SS+SL)%>%
  mutate(IN = IS + IL)

all.labs <- unique(final.populations_vax$Year)
few.labs <- seq(head(all.labs,1), tail(all.labs,1), 5 )
n.labs <- length(all.labs)
generic.labs <- seq(1, n.labs, 5)


Seroprev_vax <- final.populations_vax%>%
  mutate(SeroP_SL = RN/(NS+NL))%>%
  mutate(infected = IN/(NS+NL))%>%
  group_by(MosqYear)%>%
  summarise(SeroP_SL = max(SeroP_SL),
            infect_SL = max(infected))

mean_ann_serop <- mean(Seroprev_vax$SeroP_SL)

mean_ann_serop

#Set ggplot theme
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))

#'Sheep and Lambs
SLplot_n <-ggplot(final.populations_vax, aes(x=time)) +
  geom_line(aes(y = SN, colour = "SN"), linewidth = 2) +
  geom_line(aes(y = RN, colour = "RN"), linewidth = 2) +
  geom_line(aes(y = IN, colour = "IN"), linewidth = 2) +
  #xlim) + # 2009
  labs( x = "Year", y = "Host") +#, color = "Population") +
  scale_x_continuous(labels = generic.labs, breaks = seq(from = 1, to = end.time, by = 365*5), limits = c(1, 9490)) +
  #xlim(1, 733285) +
  scale_colour_manual( name = "Population", values =c("SN" = "black","IN"= "red", "RN" = "green")) +
  plot_theme+
  theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
        legend.position = "none")#decrease space between the legend items

SLplot_n

Figlancet <- ggarrange(SLplot_n, draw = FALSE, ncol = 1, nrow = 1)

fil.nameS3 <- "../Fig example of potential hyperendemic system for lancet paper.pdf"


ggexport(Figlancet[1], filename = fil.nameS3, width=6, height=2)
