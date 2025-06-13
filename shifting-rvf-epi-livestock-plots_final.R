library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(ggpubr)
library(egg)

#Data
dat <- read_csv("data/RVFV serology in livestock sampled in 2015 and 2017 in South Africa.csv")

dat_df <- dat%>%
  mutate(age = replace(age, Species == "Ovine" & age == 6, 4),#We are grouping sheep and goats as >=4
         Species = case_when(Species == "Bovine" ~ "Cattle",
                             Species == "Ovine" ~ "Sheep",
                             Species == "Caprine" ~ "Goats"))%>%
  mutate(cohort_year = year - age)

###All species
dsn_all <- dat_df%>%
  srvyr::as_survey_design()

tot_all <- dsn_all %>%
  group_by(year, sero)%>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(year)%>%
  mutate(across(tot, sum, .names = "{.col}_sum"))%>%
  ungroup()%>%
  filter(sero == 1)%>%
  dplyr::mutate(bin = paste("sampled", year, sep = "_"))%>%
  dplyr::select(bin, sero, tot, prop, prop_low, prop_upp, tot_sum)

#Plot theme
p.theme <-   theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        plot.title = element_text(size = rel(1.5), colour = "black", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",#Remove legend from cattle and goats
        axis.title = element_text(size = rel(1.5)))


##################
#Cattle

#Bin cattle to make the plot narrower
dat_df_c_all <- dat_df%>%
  filter(Species == "Cattle")%>%
  mutate(bin = case_when(cohort_year >2002  & cohort_year <2007 ~"2003-06",
                         cohort_year >=2007 & cohort_year < 2011 ~ "2007-10",
                         cohort_year == 2011                     ~ "\u2265 2011",
                         cohort_year >2011  & cohort_year < 2016 ~ "2012-15",
                         cohort_year == 2016                     ~ "2016"))%>%
  ungroup()


#estimate confidence interval

dsn_c <- dat_df_c_all%>%
  srvyr::as_survey_design(strata = bin)

tot_cat <- dsn_c %>%
  group_by(year, sero)%>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(year)%>%
  mutate(across(tot, sum, .names = "{.col}_sum"))%>%
  ungroup()%>%
  filter(sero == 1)%>%
  dplyr::mutate(bin = paste("sampled", year, sep = "_"))%>%
  dplyr::select(bin, sero, tot, prop, prop_low, prop_upp, tot_sum)

stat_dat_c <- dsn_c %>%
     group_by(bin, sero) %>%
     summarize(
       tot = n(),
       prop = survey_prop(vartype = "ci", level = 0.95))%>%
  ungroup()%>%
  group_by(bin) %>%
  mutate(across(tot, sum, .names = "{.col}_sum")) %>%
  ungroup()%>%
     filter(sero == 1)%>%
  mutate(bin = replace(bin, bin == "≥ 2011", "\u2264 2011"))

cat_fs_serop <- tot_cat%>%
  bind_rows( stat_dat_c)%>%
  mutate(species = "cattle")%>%
  rename(cohort_year = bin)

#Make x-axis value a factor so stays in order
stat_dat_c$bin = factor(stat_dat_c$bin, level = c("2003-06", "2007-10", "\u2264 2011",  "2012-15", "2016"))

#Plot proportion seropositive by year
cattle_serop_by_age_plot <-   ggplot(stat_dat_c, aes(bin, prop)) +
  geom_bar(stat="identity", fill = "orchid") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  labs(title = "Cattle",  y = "Proportion \nSeropositive", x = "Year of Birth")+
  annotate("text", x = stat_dat_c$bin, y = 1.1, label = stat_dat_c$tot_sum) +
  p.theme+
  theme(axis.title.x = element_blank())


cattle_serop_by_age_plot


#ggsave("./Figure of percent seropositive cattle by year of birth.pdf", cattle_serop_by_age_plot, device = cairo_pdf, width = 3.5, height = 2.75)

#Sheep
dat_df_s <- dat_df%>%
  filter( Species == "Sheep")%>%
  mutate_at("cohort_year", as.character)%>%
  mutate(cohort_year = replace(cohort_year, cohort_year == "2011", "\u2264 2011"))

dat_df_s$cohort_year = factor(dat_df_s$cohort_year, level = c("\u2264 2011", "2012", "2013", "2014", "2015", "2016"))

#Set survey design
dsn_s <- dat_df_s%>%
  srvyr::as_survey_design(strata = cohort_year)


tot_sh <- dsn_s %>%
  group_by(year, sero)%>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(year)%>%
  mutate(across(tot, sum, .names = "{.col}_sum"))%>%
  ungroup()%>%
  filter(sero == 1)%>%
  dplyr::mutate(cohort_year = paste("sampled", year, sep = "_"))%>%
  dplyr::select(cohort_year, sero, tot, prop, prop_low, prop_upp, tot_sum)

#Calculate seroprev and ci
stat_dat_s <- dsn_s %>%
  group_by(cohort_year, sero) %>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(cohort_year) %>%
  mutate(across(tot, sum, .names = "{.col}_sum")) %>%
  ungroup()%>%
  filter(sero == 1)

sh_fs_serop <- tot_sh%>%
  bind_rows( stat_dat_s)%>%
  mutate(species = "sheep")

sheep_serop_by_age_plot <-   ggplot(stat_dat_s, aes(cohort_year, prop)) +
  geom_bar(stat="identity", fill = "orchid") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  labs(title = "Sheep",  y = "Proportion \nSeropositive", x = "Year(s) of Birth")+
  annotate("text", x = stat_dat_s$cohort_year, y = 1.1, label = stat_dat_s$tot) +
  p.theme+
  theme(axis.title.y = element_blank())


sheep_serop_by_age_plot


#ggsave("./Figure of percent seropositive sheep by year of birth.pdf", sheep_serop_by_age_plot, device = cairo_pdf, width = 4.5, height = 2.75)

#########
#goats
dat_df_g <- dat_df%>%
  filter( Species == "Goats")%>%
  mutate_at("cohort_year", as.character)%>%
  mutate(cohort_year = replace(cohort_year, cohort_year == "2011", "\u2264 2011"))

dat_df_g$cohort_year = factor(dat_df_g$cohort_year, level = c("\u2264 2011", "2012", "2013", "2014", "2015", "2016"))

#Set survey design
dsn_g <- dat_df_g%>%
  srvyr::as_survey_design(strata = cohort_year)

tot_g <- dsn_g %>%
  group_by(year, sero)%>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(year)%>%
  mutate(across(tot, sum, .names = "{.col}_sum"))%>%
  ungroup()%>%
  filter(sero == 1)%>%
  dplyr::mutate(cohort_year = paste("sampled", year, sep = "_"))%>%
  dplyr::select(cohort_year, sero, tot, prop, prop_low, prop_upp, tot_sum)

#Calculate seroprev and ci
stat_dat_g <- dsn_g %>%
  group_by(cohort_year, sero) %>%
  summarize(
    tot = n(),
    prop = survey_prop(vartype = "ci", level = 0.95))%>%
  group_by(cohort_year) %>%
  mutate(across(tot, sum, .names = "{.col}_sum")) %>%
  ungroup()%>%
  filter(sero == 1)

g_fs_serop <- tot_g%>%
  bind_rows( stat_dat_g)%>%
  mutate(species = "goats")

goat_serop_by_age_plot <-   ggplot(stat_dat_g, aes(cohort_year, prop)) +
  geom_bar(stat="identity", fill = "orchid") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  labs(title = "Goats",  y = "Proportion \nSeropositive", x = "Year of Birth")+
  annotate("text", x = stat_dat_s$cohort_year, y = 1.1, label = stat_dat_s$tot) +
  p.theme +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())


goat_serop_by_age_plot


#ggsave("./Figure of percent seropositive goats by year of birth.pdf", goat_serop_by_age_plot, device = cairo_pdf, width = 3.5, height = 2.75)

##########################################
#KZN example from van den Bergh et al 2019 Table 1 & Table 2

kzn_dat <- data.frame("species" = c("Cattle", "Cattle", "Cattle", "Cattle", "Goats", "Goats","Goats"),
                      "age" = c("< 2", "2 - < 4", "4-6", ">6", "0.5 - 1.5", "> 1.5 - 3.5", "> 3.5"),
                      "cohort_year" = c("2016-2017", "2014-2015", "2011-2013", "< 2011", "2016", "2014-2015", "< 2014"),
                      "tot" = c(56, 96, 157, 114, 25, 36, 43),
                      "prop" = c(0.34, 0.23, 0.39, 0.34, 0.24, 0.28, 0.40),
                      "prop_low" = c(0.22, 0.15, 0.31, 0.25, 0.09, 0.14, 0.25),
                      "prop_upp" = c(0.46, 0.31, 0.47, 0.43, 0.45, 0.45, 0.56 )
                      )

####
#Cattle
kzn_dat_c <- filter(kzn_dat, species == "Cattle")

kzn_cattle_serop_by_age_plot <-   ggplot(kzn_dat_c, aes(cohort_year, prop)) +
  geom_bar(stat="identity", fill = "orchid") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  labs(title = "Cattle",  y = "Proportion \nSeropositive", x = "Year(s) of Birth")+
  annotate("text", x = kzn_dat_c$cohort_year, y = 1.1, label = kzn_dat_c$tot) +
  p.theme

kzn_cattle_serop_by_age_plot

####
#Goats
kzn_dat_g <- filter(kzn_dat, species == "Goats")

kzn_goat_serop_by_age_plot <-   ggplot(kzn_dat_g, aes(cohort_year, prop)) +
  geom_bar(stat="identity", fill = "orchid") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  labs(title = "Goats",  y = "Proportion \nSeropositive", x = "Year(s) of Birth")+
  annotate("text", x = kzn_dat_g$cohort_year, y = 1.1, label = kzn_dat_g$tot) +
  p.theme+
  theme(axis.title.y = element_blank())

kzn_goat_serop_by_age_plot

filname  = "output/S1 plot of seroprevalence among ages in livestock from RSA.pdf"
grDevices::cairo_pdf(filename = filname, width = 8, height = 6)

ggarrange(cattle_serop_by_age_plot, goat_serop_by_age_plot, sheep_serop_by_age_plot,
                        kzn_cattle_serop_by_age_plot, kzn_goat_serop_by_age_plot,
                        ncol = 3, nrow = 2,
                        labels = c("Free State", "", "", "KwaZulu-Natal", "")
                        )

dev.off()

#ggexport(final_plot, filename = filname, width = 7, height = 5)

tab <- cat_fs_serop%>%
  bind_rows(sh_fs_serop,g_fs_serop)%>%
  filter(cohort_year == "sampled_2015" | cohort_year == "sampled_2017")
