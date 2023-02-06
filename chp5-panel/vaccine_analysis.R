pacman::p_load(data.table, ggplot2, rstudioapi, estimatr, tidyverse, huxtable)

setwd(dirname(getActiveDocumentContext()$path)); getwd()

data_repo <- paste0(getwd())

data_panel <- read_csv(paste(data_repo, "worldbank-immunization-panel.csv", sep = "/"))

data_panel <- data_panel %>%
  filter(!(is.na(imm) | is.na(gdppc))) %>%
  mutate(c = factor(c)) %>%
  group_by(c) %>%
  mutate(balanced = min(year) == 1998 & max(year) == 2017 & length(unique(year)) == 20) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

data_balanced <- data_balanced %>%
  arrange(c, year) %>%
  group_by(c) %>%
  mutate(
    lnpop=log(pop),
    d_surv = surv- lag(surv),
    d_imm = imm - lag(imm),
    d2_imm = d_imm - lag(d_imm), 
    d_lngdppc= lngdppc- lag(lngdppc),
    d_lnpop = lnpop - lag(lnpop),
    avgpop = mean(pop), #for weights in xtreg fe
    year = factor(year)
  ) %>%
  ungroup()

fe1 <- lm_robust(surv ~ imm + year,
                 data = data_balanced,
                 se_type = "stata",
                 fixed_effects = ~ c,
                 clusters = c)
summary(fe1)

fe_lm2 <- lm_robust(surv ~ imm + year + lngdppc + lnpop,
                    data = data_balanced,
                    se_type = "stata",
                    fixed_effects = ~ c,
                    clusters = c)
summary(fe_lm2)


fd_lm <- lm_robust(d_surv~ d_imm,
                   data = data_balanced,
                   se_type="stata",
                   clusters = c); summary(fd_lm)
