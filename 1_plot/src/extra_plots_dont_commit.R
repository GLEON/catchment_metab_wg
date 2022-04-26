

# extra plots
library(targets)
library(tidyverse)

tar_load(p2_annual_data)
annual_data = p2_annual_data

options(na.action = 'na.omit')

# top GPP model for load vs. metab
summary(lm(annual_data$mean_gpp ~ annual_data$mean_c_load +
             annual_data$mean_p_load + annual_data$mean_n_p_load))

gpp_preds = exp(predict(lm(annual_data$mean_gpp ~ annual_data$mean_c_load +
                             annual_data$mean_p_load + annual_data$mean_n_p_load))) %>%
  {
    rows = names(.)
    gpp_preds = .
    tibble(rows = rows, gpp_preds = gpp_preds)
  }

# merging to get missing lakes in here
annual_data$rows = as.character(1:16)
annual_data = left_join(annual_data, gpp_preds)

# gpp plot
gpp_1 = ggplot(annual_data, aes(x = gpp_preds, y = exp(mean_gpp))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~GPP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)),na.rm = T)) +
  xlim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = 4, y = 4.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0) +
  ggtitle(label = 'ln(GPP) ~ ln(DOC load)+\n ln(TP load) + ln(TN:TP load)')

gpp_1


# top R model for load vs. metab
summary(lm(annual_data$mean_r ~ annual_data$mean_c_load +
             annual_data$mean_p_load + annual_data$mean_n_p_load))

r_preds= -exp(predict(lm(annual_data$mean_r ~ annual_data$mean_c_load +
                           annual_data$mean_p_load + annual_data$mean_n_p_load))) %>%
  {
    rows = names(.)
    r_preds = .
    tibble(rows = rows, r_preds = r_preds)
  }

# merging to get missing lakes in here
annual_data = left_join(annual_data, r_preds)

# r plot
r_1 = ggplot(annual_data, aes(x = r_preds, y = -exp(mean_r))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~R~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)),na.rm = T)) +
  xlim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = -4, y = -3.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0) +
  ggtitle(label = 'ln(-R) ~ ln(DOC load)+\n ln(TP load) + ln(TN:TP load)')

r_1

# second best R model
r_preds_2 = -exp(predict(lm(annual_data$mean_r ~ annual_data$mean_p_load +
                              annual_data$mean_n_p_load))) %>%
  {
    rows = names(.)
    r_preds_2 = .
    tibble(rows = rows, r_preds_2 = r_preds_2)
  }

# merging to get missing lakes in here
annual_data = left_join(annual_data, r_preds_2)

r_2 = ggplot(annual_data, aes(x = r_preds_2, y = -exp(mean_r))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~R~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$r_preds_2, -exp(annual_data$mean_r)),na.rm = T)) +
  xlim(range(c(annual_data$r_preds_2, -exp(annual_data$mean_r)),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = -4, y = -3.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0) +
  ggtitle(label = 'ln(-R) ~ ln(TP load) + ln(TN:TP load)')

r_2

# top NEP model for load vs. metab
summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load))

nep_preds= predict(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load)) %>%
  {
    rows = names(.)
    nep_preds = .
    tibble(rows = rows, nep_preds = nep_preds)
  }

# merging to get missing lakes in here
annual_data = left_join(annual_data, nep_preds)

# nep plot
nep_1 = ggplot(annual_data, aes(x = nep_preds, y = mean_nep)) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$nep_preds, annual_data$mean_nep),na.rm = T)) +
  xlim(range(c(annual_data$nep_preds, annual_data$mean_nep),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = .3, y = .4, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0) +
  ggtitle(label = 'NEP ~ ln(TN:TP load)')

nep_1

nep_preds_2 = predict(lm(annual_data$mean_nep ~ 1)) %>%
  {
    rows = names(.)
    nep_preds_2 = .
    tibble(rows = rows, nep_preds_2 = nep_preds_2)
  }

# merging to get missing lakes in here
annual_data = left_join(annual_data, nep_preds_2)

# nep plot
nep_2 = ggplot(annual_data, aes(x = nep_preds_2, y = mean_nep)) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$nep_preds_2, annual_data$mean_nep),na.rm = T)) +
  xlim(range(c(annual_data$nep_preds_2, annual_data$mean_nep),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = .3, y = .4, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0) +
  ggtitle(label = 'NEP ~ intercept')

nep_2

plot_out = cowplot::plot_grid(gpp_1, r_1, r_2, nep_1, nep_2, nrow = 3)

ggsave(filename = '1_plot/tmp/metab_load_all_models.png',
       plot = plot_out, width = 10, height =15 )



#######################################################
# metab vs inlake
library(targets)
library(tidyverse)

tar_load(p2_annual_data)
annual_data = p2_annual_data

options(na.action = 'na.omit')

# top GPP model for inlake vs. metab
summary(lm(annual_data$mean_gpp ~ annual_data$mean_p))

annual_data$gpp_preds= exp(predict(lm(annual_data$mean_gpp ~ annual_data$mean_p)))

# gpp plot
gpp_1 = ggplot(annual_data, aes(x = gpp_preds, y = exp(mean_gpp))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~GPP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)))) +
  xlim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = 4, y = 4.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0)+
  ggtitle(label = 'ln(GPP) ~ ln(TP)')

gpp_1

gpp_preds_2= exp(predict(lm(annual_data$mean_gpp ~ annual_data$mean_c +
                              annual_data$mean_p))) %>%
  {
    rows = names(.)
    gpp_preds_2 = .
    tibble(rows = rows, gpp_preds_2 = gpp_preds_2)
  }

# merging to get missing lakes in here
annual_data$rows = as.character(1:16)
annual_data = left_join(annual_data, gpp_preds_2)

# gpp plot
gpp_2 = ggplot(annual_data, aes(x = gpp_preds_2, y = exp(mean_gpp))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~GPP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$gpp_preds_2, exp(annual_data$mean_gpp)),na.rm = T)) +
  xlim(range(c(annual_data$gpp_preds_2, exp(annual_data$mean_gpp)),na.rm = T)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = 4, y = 4.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0)+
  ggtitle(label = 'ln(GPP) ~ DOC + ln(TP)')

gpp_2


# top R model for inlake vs. metab
summary(lm(annual_data$mean_r ~ annual_data$mean_p))

annual_data$r_preds= -exp(predict(lm(annual_data$mean_r ~ annual_data$mean_p)))

# r plot
r_1 = ggplot(annual_data, aes(x = r_preds, y = -exp(mean_r))) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~R~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)))) +
  xlim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = -4, y = -3.6, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0)+
  ggtitle(label = 'ln(-R) ~ ln(TP)')

r_1

# top NEP model for inlake vs. metab
summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

annual_data$nep_preds= predict(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

# nep plot
nep_1 = ggplot(annual_data, aes(x = nep_preds, y = mean_nep)) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$nep_preds, annual_data$mean_nep))) +
  xlim(range(c(annual_data$nep_preds, annual_data$mean_nep))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = .3, y = .4, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0)+
  ggtitle(label = 'NEP ~ ln(TN:TP)')

nep_1

annual_data$nep_preds_2= predict(lm(annual_data$mean_nep ~ annual_data$mean_p +
                                      annual_data$mean_n_p))

# nep plot
nep_2 = ggplot(annual_data, aes(x = nep_preds_2, y = mean_nep)) +
  geom_point(size = 6, alpha = .5) +
  ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
  xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
  ylim(range(c(annual_data$nep_preds_2, annual_data$mean_nep))) +
  xlim(range(c(annual_data$nep_preds_2, annual_data$mean_nep))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  annotate(geom = 'text',
           x = .3, y = .4, angle = 45, size = 6,
           label = '1:1')+
  # adding labels for outliers
  geom_text(aes(label=lake), vjust = 0, hjust = 0)+
  ggtitle(label = 'NEP ~ ln(TP) + ln(TN:TP)')

nep_2

plot_out = cowplot::plot_grid(gpp_1, gpp_2, r_1, nep_1, nep_2, nrow = 3)

ggsave(filename = '1_plot/tmp/metab_inlake_all_models.png',
       plot = plot_out, width = 10, height =15 )


