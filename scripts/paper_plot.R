
linearplot <- baselinebm %>%
  bind_rows(., baselinebm %>% distinct(cohort, .keep_all = TRUE) %>%
              mutate(timepoint = 0,
                     yi = 0)) %>%
  ggplot(aes(x = timepoint, y = exp(yi), group = cohort)) +
  geom_hline(yintercept = 1, colour = "dark grey") +
  geom_line(alpha = 0.8, colour = "grey") + 
  geom_point(aes(size = actual_n), alpha = 0.3) + 
  scale_y_continuous(limits = c(0.95, 1.27), breaks = c(0.95, 1.0, 1.05, 1.1, 1.2), labels = c("-5%", "0%", "+5%", "+10%", "+20%")) +
  #scale_y_continuous(labels = scales::percent, limits = c(-0.75, 0.25)) +
  #scale_x_continuous(trans = 'log10', limits = c(3,150)) +
  scale_size(breaks = c(50, 500, 1000), name = "Sample\nsize (n)") +
  scale_x_continuous(breaks = c(0, 24, 60, 120, 240), labels = c(0, 2, 5, 10, 20)) +
  geom_line(data = points, aes(x = x, y = pred), colour = "red", linewidth = 1.3, inherit.aes = FALSE) +
  ggdist::geom_lineribbon(points, mapping = aes(x = x, ymin = ci.lb, ymax = ci.ub), fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  ggdist::geom_lineribbon(points, mapping = aes(x = x, ymin = pi.lb, ymax = pi.ub), fill = "grey", alpha = 0.25, inherit.aes = FALSE) +
  labs(x = "Time (years)", y = "Change in BMI/bodymass\n") +
  facet_wrap(~"Time - linear scale") +
  theme_mgpub() +
  theme(panel.grid.major.x = element_line(linewidth = rel(0.5), linetype = 2))


logplot <- baselinebm %>%
  bind_rows(., baselinebm %>% distinct(cohort, .keep_all = TRUE) %>%
              mutate(timepoint = 0.1,
                     yi = 0)) %>%
  ggplot(aes(x = timepoint, y = exp(yi), group = cohort)) +
  geom_hline(yintercept = 1, colour = "dark grey") +
  geom_line(alpha = 0.8, colour = "grey") + 
  geom_point(aes(size = actual_n), alpha = 0.3) + 
  scale_y_continuous(limits = c(0.95, 1.27), breaks = c(0.95, 1.0, 1.05, 1.1, 1.2), labels = c("-5%", "0%", "+5%", "+10%", "+20%")) +
  scale_x_continuous(breaks = c(1, 6, 24, 60, 120, 240), labels = c("1 month", "6 months",2,5,10,20)) +
  coord_trans(x = "log10", xlim = c(1, 250)) +
  #scale_y_continuous(labels = scales::percent, limits = c(-0.75, 0.25)) +
  #scale_x_continuous(trans = 'log10', limits = c(3,250), breaks = c(6, 24, 60, 120, 240), labels = c(0.5,2,5,10,20)) +
  #scale_x_continuous(breaks = c(0, 24, 60, 120, 240), labels = c(0, 2, 5, 10, 20)) +
  scale_size(breaks = c(50, 500, 1000), name = "Sample\nsize (n)") +
  geom_line(data = points , aes(x = x, y = pred), colour = "red", linewidth = 1.3, inherit.aes = FALSE) +
  ggdist::geom_lineribbon(points, mapping = aes(x = x, ymin = ci.lb, ymax = ci.ub), fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  ggdist::geom_lineribbon(points, mapping = aes(x = x, ymin = pi.lb, ymax = pi.ub), fill = "grey", alpha = 0.25, inherit.aes = FALSE) +
   #geom_polygon(data = poly %>% filter(x > 3), aes(x = x, y = ci.ub), fill = "red",  alpha = 0.2, inherit.aes = FALSE) +
  labs(x = "Log Time (years)", y = "") +
  facet_wrap(~"Time - log scale") +
  theme_mgpub() +
  theme(panel.grid.major.x = element_line(linewidth = rel(0.5), linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggpubr::ggarrange(linearplot, logplot, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.15, 1))

ggsave("output/plots/primaryfigure.png", width = 8, height = 4)



bmdata %>%
  bind_rows(., bmdata %>% distinct(cohort, .keep_all = TRUE) %>%
              mutate(timepoint = 0,
                     yi = 0)) %>%
  filter(!cohort %in% c("Moksnes_2013", "Macalpine_2019", "Garcia_2020", "Hanstein_2019",
                        "Rodriguez_2022", "Whittaker_2025"),
         !(cohort == "Triplett_2022" & group == "Adolescent")) %>% # studies with mean age <18
  ggplot(aes(x = timepoint, y = exp(yi), group = cohort)) +
  geom_hline(yintercept = 1, colour = "dark grey") +
  #geom_line(alpha = 0.8, colour = "grey") + 
  #geom_point(aes(size = actual_n), alpha = 0.3) + 
  scale_y_continuous(breaks = c(0.95, 1.0, 1.05, 1.1, 1.2), labels = c("-5%", "0%", "+5%", "+10%", "+20%")) +
  #scale_y_continuous(labels = scales::percent, limits = c(-0.75, 0.25)) +
  #scale_x_continuous(trans = 'log10', limits = c(3,150)) +
  scale_x_continuous(breaks = c(0, 6, 12, 24, 72, 120, 240), labels = c(0, "", 1, 2, 6, 10, 20)) +
  geom_line(data = points, aes(x = x, y = pred), colour = "red", linewidth = 1.3, inherit.aes = FALSE) +
  geom_polygon(data = poly, aes(x = x, y = ci.ub), fill = "red",  alpha = 0.2, inherit.aes = FALSE) +
  #geom_line(data = waves3, aes(x = wave_end, y = exp(cum)), colour = "blue", linewidth = 1.3, inherit.aes = FALSE) +
  geom_lineribbon(data = result, aes(x = time, y = cum_est, ymin = cum_lwr, ymax = cum_upr), fill = "blue", colour = "blue",
                  alpha = 0.2, inherit.aes = F) +
  geom_linerange(data = result, aes(x = time, y = cum_est, ymin = cum_lwr, ymax = cum_upr), colour = "blue",
                 alpha = 0.3, inherit.aes = F) +
  geom_line(data = result, aes(x = time, y = cum_est), colour = "blue",inherit.aes = F, linewidth = 1.3, linetype = "11") +
  labs(x = "Time (years)", y = "Change in BMI/bodymass") +
  theme_mgpub() +
  theme(panel.grid.major.x = element_line(linewidth = rel(0.5), linetype = 2))

ggsave("output/plots/secondaryfigure.png", width = 5, height = 4)


