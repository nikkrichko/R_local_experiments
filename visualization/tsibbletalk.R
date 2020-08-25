
# here some coole exampe of interactive plots for tstibble

remotes::install_github("earowang/tsibbletalk")



library(tsibble)
library(tsibbletalk)
tourism_shared <- tourism %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)
p0 <- plotly_key_tree(tourism_shared, height = 900, width = 600)


library(feasts)
tourism_feat <- tourism_shared %>%
  features(Trips, feat_stl)

library(ggplot2)
p1 <- tourism_shared %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y")
p2 <- tourism_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region))

library(plotly)
subplot(p0,
        subplot(
          ggplotly(p1, tooltip = "Region", width = 900),
          ggplotly(p2, tooltip = "Region", width = 900),
          nrows = 2),
        widths = c(.4, .6)) %>%
  highlight(dynamic = TRUE)
