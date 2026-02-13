###--------------------------------
###Combine Seasons
###--------------------------------

rsf_data_weighted$season <- "March"
rsf_data_weighted_july$season <- "July"

rsf_both_seasons <- rbind(rsf_data_weighted, rsf_data_weighted_july)

##Try a simple model, one variable at a time
##--------------------------------
####Just looking at roughness
##--------------------------------

# Using aggregated vegetation layers 
rsf_data_final <- rsf_both_seasons %>%
  mutate(
    understory = (X0.25m + X0.50m + X1m) / 3,
    midstory = (X2m + X3m) / 2,
    overstory = (X4m + X5m) / 2
  )


# Model 2: Combined model with season interactions
rsf_seasonal <- glmer(used ~ (scale(canopy_cover) + scale(roughness) + 
                                scale(ENL1) + scale(understory) + 
                                scale(midstory) + scale(overstory)) * season +
                        (1|individualID) + (1|plot),
                      data = rsf_data_final,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
summary(rsf_seasonal)

library(ggeffects)
library(ggplot2)
library(patchwork)

# List of variables you want to visualize
vars <- c("canopy_cover", "understory", "midstory", 
          "roughness", "overstory", "ENL1")

vars <- c("canopy_cover", "roughness", "ENL1",
          "understory", "midstory", "overstory")

plot_list <- lapply(vars, function(v) {
  ggpredict(rsf_seasonal, terms = c(v, "season")) |>
    plot() +
    labs(title = paste("Effect of", v, "by Season"),
         x = v, y = "Predicted Probability of Use") +
    theme_bw()
})

# Combine plots into a grid
wrap_plots(plotlist = plot_list, ncol = 2)


library(ggplot2)
library(dplyr)

# Extract coefficients for plotting
coef_july <- c(
  Canopy = 0.407212,
  Roughness = 0.072027,
  ENL1 = -0.048203,
  Understory = 0.186435,
  Midstory = 0.083671,
  Overstory = 0.028284
)

coef_march <- c(
  Canopy = 0.407212 - 0.027327,
  Roughness = 0.072027 - 0.004647,
  ENL1 = -0.048203 - 0.001547,
  Understory = 0.186435 - 0.028933,
  Midstory = 0.083671 - 0.019827,
  Overstory = 0.028284 - 0.005386
)

# Create dataframe for plotting
coef_df <- data.frame(
  Variable = rep(names(coef_july), 2),
  Coefficient = c(coef_july, coef_march),
  Season = rep(c("July", "March"), each = 6)
)

###----------------------------------
# Publication-ready plot
###----------------------------------

# Ensure correct ordering of variables
coef_df$Variable <- factor(
  coef_df$Variable,
  levels = c("Roughness", "Canopy", "Understory",
             "Midstory", "Overstory", "ENL1"), 
  labels = c("Roughness", "Canopy Cover", "Understory",
             "Midstory", "Overstory", "ENL1"))

ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Season)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    color = "black",
    linewidth = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(
    values = c(
      "March" = "#66C2A5",  # spring green
      "July"  = "#FC8D62"   # warm summer orange
    ),
    name = "Season"
  ) +
  labs(
    title = "Seasonal Habitat Selection",
    subtitle = "Comparison of standardized RSF coefficients",
    y = "Selection Coefficient (β)",
    x = "Habitat Variable"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

###-------------------------------
####To make a table
###-------------------------------
library(sjPlot)

# Run separate seasonal models for cleaner comparison
rsf_july <- glmer(used ~ scale(canopy_cover) + scale(roughness) + 
                    scale(ENL1) + scale(understory) + 
                    scale(midstory) + scale(overstory) +
                    (1|individualID) + (1|plot),
                  data = rsf_both_seasons[rsf_both_seasons$season == "July", ],
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))

rsf_march <- glmer(used ~ scale(canopy_cover) + scale(roughness) + 
                     scale(ENL1) + scale(understory) + 
                     scale(midstory) + scale(overstory) +
                     (1|individualID) + (1|plot),
                   data = rsf_both_seasons[rsf_both_seasons$season == "March", ],
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

# Create comparison table
tab_model(rsf_july, rsf_march, 
          dv.labels = c("July", "March"),
          show.ci = TRUE,
          title = "Habitat Selection by Season")