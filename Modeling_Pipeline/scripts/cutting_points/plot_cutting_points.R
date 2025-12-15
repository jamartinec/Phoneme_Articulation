import("tidyverse")
import("dplyr")
import("posterior")
import("yaml")
import("purrr")
import("glue")
import("tidybayes")
import("scales")
import("ggrepel")

Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")

export("plot_base_posterior")
plot_base_posterior <- function(plot_data, df_points, y_label = "Response") {
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = age_months, y = q50, color = expected_phoneme)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(linetype = "Posterior median"),
      linewidth = 1.1
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025, ymax = q975, fill = expected_phoneme),
      alpha = 0.2, color = NA
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q25, ymax = q75, fill = expected_phoneme),
      alpha = 0.4, color = NA
    ) +
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes(x = age_months, y = y_obs, color = expected_phoneme),
      inherit.aes = FALSE, alpha = 0.5, size = 1.5
    ) +
    ggplot2::facet_wrap(~ expected_phoneme) +
    ggplot2::labs(x = "Age (months)", y = y_label) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title       = ggplot2::element_text(size = 16, face = "bold"),
      axis.text        = ggplot2::element_text(size = 14),
      strip.text       = ggplot2::element_text(size = 14, face = "bold"),
      legend.title     = ggplot2::element_blank(),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray90")
    ) 
}



add_xq_overlay <- function(
    p, xq_df, label_cutpoints,
    color       = "black",
    extend_to_max = TRUE,
    annotate      = TRUE,
    plot_data     = NULL
) {
  if (is.null(xq_df) || nrow(xq_df) == 0) return(p)
  
  # Prepare data
  df <- xq_df %>% dplyr::arrange(age_months)
  
  # add an artificial final point just to extend the lines connecting cutting points
  if (extend_to_max && !is.null(plot_data)) {
    max_age <- max(plot_data$age_months, na.rm = TRUE)
    last <- df %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::mutate(age_months = max_age)
    
    df <- dplyr::bind_rows(df, last)
  }
  
  # create the pairs (x,y) for labels
  df <- df %>%
    dplyr::mutate(
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  # add cutting points and lines 
  p <- p +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = age_months, y = x_q, linetype = !!label_cutpoints),
      color = color, linewidth = 1.05,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = age_months, y = x_q),
      color = color, size = 2,
      inherit.aes = FALSE
    )
  
  # text labels for points:
  if (annotate) {
    p <- p +
      ggrepel::geom_text_repel(
        data = df,
        ggplot2::aes(x = age_months, y = x_q, label = coord_lab),
        inherit.aes = FALSE,
        size = 3,
        color = color,
        max.overlaps = Inf,
        box.padding = 0.3,
        point.padding = 0.2,
        min.segment.length = 0
      )
  }
  
  return(p)
}



add_month_year_axis <- function(p, plot_data) {
  
  xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
  xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
  
  month_major <- seq(xmin, xmax, by = 12)
  month_minor <- seq(xmin, xmax, by = 6)
  year_major  <- seq(xmin/12, xmax/12, by = 1)
  
  p <-p + ggplot2::scale_x_continuous(
    name   = "Age (months)",
    limits = c(xmin, xmax),
    breaks = month_major,
    minor_breaks = month_minor,
    sec.axis = ggplot2::sec_axis(
      ~ . / 12,
      name   = "Age (years)",
      breaks = year_major,
      labels = scales::number_format(accuracy = 1)
    )
  )
  
  return(p)
}




export("plot_cutting_points_v1")# ----> I think we don't need this function
plot_cutting_points_v1 <- function(plot_data, df_points, y_label, xq_all,
                                   label1 = "x_q threshold",
                                   color1 = "black") {
  
  p <- plot_base_posterior(plot_data, df_points, y_label)
  
  cut_values <- xq_all %>%
    dplyr::filter(version=="prediction")
  p <- add_xq_overlay(p, cut_values, label1, color = color1)
  
  p <- add_month_year_axis(p, plot_data)
  
  
  return(p)
}



export("plot_cutting_points")
plot_cutting_points <- function(plot_data, df_points, y_label, xq_all,
                                   c = "mean", 
                                   m2 = "m2",
                                   p2 = "p2",
                                   label1 = "C&M mean",
                                   label2 = "C&M -2sd",
                                   label3 = "C&M +2sd",
                                   color1 = "black",
                                   color2 = "blue",
                                   color3 = "green") {
  
  p <- plot_base_posterior(plot_data, df_points, y_label)
  
  center_line <- xq_all%>%dplyr::filter(version == c)
  left_line   <- xq_all%>%dplyr::filter(version == m2)
  right_line  <- xq_all%>%dplyr::filter(version == p2)
  
  p <- add_xq_overlay(p, center_line, label1, color = color1, plot_data = plot_data)
  p <- add_xq_overlay(p, left_line,   label2, color = color2, plot_data = plot_data)
  p <- add_xq_overlay(p, right_line,  label3, color = color3, plot_data = plot_data)
  p <- add_month_year_axis(p, plot_data)
  
  lt_values <- stats::setNames(
    c("solid", "longdash", "longdash", "longdash"),
    c("Posterior median", label1, label2, label3)
  )
  
  p <- p + ggplot2::scale_linetype_manual(values = lt_values)
  
  
  
  return(p)
}





dummy_function <- function(){

# single plotting block
# #age_plot <- ggplot(plot_data, aes(x = (age_months + 30) / 12, y = q50, color = expected_phoneme)) +
# age_plot <- ggplot(plot_data, aes(x = (age_months) / 12, y = q50, color = expected_phoneme)) +
#   geom_line(linewidth = 1.1, color = "steelblue") +
#   geom_line(aes(y = q50, linetype = "Posterior median"), linewidth = 1.1) +
#   geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
#   geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
#   facet_wrap(~ expected_phoneme) +
#   geom_point(
#     data = df_points,
#     #aes(x = (age_months + 30) / 12, y = y_obs, color = expected_phoneme),
#     aes(x = (age_months ) / 12, y = y_obs, color = expected_phoneme),
#     inherit.aes = FALSE, alpha = 0.5, size = 1.5
#   ) +
#   labs(x = "Age (years)", y = y_label) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title   = element_text(size = 16, face = "bold"),
#     axis.text    = element_text(size = 14),
#     strip.text   = element_text(size = 14, face = "bold"),
#     legend.title = element_blank(),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(color = "gray90"),
#     plot.title = element_blank(),
#     plot.subtitle = element_blank()
#   )
# # Add the x_q overlay if present
# if (!is.null(xq_predictive)) {
#   age_plot <- age_plot +
#     geom_line(
#       data = xq_predictive,
#       #aes(x = (age_months + 30) / 12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
#       aes(x = age_months/12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
#       linewidth = 0.9,
#       inherit.aes = FALSE
#     )# +
#     #scale_linetype_manual(values = c("Posterior median" = "solid", "x_q threshold" = "longdash"))
# } 
# 
# if (!is.null(xq_mu)) {
#   age_plot <- age_plot +
#     geom_line(
#       data = xq_mu,
#       #aes(x = (age_months + 30) / 12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
#       aes(x = age_months/12, y = x_q_mu, color = expected_phoneme, linetype = "x_mu threshold"),
#       linewidth = 0.9,
#       inherit.aes = FALSE
#     ) +
#     scale_linetype_manual(values = c("Posterior median" = "solid", "x_q threshold" = "longdash", "x_mu threshold"="dotted"))
# } 
# 

age_plot <- ggplot(plot_data, aes(x = age_months/12, y = q50, color = expected_phoneme)) +
  # one median line only
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  # ribbons (inherit x from ggplot mapping)
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months/12, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (years)", y = y_label) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Overlays with fixed colors
if (!is.null(xq_predictive)) {
  age_plot <- age_plot +
    geom_line(
      data = xq_predictive,
      aes(x = age_months/12, y = x_q, linetype = "x_q threshold"),
      color = "black", linewidth = 0.9, inherit.aes = FALSE
    )
}

# if (!is.null(xq_mu)) {
#   age_plot <- age_plot +
#     geom_line(
#       data = xq_mu,
#       aes(x = age_months/12, y = x_q_mu, linetype = "x_mu threshold"),
#       color = "firebrick", linewidth = 0.9, inherit.aes = FALSE
#     )
# }

age_plot <- age_plot +
  scale_linetype_manual(values = c(
    "Posterior median" = "solid",
    "x_q threshold"    = "longdash"#,
    #"x_mu threshold"   = "dotdash"
  ))
print(age_plot)



age_plot



age_plot_crow <- ggplot(plot_data, aes(x = age_months/12, y = q50, color = expected_phoneme)) +
  # one median line only
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  # ribbons (inherit x from ggplot mapping)
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months/12, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (years)", y = y_label) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Overlays with fixed colors: dashed line + points + simple labels
if (!is.null(xq_predictive_crow)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crow %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      # if the max is already the last age, don't duplicate
      #if (x_max_months > last_row$age_months[1]) bind_rows(., last_row) else .
      bind_rows(., last_row)
    }
  
  
  xq_lab <- xq_extended %>%
    mutate(
      age_years = age_months / 12,
      coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
    )
  
  
  age_plot_crow <- age_plot_crow +
    # dashed line with legend entry
    geom_line(
      data = xq_lab,
      aes(x = age_years, y = x_q, linetype = "x_q threshold Crowe & McLeod"),
      color = "black", linewidth = 0.9, inherit.aes = FALSE
    ) +
    # points at the x_q positions
    geom_point(
      data = xq_lab,
      aes(x = age_years, y = x_q),
      color = "black", size = 2.2, inherit.aes = FALSE
    ) +
    # simple (x, y) labels near points
    geom_text(
      data = xq_lab,
      aes(x = age_years, y = x_q, label = coord_lab),
      vjust = -0.7, size = 3, color = "black", inherit.aes = FALSE
    )
  
}

# if (!is.null(xq_predictive_crowm1)) {
#   
#   xq_labm1 <- xq_predictive_crowm1 %>%
#     mutate(
#       age_years = age_months / 12,
#       coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
#     )
#   
#   age_plot_crow <- age_plot_crow +
#     # dashed line with legend entry
#     geom_line(
#       data = xq_labm1,
#       aes(x = age_years, y = x_q, linetype = "x_q C&M -1sd"),
#       color = "blue", linewidth = 0.9, inherit.aes = FALSE
#     ) +
#     # points at the x_q positions
#     geom_point(
#       data = xq_labm1,
#       aes(x = age_years, y = x_q),
#       color = "blue", size = 2.2, inherit.aes = FALSE
#     ) +
#     # simple (x, y) labels near points
#     geom_text(
#       data = xq_labm1,
#       aes(x = age_years, y = x_q, label = coord_lab),
#       vjust = -0.7, size = 3, color = "blue", inherit.aes = FALSE
#     )
# }

# if (!is.null(xq_predictive_crowp1)) {
#   
#   xq_labp1 <- xq_predictive_crowp1 %>%
#     mutate(
#       age_years = age_months / 12,
#       coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
#     )
#   
#   age_plot_crow <- age_plot_crow +
#     # dashed line with legend entry
#     geom_line(
#       data = xq_labp1,
#       aes(x = age_years, y = x_q, linetype = "x_q C&M +1sd"),
#       color = "green", linewidth = 0.9, inherit.aes = FALSE
#     ) +
#     # points at the x_q positions
#     geom_point(
#       data = xq_labp1,
#       aes(x = age_years, y = x_q),
#       color = "green", size = 2.2, inherit.aes = FALSE
#     ) +
#     # simple (x, y) labels near points
#     geom_text(
#       data = xq_labp1,
#       aes(x = age_years, y = x_q, label = coord_lab),
#       vjust = -0.7, size = 3, color = "green", inherit.aes = FALSE
#     )
# }

# Make sure the legend key matches the label above
age_plot_crow <- age_plot_crow +
  scale_linetype_manual(values = c(
    "Posterior median"                = "solid",
    "x_q threshold Crowe & McLeod"    = "longdash",
    "x_q C&M -1sd" = "longdash",
    "x_q C&M +1sd" = "longdash"
    
    
    
  ))
age_plot_crow

age_plot_crow2 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (months)", y = y_label) 

# Primary axis in months; secondary axis (top) in years
# scale_x_continuous(
#   breaks = pretty_breaks(n = 6),
#   minor_breaks = pretty_breaks(n = 12),
#   sec.axis = sec_axis(
#     ~ . / 12,
#     name   = "years",
#     breaks = pretty_breaks(n = 6),
#     labels = number_format(accuracy = 1)
#   )
# ) +


# compute stable limits & ticks from your data
xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback

month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
year_major  <- seq(xmin/12, xmax/12, by = 1)

age_plot_crow2 <- age_plot_crow2 +
  scale_x_continuous(
    name   = "Age (months)",
    limits = c(xmin, xmax),
    breaks = month_major,
    minor_breaks = month_minor,
    sec.axis = sec_axis(
      ~ . / 12,
      name   = "years",
      breaks = year_major,
      labels = scales::number_format(accuracy = 1)
    )
  ) + theme_minimal(base_size = 14) +
  theme(
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    # style the secondary (top) axis in a subtle grey
    axis.title.x.top = element_text(color = "grey30", face = "bold"),
    axis.text.x.top  = element_text(color = "grey40"),
    axis.ticks.x.top = element_line(color = "grey60")
  )

## 2) Overlay: extend x_q series and map x in MONTHS
if (!is.null(xq_predictive_crow)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crow %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
  age_plot_crow2 <- age_plot_crow2 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q threshold Crowe & McLeod"),
      color = "black", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "black", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}

## 3) Keep your linetype legend mapping
age_plot_crow2 <- age_plot_crow2 +
  scale_linetype_manual(values = c(
    "Posterior median"             = "solid",
    "x_q threshold Crowe & McLeod" = "longdash",
    "x_q C&M -1sd"                 = "longdash",
    "x_q C&M +1sd"                 = "longdash"
  ))

age_plot_crow2

####### new plot adding deviations for the cut points definitions#############

age_plot_crow3 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (months)", y = y_label) 

# Primary axis in months; secondary axis (top) in years
# scale_x_continuous(
#   breaks = pretty_breaks(n = 6),
#   minor_breaks = pretty_breaks(n = 12),
#   sec.axis = sec_axis(
#     ~ . / 12,
#     name   = "years",
#     breaks = pretty_breaks(n = 6),
#     labels = number_format(accuracy = 1)
#   )
# ) +

xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback

month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
year_major  <- seq(xmin/12, xmax/12, by = 1)

age_plot_crow3 <- age_plot_crow3 +
  scale_x_continuous(
    name   = "Age (months)",
    limits = c(xmin, xmax),
    breaks = month_major,
    minor_breaks = month_minor,
    sec.axis = sec_axis(
      ~ . / 12,
      name   = "years",
      breaks = year_major,
      labels = scales::number_format(accuracy = 1)
    )
  ) + theme_minimal(base_size = 14) +
  theme(
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    # style the secondary (top) axis in a subtle grey
    axis.title.x.top = element_text(color = "grey30", face = "bold"),
    axis.text.x.top  = element_text(color = "grey40"),
    axis.ticks.x.top = element_line(color = "grey60")
  )

## Overlay: extend x_q series and map x in MONTHS
if (!is.null(xq_predictive_crow)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crow %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
  
  age_plot_crow3 <- age_plot_crow3 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M"),
      color = "black", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "black", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}


if (!is.null(xq_predictive_crowm3)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowm3 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  age_plot_crow3 <- age_plot_crow3 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M -3sd"),
      color = "blue", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "blue", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}

if (!is.null(xq_predictive_crowp3)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowp3 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  age_plot_crow3 <- age_plot_crow3 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M +3sd"),
      color = "blue", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "blue", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}


## 3) Keep your linetype legend mapping
age_plot_crow3 <- age_plot_crow3 +
  scale_linetype_manual(values = c(
    "Posterior median"             = "solid",
    "x_q C&M" = "longdash",
    "x_q C&M -3sd"                 = "longdash",
    "x_q C&M +3sd"                 = "longdash"
  ))


age_plot_crow3




# using 2 sd deviations:
####### new plot adding deviations for the cut points definitions#############

age_plot_crow4 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (months)", y = y_label) 

# Primary axis in months; secondary axis (top) in years
# scale_x_continuous(
#   breaks = pretty_breaks(n = 6),
#   minor_breaks = pretty_breaks(n = 12),
#   sec.axis = sec_axis(
#     ~ . / 12,
#     name   = "years",
#     breaks = pretty_breaks(n = 6),
#     labels = number_format(accuracy = 1)
#   )
# ) +

xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback

month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
year_major  <- seq(xmin/12, xmax/12, by = 1)

age_plot_crow4 <- age_plot_crow4 +
  scale_x_continuous(
    name   = "Age (months)",
    limits = c(xmin, xmax),
    breaks = month_major,
    minor_breaks = month_minor,
    sec.axis = sec_axis(
      ~ . / 12,
      name   = "years",
      breaks = year_major,
      labels = scales::number_format(accuracy = 1)
    )
  ) + theme_minimal(base_size = 14) +
  theme(
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    # style the secondary (top) axis in a subtle grey
    axis.title.x.top = element_text(color = "grey30", face = "bold"),
    axis.text.x.top  = element_text(color = "grey40"),
    axis.ticks.x.top = element_line(color = "grey60")
  )

## Overlay: extend x_q series and map x in MONTHS
if (!is.null(xq_predictive_crow)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crow %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
  
  age_plot_crow4 <- age_plot_crow4 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M"),
      color = "black", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "black", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}


if (!is.null(xq_predictive_crowm2)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowm2 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  age_plot_crow4 <- age_plot_crow4 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M -2sd"),
      color = "blue", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "blue", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}

if (!is.null(xq_predictive_crowp2)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowp2 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  age_plot_crow4 <- age_plot_crow4 +
    geom_line(
      data = xq_extended,
      aes(x = age_months, y = x_q, linetype = "x_q C&M +2sd"),
      color = "blue", linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = xq_extended,
      aes(x = age_months, y = x_q),
      color = "blue", size = 2.2, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}


##Keep  linetype legend mapping
age_plot_crow4 <- age_plot_crow4 +
  scale_linetype_manual(values = c(
    "Posterior median"             = "solid",
    "x_q C&M" = "longdash",
    "x_q C&M -2sd"                 = "longdash",
    "x_q C&M +2sd"                 = "longdash"
  ))


age_plot_crow4


###########################################################################

# Unir el n-esimo punto de cada uno de las 3 curvas:

df_m2 <- xq_predictive_crowm2 %>%
  arrange(age_months) %>%
  mutate(
    cut_idx = row_number(),      # 1,2,3...
    sd_level = "-2sd"
  )

df_mean <- xq_predictive_crow %>%
  arrange(age_months) %>%
  mutate(
    cut_idx = row_number(),
    sd_level = "mean"
  )

df_p2 <- xq_predictive_crowp2 %>%
  arrange(age_months) %>%
  mutate(
    cut_idx = row_number(),
    sd_level = "+2sd"
  )

df_cross <- bind_rows(df_m2, df_mean, df_p2) %>%
  mutate(
    # row position determines which of the 3 accuracy levels it is
    cut_label = case_when(
      cut_idx == 1 ~ "C&M age range 50%  accuracy",
      cut_idx == 2 ~ "C&M age range 75%  accuracy",
      cut_idx == 3 ~ "C&M age range 90%  accuracy"
    )
  )




# Now, recreate the plot of each line:

age_plot_crow5 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
  geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  facet_wrap(~ expected_phoneme) +
  geom_point(
    data = df_points,
    aes(x = age_months, y = y_obs, color = expected_phoneme),
    inherit.aes = FALSE, alpha = 0.5, size = 1.5
  ) +
  labs(x = "Age (months)", y = y_label) 


xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback

month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
year_major  <- seq(xmin/12, xmax/12, by = 1)

age_plot_crow5 <- age_plot_crow5 +
  scale_x_continuous(
    name   = "Age (months)",
    limits = c(xmin, xmax),
    breaks = month_major,
    minor_breaks = month_minor,
    sec.axis = sec_axis(
      ~ . / 12,
      name   = "years",
      breaks = year_major,
      labels = scales::number_format(accuracy = 1)
    )
  ) + theme_minimal(base_size = 14) +
  theme(
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14),
    strip.text       = element_text(size = 14, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    # style the secondary (top) axis in a subtle grey
    axis.title.x.top = element_text(color = "grey30", face = "bold"),
    axis.text.x.top  = element_text(color = "grey40"),
    axis.ticks.x.top = element_line(color = "grey60")
  )

## Overlay: extend x_q series and map x in MONTHS
if (!is.null(xq_predictive_crow)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crow %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
  #   
  #   age_plot_crow5 <- age_plot_crow5 +
  #     geom_line(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q, linetype = "x_q C&M"),
  #       color = "black", linewidth = 0.9, inherit.aes = FALSE
  #     ) +
  #     geom_point(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q),
  #       color = "black", size = 2.2, inherit.aes = FALSE
  #     ) +
  age_plot_crow5 <- age_plot_crow5 +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}
# 
# 
if (!is.null(xq_predictive_crowm2)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowm2 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  #   
  #   age_plot_crow5 <- age_plot_crow5 +
  #     geom_line(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q, linetype = "x_q C&M -2sd"),
  #       color = "blue", linewidth = 0.9, inherit.aes = FALSE
  #     ) +
  #     geom_point(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q),
  #       color = "blue", size = 2.2, inherit.aes = FALSE
  #     ) +
  age_plot_crow5 <- age_plot_crow5 +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}
# 
if (!is.null(xq_predictive_crowp2)) {
  x_max_months <- max(plot_data$age_months, na.rm = TRUE)
  
  xq_extended <- xq_predictive_crowp2 %>%
    arrange(age_months) %>%
    {
      last_row <- slice_tail(., n = 1) %>%
        mutate(age_months = x_max_months)
      bind_rows(., last_row)
    } %>%
    mutate(
      age_years = age_months / 12,
      
      coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
    )
  
  #   age_plot_crow5 <- age_plot_crow5 +
  #     geom_line(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q, linetype = "x_q C&M +2sd"),
  #       color = "blue", linewidth = 0.9, inherit.aes = FALSE
  #     ) +
  #     geom_point(
  #       data = xq_extended,
  #       aes(x = age_months, y = x_q),
  #       color = "blue", size = 2.2, inherit.aes = FALSE
  #     ) +
  age_plot_crow5 <- age_plot_crow5 +
    geom_text_repel(
      data = xq_extended,
      aes(x = age_months, y = x_q, label = coord_lab),
      inherit.aes = FALSE,
      size = 3,
      color = "black",
      direction = "both",        # spread labels vertically
      nudge_y = 0.08,         # gentle lift
      nudge_x = 0.02 * xrange,
      box.padding = 0.3,
      point.padding = 0.20,
      min.segment.length = 0, # always draw a leader line
      max.overlaps = Inf
    ) +
    theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
}

age_plot_crow5 <- age_plot_crow5 +
  geom_line(
    data = df_cross,
    aes(
      x     = age_months,
      y     = x_q,
      group = cut_label,
      colour = cut_label
    ),
    linewidth = 1.1,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = df_cross,
    aes(
      x      = age_months,
      y      = x_q,
      colour = cut_label
    ),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    name   = "C&M cuts",
    values = c(
      "C&M age range 50%  accuracy" = "red",
      "C&M age range 75%  accuracy" = "darkgreen",
      "C&M age range 90%  accuracy" = "purple"
    )
  )



## Keep linetype legend mapping
age_plot_crow5 <- age_plot_crow5 +
  scale_linetype_manual(values = c(
    "Posterior median"             = "solid"#,
    # "x_q C&M" = "longdash",
    # "x_q C&M -2sd"                 = "longdash",
    # "x_q C&M +2sd"                 = "longdash"
  ))


age_plot_crow5
}


