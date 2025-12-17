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


# This function plots the median and the central credible intervals and also overlay the original data points

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


add_interval_lines <- function(
    p, 
    xq_df, 
    label,
    color = "black"
    
) {
  if (is.null(xq_df) || nrow(xq_df) == 0) return(p)
  
  # Prepare data
  df <- xq_df %>% dplyr::arrange(age_months)
  # add interval lines 
  p <- p +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = age_months, y = x_q, linetype = !!label),
      color = color, linewidth = 1.05,
      inherit.aes = FALSE
    ) 
  
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


export("plot_cutting_points_intervals")
plot_cutting_points_intervals <- function(plot_data, df_points, y_label, xq_all,
                                c = "mean", 
                                m2 = "m2",
                                p2 = "p2",
                                label1 = "C&M mean",
                                label2 = NULL,
                                label3 = NULL,
                                label_in50 = "C&M age range 50% accuracy",
                                label_in75 = "C&M age range 75% accuracy",
                                label_in90 = "C&M age range 90% accuracy",
                                color1 = "grey60",
                                color2 = "grey40",
                                color3 = "grey40",
                                colorin1 = "red",
                                colorin2 = "darkgreen",
                                colorin3 = "purple"
                                ) {
  
  p <- plot_base_posterior(plot_data, df_points, y_label)
  
  center_line <- xq_all%>%dplyr::filter(version == c)
  left_line   <- xq_all%>%dplyr::filter(version == m2)
  right_line  <- xq_all%>%dplyr::filter(version == p2)
  
  p50line <- xq_all%>%dplyr::filter(q_age== 0.5) 
  p75line <- xq_all%>%dplyr::filter(q_age== 0.75) 
  p90line <- xq_all%>%dplyr::filter(q_age== 0.90) 
  
  p <- add_xq_overlay(p, center_line, label1, color = color1, plot_data = plot_data)
  p <- add_xq_overlay(p, left_line,   label2, color = color2, plot_data = plot_data)
  p <- add_xq_overlay(p, right_line,  label3, color = color3, plot_data = plot_data)
  p <- add_interval_lines(p, p50line, label_in50, color = colorin1)
  p <- add_interval_lines(p, p75line, label_in75, color = colorin2)
  p <- add_interval_lines(p, p90line, label_in90, color = colorin3)
  
  
  
  p <- add_month_year_axis(p, plot_data)
  
  lt_values <- stats::setNames(
    c("solid", "longdash", "longdash", "longdash","solid","solid","solid"),
    c("Posterior median", label1, label2, label3,label_in50,label_in75,label_in90)
  )
  
  p <- p + ggplot2::scale_linetype_manual(values = lt_values)
  
  
  
  return(p)
}
