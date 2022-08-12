# Data =========================================================================
# htl <- readxl::read_xlsx("~/DataG/Hotel data/HotelCustomersDataset.xlsx")
# full_country_name <- read_csv("C:/Users/AYOMIDE/Documents/DataG/country_code.csv")
# 
# 
# names_country <- select(htl, Nationality) |>
#   left_join(full_country_name, by = c("Nationality" = "ISO3"), keep = TRUE) |>
#   select(Nationality, country_name) |>
#   deframe()
# 
# htl <- htl |>
#   janitor::clean_names() |> 
#   mutate(across(c(lodging_revenue, other_revenue), as.double)) |>
#   mutate(age = as.integer(age),
#          total_revenue = lodging_revenue + other_revenue,
#          total_bookings = bookings_canceled + bookings_no_showed + bookings_checked_in,
#          country_IS03 = nationality,
#          nationality = names_country) |> 
#   select(-c(id, name_hash, doc_id_hash)) |>
#   ## Removing guest that did not lodge in.
#   filter(days_since_last_stay > 0 & days_since_first_stay > 0)



# global =======================================================================
guest_request_label <- 
  list(sr_high_floor = "A Higher Room Floor",
       sr_high_floor = "A Lower Room Floor",
       sr_medium_floor = "A Middle Floor",
       sr_accessible_room = "A Room Which Is Easily Accessible",
       sr_bathtub = "A Room With A Bathtub",
       sr_shower = "A Room With Shower",
       sr_crib = "A Room With A Crib",
       sr_king_size_bed = "A Room With A King Size Bed",
       sr_twin_bed = "A Room With A Twin Bed",
       sr_near_elevator = "A Room Close To An Elevator",
       sr_away_from_elevator = "A Room Away From An Elevator",
       sr_no_alcohol_in_mini_bar = "A Room With No Alcohol In The Mini Bar",
       sr_quiet_room = "A Quiet Room")

comp_opt <- list(">"  = "greater than",
                 ">=" = "greater than or equal to",
                 "==" = "equal to",
                 "!=" = "not equal to",
                 "<"  = "less than",
                 "<=" = "less than or equal to")

agg_labels <- list("minimum"= "Minimum",
                   "Q25"    = "25th Quantile",
                   "mean"   = "Average",
                   "median" = "Median",
                   "Q75"    = "75th Quantile",
                   "maximum"= "Maximum",
                   "sum"    = "Total")

request_types = list(
  "floor_level" = c("sr_high_floor", "sr_medium_floor", "sr_low_floor", "sr_accessible_room"),
  "room_features" = c("sr_king_size_bed", "sr_twin_bed", "sr_bathtub", "sr_shower", "sr_crib", "sr_no_alcohol_in_mini_bar"),
  "room_environment" = c("sr_quiet_room", "sr_near_elevator", "sr_away_from_elevator")
)


# Font ------------------------------------------------------------------------|
gt_num_font <- "Centaur"
ax_num_font <- "Rockwell"

tl_txt_font <- "Lucida Sans"
ax_txt_font <- "Lucida Sans Typewriter"


# Color -----------------------------------------------------------------------|
e_black1 <- "#131515"
jet2 <- "#2B2C28"
p_green3 <- "#339989"
m_bgreen4 <- "#7DE2D1"
snow5 <- "#FFFAFB"



#' Tidy Plot Labels
#'
#' @param label A string
#' @param type The type of operation to perform, either 'clean' only or clean 
#' and 'warp' text.
#' @param str_remove Any type of string to remove.
#' @param ... Additional argument passed to `strwarp()` function.
#'
#' @return A string.
#'
#' @examples clean_plot_label('the_plot_title', 'clean', 'the')
clean_plot_label <- function(label, type = "clean", str_remove, ...) {
  type <- match.arg(type, c("clean", "wrap"))
  
  if (!missing(str_remove)) {
    label <- stringr::str_replace_all(label, str_remove, "")
  }
  
  if (type == "clean") {
    stringr::str_replace_all(label, "[:punct:]", " ") |>
      stringr::str_to_title() |>
      stringr::str_trim(side = "both")
    
  } else if (type == "wrap") {
    paste(
      strwrap(label, ...),
      collapse = "\n"
    )
  }
}



#' Revenue summary by additional guest request.
#'
#' @param df Hotel data.
#' @param request_type A category of guest request. It can be any of 'floor_level', 
#' 'room_features' or 'room_environment'.
#' @param revenue_type Type of revenue. It can be any of 'total_revenue', 
#' 'lodging_revenue' or 'other_revenue'.
#' @param agg_fun An aggregate function to sort or plot by.
#' @param output_type The type of output, Either 'plot' or 'table'.
#'
#' @return A list of tibble if output_type is 'table' else a ggplot object.
#' @export
#'
#' @examples guest_request_revenue(hotel_data, 'floor_level', 'total_revenue', 'sum', 'table')
guest_request_revenue <- function(df, 
                                  request_type, revenue_type, agg_fun,
                                  output_type = "plot") {
  request_type <- match.arg(request_type, c("floor_level", "room_features", "room_environment"))
  output_type <- match.arg(output_type, c("plot", "table"))
  
  f_tbl <- purrr::map_dfr(
    request_types[[request_type]],
    function(.x) {
      dplyr::group_by(df, .data[[.x]]) |>
        numeric_summary(revenue_type) |>
        dplyr::rename("choice" = .x) |>
        dplyr::mutate(request = clean_plot_label(.x, str_remove = "sr_"),
                      request = ifelse(request == "No Alcohol In Mini Bar", "No Alcohol", request),
                      choice = ifelse(choice == 0, "No", "Yes"))
    }
  )
  if (output_type == "plot") {
    plot_title <- str_glue(
      "{agg_labels[[agg_fun]]} {clean_plot_label(revenue_type, str_remove = 'total_')} Based On Guest {clean_plot_label(request_type)} Request"
    )
    f_tbl |>
      dplyr::mutate(choice = ifelse(choice == "No", "No Request", "Request")) |>
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_rev(reorder(request, .data[[agg_fun]])),
                                   y = .data[[agg_fun]])) +
      ggplot2::geom_col(fill = jet2) +
      ggplot2::geom_text(ggplot2::aes(y = 150, label = round(.data[[agg_fun]], 2), angle = 90),size = 10,  alpha = 0.1, family = gt_num_font) +
      ggplot2::facet_wrap(ggplot2::vars(choice), scales = "free_y") +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::comma_format(1),
                                  expand = expansion(mult = c(0, 0.05))) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
      ggplot2::labs(x = NULL, y = NULL, title = plot_title) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(family = ax_num_font),
        plot.title = ggplot2::element_text(family = tl_txt_font),
        strip.text = ggplot2::element_text(family = ax_txt_font, color = p_green3))
    
  } else if (output_type == "table") {
    f_tbl
  }
}



#' Numeric data type summary.
#'
#' @param df A data.frame
#' @param var A numeric variable from the data.
#' @param groups If 'df' is a grouped data.frame, then how to treat the result data.frame.
#' see ?dplyr::summarise for more information. 
#' @param pivot Reshape the data from a wide shape to a long shape if TRUE
#'
#' @return A summary data.frame containing the count, min, max, average, median, sum
#' 25th & 27th quantile.
#' @export
#'
#' @examples numeric_summary(data, revenue, 'drop_last', TRUE)
numeric_summary <- function(df, var, groups = "drop_last", pivot = FALSE) {
  if (!is.numeric(df[[var]])) {
    stop("argument `var` must be numeric")
  }
  
  f_tbl <- df |>
    dplyr::summarise(count = dplyr::n(),
                     minimum = min(.data[[var]]),
                     Q25 = quantile(.data[[var]], 0.25),
                     mean = mean(.data[[var]]),
                     median = median(.data[[var]]),
                     Q75 = quantile(.data[[var]], 0.75),
                     maximum = max(.data[[var]]),
                     sum = sum(.data[[var]]), .groups = groups)
  if (pivot) {
    f_tbl <- f_tbl |> 
      tidyr::pivot_longer(cols = count:sum, 
                          names_to  = "statistics", 
                          values_to = "value")
    return(f_tbl)
    
  } else {
    return(f_tbl)
  }
}



#' Wrapper for the`lapply()` function with assigned names.
#'
#' @param X A vector 
#' @param FUN The function to be applied to each element of X
#' @param ... Additional arguments passed to the `lapply()` function see ?lapply
#' for more information.
#'
#' @return A list.
#' @export
#'
#' @examples named_lapply(c("mpg", "disp"), \(.x) sum(mtcars[[.x]]))
named_lapply <- function(X, FUN, ...) {
  output <- lapply(X = X, FUN, ...)
  names(output) <- X
  return(output)
}



#' Rowwise percentage
#'
#' @param col A value to extract it proportion.
#' @param total_col A value to use as the overall total.
#' @param round A numeric value to round by.
#'
#' @return A proportion of the total.
#' @export
#'
#' @examples prop_cols(25, 50, 2)
prop_cols <- function(col, total_col, round = 3) {
  if (total_col <= 0) 0 else round(col / total_col*100, round)
}

#' Guest bookings proportion summary
#'
#' @param df hotel data with a column name as the value supplied to `booking_res` and 
#' 'total_bookings'
#' @param booking_res Any of 'bookings_canceled', 'bookings_no_showed' or 'bookings_checked_in'
#'
#' @return A tibble with an additional column based on the `booking_res` value.
#' 'canceled_percent'-'bookings_canceled', 'no_show_percent'-'bookings_no_showed', 
#' 'checked_in_percent'-'bookings_checked_in'
#' @export
#'
#' @examples bookings_sumry(hotel_data, 'bookings_no_showed')
bookings_sumry <- function(df = htl, booking_res) {
  booking_res <- match.arg(booking_res, c("bookings_canceled", "bookings_no_showed", "bookings_checked_in"))
  bk_name <- switch(booking_res,
                    bookings_canceled   = "canceled_percent",
                    bookings_no_showed  = "no_show_percent",
                    bookings_checked_in = "checked_in_percent")
  df |>
    dplyr::rowwise() |>
    dplyr::mutate("{bk_name}" := prop_cols(.data[[booking_res]], total_bookings)) |>
    dplyr::ungroup()
} 



#' Country and distribution channel summary.
#'
#' @param df hotel_data with variables such as distribution_channel & nationality
#' @param n Number of countries to use in the plot. default is 10
#' @param output_type The type of output, Either 'table', 'table_+distribution channel',
#' or 'plot_+distribution channel'
#'
#' @return A tibble if output_type is table or 'table_+distribution channel' else 
#' a ggplot2 object
#' @export
#'
#' @examples country_dis_channel(hotel_data, 5, 'plot_Direct')
country_dis_channel <- function(df = htl, n = 10, output_type = "table") {
  
  # output_type can be table, table_+distribution_channel or 
  # plot_+distribution_channel.
  
  unique_dist_channel <- unique(df$distribution_channel)
  
  if (output_type == "table") {
    dist_channel_countries <- named_lapply(
      unique_dist_channel,
      function(.x) {
        df |>
          dplyr::count(nationality, distribution_channel, name = "count") |>
          dplyr::mutate(overall_proportion = round(proportions(count)*100, 2)) |>
          dplyr::filter(distribution_channel == .x) |>
          dplyr::mutate(within_group_proportion = round(proportions(count)*100, 2)) |>
          dplyr::select(nationality, `number of guest` = count, overall_proportion, within_group_proportion) |>
          dplyr::arrange(desc(`number of guest`))
      })
    
    return(dist_channel_countries)
    
  } else {
    dist_channel_val <- gsub(".*_", "", output_type)
    
    if (!dist_channel_val %in% unique_dist_channel) {
      stop(paste(dist_channel_val, "is not a valid distribution channel valid",
                 "channels are: ",
                 paste(unique_dist_channel, collapse = ", ")))
    }
    
    dist_channel_tbl <- df |>
      dplyr::count(nationality, distribution_channel, name = "count") |>
      dplyr::mutate(overall_proportion = proportions(count)*100) |>
      dplyr::filter(distribution_channel == dist_channel_val) |>
      dplyr::mutate(within_group_proportion = proportions(count)*100) |>
      dplyr::select(nationality, count, overall_proportion, within_group_proportion) |>
      dplyr::arrange(desc(count)) 
    
    
    if (grepl("table", output_type)) {
      return(dist_channel_tbl)
      
      
    } else if (grepl("plot", output_type)) {
      dist_channel_label <- clean_plot_label(dist_channel_val, type = "clean")
      plot_title <- clean_plot_label(
        paste("Top", n, 
              "Countries In Which Guest Made Booking Through",
              dist_channel_label, "Channel"),
        type = "wrap",
        width = 70
      )
      
      dist_channel_tbl |>
        head(n) |>
        ggplot2::ggplot(ggplot2::aes(x = count, y = reorder(nationality, count))) +
        ggplot2::geom_col() +
        ggplot2::scale_x_continuous(labels = scales::comma_format()) +
        ggplot2::labs(x = "Number Of Bookings", y = NULL,
                      title =  plot_title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title.position = "plot")
      
    } else {
      stop(
        paste("Value Error: `output_type` received an invalid value, only table,",
              paste0("plot_", unique_dist_channel, collapse = ", "), ",",
              paste0("table_", unique_dist_channel, collapse = ", "), "are valid")
      )
    }
  }
}



#' Summary based on guest active period.
#'
#' @param df hotel data with a variable such as nationality
#' @param days_var A variable from the hotel data that represent a period/days
#' @param agg_fun An aggregate function.
#' @param count_threshold A value to use as the minimum number of guest a country
#' should have to appear on the plot. default 10
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples days_summary(hotel_data, 'average_lead_time', 'sum', 5)
days_summary <- function(df, days_var, agg_fun, count_threshold = 10) {
  # Table ---------------------------------------------------------------------|
  f_tbl <- dplyr::group_by(df, nationality) |>
    numeric_summary(days_var)
  
  f_tbl_low <- dplyr::arrange(f_tbl, .data[[agg_fun]]) |>
    dplyr::filter(count > count_threshold) |> 
    head(10) 
  
  f_tbl_high <- dplyr::arrange(f_tbl, desc(.data[[agg_fun]])) |>
    dplyr::filter(count > count_threshold) |> 
    head(10)
  
  low_x_val <- max(f_tbl_low[[agg_fun]])
  max_high_val <- max(f_tbl_high[[agg_fun]])
  high_x_val <- ifelse(max_high_val > 1e+06, 
                       max_high_val-250000, max_high_val-18)
  
  # Plot ----------------------------------------------------------------------|
  cus_theme <- ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                              axis.ticks.x = ggplot2::element_blank(),
                              axis.text.y = ggplot2::element_text(family = ax_txt_font, 7),
                              panel.background = ggplot2::element_blank())
  
  # Plot_low ------------------------------------------------------------------|
  plot_low <- f_tbl_low |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[agg_fun]],
                                 y = forcats::fct_rev(reorder(nationality, .data[[agg_fun]])))) +
    ggplot2::geom_col(alpha = 0.8, fill = jet2) +
    ggplot2::geom_text(ggplot2::aes(x = low_x_val, label = scales::comma(.data[[agg_fun]], 1)),
                       size = 6, color = p_green3, family = gt_num_font) +
    ggplot2::labs(y = NULL,  x = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
    cus_theme
  
  # Plot_high -----------------------------------------------------------------|
  plot_high <- f_tbl_high |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[agg_fun]], y = reorder(nationality, .data[[agg_fun]]))) +
    ggplot2::geom_col(fill = p_green3) +
    ggplot2::geom_text(ggplot2::aes(x = high_x_val, label = scales::comma(.data[[agg_fun]], 1)),
                       size = 6, color = jet2, family = gt_num_font) +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    cus_theme
  
  # Combine plot --------------------------------------------------------------|
  days_var_label <- clean_plot_label(days_var) |> 
    stringr::str_replace_all(" ", "-")
  
  plot_low + plot_high +
    patchwork::plot_annotation(
      title = stringr::str_glue(
        "{agg_labels[[agg_fun]]}-   <span style= 'color:{jet2};'> Lowest</span> & <span style= 'color:{p_green3};'>Highest</span> {days_var_label}"
      ),
      subtitle = stringr::str_glue(
        "Filtered For Countries With More Than {count_threshold} guests."
      ),
      theme = ggplot2::theme(
        plot.title = ggtext::element_markdown(family = tl_txt_font),
        plot.subtitle = ggplot2::element_text(family = tl_txt_font)
      )
    )
}



#' Table summary of guest average lead time by nationality.
#'
#' @param df hotel_data with variables such as 'nationality' & 'average_lead_time'
#' @param agg_fun An aggregate function.
#' @param count_threshold A value to use as the minimum number of guest a country
#' should have to appear on the plot. default 10
#'
#' @return A list of tibble.
#' @export
#'
#' @examples country_lead_time_table(hotel_data, 'mean', 5)
country_lead_time_table <- function(df, agg_fun, count_threshold = 10) {
  agg_fun <- ifelse(missing(agg_fun), "median", agg_fun)
  
  create_table <- function(desc) {
    ff_tbl <- dplyr::group_by(df, nationality) |>
      numeric_summary("average_lead_time") 
    if (desc) {
      ff_tbl <- dplyr::arrange(ff_tbl, desc(.data[[agg_fun]])) 
    } else {
      ff_tbl <- dplyr::arrange(ff_tbl, .data[[agg_fun]])
    }
    dplyr::filter(ff_tbl, count > count_threshold) |>
      dplyr::select(Nationality = nationality, Minimum = minimum, 
                    `25th Quantile` = Q25, Average = mean, Median = median, 
                    `75th Quantile` = Q75, Maximum = maximum)
  }
  
  lab <- clean_plot_label(agg_fun)
  dplyr::lst("Top {lab}"    := create_table(TRUE),
             "Bottom {lab}" := create_table(FALSE))
}



guest_additonal_request_plot <- function() {
  f_tbl <- map_dfr(
    names(select(htl, contains("sr_"))),
    function(.x) {
      htl |>
        count(.data[[.x]]) |>
        mutate(percentage = round(proportions(n)*100, 2),
               request = .x) |>
        rename(request_type = .x)
    }
  ) |>
    mutate(request = clean_plot_label(request, str_remove = "sr"))
  
  level <- f_tbl |>
    filter(request_type == 1) |>
    arrange(desc(n))  |>
    pull(request)
  
  
  yes_plt <- f_tbl |>
    filter(request_type == 1) |>
    mutate(request = factor(request, levels = level)) |>
    ggplot(aes(x = n, y = fct_rev(request))) +
    geom_col(fill = grn_colr) +
    geom_text(aes(x = 4000, label = paste0(percentage, "%")), 
              size = 5, family = gt_num_font) +
    scale_x_reverse(expand = expansion(mult = c(0.03, 0))) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = ax_txt_font),
          panel.background = element_blank())
  
  
  no_plt <- f_tbl |>
    filter(request_type == 0) |>
    mutate(request = factor(request, levels = level)) |>
    ggplot(aes(x = n, y = fct_rev(request))) +
    geom_col(fill = blk_colr) + 
    geom_text(aes(x = 8000, label = paste0(percentage, "%")), 
              size = 5, family = gt_num_font, color = "white") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank())
  
  plt_subtitle <- str_glue(
    "Percentage of guest that <span style = 'color:{grn_colr};'>Requested</span> & <span style = 'color:{blk_colr};'>Did Not</span>"
  )
  
  yes_plt + no_plt +
    plot_layout(ncol = 2, widths = c(1.3, 2.2)) +
    plot_annotation(subtitle = plt_subtitle,
                    theme = theme(plot.subtitle = element_markdown()))
  # title = "Guests Additional Resevation Request",
}



source_revenue_doughnut_chart <- function() {
  htl |>
    summarise(across(c(lodging_revenue, other_revenue), sum)) |>
    pivot_longer(cols = everything()) |>
    mutate(percentage = round(proportions(value)*100, 2),
           label = clean_plot_label(name),
           ymax = cumsum(percentage),
           ymin = c(0, 81.6),
           labelPosition = (ymax + ymin)/2)  |>
    
    ggplot(aes(ymax = ymax, ymin = ymin, xmin = 3, xmax = 4, fill = label)) +
    geom_rect() +
    coord_polar(theta = "y") +
    theme_void() +
    xlim(c(1, 4)) +
    geom_text(x = 2, aes(y = labelPosition, label = paste0(percentage, "%"),
                         color = label), size = 4, family = gt_num_font) +
    scale_fill_manual(values = c(p_green3, jet2)) +
    scale_color_manual(values = c(p_green3, jet2)) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = ax_txt_font)) 
}



country_distribution_channel_table <- function() {
  # Needs:: country_dis_channel, clean_plot_label, named_lapply
  
  list_tbl <- country_dis_channel(df = htl, output_type = "table")
  names(list_tbl) <- c("Corporate", "Travel Agent", "Direct", "E-Platforms")
  
  for (nm in c("Corporate", "Travel Agent", "Direct", "E-Platforms")) {
    list_tbl[[nm]] <- list_tbl[[nm]] |> rename_with(clean_plot_label)
  }
  
  dt_bars_style <- function(df, mv = NULL) {
    reactablefmtr::data_bars(
      data = df,
      text_position = "above",
      fill_color = jet2,
      background = m_bgreen4,
      text_color = e_black1,
      max_value = mv
    )
  }
  outer_tbl <- tibble::tibble(`Distribution Channel` = names(list_tbl))
  
  reactable::reactable(
    data = outer_tbl,
    details = function(index) {
      inner_tbl <- list_tbl[[index]]
      
      htmltools::div(
        style = "padding: 16px",
        reactable::reactable(
          data = inner_tbl,
          outlined = TRUE,
          columns = list(
            `Overall Proportion` = reactable::colDef(cell = dt_bars_style(inner_tbl, 12.8)),
            `Within Group Proportion` = reactable::colDef(cell = dt_bars_style(inner_tbl))
          ),
          highlight = TRUE,
          paginationType = "simple",
          language = reactable::reactableLang(pagePrevious = "\u276e", pageNext = "\u276f"),
          
          theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                            headerStyle = list(borderColor = e_black1,
                                                               backgroundColor = snow5,
                                                               fontSize = "13px"))
        )
      )
    },
    theme = reactableTheme(style = list(fontFamily = tl_txt_font),
                           headerStyle = list(borderColor = m_bgreen4))
  )
}



country_distibution_channel_plot <- function(dis_channel) {
  # Needs:: clean_plot_label
  dis_channel_lab <- ifelse(dis_channel == "Electronic Distribution",
                            "E-Platforms", dis_channel)
  plt_label <- stringr::str_glue(
    "Top 10 Countries In Which Guest Made Booking Through {dis_channel_lab} Channel"
  ) |>
    clean_plot_label(type = "wrap", width = 70)
  
  
  htl |>
    count(nationality, distribution_channel, name = "count") |>
    filter(distribution_channel == dis_channel) |>
    arrange(desc(count)) |>
    head(10) |>
    
    ggplot(aes(x = count, y = reorder(nationality, count))) +
    geom_col(alpha = 0.8, fill = p_green3) +
    scale_x_continuous(labels = scales::comma_format(),
                       expand = expansion(mult = c(0.01, 0.03))) +
    labs(x = "Number Of Bookings", y = NULL, title = plt_label) +
    theme_minimal() +
    theme(plot.title.position = "plot", 
          plot.title = element_text(family = tl_txt_font),
          axis.title.x = element_text(family = tl_txt_font),
          axis.text.x = element_text(family = ax_txt_font, size = 10),
          axis.text.y = element_text(family = ax_num_font, size = 13))
}



country_average_lead_time_table <- function(...) {
  f_tbl <- country_lead_time_table(...)
  f_tbl <- named_lapply(
    c("Top Median", "Bottom Median"),
    function(.x) {
      f_tbl[[.x]] |>
        mutate(color_pal = ifelse(Median > 60, p_green3, m_bgreen4))
    }
  )
  outer_tbl <- tibble::tibble(`Average Lead Time` = names(f_tbl))
  
  reactable::reactable(
    data = outer_tbl,
    details = function(index) {
      inner_tbl <- f_tbl[[index]]
      
      htmltools::div(
        style = "padding: 16px",
        
        reactable::reactable(
          data = inner_tbl,
          outlined = TRUE,
          columns = list(
            Average = colDef(format = reactable::colFormat(digits = 2)),
            Median = colDef(cell = reactablefmtr::data_bars(
              data = inner_tbl, fill_color_ref = "color_pal", background = jet2, 
              text_color = snow5, text_position = "outside-end", box_shadow = TRUE)),
            color_pal = colDef(show = FALSE)
          ),
          highlight = TRUE,
          paginationType = "simple",
          language = reactableLang(pagePrevious = "\u276e", pageNext = "\u276f"),
          theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                            headerStyle = list(borderColor = e_black1, 
                                                               backgroundColor = snow5, 
                                                               fontSize = "13px"))
        )
      )
    },
    theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                      headerStyle = list(borderColor = m_bgreen4))
  )
}



market_segment_request <- function() {
  # Needs:: 
  tbl_seg <- c("sr_low_floor", "sr_accessible_room", "sr_quiet_room", "sr_shower", 
               "sr_near_elevator")
  
  list_tbl <- named_lapply(
    tbl_seg,
    function(.x) {
      htl |>
        count(market_segment, .data[[.x]]) |>
        pivot_wider(id_cols = market_segment, 
                    names_from = .data[[.x]], 
                    values_from = n,
                    values_fill = 0) |>
        mutate(total_count = `1` + `0`,
               `Yes(%)` = round((`1`/total_count)*100, 2),
               `No(%)` = round((`0`/total_count)*100, 2)) |>
        select(`Market Segment` = market_segment, `Yes(%)`, `No(%)`) |>
        arrange(desc(`Yes(%)`)) 
    }
  )
  
  names(list_tbl) <- clean_plot_label(names(list_tbl), str_remove = "sr")
  
  outer_tbl <- tibble::tibble(`Guest Request` = names(list_tbl))
  
  reactable::reactable(
    data = outer_tbl,
    details = function(index) {
      inner_tbl <- list_tbl[[index]]
      
      htmltools::div(
        style = "padding: 16px",
        
        reactable::reactable(
          data = inner_tbl,
          outlined = TRUE,
          defaultColDef = reactable::colDef(
            cell = reactablefmtr::data_bars(data = inner_tbl,
                                            text_position = "inside-base",
                                            fill_color = jet2,
                                            text_color = "#C4C4C4",
                                            background = m_bgreen4,
                                            round_edges = TRUE,
                                            box_shadow  = TRUE) 
          ),
          columns = list(
            `Yes(%)` = colDef(format = colFormat(digits = 3, separators = TRUE)),
            `No(%)` = colDef(format = colFormat(digits = 2, separators = TRUE))
          ),
          
          highlight = TRUE,
          
          theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                            headerStyle = list(borderColor = e_black1,
                                                               backgroundColor = snow5,
                                                               fontSize = "13px"))
        )
      )
    },
    theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                      headerStyle = list(borderColor = m_bgreen4))
  )
}





# Not Used Yet
market_segment_request_plot <- function(request) {
  req_label <- guest_request_label[[request]]
  plot_title <- stringr::str_glue(
    "Guest Request For {req_label} By Market Segment"
  )
  dplyr::count(htl, market_segment, .data[[request]]) |>
    tidyr::pivot_wider(id_cols = market_segment, 
                       names_from = .data[[request]], 
                       values_from = n,
                       values_fill = 0) |>
    dplyr::mutate(total_count = `1` + `0`,
                  percentage = round((`1`/total_count)*100, 2)) |>
    dplyr::select(market_segment, count = `1`, percentage) |>
    dplyr::arrange(desc(percentage)) |>
    
    ggplot2::ggplot(ggplot2::aes(x = count, 
                                 y = forcats::fct_reorder(market_segment, count))) +
    ggplot2::geom_col(alpha = 0.8, fill = jet2) +
    ggplot2::geom_text(ggplot2::aes(x = 2000, label = paste0(percentage, "%")), 
                       size = 6, family = gt_num_font, color = p_green3) +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = "Count of Request", y = NULL, title = plot_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title.position = "plot",
                   plot.title = ggplot2::element_text(family = tl_txt_font),
                   axis.text.x = ggplot2::element_text(family = ax_num_font),
                   axis.text.y = ggplot2::element_text(family = ax_txt_font),
                   axis.title.x = ggplot2::element_text(size = 8))
}



unique_guest_distribution_channel <- function() {
  htl |>
    count(distribution_channel, sort = TRUE, name = "count") |>
    mutate(
      distribution_channel = case_when(distribution_channel == "Travel Agent/Operator" ~ "Travel Agent",
                                       distribution_channel == "Electronic Distribution" ~ "E-platforms",
                                       TRUE ~ distribution_channel),
      proportion = round(proportions(count)*100, 2)
    )  |>
    
    ggplot(aes(x = count, y = reorder(distribution_channel, count))) +
    geom_col(alpha = 0.9, fill = jet2) +
    geom_text(aes(x = 45000, label = paste0(proportion, "%"), family = gt_num_font), 
              size = 9, color = p_green3, face = "bold"
    ) +
    scale_x_continuous(labels = scales::comma_format()) +
    labs(x = NULL, y = NULL,
         title = "Number Of Unique Guest By Distribution Channel") +
    theme_minimal() +
    theme(plot.title.position = "plot", 
          plot.title = element_text(family = tl_txt_font, face = "bold"),
          axis.text.x = element_text(family = ax_txt_font, size = 13),
          axis.text.y = element_text(family = ax_num_font, size = 13))
}



distribution_channel_canceled_bookings <- function() {
  htl |>
    filter(bookings_canceled > 0) |>
    count(distribution_channel, sort = TRUE, name = "count") |>
    mutate(percentage = proportions(count)*100,
           distribution_channel = ifelse(distribution_channel == "Travel Agent/Operator", 
                                         "Travel Agent", distribution_channel)) |>
    bind_rows(tibble(distribution_channel = "E-Platforms",
                     count = 0,
                     percentage = 0)) |>
    ggplot(aes(x = count, y = reorder(distribution_channel, count))) +
    geom_segment(aes(x = 0, 
                     xend = count, 
                     y = reorder(distribution_channel, count), 
                     yend = distribution_channel),
                 size = 1, color = p_green3) +
    
    geom_text(aes(label = paste0(count, " | ", round(percentage, 2), "%"), 
                  hjust = -0.1), face = "bold", size = 8, color = jet2, family = gt_num_font) +
    labs(x = NULL, 
         y = NULL, 
         title = "Number & Percentage of Canceled Bookings By Distribution channel.") +
    scale_x_continuous(expand = expansion(mult = c(0, .20))) +
    theme(panel.background = element_blank(),
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(size = 1, color = "rosybrown1"),
          axis.text.y = element_text(colour = jet2, 
                                     size = 13, 
                                     family = ax_txt_font),
          plot.title.position = "plot",
          plot.title = element_text(color = e_black1, family = tl_txt_font, face = "bold"))
}



distribution_channel_canceled_rates <- function() {
  bookings_sumry(df = htl, booking_res = "bookings_canceled") |>
    
    select(distribution_channel, bookings_canceled, 
           total_bookings, canceled_percent) |>
    filter(bookings_canceled > 0) |>
    group_by(distribution_channel) |>
    numeric_summary("canceled_percent") |>
    arrange(desc(median)) |>
    mutate(distribution_channel = ifelse(distribution_channel == "Travel Agent/Operator", 
                                         "Travel Agent", distribution_channel)) |>
    select(`Distribution Channel` = distribution_channel, Average = mean, Median = median) |>
    bind_rows(
      tibble(`Distribution Channel` = "E-Platforms", Average = 0, Median = 0)
    ) |>
    mutate(Average = round(Average, 2),
           Median = round(Median, 2))  |>
    rename(`Average(%)` = Average, `Median(%)` = Median) %>% 
    
    reactable(
      defaultColDef = colDef(cell = data_bars(data = ., 
                                              text_position = "inside-base",
                                              fill_color = jet2,
                                              background = m_bgreen4,
                                              text_color = snow5,
                                              box_shadow = TRUE)),
      theme = reactableTheme(color = e_black1, 
                             backgroundColor = snow5,
                             headerStyle = list(borderColor = e_black1))
    )
}



distribution_channel_checked_in_bookings <- function(output_type = "plot") {
  output_type <- match.arg(output_type, c("plot", "table"))
  
  f_tbl <- htl |>
    dplyr::filter(bookings_checked_in > 0) |>
    dplyr::count(distribution_channel, name = "count") |>
    dplyr::mutate(checked_in = proportions(count)*100,
                  distribution_channel = dplyr::case_when(
                    distribution_channel == "Travel Agent/Operator" ~ "Travel Agent",
                    distribution_channel == "Electronic Distribution" ~ "E-Platforms",
                    TRUE ~ distribution_channel
                  )) |>
    dplyr::select(distribution_channel, checked_in) |>
    dplyr::bind_cols(
      purrr::map_dfc(
        c("bookings_canceled", "bookings_no_showed"),
        function(.x) {
          htl |>
            dplyr::filter(.data[[.x]] > 0) |>
            dplyr::count(distribution_channel, name = "count") |>
            dplyr::mutate("{.x}" := proportions(count)*100) |>
            dplyr::select(.x)
        }
      ) |>
        dplyr::add_row(bookings_canceled = 0, bookings_no_showed = 0) |>
        dplyr::rename(canceled = bookings_canceled, no_show = bookings_no_showed)
    ) |>
    dplyr::rename_with(clean_plot_label)
  
  
  if (output_type == "table") {
    f_tbl
    
  } else if (output_type == "plot") {
    f_tbl |>
      tidyr::pivot_longer(cols = `Checked In`:`No Show`, 
                          names_to = "bookings", 
                          values_to = "percentage") |>
      dplyr::mutate(txt_position = dplyr::case_when(
        `Distribution Channel` == "Corporate" ~ 8,
        `Distribution Channel` == "Direct" ~ 2.2,
        `Distribution Channel` == "E-Platforms" ~ 0.8,
        `Distribution Channel` == "Travel Agent" ~ 8,
        TRUE ~ 0)) |>
      
      ggplot2::ggplot(ggplot2::aes(x = tidytext::reorder_within(bookings, 
                                                                percentage,
                                                                `Distribution Channel`),
                                   y = percentage)) +
      ggchicklet::geom_chicklet(radius = grid::unit(2, "mm"), fill = m_bgreen4) +
      tidytext::scale_x_reordered() +
      ggplot2::facet_wrap(vars(`Distribution Channel`), scales = "free") +
      ggplot2::geom_text(aes(y = txt_position, label = paste0(round(percentage, 2), "%")),
                         size = 6, family = gt_num_font, color = e_black1, face = "bold") +
      ggplot2::labs(x = NULL, y = NULL, 
                    title = "Percentage Of Guest Check In By Distribution Channel") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(family = ax_txt_font, size = 13, color = jet2),
                     strip.background = ggplot2::element_rect(fill = "white"),
                     strip.text = ggplot2::element_text(family = ax_txt_font, size = 13, color = "#ADADAD", face = "bold"),
                     plot.title = ggplot2::element_text(family = tl_txt_font, face = "bold"),
                     panel.background = ggplot2::element_blank())
  }
  
}



month_guest_last_stayed <- function() {
  txt_str <- c(
    "for more than <span style= 'font-size:18pt;'>16</span> months", 
    "within <span style= 'font-size:18pt;'>9</span> to <span style= 'font-size:18pt;'>16</span> months", 
    "within <span style= 'font-size:18pt;'>4</span> to <span style= 'font-size:18pt;'>8</span> months", 
    "within last <span style= 'font-size:18pt;'>3</span> months"
  )
  
  htl |> 
    mutate(recent_month = case_when(days_since_last_stay <= 121 ~ "0-3",
                                    days_since_last_stay <= 274 ~ "4-8",
                                    days_since_last_stay <= 515 ~ "9-16",
                                    days_since_last_stay <= 1104 ~ ">16",
                                    TRUE ~ ">36")) |>
    
    count(recent_month, sort = TRUE, name = "count") |>
    mutate(proportion = round(proportions(count)*100, 2)) |>
    mutate(month_label = txt_str,
           take = str_glue(
             "<span style='font-size:18pt;'>{proportion}%</span> {month_label}"
           )) |>
    select(-month_label) |>
    arrange(proportion) |>
    
    ggplot(aes(x = count, y = fct_rev(reorder(recent_month, count)))) +
    geom_col(fill = p_green3) +
    scale_x_continuous(label = scales::comma_format()) +
    labs(x = "Number Of Guest", y = NULL, 
         title = "Months Since Guest Last Stayed At The Hotel") +
    geom_richtext(x = 25000, aes(label = take), color = e_black1,
                  size = 5, fill = NA, label.color = NA, family = gt_num_font) +
    theme(panel.background = ggplot2::element_blank(),
          plot.title = element_text(family = tl_txt_font),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(family = ax_num_font),
          axis.title.x = element_text(family = ax_txt_font))
}


guest_bookings_summary_since_last_stay <- function() {
  canl_rename <- function(x) {
    x <- clean_plot_label(x, str_remove = "bookings") 
    stringr::str_glue("Number of {x} bookings")
  }
  
  tbl_list <- named_lapply(
    c("bookings_canceled", "bookings_no_showed","bookings_checked_in", "total_bookings"),
    function(.x) {
      htl |> 
        mutate(recent_month = case_when(days_since_last_stay <= 121 ~ "0-3",
                                        days_since_last_stay <= 274 ~ "4-8",
                                        days_since_last_stay <= 515 ~ "9-16",
                                        days_since_last_stay <= 1104 ~ ">16",
                                        TRUE ~ ">36"),
               recent_month = factor(recent_month, levels = c("0-3", "4-8", "9-16", ">16"))) |>
        filter(.data[[.x]] > 0) |>
        count(recent_month, name = "count") |>
        mutate(`percentage(%)` = round(proportions(count)*100, 2),
               recent_month = case_when(
                 recent_month == ">16"  ~ "More Than 16",
                 recent_month == "9-16" ~ "9 to 16",
                 recent_month == "4-8"  ~ "4 to 8",
                 recent_month == "0-3"  ~ "Last 3"
               )) |>
        rename_with(clean_plot_label) |>
        rename("{canl_rename(.x)}" := Count)
    })
  names(tbl_list) <- clean_plot_label(names(tbl_list), str_remove = "bookings")
  
  outer_tbl <- tibble::tibble(`Guest Bookings` = names(tbl_list))
  
  reactable::reactable(
    data = outer_tbl,
    details = function(index) {
      inner_tbl <- tbl_list[[index]]
      
      htmltools::div(
        style = "padding: 16px",
        
        reactable::reactable(
          data = inner_tbl,
          outlined = TRUE,
          defaultColDef = reactable::colDef(
            format = colFormat(digits = 1, separators = TRUE),
            style = highlight_min_max(inner_tbl,
                                      min_font_color = m_bgreen4,
                                      max_font_color = p_green3)
          ),
          columns = list(
            Percentage = colDef(
              cell = reactablefmtr::data_bars(data = inner_tbl,
                                              fill_color = jet2,
                                              text_position = "inside-base",
                                              background = m_bgreen4,
                                              round_edges = TRUE,
                                              box_shadow = TRUE)
            )
          ),
          highlight = TRUE,
          
          theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                            headerStyle = list(borderColor = e_black1,
                                                               backgroundColor = snow5,
                                                               fontSize = "13px"))
        )
      )
    },
    theme = reactable::reactableTheme(style = list(fontFamily = tl_txt_font),
                                      headerStyle = list(borderColor = m_bgreen4))
  )
}





revenue_generated <- function() {
  htl |>
    select(contains("revenue")) |>
    summarise(across(everything(), sum)) |>
    pivot_longer(cols = everything(), names_to = "revenue", values_to = "value") |>
    mutate(revenue = clean_plot_label(revenue)) |>
    
    ggplot(aes(x = value, 
               y = fct_rev(fct_reorder(revenue, value)),
               fill = revenue)) +
    geom_col(show.legend = FALSE, width = 0.8) +
    scale_fill_manual(values = c(jet2, jet2, p_green3)) +
    labs(x = NULL, 
         y = NULL,
         title = "Sum of All Revenue Generated From Lodging And Other Purchase",
         subtitle = "And The Sum of Both Revenue") +
    scale_x_continuous(label = scales::comma_format()) +
    theme_minimal() +
    theme(plot.title.position = "plot",
          plot.title = element_text(family = tl_txt_font, face = "bold"),
          plot.subtitle = element_text(color = p_green3, family = tl_txt_font, face = "bold"),
          axis.text.x = element_text(size = 13, family = ax_num_font),
          axis.text.y = element_text(size = 13, family = ax_txt_font))
}



revenue_market_segment <- function() {
  htl |>
    group_by(market_segment) |>
    summarise(count = n(),
              mean = mean(total_revenue),
              median = median(total_revenue),
              sum = sum(total_revenue)) |>
    mutate(sum_proportion = round(proportions(sum)*100, 2),
           median_proportion = round(proportions(median)*100, 1),
           market_segment = ifelse(market_segment == "Travel Agent/Operator",
                                   "Travel Agent", market_segment)) |>
    
    ggplot(aes(x = median, y = reorder(market_segment, median))) +
    geom_col(width = 0.7, fill = jet2) +
    geom_point(size = 16, color = p_green3, shape = 15) +
    geom_text(aes(label = paste0(median_proportion, "%")), 
              color = "white", family = gt_num_font, size = 5) +
    labs(x = NULL, y = NULL, title = "Median Revenue Generated By Market Segment") +
    theme_minimal() +
    theme(plot.title = element_text(family = tl_txt_font),
          axis.text.x = element_text(family = ax_num_font),
          axis.text.y = element_text(family = ax_txt_font))
}
