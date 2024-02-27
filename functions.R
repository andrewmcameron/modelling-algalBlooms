# ---- CUSTOM FUNCTIONS USED IN CHLa MODELLING, ANALYSIS, and DATA VISUALIZATION ----- #


#  function:   plot_allYears(data, x, y, formula, method, title, x.title, y.title, axis.title.size)

# Plots CHLa ~ T:Q for all available data across all years
## arguments
#### data = dataframe with model variables 
#### x = column name in `data` containing the values of x
#### y = column name in `data` containing the values of y
#### formula = model formula, unquoted (e.g., y ~ log(x))
#### method = quoted, either "lm" or "nls"
#### title = defines plot title, character string 
#### x.title = character string to label variable on x axis
#### y.title= character string to label variable on y axis 
#### axis.title.size = numeric; axis title font size. defaults to 16

plot_allYears <- function(data, x, y, formula, method, title, x.title, y.title, axis.title.size = 16, point.col) {
  
  y.coef <- round(reg.stats[1, 2], 4)
  R2 <- round(reg.stats[1, 5], 4)
  p.val <- round(reg.stats[1, 4], 2)
  
  p <- ggplot(data = data[!is.na(data[[x]]) & !is.na(data[[y]]),]) +
    geom_point(aes(x = !!sym(x),  y = !!sym(y), color = factor(period)), 
               size = 2.25,
               alpha = 0.55) +
    geom_smooth(aes(x = !!sym(x), y = !!sym(y)),
                color = "grey45",
                linewidth = 1,
                alpha = 0.5,
                formula = formula, 
                method = method, 
                show.legend = FALSE) +
    labs(title = NULL,
         x = x.title,
         y = y.title) +
    theme_classic() +
    theme(legend.position = "right",
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 16, margin = margin(b = 10)),
          axis.text.x = element_text(size = 11.5),
          axis.text.y = element_text(size = 11.5),
          axis.line = element_line(color = "grey"),
          axis.title.x = element_text(size = axis.title.size, color = "grey20", margin = margin(t = 5)),
          axis.title.y = element_text(size = axis.title.size, color = "grey20", margin = margin(r = 5))) +
    scale_color_manual(values = point.col, name = NULL)
  
  if (x == "t_q" | x == "t_q_avg") {
    plot <- p +
      geom_text(aes(x = .6, y = 1.1),
                label = paste("y = ", round(reg.stats[1, 2],4), "\n",
                              " R\u00B2 = ", round(reg.stats[1 ,5],4)),
                size = 4, hjust=0) +
      scale_x_continuous(breaks = seq(0, max(data[[x]], na.rm = TRUE), by = 0.2))
  } else if (x == "TtoCond") {
    plot <- p +
      geom_text(aes(x = 22, y = 1.1),
                label = paste("y = ", round(reg.stats[1, 2],4), "\n",
                              " R\u00B2 = ", round(reg.stats[1 ,5],4)),
                size = 4, hjust=0) +
      xlim(c(0, 40)) +
      scale_x_continuous(breaks = seq(0, max(data[[x]], na.rm = TRUE), by = 10))
  }
  
  print(plot)
}



# ------------------------------------------------------------------------------------

# function:   plot_eachYear(data, x, formula, method, reg.stats_, xlab) 

# Generates linear regression plots by year for time series data
## arguments
#### data = dataframe with model variables
#### x = column name in `data` containing the values of x
#### formula = model formula, unquoted
#### method = either one of "lm" or "nls"
#### reg.stats_ = the dataframe containing derived model regression statistics
#### xlab = character string containing the ratio used as predictor, e.g., "Temperature:Discharge"

plot_eachYear <-function(data, x, formula, method, reg.stats_, xlab) {
  for (yr in years_vec) {
    max_x <- max(data[year(data$date) == yr, x], na.rm = TRUE)
    max_y <- max(data[year(data$date) == yr, "ln_chla"], na.rm = TRUE)
    
    text_x <- max_x * 0.75  # 75% of the max x value
    text_y <- max_y * 0.30  # 30% of the max y values
    
    plot <- 
      ggplot() +
      geom_smooth(data = data[year(data$date) == yr,],
                  aes(x = !!sym(x), y = ln_chla), 
                  color = "grey30",
                  alpha = .75,
                  formula = formula, 
                  method = method,
                  show.legend = FALSE) +
      geom_point(data = data[year(data$date) == yr,],
                 aes(x = !!sym(x), y = ln_chla), 
                 color = ifelse(yr < 2019, "#ED254E", "#0096FF"), 
                 alpha = .6) +
      geom_text(aes(x = text_x, y = text_y),
                label = paste(" R\u00B2 = ", round(reg.stats_[reg.stats_$Model.Year == yr ,5],4), "\n", 
                              "coef. = ", round(reg.stats_[reg.stats_$Model.Year == yr, 2],4), "\n",
                              "p = ", round(reg.stats_[reg.stats_$Model.Year == yr, 4], 2) ),
                size = 3.5, hjust=0) +
      labs(title = yr,
           x = xlab,
           y = "ln(CHLa) (\u03BCg/L)") +
      theme(title = element_text(size = 8)) +
      theme_minimal()
    
    suppressWarnings(
      print(plot)
    )
  }
}



# ------------------------------------------------------------------------------------

# function:   plot.residuals_timeSeries()

## use: generate time series plot for observed CHLa with points colored to indicate model accuracy in predicting a bloom day for a given date.
## arguments
#### data = dataframe containing model residuals and observation dates 
#### title = string defining plot title 
#### axis.title.size = numeric; axis title font size. defaults to 16


plot.residuals_timeSeries <- function(data, title = NULL, axis.title.size = 16) {
  plot <- 
    ggplot(data = data) +
    geom_hline(yintercept = 0, linetype = "solid", color = "salmon") +
    geom_line(aes(x = date, y = residuals), col="grey75" ) +
    geom_point(aes(x = date, y = residuals), alpha = .65) +
    labs(title = title,
         y = "Residuals",
         x = "Date",
         color = "Year") +
    theme_classic() +
    theme(legend.position = "top",
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 16, margin = margin(b = 15)),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11.5),
          axis.text.y = element_text(size = 11.5),
          axis.line = element_line(color = "grey"),
          axis.title.x = element_text(size = axis.title.size, color = "grey20", margin = margin(t = 15)),
          axis.title.y = element_text(size = axis.title.size, color = "grey20", margin = margin(r = 15))) +
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years",
                     limits = c(as.POSIXct(paste0(min_yr, "-01-01")), as.POSIXct(paste0(max_yr,"-12-31"))
                     )
    )
  
  
  print(plot)
  
}



# ------------------------------------------------------------------------------------

# function:   plot_bloomday_predictions(bloom.data)

## use:  generate time series plot for observed CHLa with points colored to indicate the model accuracy in predicting a bloom day for a given date.
## arguments:
#### data = data frame containing model variables
#### x = quoted column name of model predictor variable
#### method = "lm" or "nls", the method used to define the model formula
#### axis.title.size = numeric; axis title font size. defaults to 16

plot_bloomday_predictions <- function(data, x, method, axis.title.size = 16) {
  
  if (method == "lm") {
    formula_string <- paste0("ln_chla ~ log(", x, ")")
  } else {
    if (method == "nls") {
      formula_string <- paste0("CHLa ~ CHLamax * tanh((alpha *", x, ")/CHLamax)")
    }
  }
  
  if (method == "lm") {
    model <- lm(as.formula(formula_string), data = data)
    residuals <- model$residuals
  } else {
    if (method == "nls"){
      model <- nls(as.formula(formula_string), data = data,
                   start = c(alpha = 50, CHLamax = 15),
                   control = nls.control(maxiter = 2000))
      residuals <- residuals(model)
    }
  }
  
  predicted_vals <- fitted(model)
  
  # Create vector corresponding to row numbers for all non-NA CHLa values.
  if (x == "TtoCond") {
    notNA_indices <- which(!is.na(data$CHLa) & !is.na(data$TtoCond)) 
  } else {
    notNA_indices <- which(!is.na(data$CHLa))
  }
  
  # Create data frame with residuals, predicted vals, and the row number corresponding to each observation in the original data frame
  observed_predicted <- 
    data.frame(
      rownum = notNA_indices,
      residuals = residuals,
      predicted = predicted_vals,
      observed = data$CHLa[notNA_indices],
      predicted_exp = NA)
  
  
  # Exponentiate predicted values
  observed_predicted$predicted_exp <- exp(observed_predicted$predicted)
  
  observed_predicted <- observed_predicted %>%
    mutate( bloom_days = case_when(
      observed >= 40 & predicted_exp >= 40 ~ "correctly predicted",
      observed <= 40 & predicted_exp <= 40 ~ "correctly predicted",
      observed >= 40 & predicted_exp <= 40 ~ "false negative",
      observed <= 40 & predicted_exp >= 40 ~ "false positive",
      TRUE ~ NA
    ) )
  
  # Define a column that contains the row number for each observed value, to be used in joining bloom day data
  data$row <- which(data$date==data$date)
  
  data <-
    data %>%
    left_join(observed_predicted %>%
                select(rownum, bloom_days),
              by = join_by(row == rownum) )
  
  # Create mode accuracy table
  bloom_table <- 
    as.data.frame(table(data$bloom_days))
  colnames(bloom_table) <- c("Model Result", "Count")
  
  bloom_table$Percentage <- 
    paste0(
      round(bloom_table$Count/sum(bloom_table$Count), 2) * 100,
      "%")
  
  table <- bloom_table %>%
    kable(align = "c", 
          caption = NULL) %>%
    kable_paper() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed") )
  
  # Create and generate plot
  
  p <- 
    suppressWarnings(
      ggplot(data = na.omit(data)) +
        geom_line(aes(x = date, y = CHLa), col = "#D9E0E8") +
        geom_point(aes(x = date, y = CHLa, color = bloom_days), alpha = .65) +
        geom_hline(yintercept = 40, linetype = "dashed", color = "grey") +
        geom_segment(aes(x = as.POSIXct("2010-01-01"), y = 40, 
                         xend = as.POSIXct("2010-01-01"), yend = 25), 
                     linetype = "dashed", color = "grey60") +
        geom_text(aes(x = as.POSIXct("2009-10-01"), y = 3), 
                  label = "bloom\nthreshold",
                  hjust = 0, vjust = -1, color = "grey60", size = 3) +
        labs(title = paste0("Predictive Success of T:Q Model, 2010-", max_yr),
             subtitle = NULL,
             y = paste0("Observed total Chla (\u03BCg/L)"),
             x = "Date",
             color = "Year") +
        theme_classic() +
        theme(legend.position = "top",
              legend.text = element_text(size = 10),
              plot.title = element_text(size = 16, margin = margin(b = 10)),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 11.5),
              axis.text.y = element_text(size = 11),
              axis.line = element_line(color = "grey"),
              axis.title.x = element_text(size = axis.title.size, color = "grey20", margin = margin(t = 15)),
              axis.title.y = element_text(size = axis.title.size, color = "grey20", margin = margin(r = 15))) +
        scale_x_datetime(date_labels = "%Y", date_breaks = "1 years",
                         limits = c(as.POSIXct("2009-10-01"), as.POSIXct("2023-12-31"))) +
        scale_y_continuous(breaks = seq(from = 0, max(df.james$CHLa, na.rm = TRUE), by=20)) +
        scale_color_manual(values = c("correctly predicted" = "grey30", 
                                      "false negative" = "#0096FF", 
                                      "false positive" = "salmon"),
                           name = NULL)
    )
  
  
  print(p)
  table
  
}



# ------------------------------------------------------------------------------------

#  function:   regstat_eachyear(reg.df, data, x)

## use: derive regression statistics for individual model years and store them in a single dataframe
## arguments
#### reg.df = dataframe to contain regression statistics
#### data = dataframe containing model variable data
#### x = column name in dataframe containing the values of x
regstat_eachyear <- function(reg.df, data, x) {
  for (i in seq_along(reg.df$Model.Year)) {
    subset_data <- data[year(data$date) == reg.df$Model.Year[i], ]
    
    res <- summary(
      lm(ln_chla ~ log(subset_data[[x]]), data = subset_data)
    )
    
    reg.df$coefficient[i] <- round(res$coefficients[2,1], 4)  # add reg. coefficient to results summary
    reg.df$coef.error[i] <- round(res$coefficients[2,2], 4)  # add coefficient error to results summary
    reg.df$p.val[i] <- round(res$coefficients[2,4], 4)   # add p values to results summary
    reg.df$RSQR[i] <- round(res$adj.r.squared, 4)   # add adj. r sq to results summary
  }
  return(reg.df)
}

# ------------------------------------------------------------------------------------

# function:    regstat_combined(reg.df, data, x)

# use: derive regression statistics for all years combined and store in a one-row dataframe
## arguments
#### reg.df = dataframe to contain regression statistics
#### data = dataframe containing model variable data
#### x = column name in dataframe containing the values of x
regstat_combined <- function(reg.df, data, x) {
  res <- 
    summary(lm(ln_chla ~ log(data[[x]]), data = data))
  
  reg.df[1, 2] <- round(res$coefficients[2,1], 4)
  reg.df[1, 3] <- round(res$coefficients[2,2], 4)  
  reg.df[1, 4] <- round(res$coefficients[2,4], 4)   
  reg.df[1, 5] <- round(res$adj.r.squared, 4)   
  
  return(reg.df)
}

# ------------------------------------------------------------------------------------


