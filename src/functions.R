# summary functions

summarize_sims <- function(x, prob, hdi, agg_fx) {
  r <- function(x) round(x, digits=2)
  
  if(hdi){
    hdi <- hdi(x, ci=prob)
    res <- glue::glue(
      "{r(agg_fx(x))} [{r(hdi$CI_low)}, {r(hdi$CI_high)}]"
    )
  } else {
    probs <- c((1 - prob)/2, 1 - (1 - prob)/2)
    quantiles <- quantile(x, probs=probs)
    
    res <- glue::glue(
      "{r(agg_fx(x))} [{scales::percent(prob)} Interval:{r(quantiles[[1]])}, {r(quantiles[[2]])}]"
    )
  }
  list(summary=res)
}


recode_methods <- function(method, named_vector){
  recode(
    method,
    !!!setNames(names(named_vector), named_vector),
    .default=method
  )
}


# plotting functions
get_plot_data <- function(data, factor_levels) {
  
  pivoted_data <- pivot_longer(data, !n_sim)
  
  if(is.null(factor_levels)){
    factor_levels <- data %>% select(-n_sim) %>% names()
  }
  
  pivoted_data$name <- factor(pivoted_data$name, levels=factor_levels)
  
  pivoted_data
}

add_interval <- function(data, ci=0.95, hdi=F) {
  if(hdi) {
    cred.int <-
      data %>%
      group_by(name) %>%
      summarise(CI=list(hdi(value, ci=ci)),
                m=median(value, na.rm=TRUE)) %>%
      unnest_wider(CI)
    
    data <-
      left_join(data, cred.int, by="name") %>%
      mutate(in_interval=value > CI_low & value < CI_high)
  } else {
    probs <- c((1 - ci)/2, 1 - (1 - ci)/2)
    
    data <-
      data %>%
      group_by(name) %>%
      arrange(value) %>%
      mutate(percentile=row_number()/n()) %>%
      ungroup() %>%
      mutate(in_interval = percentile > probs[1] & percentile < probs[2])
  }
  data
}

plot_fw_histogram <- function(data, inb_ref_col=NA, ci=0.95, hdi=F, limit_y=FALSE, subtitle="",
                              factor_levels=NULL, agg_fx=median, n_bins=40,
                              n_breaks=3, plot_labels=labs(x="", y=""),
                              agg_line_alpha=0.6, agg_line_size=2, remove_axis=F,
                              label_wrap_width=10) {
  
  if(!is.na(inb_ref_col)) {
    data <-
      data %>%
      mutate(across(!n_sim, function(x) x - !!rlang::sym(inb_ref_col))) %>%
      select(-all_of(inb_ref_col))
  }
  
  p_data <-
    get_plot_data(data, factor_levels=factor_levels) %>%
    add_interval(ci=ci, hdi=hdi)
  
  df_agg <-
    p_data %>%
    group_by(name) %>%
    summarize(m=agg_fx(value))
  
  p <-
    p_data %>%
    ggplot(aes(value, fill=in_interval)) +
    geom_histogram(bins=n_bins) +
    coord_flip() +
    facet_wrap(~name, labeller=label_wrap_gen(width=label_wrap_width), nrow=1) +
    theme_bw() +
    scale_fill_manual(values=c("grey50","grey50", "#ADD8E6")) +
    guides(fill="none") +
    scale_y_continuous(n.breaks=n_breaks) +
    plot_labels
  
  if(!limit_y){
    p <- p + scale_x_continuous(labels=scales::dollar_format())
  }
  
  my_plot_innards <- ggplot_build(p)
  
  extracted_points <- tibble(
    outcome = my_plot_innards[["data"]][[1]][["x"]],
    count = my_plot_innards[["data"]][[1]][["y"]],
    in_interval = (my_plot_innards[["data"]][[1]][["group"]]) %>% as.factor(),
    method = (my_plot_innards[["data"]][[1]][["PANEL"]] %>% as.character)
  )
  
  heights <-
    df_agg %>%
    rownames_to_column(var="method_n") %>%
    left_join(extracted_points, by=c("method_n"="method")) %>%
    mutate(diff=abs(m-outcome)) %>%
    group_by(name) %>%
    arrange(diff) %>%
    slice(1)
  
  p <- p + geom_segment(data=heights, aes(x=m, xend=m, y=0, yend=count), size=agg_line_size, alpha=agg_line_alpha)
  
  if(remove_axis){
    p <- p + scale_y_continuous(breaks = NULL)
  }
  p
}
