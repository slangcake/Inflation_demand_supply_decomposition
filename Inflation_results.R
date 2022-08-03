#' ---
#' title: Inflation decomposition
#' ---
#+ fig.width=6, fig.height=6, dpi=50


#' Shapiro inflation breakdown under different specifications

for(i in 1:length(results)){

results[[i]]$lag %>% print()

results[[i]]$window %>% print()

results[[i]]$chart_yy %>% print()
  
}

# cr %>% print