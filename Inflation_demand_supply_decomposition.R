#---------------------------------------------
# Is inflation a demand or supply problem?
# Sean Langcake August 2022
# This code replicates Shapiro 2022 for Australia
#---------------------------------------------

#---------------------------------------------
# Preamble

library('oxgraphs')
library('seasonal')
library('openxlsx')
ox_setup()

this_path <- dirname(getActiveDocumentContext()$path)
setwd(this_path)

#---------------------------------------------
# User inputs

# Additional user inputs at lines 93 and 400 and determine data import and model thresholds

# Multiple lag and window choices can be specified
#How many lags to have in each regression
lag <- c(3)

#Size on the rolling window
window = c(60) 

# Name of file to write results to
results_write_file <- 'Inf_decomp_results.csv'

#---------------------------------------------
# Functions

# Create price indices
create_p_ind <- function(v,q,n){
  p <- n[[v]]/q[[v]]*100
}

# Seasonally adjust
seas_df <- function(v,x){
  ts <- ts(x[[v]], 
           frequency = 4, 
           end = c(year(tail(x[['Dates']],1)), quarter(tail(x[['Dates']],1)))) %>% seas
  tmp <- ts$data[,1] %>% as.numeric %>% as.data.frame()
  r <-  ifelse(nrow(tmp)==nrow(x['Dates']),
               tmp,
               rbind(rep(NA,nrow(x['Dates'])-nrow(tmp)) %>% as.data.frame(),tmp))
  return(r)
}

# Create seasonally adjusted data frames
create_seas_df <- function(x){
  tmp <- cbind(x['Dates'],
               lapply(names(x)[!(names(x)=='Dates')], seas_df, x) %>% as.data.frame())
  names(tmp) <- names(x)  
  return(tmp)
}

# Add lags to df
create_lags <- function(l,x){
  apply(x,2,lag,l) %>% as.data.frame()
}

# Rolling regressions
roll_regress <- function(v, lg = 3 , wnd = 60){
  
  #Independent variables
  lhs <- data.frame(p= lp[[v]], q = lq[[v]])
  
  #Explanatory variables
  df <- lapply(seq(0:lg), create_lags, lhs) %>% flatten %>% as.data.frame() %>% drop_na
  
  t <- 1
  
  #Pre-allocate residuals data frame
  res <- data.frame(p_res = rep(NA,nrow(df) - wnd + 1), q_res = rep(NA,nrow(df) - wnd + 1))
  
  #Rolling window
  while((t+wnd-1)<=nrow(df)){
    
    #Equations
    p_eq <- lm(p~.,data = select(df,-q)[t:(t+wnd-1),])
    q_eq <- lm(q~.,data = select(df,-p)[t:(t+wnd-1),])
    
    #Store residuals
    res$p_res[t] <- p_eq$residuals %>% tail(.,1)
    res$q_res[t] <- q_eq$residuals %>% tail(.,1)
    
    #Is rediausl 'close' to zero? Adjust quantiles to change tolerance
    res$p_res_sig[t] <- ifelse(
      res$p_res[t] >= quantile(p_eq$residuals, probs = seq(0, 1, .025))[24] | 
        res$p_res[t] <= quantile(p_eq$residuals, probs = seq(0, 1, .025))[18],1,0)
    
    res$q_res_sig[t] <- ifelse(
      res$q_res[t] >= quantile(q_eq$residuals, probs = seq(0, 1, .025))[24] | 
        res$q_res[t] <= quantile(q_eq$residuals, probs = seq(0, 1, .025))[18],1,0)
    
    #Marking residual as a, d or s
    res$marker[t] <- ifelse(res$p_res_sig[t] == 0 | res$q_res_sig[t] == 0,
                            'a',
                            ifelse(res$p_res[t] > 0 & res$q_res[t] > 0 | res$p_res[t] < 0 & res$q_res[t] < 0 ,
                                   'd',
                                   's' ))
    # Cause of supply shock (quantity up or down)
    res$sup_cause[t] <- ifelse(res$marker[t]=='s', 
                               ifelse(res$p_res[t] > res$q_res[t], 'q_down', 'q_up')
                               ,NA)
    
    t <- t+1
  }
  return(res)
}

# Weighted inflation
weighted_inflation <- function(v , d = inf_q, e = n_shares){
  tmp <- e[[v]]*d[[v]]
}

# Construct contribution series
make_contribs <- function(v , a, d = contrib_trim, e = markers){
  tmp <- ifelse(e[[v]]==a, d[[v]], 0)
}

#Compares what is in each category for 2 specifications and a set of Dates
compare_results <- function(results, d){
  
  if(length(results)!=2){stop("Only works if results list is length 2")}
  
  d1 <- list()
  d2 <- list()
  
  for(i in 1:length(d)){
    
    a1 <- results[[1]]$comps$Demand[results[[1]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    a2 <- results[[2]]$comps$Demand[results[[2]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    
    d1d <- a1[[1]][!(a1[[1]] %in% a2[[1]])]
    d2d <- a2[[1]][!(a2[[1]] %in% a1[[1]])]
    
    a1 <- results[[1]]$comps$Supply[results[[1]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    a2 <- results[[2]]$comps$Supply[results[[2]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    
    d1s <- a1[[1]][!(a1[[1]] %in% a2[[1]])]
    d2s <- a2[[1]][!(a2[[1]] %in% a1[[1]])]
    
    a1 <- results[[1]]$comps$Ambiguous[results[[1]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    a2 <- results[[2]]$comps$Ambiguous[results[[2]]$comps$Dates==d[i]] %>% str_split(.,'\\.')
    
    d1a <- a1[[1]][!(a1[[1]] %in% a2[[1]])]
    d2a <- a2[[1]][!(a2[[1]] %in% a1[[1]])]
    
    d1[[i]] <- list(d1d,d1s,d1a)
    d2[[i]] <- list(d2d,d2s,d2a)
    
    names(d1)[i] <- d[i]
    names(d2)[i] <- d[i]
    
    names(d1[[i]]) <- c('Demand','Supply','Ambiguous')
    names(d2[[i]]) <- c('Demand','Supply','Ambiguous')
    
  }
  
  return(list(In_1=d1,In_2=d2))
  
}

# How many a, d or s observations for each component
count_freq <- function(n,m){
  
  tmp <-  m[[n]] %>% count
  
  if(nrow(tmp)<3){
    if(!('a' %in% tmp$x)){tmp <- rbind(c('a',0),tmp)}
    if(!('d' %in% tmp$x)){tmp <- rbind(tmp[1,],c('d',0),tmp[2,])}
    if(!('s' %in% tmp$x)){tmp <- rbind(tmp,c('s',0))}
  }
  
  row.names(tmp) <- tmp[,1]
  tmp$freq %<>% as.numeric
  tmp <- tmp[,-1] 
  tmp <- tmp/sum(tmp)*100
  return(tmp)
}

#Some additional chart functions used in the RB
oth <- function(leg_pos,oxscale=1,flip=0){
  
  if(flip==1){pm <- margin(0,30,0,5)*oxscale}else{pm <- margin(0,10,0,5)*oxscale}
  
  theme_cowplot() %+replace%
    
    theme(text = element_text(angle=0,
                              size = 20*oxscale,
                              face="plain",
                              colour= "#495057",
                              family = "Segoe UI"),
          line = element_line(colour="#495057",size=1*oxscale),
          
          axis.title.y.left = element_text(angle = 0,margin=unit(c(0,-1.3,0,0.75)*oxscale, "cm")),
          axis.title.y.right = element_text(angle=0,vjust=1,hjust=0,margin=unit(c(0,0,0,-2.5)*oxscale, "cm")),
          axis.title.x =element_blank(),
          
          axis.ticks = element_line(size=1*oxscale, colour= "#495057"),
          axis.ticks.length=unit(0.15*oxscale, "cm"),
          
          axis.text = element_text(angle = 0,colour="#495057",size=18*oxscale),
          axis.text.x = element_text(margin=unit(c(0.35,0.35,0,0.5)*oxscale, "cm")),
          axis.text.y = element_text(margin=unit(c(0.5,0.35,0.5,0.5)*oxscale, "cm")),
          axis.text.y.right= element_text(margin=unit(c(0.5,0.5,0.5,0.35)*oxscale, "cm")),
          
          axis.line.x = element_line(size=1*oxscale, colour= "#495057"),
          axis.line.y = element_line(size=1*oxscale, colour= "#495057"),
          
          legend.key = element_rect(colour=NA, fill=NA),
          legend.margin = margin(0,0,0,0),
          legend.text = element_text(size = 12*oxscale),
          legend.title = element_blank(),
          legend.background = element_rect(fill=alpha('white', 0)),
          legend.spacing.x = unit(0,'cm'),
          legend.spacing.y = unit(0,'cm'),
          legend.position=leg_pos,
          
          plot.title = element_text(margin=unit(c(0.2,0,0.15,0)*oxscale,"cm"),size=24*oxscale, hjust=0.0, face = 'plain'),
          plot.subtitle=element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0)*oxscale,"cm"),size=18*oxscale),
          plot.caption=element_text(hjust=0.0,size=16*oxscale,margin=unit(c(0,0,0.15,0)*oxscale,"cm")),
          plot.margin = pm,
          
          panel.background = element_rect(fill = "white")
    )
}

odbg <- function(a,ttl,lh_units,x_range,y_range,x_break="1 year",srce="Source: Haver Analytics, BIS Oxford Economics",
                 leg=NULL,leg_pos=c(0.02,0.9),leg_col=1,fc=0,fc_date=NULL,y2_range=NULL,
                 no_leg=0,rh_units=lh_units,nudge_rh_units=0,rhs_var=NULL,FY=0,colours=NULL,stack=1,x_seq=3,
                 x_format="%Y",var_order=NULL,no_forc=0,date_adj=1,edit=0,no_zero=0,thm = 'ox_theme'){
  
  th <- ifelse(thm=='ox_theme_html',oth,ox_theme)
  #Some checks
  if(fc==1 & is.null(fc_date)){stop("If you're going to have a forecast line, you need to specify the forecast date")}
  if(is.null(y2_range)){second_axis <- 0}else{second_axis <- 1}
  if(second_axis==1 & is.null(rhs_var)){stop("If you're going to have a second axis, you need to specify at least one variable as the rhs_var")}
  
  bar_pos=position_stack()
  if(stack==0){bar_pos <- position_dodge()}
  
  #Define the colour pallette
  ox_colours <- ox_pallette()
  if(!is.null(colours)){ox_colours <- c(ox_colours[colours],ox_colours[-colours])}
  
  
  if(second_axis==1){
    # Renaming and rescaling the variables that are on the RHS
    # Work out the linear transformation from primary to secondary axis
    
    y1n <- y_range[1] #first axis min
    y2n <- y2_range[1] #second axis min
    y1x <- y_range[2] #first axis max
    y2x <- y2_range[2] #second axis max
    
    a2 <- (y1x*y2n-y2x*y1n)/(y2n-y2x)
    if(y2n==0){a1 <- (y1x-a2)/y2x}else{a1 <- (y1n-a2)/y2n}
    
    trans <- ~ (. - a2) / a1
    
    for (j in rhs_var){
      a$value[a$variable==j] <- a$value[a$variable==j]*a1+a2
      levels(a$variable)[levels(a$variable)==j] <- paste0(j," (RHS)")
    }
  }
  #Defining the x axis range
  if(is.numeric(x_range)){
    x_range <- x_rng(x_range,a,FY,bar=1)} else {
      x_range <- as.Date(x_range,"%d/%m/%Y")
    }
  
  #If no forecast line is selected, this checks whether data from the Oxford model are being plotted, and automates a forecast line
  if(fc==0 & exists('h_end') & no_forc==0){
    dts <-h_end[which(h_end$variable %in% unique(a$variable)),]
    if(!is_empty(dts$hist_end)){
      md <- min(dts$hist_end)
      aa <- drop_na(select(a,Dates,variable,value))
      
      if(!is.na(md) & md < x_range[2] & md < max(aa$Dates)){
        fc=2
        fc_date <- md
        mg <- month(a$Dates[2])-month(a$Dates[1])
        
        if(mg==3|-9){fc_date <- fc_date%m-%months(2)}
        if(mg==0){
          ld <- a$Dates[a$Dates<=md] %>% max()
          fc_date <- ld%m+%months(1)}
      }}}
  
  #The sequence for ticks and labels on the x-axis
  b_seq <- seq.Date(x_range[1],x_range[2],x_break)
  if(FY==0){l_seq<-as.character(b_seq,x_format)}else{l_seq<-as.character(b_seq%m+%years(1),x_format)}
  l_seq[c(FALSE,rep(TRUE,x_seq-1))] <- ""
  
  #Changing dates so that bars sit between tick marks
  if(date_adj==1){
    b <- spread(a,variable,value)   #Convert to wide format
    
    eop <- b$Dates
    day(eop) <- days_in_month(eop)  #End of period dates
    dv <- round(as.numeric(diff(eop))/2)  %>%
      c(round(mean(.)),.)   #Vector of difference between dates. Take the mean of the vector for the first element
    b$Dates <- eop-dv   #Adjust the dates column by subtracting the difference
    
    a <- melt(b, "Dates") %>% droplevels(.)}
  
  
  if(!is.null(var_order)){a$variable <- factor(a$variable,levels=var_order)
  if(length(var_order)!=length(unique(droplevels(a$variable)))){stop("Your variable order doesn't equal the number of variables you want to plot")}
  }
  
  #Building the plot
  
  h <- ggplot(a)+
    
    geom_col(aes(Dates,value,fill=variable),size=1.05833,position=bar_pos)+
    
    th(leg_pos)+
    
    scale_x_date(breaks=b_seq,labels=l_seq,limits=x_range[1:2],expand=c(0,0))+
    
    labs(y="",caption=srce,title=ttl,subtitle=lh_units)
  
  if(is.null(leg)){
    h <- h+scale_fill_manual(values=ox_colours)
  }else{
    h <- h+scale_fill_manual(values=ox_colours,labels=leg)}
  
  if(second_axis==1){
    h <- h+ scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0),
                               sec.axis = sec_axis(trans=trans,breaks=seq(y2_range[1],y2_range[2],y2_range[3])))+
      annotate("text",label=rh_units,y=y_range[2],x=x_range[2],hjust=0.5+nudge_rh_units,vjust=-1,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))}
  else{h <- h+ scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0),labels=comma)}
  
  if(fc==1){
    h <- h+geom_vline(xintercept = as.numeric(as.Date(fc_date,"%d/%m/%Y")),size=1,
                      color = ifelse(thm=='ox_theme_html',"#495057", "black")) +
      annotate("text",label="Forecast",y_range[2],x=as.Date(fc_date,"%d/%m/%Y"),hjust=-0.05,vjust=1,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))
    
  }else
    if(fc==2){h <- h+geom_vline(xintercept = as.numeric(as.Date(fc_date,"%d/%m/%Y")),size=1,
                                color = ifelse(thm=='ox_theme_html',"#495057", "black")) +
      annotate("label",label="Forecast",label.size=0,y_range[2],x=as.Date(fc_date,"%d/%m/%Y"),hjust=0.5,vjust=0.5,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))
    }
  
  if(y_range[1]<0 & y_range[2]>0 & no_zero==0){
    h <- h+geom_hline(yintercept = 0,size=1,color = ifelse(thm=='ox_theme_html',"#495057", "black"))}
  
  if(leg_col!=1){h <- h+guides(fill=guide_legend(ncol=leg_col))}
  
  if(FY==1){
    h <- h+annotate("text",label="FY",y=y_range[1],x=x_range[2],hjust=1,vjust=3.2,
                    family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
                    size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465),
                    color = ifelse(thm=='ox_theme_html',"#495057", "black"))}
  
  if(no_leg==1){h <- h+theme(legend.position="none")}
  if(edit==0){h <- titles_left(h)}
  return(h)
}

# Tally the cause of supply shocks
count_cause <- function(n,sc){
  tmp <- sc[[n]] %>% count %>% drop_na()
  z <- tmp$freq[tmp$x=='q_down']/(tmp$freq[tmp$x=='q_down']+tmp$freq[tmp$x=='q_up'])*100
  return(z)
}

# Count number of periods spent in each category
marker_count <- function(n,m){
  
  for(r in 2:length(m[[n]])){
    if(r==2){tmp <- ifelse(m[[n]][r]==m[[n]][(r-1)],1,0)
    }else{
      tmp <- ifelse(m[[n]][r]==m[[n]][(r-1)],tmp+1,tmp)
    }
  }
  return(tmp)
}

#---------------------------------------------
# Data import and tidy

# Brings in data on real and nominal consumption from the ABS website
raw <- data_import('ABS', abs_cat_no = '5206.0', abs_table = '8')

raw_filt <- filter(raw, series_type == 'Original', unit == '$ Millions') %>% select(., Dates, variable, value) %>% l2w

#Remove aggregated series
agg <- c('Alcoholic beverages and tobacco',
         'Housing, water, electricity, gas and other fuels',
         'Furnishings and household equipment',
         'Health',
         'Transport',
         'Recreation and culture',
         'Recreational and cultural services',
         'Hotels, cafes and restaurants',
         'Miscellaneous goods and services')

# Real
q <- select(raw_filt, Dates, contains(": Chain volume measures ;"))

# Nominal
n <- select(raw_filt, Dates, contains(": Current prices ;"))

names(q) <- gsub(": Chain volume measures ;",'', names(q))
names(n) <- gsub(": Current prices ;",'',names(n))

q <- select(q,-all_of(agg))
n <- select(n,-all_of(agg), -names(n)[!(names(n) %in% names(q))])

# Prices
p <- cbind(n['Dates'],
           lapply(names(n)[!(names(n)=='Dates')],create_p_ind,q,n) %>% as.data.frame())

names(p) <- names(n)

# Seasonally adjust
seas <- lapply(list(n,p,q),create_seas_df) %>% lapply(.,drop_na)

trim <- lapply(seas,nrow) %>% as.data.frame() %>% min

seas2 <- lapply(seas, tail, trim)

names(seas2) <- c('n_sa','p_sa','q_sa')
list2env(seas2,envir=.GlobalEnv)

# Log
lseas <- lapply(seas2, function(x) cbind(x['Dates'], transmute(x %>% select(.,-Dates), across(everything(), log))))

names(lseas) <- c('ln','lp','lq')

list2env(lseas,envir=.GlobalEnv)

# Shares of consumption
n_shares <- cbind(n_sa['Dates'], 
                  transmute(n_sa %>% select(,-Dates), across(everything(), ~ .x / n_sa$`FINAL CONSUMPTION EXPENDITURE`)))

n_shares$tot <- n_shares %>% select(.,-Dates, -`FINAL CONSUMPTION EXPENDITURE`) %>%  rowSums()

n_shares <- cbind(n_sa['Dates'], 
                  transmute(n_shares %>% select(,-Dates), across(everything(), ~ .x / n_shares$tot)))

# Inflation rates
inf_q <- p_sa %>% w2l %>% growth(.,p=1) %>% l2w

# Weighted inflation contribution
contribs <- cbind(n_shares['Dates'], 
                  lapply(names(inf_q)[!(names(inf_q)=='Dates')],weighted_inflation) %>% as.data.frame()) 
names(contribs) <- names(inf_q)

#---------------------------------------------
# Estimation

#For each model specification tested, results will be written as an element of the results list
k <- 1

results <- list()

for(w in 1:length(lag)){
  for(y in 1: length(window)){
    
    residuals <- lapply(names(lp)[!(names(lp)=='Dates')], roll_regress, lag[w], window[y])
    
    # Weight together price indices
    markers <- lapply(residuals, pluck, 'marker') %>% as.data.frame()
    names(markers) <- names(lp)[!(names(lp)=='Dates')]
    
    markers <- cbind(lp['Dates'] %>% tail(.,nrow(markers)), markers)
    
    contrib_trim <- contribs %>% tail(.,nrow(markers))
    
    demand <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE'))], 
                     make_contribs, 'd') %>% as.data.frame()
    supply <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE'))], 
                     make_contribs, 's') %>% as.data.frame()
    amb <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE'))], 
                  make_contribs, 'a') %>% as.data.frame()
    
    # Additional diagnostics on cause of supply shocks
    # sup_cause <- lapply(residuals, pluck, 'sup_cause') %>% as.data.frame()
    # names(sup_cause) <- names(lp)[!(names(lp)=='Dates')]
    
    # sup_cause_df <- lapply(names(sup_cause), count_cause, sup_cause) %>% as.data.frame %>% t %>% as.data.frame()
    # row.names(sup_cause_df) <- names(sup_cause)
    
    indices <- data.frame(Dates = markers['Dates'],
                          Demand = rowSums(demand),
                          Supply = rowSums(supply),
                          Ambiguous = rowSums(amb))
    
    #Storing charts
    h_long <- ox_dated_bar_graph(indices %>% w2l ,
                                 'Consumption deflator', 'ppt contribution to q/q % change',
                                 c(2000, 2022), c(-0.5,1.5,0.5), thm = 'ox_theme_html')
    
    
    h_short <- ox_dated_bar_graph(indices %>% w2l ,
                                  'Consumption deflator', 'ppt contribution to q/q % change',
                                  c(2016, 2022), c(-0.5,1.5,0.5), thm = 'ox_theme_html',
                                  x_seq = 3, x_break = '6 months', x_format = '%b %Y')
    
    h_yy <- ox_dated_bar_graph(indices %>% w2l %>% trail_sum(.,p=4),
                               'Consumption deflator', 'ppt contribution to y/y % change',
                               c(2016, 2022), c(-1,4,1), thm = 'ox_theme_html',
                               x_seq = 3, x_break = '6 months', x_format = '%b %Y')
    
    # Which components fall into each category
    comps <- data.frame(Dates = markers['Dates'],
                        apply(markers, 1, function(x) names(which(x =='d')) %>% paste0(., collapse = ".")) %>% as.data.frame(),
                        apply(markers, 1, function(x) names(which(x =='s')) %>% paste0(., collapse = ".")) %>% as.data.frame(),
                        apply(markers, 1, function(x) names(which(x =='a')) %>% paste0(., collapse = ".")) %>% as.data.frame())
    names(comps) <- c('Dates', 'Demand', 'Supply', 'Ambiguous')
    
    
    results[[k]] <- list(lag = lag[w], window = window[y], markers = markers, 
                         chart_short = h_short, chart_long = h_long, indices = indices,
                         chart_yy = h_yy, comps = comps)
    
    k <- k+1
    
  }
}

#---------------------------------------------
# Analysis

# Which components change categories between model specifications
# results list must have two elements

# cr <- compare_results(results, '2020-03-01')

# html output of model results

render('Inflation_results.R')
shell.exec('Inflation_results.html')

# Aggregate smaller category contributors for charts
mats <- c('demand','supply','amb')

for(x in mats){
  
  tmp <- get(x)
  names(tmp) <- names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE'))]
  tmp <- tmp[,apply(tmp, 2, function(x) !(all(x==0)))]
  
  assign(x,cbind(markers['Dates'],tmp))
  
  tmp_trim <- cbind(markers['Dates'],
                    select(tmp, all_of(tmp %>% tail(.,10) %>% abs %>% colSums() %>% desc %>% rank %>% .[. %in% 1:7] %>% names)),
                    rowSums(tmp[,-1])) %>% 
    mutate(., Other = .[,ncol(.)] - rowSums(.[,2:(ncol(.)-1)])) %>% select(.,-(ncol(.)-1))
  
  assign(paste0(x,'_trim'),tmp_trim)
}

# Final results table in RB
fin_tab <- lapply(names(markers)[!(names(markers)=='Dates')],count_freq,markers %>% filter(.,year(Dates)>=2019)) 
names(fin_tab) <- names(markers)[!(names(markers)=='Dates')]

ft <- fin_tab %>% as.data.frame() 
row.names(ft) <- c('a','d','s')

n_sh <- n_shares %>% filter(year(Dates)==2019) %>% select(.,-Dates) %>% colMeans()
names(n_sh) <- names(n_shares)[-1] %>% gsub(" ",".",.) %>% gsub(",",".",.)

ft[4,] <- NA

for(i in names(ft)){
  ft[[i]][4] <- n_sh[[i]]*100
}

mc <- lapply(names(markers)[-1],marker_count,markers)
names(mc) <- names(markers)[-1]
mc <- mc %>% as.data.frame()

ft[5,] <- NA

for(i in names(ft)){
  ft[[i]][5] <- mc[[i]]
}

ft <- ft %>% t %>% as.data.frame()

write.csv(ft,results_write_file)

#Exclusion measures

demand_exc <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE',
                                                            'Actual and imputed rent for housing',
                                                            'Operation of vehicles'))], 
                     make_contribs, 'd') %>% as.data.frame()
supply_exc <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE',
                                                            'Actual and imputed rent for housing',
                                                            'Operation of vehicles'))], 
                     make_contribs, 's') %>% as.data.frame()
amb_exc <- lapply(names(markers)[!(names(markers) %in% c('Dates','FINAL CONSUMPTION EXPENDITURE',
                                                         'Actual and imputed rent for housing',
                                                         'Operation of vehicles'))], 
                  make_contribs, 'a') %>% as.data.frame()

indices_exc <- data.frame(Dates = markers['Dates'],
                          Demand_exc = rowSums(demand_exc),
                          Supply_exc = rowSums(supply_exc),
                          Ambiguous_exc = rowSums(amb_exc))

#---------------------------------------------
# Charts

odbg(demand_trim %>% w2l ,
     'Consumption deflator - demand components', 'ppt contribution to q/q % change',
     c(2016, 2022), c(-0.8,0.8,0.2), thm = 'ox_theme_html',leg_col = 2,
     x_seq = 3, x_break = '6 months', x_format = '%b %Y')
ox_save('dem')

odbg(supply_trim %>% w2l ,
     'Consumption deflator - supply components', 'ppt contribution to q/q % change',
     c(2016, 2022), c(-0.2,0.6,0.2), thm = 'ox_theme_html', leg_col=2,
     x_seq = 3, x_break = '6 months', x_format = '%b %Y')
ox_save('sup')

odbg(amb_trim %>% w2l ,
     'Consumption deflator - ambiguous components', 'ppt contribution to q/q % change',
     c(2016, 2022), c(-0.2,0.4,0.2), thm = 'ox_theme_html',
     x_seq = 3, x_break = '6 months', x_format = '%b %Y')

results[[1]]$chart_short
ox_save('qq')
results[[1]]$chart_yy
ox_save('yy')

ox_dated_bar_graph(indices_exc %>% w2l %>% trail_sum(.,p=4),
                   'Consumption deflator - excl. housing & vehicle operation', 'ppt contribution to y/y % change',
                   c(2019, 2022), c(-0.2,1,0.2), thm = 'ox_theme_html',,stack=0,
                   x_seq = 3, x_break = '6 months', x_format = '%b %Y')
ox_save('trim')

