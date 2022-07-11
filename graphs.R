#########################################
## visualization of simulation results ##
#########################################

par(bg="transparent")

#-------------------------------------------------------------------------------
# stacked bar plot of spreads and fees
plot_spreads_fees <- function(){
  par(bg="transparent")
  #aggregate spreads and fees
  #take 95% quantile
  fees <- aggregate(step3$costs, by=list(step3$date), 
                    function(x)quantile(x,probs = 0.95))
  
  spreads <- aggregate(step3$spread.10, by=list(step3$date), 
                       function(x)quantile(x,probs = 0.5))
  fees$type <- "trading_fee"
  names(fees) <- c("date", "quantile", "type")
  spreads$type <- "spread"
  names(spreads) <- c("date", "quantile", "type")
  #merge
  sf.df <- rbind(fees, spreads)
  sf.df$type <- factor(sf.df$type)
  sf.df$date <- as.Date(sf.df$date, format="%Y-%m-%d")
  
  #plot
  ggplot(sf.df, aes(fill=type, y=quantile, x=date)) + 
    geom_bar(position="stack", stat="identity")+
    scale_fill_viridis(discrete = T, begin = 0.5) +
    ylab("")+
    #scale_x_date(limits = c(as.Date("2020-07-01"), as.Date("2021-06-01")))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  ggsave("plot_stacked_fees_spreads.png")
}



#-------------------------------------------------------------------------------
# dominance of BTC and ETH
plot_dominance_btc_eth <- function(){
  par(bg="transparent")
  #extract BTC/ ETH weights
  btc.eth <- step3[which(step3$index_members %in% 
                           c("bitcoin", "ethereum")), c("index_members", "weights", "date")]
  btc.eth$index_members <- factor(btc.eth$index_members)
  btc.eth$date <- as.Date(btc.eth$date, format="%Y-%m-%d")
  
  aggr.btc.eth <- aggregate(btc.eth$weights, by=list(btc.eth$date), FUN=sum)
  btc.eth <- merge(btc.eth, aggr.btc.eth, by.x="date", by.y="Group.1")
  
  #plot
  ggplot(btc.eth, aes(fill=index_members, y=weights, x=date)) + 
    geom_bar(position="stack", stat="identity")+
    scale_fill_viridis(discrete = T, begin = 0.5) +
    #scale_x_date(limits = c(as.Date("2020-07-01"), as.Date("2021-06-01")))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  ggsave("dominance_btc_eth.png")
}
#-------------------------------------------------------------------------------
# check: ETF vs. CRIX (set type == "png", otherwise pdf)
plot_crix_etf <- function(){
  
  #inflate CRIX by "rebalancing date deposits"
  crix.df <- data.frame()
  crix.df <- crix.deposits[which(crix.deposits$date %in% as.factor(dates$d)), 
                           c("date", "scaled", "rw.diff")]
  #crix.df$date <- factor(crix.df$date)
  crix.df$diff <- c(1000,diff(crix.df$scaled))
  crix.df$growth.rate <- crix.df$diff / lag(crix.df$scaled)
  
  for (level in levels(as.factor(crix.df$date))) { #inflated: deposits reinvested at each rebalancing date
    if(level == crix.df$date[1]){crix.df$inflated[which(crix.df$date==level)] <- 1000}
    else
      crix.df$inflated[which(crix.df$date==level)] <-  (crix.df$scaled[which(crix.df$date==level)]  / 
                                                          crix.df$scaled[which(crix.df$date==level)-1]) *
        crix.df$inflated[which(crix.df$date==level)-1] +
        crix.df$rw.diff[which(crix.df$date==level)]
    
  }
  
  #test.agg <- aggregate(crix.weights$weights, by=list(crix.weights$date), FUN=sum)
  #crix.df$test.agg <- c(1,test.agg$x[2:3])
  #crix.df$test.crix <- crix.df$inflated * crix.df$test.agg
  
  png(file="plot_crix_etf.png")
  par(bg="transparent")
  
  plot(crix.df$inflated~
         as.Date(crix.df$date,format="%Y-%m-%d"), 
       type="l",  xlab="", ylab="", col="#008B00") #green
  lines(step3$value.crix.pf.aggr.a.r~ as.Date(step3$date, 
                                              format="%Y-%m-%d"), col="#8B0A50", type="l")#pink
  dev.off()
}

#-------------------------------------------------------------------------------
# coins ordered by delta: welche coins werden am meisten gerebalanced?
plot_coin_delta <- function(){
  
  lib <-  "RColorBrewer"
  invisible(lapply(lib, function(x){
    result <- library(x, logical.return=T, character.only =T)
    if(result == F) install.packages(x)
    library(x, character.only =T)
    print(paste0(x, " loaded"))
  }))
 
  ggplot(step3, aes(fill=index_members, y=delta, x=as.Date(date))) + 
    geom_bar(position="stack", stat="identity")+
    geom_hline(yintercept=0)+
    scale_fill_manual(values=rep(brewer.pal(12,"Paired"),times=2))+
    geom_text(aes(label = index_members), 
              position = position_stack(vjust = 0.5), size=2,
              angle =90)+
    theme(legend.position = "none")+
    xlab("") + ylab("")+
    #scale_x_date(date, date_breaks = "6 months")+
    #scale_x_date(limits = c(as.Date(dates$d[1]), as.Date(dates$d[3])))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  ggsave("coin_delta.png", width = 12, height = 8, dpi = 600)
}
#-------------------------------------------------------------------------------

# aggregated costs:  number of constituents, deltas (percentage of portfolio that needs to be rebalanced)
# and fees over time

plot_noc_delta_fees <- function(){
  #aggregate costs by rebalancing date     add spreads
  costs_aggr <- aggregate(step3$costs, by=list(step3$date),mean)
  #number of constituents (noc) over time
  noc <- data.frame(table(crix.weights$date))
  noc2 <- merge(noc, costs_aggr, by.x = "Var1", by.y = "Group.1")
  colnames(noc2) <- c("Datetime", "NOC", "aggr.costs")
  save(noc2, file="noc2.rda")
  
  #aggregate deltas 
  delta.aggr <- aggregate(step3$delta, by=list(step3$date), sum)
  names(delta.aggr) <- c("date", "sum_delta")
  
  #visualize
  png(file="plot_noc_deltas_fees.png")
  
  #set graph parameters
  layout(matrix(1:2, ncol = 1), widths = 1,
         heights = c(1,1),
         respect = FALSE)
  par(mar=c(2, 5, 0.5, 0.5), bg="transparent")
  
  #plot
  plot(table(crix.weights$date), xlab="", ylab="# CRIX constituents")
  plot(delta.aggr$sum_delta~delta.aggr$date, ylab="delta", xlab="")
  #plot(noc2$aggr.costs~noc2$Datetime, type="p", xlab="", ylab="rebalancing costs")
  
  dev.off()
}

#-------------------------------------------------------------------------------
# spreads and investment volume
plot_spreads_investment_volume <- function(){
  
  #aggregate spreads by rebalancing date
  spreads_aggr <- aggregate(step3$spread.2, by=list(step3$date), sum)
  
  #set graphical parameters and draw plot
  png("plot_spreads_investment_volume.png")
  par(mar = c(5, 4, 4, 4) + 0.3,bg="transparent") 
  plot(spreads_aggr$x~spreads_aggr$Group.1, xlab="", ylab="spreads",
       col="#20639B") 
  par(new = TRUE)   # add new plot
  plot(crix.deposits$inflated~crix.deposits$date,
       col = "#3CAEA3",axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, at = pretty(range(crix.deposits$inflated)))  # add second axis
  mtext("portfolio value", side = 4, line = 3)  # add second axis label
  dev.off()
}

# vielleicht noch nen plot spreads vs quantity.a.r
#-------------------------------------------------------------------------------
#boxplot of costs
boxplot_costs_spread <- function(){
  pdf(file="boxplot_costs_spreads.pdf")
  par(bg="transparent", mfrow=c(1,2), mar=c(5, 5, 3, 1)) # 
  #layout(matrix(1:2, ncol = 2), widths = 1, heights = c(1,1),  respect = FALSE)
  boxplot(data.frame(step3$spread.2, step3$spread.5,step3$spread.10), 
          #ylim=range(step3$spread.5, step3$spread.10),
          xlab="spreads", names = c("a=2","a=5", "a=10"), ylim=c(-0.01, 0.04))
  boxplot(step3$costs, xlab= "fees")
  dev.off()
}

#-------------------------------------------------------------------------------
# (delta * value_etf) & spreads
plot_deposits_spreads <- function(){
  plot(step3$deposits ~ step3$date)
  plot((step3$value.crix.pf.aggr.a.r* step3$delta) ~ step3$date)
}
#-------------------------------------------------------------------------------

#function that calls all plots
draw_plots <- function(){
  plot_dominance_btc_eth()
  plot_spreads_fees()
  plot_coin_delta()
  plot_crix_etf()
  boxplot_costs_spread()
  plot_noc_delta_fees()
  #plot_spreads_investment_volume()
  
}
