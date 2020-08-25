library(ggplot2)
library(ggiraph)
library(tidyverse)
library(data.table)
library(esquisse)
library(lubridate)
library(plotly)



esquisse::esquisser()


input_dt <- clean_jmeter_log(report.preprocessing::jdt_dirty)
table(input_dt$all_threads, input_dt$request_name)
scenario_name <- "HELLO_WORLD_SCENARIO_NAME"
input_request_name <- "GET - get_paymants"
interactive <- FALSE
total <- TRUE

get_response_time_violin_plot_vs_threads <-
  function(input_dt, scenario_name=NULL,input_request_name=NULL, group_by_label=TRUE, interactive=FALSE,split_by_responseCode=FALSE){

    if(!is.null(request_name)){
      input_dt <- input_dt[request_name == input_request_name]
    }
    input_dt$all_threads <- as.factor(input_dt$all_threads)
    input_dt$request_name <- as.factor(input_dt$request_name)


    plot_title <- "Response time box plot vs threads"

    gp <- ggplot(input_dt) +
      theme_minimal() +
      labs(x="active threads",
           y="response time",
           title = plot_title,
           subtitle = toupper(scenario_name),
           caption = NULL,
           color="Request name")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            plot.subtitle = element_text(face = "bold",size =15))


    if(group_by_label){
      result_plot <-  gp + geom_violin(aes(x=all_threads, y=response_time, fill=request_name))
    } else {
      result_plot <-  gp + geom_violin(aes(x=all_threads, y=response_time), fill="#0c4c8a")
    }

    if(split_by_responseCode){
      result_plot <- result_plot + facet_wrap(.~response_code)
    }

    if(interactive){
      result_plot <- plotly::ggplotly(result_plot) %>%
        plotly::layout(title = list(text = paste0(plot_title,
                                                  '<br>',
                                                  '<sup><b style="font-size:15px">',
                                                  toupper(scenario_name),
                                                  '</b></sup>')))

    }

    result_plot
  }





####  experiments ----------------------------------------------

get_bar_quantile_plot <- function(input_dt, scenario_name=NULL, interactive=FALSE){


csrt <- input_dt[,.(request_name,response_time)]
csrtshort <- csrt[, .("quantile_50" = as.double(median(response_time)),
                      "quantile_75" = quantile(response_time,0.75),
                      "quantile_90" = quantile(response_time,0.9),
                      "quantile_95" = quantile(response_time,0.95),
                      "quantile_99" = quantile(response_time,0.99)), by=request_name]
csrtshort[,"q50":=quantile_50]
csrtshort[,"q75":=quantile_75-quantile_50]
csrtshort[,"q90":=quantile_90-quantile_75]
csrtshort[,"q95":=quantile_95-quantile_90]
csrtshort[,"q99":=quantile_99-quantile_95]

gather_csrtshort<- gather(csrtshort, key = "quantile",value = "value", q50,q75,q90,q95,q99) %>% as.data.table()
setorder(gather_csrtshort, request_name)

gather_csrtshort <- gather_csrtshort[,.(request_name,quantile_99,quantile, value)]


gather_csrtshort_cum <- plyr::ddply(gather_csrtshort, "request_name",
                              transform,
                              label_ypos=round(cumsum(value),0)) %>% as.data.table()
#maxbp <- 3000


maxbp <- plyr::round_any((max(csrtshort$quantile_99)+100), 100, f = ceiling)
if(maxbp < 1000) {maxbp <- 1500}
# listOfThreads <- paste0(sort("inputThread"), collapse = ", ")
plot_title <- paste("Barplot response time witnin for different quantiles")
gp <- ggplot(data=gather_csrtshort, aes(x=reorder(request_name, quantile_99), y=value, fill=quantile)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values=c("#33FFFF", "#33FF33","#FFFF33","#FF9933","#FF3333")) +
  coord_flip() +
  labs(title = plot_title,
       subtitle = scenario_name,
       x= "Request name",
       y = "response time in ms") +
  scale_y_continuous(breaks = seq(0, maxbp, by = plyr::round_any(maxbp/10,100, f = ceiling))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

result_plot <- gp

  if(interactive){
    result_plot <- plotly::ggplotly(result_plot) %>%
      plotly::layout(title = list(text = paste0(plot_title,
                                                '<br>',
                                                '<sup><b style="font-size:15px">',
                                                toupper(scenario_name),
                                                '</b></sup>')))

  }

  result_plot
}


#####

gp + geom_boxplot(aes(x=all_threads,fill=request_name,group=request_name))

input_dt$all_threads <- rep(c(1,2,1,2,1,1,2,2), 2192/8)
table(input_dt$all_threads, input_dt$request_name)

`ggplot(input_dt, aes(x=as.factor(request_name), y=response_time, fill=as.factor(all_threads))) +
  geom_boxplot()`11

ggplot(input_dt, aes(x=as.factor(all_threads), y=response_time, fill=as.factor(request_name))) +
  geom_boxplot()

ggplot(input_dt[request_name == "auth"], aes(x=as.factor(request_name), y=response_time, fill=as.factor(all_threads))) +
  geom_boxplot()

ggplot(input_dt[request_name == "auth"], aes(x=as.factor(all_threads), y=response_time, fill=as.factor(request_name))) +
  geom_boxplot()



s = sort(seq_len(NROW(input_dt)))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
range01(s)



df <- data.frame(y = rt(200, df = 5))
p <- ggplot(input_dt, aes(sample = response_time, color=label))
p + stat_qq()

colour = factor(cyl)

input_dt <- clean_jmeter_log(report.preprocessing::jdt_dirty)
input_dt$request_name <- as.factor(input_dt$label)
rt_q50 <- quantile(input_dt$response_time, 0.5)
rt_q75 <- quantile(input_dt$response_time, 0.75)
rt_q90 <- quantile(input_dt$response_time, 0.9)
rt_q95 <- quantile(input_dt$response_time, 0.95)
rt_q99 <- quantile(input_dt$response_time, 0.99)

input_dt$quantile_group <- "q100"
input_dt[response_time < rt_q99,]$quantile_group <- "q99"
input_dt[response_time < rt_q95,]$quantile_group <- "q95"
input_dt[response_time < rt_q90,]$quantile_group <- "q90"
input_dt[response_time < rt_q75,]$quantile_group <- "q75"
input_dt[response_time < rt_q50,]$quantile_group <- "q50"

cols <- c("q100" = "black",
          "q99" = "red",
          "q95" = "orange",
          "q90" = "lightgreen",
          "q75" = "green",
          "q50" = "darkgreen")
p + scale_colour_manual(values = cols)




temp <- data.frame(name=letters[1:100], value=rnorm(100), quartile=rep(NA, 100))
temp$quartile <- with(temp, cut(value,
                                breaks=quantile(value, probs=seq(0,1, by=0.25), na.rm=TRUE),
                                include.lowest=TRUE))
temp$quartile <- with(temp, cut(value,
                                breaks=quantile(value, probs=c(0,0.25,0.5,0.75,0.9,0.95,0.99,1), na.rm=TRUE),
                                include.lowest=TRUE))



