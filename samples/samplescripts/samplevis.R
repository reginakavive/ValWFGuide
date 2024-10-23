library(dplyr)
library(tools)
library(ggplot2)
library(plotly)
# Theme
them2 <- theme(panel.background = element_rect(fill = "white"), # bg of the panel
               plot.background = element_rect(fill = "white", color = NA), # bg of the plot
               panel.grid.major = element_blank(),
               plot.title =element_text(color = "#CD7F32",size=13),
               strip.text.x = element_text(size = 10, color = "black", face = "bold"),
               axis.text=element_text(color = "black",face = "bold",size=10),
               axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
               axis.title=element_text(color = "black",size=12),
               legend.title = element_blank(),
               legend.text = element_text(size = 12),
               legend.background = element_rect(fill = "white"),
               panel.border = element_blank() ,
               axis.line.x = element_blank(),
               axis.line.y = element_blank())
#Pie
dataHm.pp1 <- read.csv("./samples/sampledata/sample_yield_data.csv")
subset_df <- dataHm.pp1[, c("household","T1_yieldha","T2_yieldha","T3_yieldha", "eSSR","incrSSR" )]
subset_df<-distinct(subset_df)
x <- (subset_df[! is.na(subset_df$eSSR),] )$eSSR
xi <- x[x<0]
xj <- x[x>0]
pos <- (length(xj)/length(x))*100
neg <- (length(xi)/length(x))*100
ds <- data.frame(labels = c("Yield Difference <br> (SR yield - BR yield)", "Positive change", "Negative change"),
                 values = c(NA, pos, neg))
plot_ly(data = ds,
        labels = ~labels,
        values = ~values,
        parents = c("", "Yield Difference <br> (SR yield - BR yield)", "Yield Difference <br> (SR yield - BR yield)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry",
        hoverinfo = "text",
        hovertext = paste("% of farmers experiencing<br>", tolower(ds$labels), "from SR")) %>%
  layout(title = 'Effects on grain yield of SR vs BR')

#Bars
dataHNm.nn1 <- read.csv("./samples/sampledata/sample_nue_data.csv")
nue_df <- dataHNm.nn1[, c( "Region", "plot", "useN" )]
nue_df<-distinct(nue_df)
nue_by_state <- nue_df %>%
  group_by(stateEA, plot) %>%
  summarise(avg_useN = mean(useN, na.rm = TRUE))
ggplot(nue_by_state, aes(fill=plot    , y=avg_useN, x=stateEA )) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Location") +
  ylab("Average (Kg grain per kg applied N))") + 
  scale_fill_manual(values = c("CT" = "#004080", "SR" = "#4caf50", "BR" = "#c26e60"))+  # Set colors
  them2
# Detail
dataHm.pp1 <- dataHm.pp1 %>%
  mutate( riceSystem = case_when( riceSystem == "rainfedLowland" ~ "Rainfed lowland", riceSystem == "irrigated" ~ "Irrigated", TRUE ~ riceSystem    ),
          plot = case_when(  plot == "SSR" ~ "SR", plot == "BRR" ~ "BR", TRUE ~ plot  )  )
dataHm.pp1$stateEA<-toTitleCase(dataHm.pp1$stateEA)
dataHm.pp1<-dataHm.pp1[! is.na(dataHm.pp1$ZCC_Yplot),]
ggplot(dataHm.pp1, 
       aes(ZCC_Yha, yield, colour= plot))+
  geom_point(size=1)+
  geom_abline(slope=1, intercept = 0, size = 0.5, colour = "grey")+
  scale_x_continuous(minor_breaks = seq(min(dataHm.pp1$ZCC_Yha, na.rm = TRUE), max(dataHm.pp1$ZCC_Yha, na.rm = TRUE), by = 0.5)) +
  scale_y_continuous(minor_breaks = seq(min(dataHm.pp1$yield, na.rm = TRUE), max(dataHm.pp1$yield, na.rm = TRUE), by = 0.5))+
  facet_wrap(~stateEA)+
  xlab("Grain yield (t/ha) of the control (CT)" ) +
  ylab(" Grain yield (SR and BR) (t/ha)") + 
  labs(title="Yield distribution")+
  them2