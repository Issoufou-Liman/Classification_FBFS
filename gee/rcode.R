library(ggplot2)

# x <- read.csv('C:/Users/LIssoufou/OneDrive - CGIAR/Downloads/export_Points.csv')
x <- read.csv('C:/Users/LIssoufou/OneDrive - CGIAR/Downloads/Tigray_unsuper_extracted (2).csv')

x <- x[-length(x)]
id_cols <- stringr::str_split_fixed(x$`system.index`, "_", 2)
colnames(id_cols) <- c('Month', 'Point_id')
x <- data.frame(id_cols[, c(2,1)], x[, -1])
x$Month <- as.integer(x$Month)
head(x)
x <- x[order(x$Month, decreasing = FALSE),]  
x <- transform(x, MonthAbb = month.abb[Month])
x$MonthAbb <- factor(x$MonthAbb, levels = unique(x$MonthAbb))

x$class <- paste("Class", x$class)
x$class <- factor(x$class, unique(x$class))
x  <- reshape2::melt(x, id.vars = c("Point_id", "Month", "MonthAbb", "class"))
head(x)
tail(x)

ggplot(x[(x$variable != 'elev') & (x$variable != 'slope'), ], aes(MonthAbb, value, fill=Point_id, group=variable))+
  # geom_line(aes(color = variable))+
  geom_smooth(aes(color = variable), method = 'loess', se=TRUE, span = 0.3)+
  facet_wrap(.~class)+
  scale_colour_manual(values = c("lightgreen", 'forestgreen', 'blue', 'green'))+
  theme_minimal()+
  theme(
    legend.position = 'bottom',
    axis.title.x = element_blank(),
    legend.title = element_blank()
        )+
  labs(y="spectral index")

ggplot(x[(x$variable == 'elev'), ], aes(class, value, fill=variable))+
  geom_line(aes(color = variable))+
  geom_smooth()+
  # facet_wrap(.~class)+
  # geom_boxplot()
  scale_colour_manual(values = c("orange", 'red'))+
  theme_minimal()+
  theme(
    legend.position = 'bottom',
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )+
  labs(y="slope or elevation")
# 
  
## Roc ####

roc <- read.csv('C:/Users/LIssoufou/OneDrive - CGIAR/Downloads/Tigray_ROC_16classes_LSR_L8_L5_L4_MOD09A1_2015_2020_NDSI (1).csv')
head(roc)
roc$class = as.numeric(sub("\\_.*", "", roc$system.index))
head(roc)
roc <- roc[with(roc, order(class)), ]
head(roc)
roc$fill <- paste0('Class', ' ', roc$class, ' ', '(', round(roc$AUC,2) ,')')
roc$fill <- factor(roc$fill, levels = unique(roc$fill))
ggplot(roc, aes(FPR, TPR, colour=fill))+
  geom_line()
  # geom_smooth(method = 'loess')
