# ---- setup, include=FALSE
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)
library(ggplot2)
library(gridExtra)
library(outliers)
library(dplyr)
library(data.table)
library(psych)
library(GGally)
library(corrgram)
library(ggthemes)
library(reshape2)
library(RColorBrewer)
theme_set(theme_minimal())

# ---- winedata
red <- read.csv("Data/wineQualityReds.csv")
white <- read.csv("Data/wineQualityWhites.csv")
red$color <- "Red"
white$color <- "White"
all <- merge(red, white, all=TRUE)
all$X <- NULL
red$color <- NULL
white$color <- NULL
all$quality <- factor(all$quality, ordered=TRUE)
all$sweetness <- cut(all$residual.sugar, breaks = c(0,10,500), labels=c('dry', 'semi-dry'))

# ---- overview, echo=FALSE
goods <- lapply(all[0:11], quantile, probs=c(.01, .99))
for (i in colnames(all[1:11])) {
    all[i][!between(all[i], goods[[i]][1], goods[[i]][2])] <- NA }
describe(all[1:12], omit=TRUE)[c(3, 4, 5, 8:9, 11:12)]

# ---- corrgram
corrgram(all)

# ---- univariate counts fixed acidity
grid.arrange(ggplot(all, aes(fixed.acidity)) + geom_histogram(aes(y=..density..),binwidth = .1, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5), ggplot(all, aes(volatile.acidity)) + geom_histogram(aes(y=..density..),binwidth = .01, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5), nrow=1)  

# ---- univariate counts citric acid
ggplot(all, aes(x=citric.acid)) + geom_histogram(aes(y=..density..), binwidth=.01, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5)

# ---- univariate counts residual sugar
ggplot(all, aes(residual.sugar)) + geom_histogram(aes(y=..density..),binwidth=.5) + geom_density(aes(fill=color), alpha=.5) 

# ---- univariate counts chlorides
ggplot(all, aes(chlorides)) + geom_histogram(aes(y=..density..),binwidth=.005,color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5) 

# ---- univariate counts sulfur dioxide
grid.arrange(ggplot(all, aes(free.sulfur.dioxide)) + geom_histogram(aes(y=..density..),binwidth=2, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5), ggplot(all, aes(total.sulfur.dioxide)) + geom_histogram(aes(y=..density..),binwidth = 5,color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5), nrow=1) 

# ---- univariate counts density
ggplot(all, aes(density)) + geom_histogram(aes(y=..density..), bins=50, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5)

# ---- univariate counts pH
ggplot(all, aes(pH)) + geom_histogram(aes(y=..density..), bins=40, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5) + scale_x_continuous(trans='reverse')

# ---- univariate counts sulphates
ggplot(all, aes(sulphates)) + geom_histogram(aes(y=..density..),binwidth=.02, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5)

# ---- univariate counts alcohol
ggplot(all, aes(alcohol)) + geom_histogram(aes(y=..density..),binwidth = .1, color='black', fill='grey') + geom_density(aes(fill=color), alpha=.5) 

# ---- univariate counts quality
ggplot(all, aes(quality)) + geom_histogram(aes(group=color, fill=color), position = position_dodge(width=.5), stat='density') 

# ---- ggpairs
ggpairs(all)

# ---- acidity
grid.arrange(qplot(data=all, x=pH, y=fixed.acidity) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm), qplot(data=all, x=pH, y=volatile.acidity) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm), qplot(data=all, x=pH, y=citric.acid) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm), nrow=1)

# ---- acidity by color
grid.arrange(qplot(data=all, x=pH, y=fixed.acidity, color=color) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm, size=2, fill='black'), qplot(data=all, x=pH, y=volatile.acidity, color=color) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm, size=2, fill='black'), qplot(data=all, x=pH, y=citric.acid, color=color) + scale_x_continuous(trans='reverse') + geom_smooth(method=lm, size=2, fill='black'), nrow=1)

# ---- density
grid.arrange(qplot(data=all, x=residual.sugar, y=density) + geom_smooth(method=lm, size=2), qplot(data=all, x=chlorides, y=density) + geom_smooth(method=lm, size=2), qplot(data=all, x=sulphates, y=density) + geom_smooth(method=lm, size=2), qplot(data=all, x=alcohol, y=density) + geom_smooth(method=lm, size=2), nrow=2)

# ---- density by color
grid.arrange(qplot(data=all, x=residual.sugar, y=density, color=color) + geom_smooth(method='loess', fill='black'), qplot(data=all, x=chlorides, y=density, color=color) + geom_smooth(method=loess, fill='black'), qplot(data=all, x=sulphates, y=density, color=color) + geom_smooth(method=lm, size=2, fill='black'), qplot(data=all, x=alcohol, y=density, color=color) + geom_smooth(method=lm, size=2, fill='black'), nrow=2)

# ---- sulfur dioxide
qplot(data=all, x=free.sulfur.dioxide, y=total.sulfur.dioxide) + geom_smooth(method=lm, size=2)

# ---- sulfur dioxide by color
qplot(data=all, x=free.sulfur.dioxide, y=total.sulfur.dioxide, color=color) + geom_smooth(method=lm, size=2)

# ---- acidity~quality
grid.arrange(qplot(data=all, x=quality, y=fixed.acidity, fill=color, geom='boxplot'), qplot(data=all, x=quality, y=volatile.acidity, fill=color, geom='boxplot'), qplot(data=all, x=quality, y=citric.acid, fill=color, geom='boxplot'), qplot(data=all, x=quality, y=pH, fill=color, geom='boxplot') + scale_y_continuous(trans='reverse'),nrow=2)

# ---- residual.sugar~quality
qplot(data=all, x=quality, y=residual.sugar, fill=color, geom='boxplot')

# ---- residual.sugar~quality whites
qplot(data=subset(all, color = 'White'), x=quality, y=residual.sugar, fill = sweetness, geom = 'boxplot') + scale_fill_manual(values =c('#EEEEEE', "#BBBBBB"), guide = guide_legend(reverse = TRUE))

# ---- solutes~quality
grid.arrange(qplot(data=all, x=quality, y=chlorides, fill=color, geom='boxplot'), qplot(data=all, x=quality, y=sulphates, fill=color, geom='boxplot'), nrow=1)

# ---- sulfur.dioxide~quality
grid.arrange(qplot(data=all, x=quality, y=free.sulfur.dioxide, fill=color, geom='boxplot'), qplot(data=all, x=quality, y=total.sulfur.dioxide, fill=color, geom='boxplot'), nrow=1)

# ---- alcohol~quality
qplot(data=all, x=quality, y=alcohol, fill=color, geom='boxplot')

# ---- sugar~alcohol
qplot(data=all, x=residual.sugar, y=alcohol) + stat_smooth(method='lm', se=FALSE, size=2, fullrange=TRUE)

# ---- sugar~alcohol by quality
all <- group_by(all, quality)
qplot(data=subset(all, color=='White'), x=residual.sugar, y=alcohol, color=quality) + stat_smooth(method='lm', se=FALSE, size=2, fullrange=TRUE)

# ---- sugar~alcohol by quality faceted
qplot(data=subset(all, color="White"), x=residual.sugar, y=alcohol) + stat_smooth(method='lm', size=2) + facet_wrap(~quality) 

# ---- corrcoef
fitted_models = all %>% group_by(quality) %>% do(model = lm(alcohol ~ residual.sugar, data = .))
for (i in 1:7){
  cat(c("Quality", levels(fitted_models$quality)[i], ":" ,  round(fitted_models$model[[i]][['coefficients']][2], 4), "\n"))
}

# ---- Final1 (Sulphates)
ggplot(all) + theme_few() + theme(legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"), legend.position = c(.9, .7), legend.key.size = unit(5, 'line'), legend.title = element_blank(),  text = element_text(size=20, face="bold"), axis.text = element_text(size = 16), plot.title = element_text(size = 25, hjust=.5)) + labs(title = "Comparing Sulphate Content in Red and White Vinho Verdes", x= "Wine Quality", y = "Sulphate Content  (g / L)") + geom_boxplot(aes(x=quality, y=sulphates, fill=color), alpha=.8) + scale_fill_manual(all$color, values= c('darkred', 'palegreen'))

# ---- sugar~alcohol by quality (below avg. vs above avg.)
all$quality_group = cut(as.integer(all$quality), breaks=c(0, 3, 4, 20), labels = c("below_average", "average", "above_average"))

fitted_models = all %>% group_by(quality_group) %>% do(model = lm(alcohol ~ residual.sugar, data = .))

low_m = fitted_models$model[[1]]['coefficients']
high_m = fitted_models$model[[3]]['coefficients']

# ---- Final2 (Sugar~alcohol)
qplot(data = filter(all, color=='White', quality_group != 'average'), x= residual.sugar, alcohol, color=quality_group) + geom_smooth(method='lm', size=2) + scale_color_manual(all$quality_group, values= c('blue', 'orange')) + theme_few() + theme(legend.position=c(.865, .8), legend.background = element_rect(fill="lightblue", size=0.8, linetype="solid"), legend.key.size = unit(5, 'line'),  text = element_text(size=20, face="bold"), axis.text = element_text(size = 16), plot.title = element_text(size = 25, hjust=.5), plot.subtitle = element_text(hjust=.5)) + labs(title = "Fermentation profile of white Vinho Verdes", subtitle="Comparing lower and higher quality whites.",x= "Residual Sugar (g / L)", y = "Alcohol (% by vol)") + guides(color = guide_legend(title="Wine Quality", reverse = TRUE)) + annotate('text', x=12, y=12.8, label=paste(round(high_m[[1]][[2]], 2), 'x + ', round(high_m[[1]][[1]], 2)), color = 'darkorange3', face='bold', size=8) + annotate('text', x=12, y=12, label=paste(round(low_m[[1]][[2]], 2), 'x + ', round(low_m[[1]][[1]], 2)), color = 'darkblue', face='bold', size=8) + annotate('rect', xmin=10, xmax=14, ymin=12.6, ymax=13, alpha=.2) + annotate('rect', xmin=10, xmax=14, ymin=11.8, ymax=12.2, alpha=.2)

# ---- Final3 (alcohol~quality)
ggplot(all) + geom_boxplot(aes(x=quality, y=alcohol, fill=color, geom='boxplot'), alpha=.8) + theme_few() + theme(legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"), legend.position = c(.9, .25), legend.key.size = unit(5, 'line'), legend.title = element_blank(),  text = element_text(size=20, face="bold"), axis.text = element_text(size = 16), plot.title = element_text(size = 25, hjust=.5)) + labs(title = "Does more alcohol mean a better Vinho Verde?", x= "Wine Quality", y = "Alcohol (% by volume)") + scale_fill_manual(all$color, values= c('darkred', 'palegreen'))


