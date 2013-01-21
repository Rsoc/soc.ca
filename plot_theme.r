theme_min = function (size=10, font="sans", face='plain', 
	backgroundColor='white', panelColor='white', 
    axisColor='black', gridColor='grey70', textColor='black') 
{
    #opts(
    theme(
        panel.border = element_rect(colour=gridColor, linetype="solid", fill=NA),
        axis.text.x = element_text(vjust=1, hjust=0.5, colour=axisColor, family=font, face=face, size=8),
        axis.text.y = element_text(hjust=1, vjust=0.5, colour=axisColor, family=font, face=face, size=8),
	#axis.text.x = theme_text(vjust=1, hjust=0.5, colour=axisColor, family=font, face=face, size=size),
        #axis.text.y = theme_text(hjust=1, vjust=0.5, colour=axisColor, family=font, face=face, size=size),
        axis.title.x = element_text(family=font, face=face, colour=axisColor, size=size),
        axis.title.y = element_text(angle=90, family=font, face=face, colour=axisColor, size=size),
        axis.line = element_blank(),
        #axis.ticks = theme_segment(colour=axisColor, size=0.25),
	      axis.ticks = element_blank(),
        #panel.border = theme_rect(colour=axisColor, linetype="dashed"),
	      #panel.border = theme_rect(colour=axisColor, linetype="dashed"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        legend.title = element_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        panel.background = element_rect(fill=panelColor, colour=NA),
        plot.background = element_rect(fill=backgroundColor, colour=NA),
        panel.grid.major = element_line(colour=gridColor, size=0.33, linetype="dotted"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill=NA, colour=NA),
        strip.text.x = element_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        strip.text.y = element_text(angle=-90, family=font, face=face, colour=textColor, size=size),
        plot.title = element_text(hjust=0, vjust=1, family=font, face=face, colour=textColor, size=12),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), 'lines'))
  
}

## Theme til singular akse plot


### Theme til bar plots

theme_min_bar = function (size=10, font="sans", face='plain', 
	backgroundColor='white', panelColor='white', 
    axisColor='black', gridColor='#999999', textColor='black') 
{
    opts(
        axis.text.x = theme_text(vjust=1, hjust=0.5, colour=axisColor, family=font, face=face, size=size),
        axis.text.y = theme_text(hjust=1, vjust=0.5, colour=axisColor, family=font, face=face, size=size),
        axis.title.x = theme_text(family=font, face=face, colour=textColor, size=size),
        axis.title.y = theme_text(angle=90, family=font, face=face, colour=textColor, size=size),
        axis.line = theme_blank(),
        axis.ticks = theme_segment(colour=axisColor, size=0.25),
        panel.border = theme_rect(colour=axisColor, linetype="solid"),
        legend.background = theme_blank(),
        legend.key = theme_blank(),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        legend.title = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        panel.background = theme_rect(fill=panelColor, colour=NA),
        plot.background = theme_rect(fill=backgroundColor, colour=NA),
        panel.grid.major = theme_line(colour=gridColor, size=0.33, linetype="dotted"),
        panel.grid.minor = theme_blank(),
        strip.background = theme_rect(fill=NA, colour=NA),
        strip.text.x = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        strip.text.y = theme_text(angle=-90, family=font, face=face, colour=textColor, size=size),
        plot.title = theme_text(hjust=0.5 , vjust=1, family=font, face=face, colour=textColor, size=13),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}

