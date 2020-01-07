
#Graph for negation weighting 
t=seq(from = (-0.5), to=(2), by=0.1)
y=-sin(t)
sine <- cbind(t, y) %>% 
  tibble()
sine_curve <- ggplot(sine, aes(t, y))+geom_path(size=2)+
  geom_vline(xintercept = 1, color='#440154FF', size=1.5)+
  geom_vline(xintercept = (0.2), color='#21908CFF', size=1.5)+
  geom_vline(xintercept = 0, size=1.5, alpha=.4)+
  geom_hline(yintercept = 0, size=1.5, alpha=.4)+ 
  labs(y="Shift factor: -sin(1/a)", x = "Values for 1/a")+
  annotate('text', x=1.35, y=-0.2, label='Maximum factor \n for a=1', color='#440154FF', family='Times', size=9)+
  annotate('text', x=0.55, y=-0.2, label='Minimum factor \n for a=5', color='#21908CFF',family='Times', size=9)+
  annotate('text', x=1.5, y=0.3, label='Inverted Sine function', family='Times', size=9)+
  scale_x_continuous(breaks=seq(from=-.5, to=2, by=0.5))+
  theme(axis.text = element_text(size = 25),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 25), 
        # legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 25), 
        legend.title = element_text(size = 25))

sine_curve

ggsave(file="/Users/Constanze/Desktop/uni/ba arbeit/Sine_curve.png", plot=sine_curve, width=10, height=8)

viridis(5)

original_values <- seq(from=-1, to=1, by=0.25)
a <- c(1,2,3,4,5)

shift <- function(value, distance){
  shift_value <- value*-sin(1/distance)
}
shifted <- as.data.frame(map(.x=original_values, .f=shift, distance=a))

names(shifted) <- original_values
library(tidyverse)

shifted_tidy <- gather(shifted, key = shifted_value)
shifted_tidy$distance <- rep(a, 9)

shifted_tidy$shifted_value <- as.numeric(shifted_tidy$shifted_value)
shifted_tidy$distance <- factor(shifted_tidy$distance)


shifted <- ggplot(shifted_tidy, aes(x=value, y= shifted_value, col=distance))+
  geom_line(size=1)+
  geom_vline(xintercept = 0, alpha=.4, size=1)+
  geom_hline(yintercept = 0, alpha=.4, size=1)+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom',
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(x='Original sentiment value', 
       y='Shifted sentiment value')+
  scale_color_viridis(name='Distance a', discrete = T)

shifted
ggsave(filename = '/Users/Constanze/Desktop/uni/ba arbeit/shifted.pdf', plot = shifted)         
