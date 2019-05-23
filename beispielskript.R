#####################################################
#Beispielskript fuer eine einfache Datenaufbereitung#
#####################################################
rm(list=ls())

# load packages -----------------------------------------------------------
library(tidyverse)
library(scales)
library(readxl)


# load data ---------------------------------------------------------------
dat <- read_excel("data_beispielskript.xlsx",
                  sheet=1)


# Codierung der ersten Frage ----------------------------------------------
q1 <- dat %>%
  select(v1_A,v1_B,v1_C,v1_D,v1_E,v1_F,v1_G,v1_H,v1_I) %>% 
  gather(item,value) %>%
  mutate(value = case_when(.$value==1 ~ 9,
                           .$value==2 ~ 8,
                           .$value==3 ~ 7,
                           .$value==4 ~ 6,
                           .$value==5 ~ 5,
                           .$value==6 ~ 4,
                           .$value==7 ~ 3,
                           .$value==8 ~ 2,
                           .$value==9 ~ 1,
                           .$value==9999999 ~ 0)) %>%
  group_by(item) %>%
  dplyr::summarise(summe = sum(value)) %>%
  mutate(item = case_when(.$item == 'v1_A' ~ 'Antwort 1',
                          .$item == 'v1_B' ~ 'Antwort 2',
                          .$item == 'v1_C' ~ 'Antwort 3',
                          .$item == 'v1_D' ~ 'Antwort 4',
                          .$item == 'v1_E' ~ 'Antwort 5',
                          .$item == 'v1_F' ~ 'Antwort 6',
                          .$item == 'v1_G' ~ 'Antwort 7',
                          .$item == 'v1_H' ~ 'Antwort 8',
                          .$item == 'v1_I' ~ 'Antwort 9')) %>%
  arrange(summe) %>%
  mutate(label = paste0(item,': ',summe, ' points'))

ggplot(q1,aes(reorder(item,summe),summe)) +
  geom_bar(stat = 'identity', aes(fill=summe)) +
  geom_text(aes(label = label),y=1,fontface='bold',hjust=0,size=10) +
  labs(y = '\npoints\n', 
       x = '', 
       title = "", 
       subtitle = "", 
       caption = paste0('cs09 / q1\n10 points for best remembered, 1 point for ninth best remembered, 0 point for no answer')) +
  coord_flip() +
  scale_fill_gradient2(low = "#FAD089", mid = "#FF9C5B",
                       high = "#ED303C", midpoint = median(q1$summe)) +
  scale_y_continuous(expand=c(0,0))+
  theme(#panel.grid = element_blank()
    axis.line = element_line(color="black", size = 1),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color="black", size = 25,face = "bold"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'),
    #legend.justification=c(0,1), 
    legend.position='none',
    legend.direction = 'horizontal',
    legend.background = element_rect(fill=alpha("white", 0)),
    legend.title=element_blank(),
    legend.text= element_text(color="black", size=20),
    legend.key.size = unit(2.5,"line"),
    panel.spacing = unit(4, "lines"),
    plot.margin = margin(10, 100, 10, 10),
    strip.text = element_text(color="black", size = 30,face = "bold", hjust=0),
    strip.background = element_rect(colour="white", fill="white"),
    plot.caption= element_text(color="black", size=20))

ggsave('q1.png', plot = last_plot(),width = 22.86, height =12.9, units = c("cm"),dpi=300,scale=3)




# q3 ----------------------------------------------------------------------
q3 <- dat %>%
  select(v3_A,v3_B, v3_C,v3_D,v3_E,v3_F,v3_G,v3_H,v3_I) %>%
  gather(item,value) %>%
  mutate(value = replace(value,value==9999999,-99)) %>%
  group_by(item,value)%>%
  dplyr::summarise(anzahl = n()) %>%
  mutate(share = anzahl/sum(anzahl)) %>%
  ungroup()%>%
  complete(item,value,fill=list(anzahl=0, share=0)) %>%
  mutate(item = case_when(.$item == 'v3_A' ~ 'Antwort 1',
                          .$item == 'v3_B' ~ 'Antwort 2',
                          .$item == 'v3_C' ~ 'Antwort 3',
                          .$item == 'v3_D' ~ 'Antwort 4',
                          .$item == 'v3_E' ~ 'Antwort 5',
                          .$item == 'v3_F' ~ 'Antwort 6',
                          .$item == 'v3_G' ~ 'Antwort 7',
                          .$item == 'v3_H' ~ 'Antwort 8',
                          .$item == 'v3_I' ~ 'Other')) %>%
  mutate(value = case_when(.$value == 1 ~ 'Much expected',
                          .$value == 2 ~ 'Expected',
                          .$value == 3 ~ 'Not expected',
                          .$value == 4 ~ 'Not at all expected',
                          .$value == -99 ~ 'No answer')) %>%
  mutate(value = factor(value,levels=c( 'Much expected','Expected','Not expected','Not at all expected','No answer'),
                        labels=c( 'Much\nexpected','Expected','Not\nexpected','Not at\nall expected','No\nanswer')))%>%
  mutate(share = replace(share,value == 'No\nanswer',NA))

q3.HELP <- dat %>%
    select(v3_A,v3_B, v3_C,v3_D,v3_E,v3_F,v3_G,v3_H,v3_I) %>%
    gather(item,value) %>%
    mutate(value = replace(value,value==9999999,-99)) %>%
    mutate(value = case_when(.$value==1 ~ 4,
                           .$value==2 ~ 3,
                           .$value==3 ~ 2,
                           .$value==4 ~ 1,
                           .$value==-99 ~ 0)) %>%
    group_by(item) %>%
  dplyr::summarise(anzahl = sum(value)) %>%
  mutate(item = case_when(.$item == 'v3_A' ~ 'Antwort 1',
                          .$item == 'v3_B' ~ 'Antwort 2',
                          .$item == 'v3_C' ~ 'Antwort 3',
                          .$item == 'v3_D' ~ 'Antwort 4',
                          .$item == 'v3_E' ~ 'Antwort 5',
                          .$item == 'v3_F' ~ 'Antwort 6',
                          .$item == 'v3_G' ~ 'Antwort 7',
                          .$item == 'v3_H' ~ 'Antwort 8',
                          .$item == 'v3_I' ~ 'Other')) %>%
  arrange(-anzahl) %>%
  mutate(item = factor(item,levels = rev(c(unique(.$item)[.$item!='Other'],'Other'))))
  
q3 <- q3 %>% mutate(item = factor(item,levels =  rev(levels(q3.HELP$item))))
    

ggplot(q3,aes(value,anzahl)) + geom_bar(stat = 'identity', aes(fill=share)) +
  geom_text(aes(label = anzahl),y=1,fontface='bold',hjust=0.5,size=7) +
  
  geom_text(data=q3.HELP,aes(label = paste0('Total interest points: ',anzahl)),x= 5,y=20, hjust=1, size = 10) +
  
  scale_fill_gradient2(low = "#FAD089", mid = "#FF9C5B",
                       high = "#ED303C", midpoint = median(q3$share,na.rm=T), na.value = "lightgrey") +
  facet_wrap(~item,ncol=3) +
  
  labs(y = 'No. of answers\n', 
       x = ' ', 
       title = "", 
       subtitle = "", 
       caption = paste0('q3 / cs09\nMuch expected 4 points, Not at all expected 1 point, No answer 0 point'))+
  theme(#panel.grid = element_blank()
    axis.line = element_line(color="black", size = 1),
    axis.text.y = element_text(color="black", size = 15,face = "bold"),
    axis.title.y = element_text(color="black", size = 20,face = "bold"),
    axis.text.x = element_text(color="black", size = 15,face = "bold"),
    axis.title = element_text(color="black", size = 10,face = "bold"),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'),
    legend.position='none',
    panel.spacing = unit(4, "lines"),
    plot.margin = margin(10, 100, 10, 10),
    strip.text = element_text(color="black", size = 15,face = "bold",hjust = 0),
    strip.background = element_rect(colour="white", fill="white"),
    plot.caption= element_text(color="black", size=20))

ggsave('q3.png', plot = last_plot(),width = 22.86, height =12.9, units = c("cm"),dpi=300,scale=3)

  
  
  
  

