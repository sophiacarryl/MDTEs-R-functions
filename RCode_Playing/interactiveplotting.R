library(plotly)
library("dplyr")
library("plyr")
library("tidyverse")
library("sunburstR")


custom_final28 = c(
  "#44AAAA", "blueviolet", "#77AADD", "#117777", "gold1", "darkseagreen4",
  "#77CCCC" ,"#771155", "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA")

datum = rownames_to_column(USArrests)
colnames(datum)[1] = "States"

p = ggplot(datum, aes(x = States, y = sort(Murder))) +
  theme_classic()+
  geom_bar(stat = "identity", aes (fill = States))

ggplotly(p)

j = ggplot(datum, aes (x = Rape, y = Assault, color = States)) +
  theme_classic() +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x)

ggplotly(j)

plot_ly(datum, labels = ~States,values = ~Rape, type = 'pie')  

plot_ly(df, labels = ~Name, hovertext = ~Height, type = 'pie', sort = F)



plot_ly(datum) %>% 
  add_pie(labels = ~Rape, values = ~Rape, #Donut
          type = 'pie', hole = 0.7, sort = F) %>% 
  add_pie(data, labels = ~States, values = ~Rape, #Pie chart
          domain = list(x = c(0.15, 0.85),y = c(0.15, 0.85)),sort = F)

-------
  
plot_ly(df ,labels = ~Name, values = ~Height,
          showlegend = FALSE) %>%
  add_pie(hole = 0.6,
          textinfo = 'label',
          textposition = 'inside',
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(line = list(color = '#FFFFFF', width = 1)),
          direction = 'clockwise',
          sort = FALSE) %>%
  add_pie(freefilter(),labels = ~Location, values = ~Height,
          textinfo = 'label',
          textposition = 'inside',
          direction = 'clockwise',
          sort = FALSE)


p =plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~cyl)
add_trace(p, type = "scatter", mode = "markers+lines")

-------
  
data <- data.frame(c('cat', 'dog', 'deer','chicken', 'cat', 'dog',
                     'duck', 'monkey','fish','cow','horse','dog'),
                   c('US', 'US', 'US','US', 'UK', 'UK','UK', 'UK',
                      'China','China','China','China'),
                   c(15,70,120,55,47,300,89,62,40,27,103,8))
colnames(data) <- c('animal', 'country', 'total_num')

plot_ly(data) %>% 
  add_pie(labels = ~animal, values = ~total_num, 
          type = 'pie', hole = 0.7, sort = T) %>%
  add_pie(data, labels = ~country, values = ~total_num, 
          domain = list(x = c(0.15, 0.85),y = c(0.15, 0.85)),sort = T)

USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]
----
  
  
library(sunburstR)

leafs <- c("base","base-child1-grandchild1","base-child1-grandchild2","base-child1","base-child2-grandchild3","base-child2","base-child3-grandchild4","base-child3-grandchild5","base-child3-grandchild6","base-child3-grandchild7","base-child3")
values <- c(200,15,10,20,55,10,120,30,30,20,10)  

# colors
colors <- c("#c994c7","#74c476","#c7e9c0","#fcbba1","#fc9272","#ef3b2c","#cb181d","#99000d","#6a51a3","#807dba","#bcbddc")
# match those colors to leaf names, matched by index
labels <- c("base","grandchild1","grandchild2","child1","child2","grandchild3","grandchild4","grandchild5","grandchild6","grandchild7","child3")

df = data.frame(v1=leafs, v2=values);

sunburst(df,style="display: flex; align-items:center;",colors = list(range = colors, domain = labels))



