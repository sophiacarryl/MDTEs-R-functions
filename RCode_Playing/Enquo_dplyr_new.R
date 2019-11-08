survival.plot.by <- function(df, by_column_name, days.cutoff=2000) {
  by_column <- enquo(by_column_name)
  cts <- df %>%
    filter(!is.na(Days_To_Death) & ICDOSite == "PROSTATE GLAND" & !is.na(!!by_column)) %>%
    count(!!by_column)
  p <- df %>%
    filter(!is.na(Days_To_Death) & ICDOSite == "PROSTATE`GLAND" & !is.na(!!by_column)) %>%
    count(Days_To_Death, !!by_column) %>%
    left_join(cts, by=c(paste(rlang::quo_get_expr(by_column)))) %>%
    group_by(!!by_column) %>%
    mutate(survive=(n.y-cumsum(n.x))/n.y) %>%
    ungroup() %>%
    filter(Days_To_Death <= days.cutoff) %>%
    ggplot(aes(x=Days_To_Death, y=survive, color=!!by_column)) +
    geom_line() +
    geom_point(shape="+", size=3) +
    theme_bw()
  return(p)
}

p <- survival.plot.by(joined.df, SeerSummaryStage2000)

###
colors <- c("#91a3b0","#AA4488","#117777","#537b5d","#6d9c79",
            "#77AADD",  "gold1","#344c3a", "darkseagreen4","#a1caf1","#98777b",
            "#77CCCC" ,"#771155",  "#EA6CC0", "#CC99BB","#88CCAA",
            "#771122", "#AA4455", "plum4", "slateblue", "violetred","#5F7FC7",
            "#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
            "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711","#E69F00")

####

df <- data.frame(
                 Sex = c("Male", "Female", "Female", "Male", "Male","NA"),
                 Height = c(12,11,7,3,9,NA),
                 Name = c("John", "Dora", "Dora","Dora", "Anna", "John"),
                 Last = c("Henry", "Paul", "House", "Houze", "Henry", "Bill"),
                 Location = c("Chicago", "Chicago", "Portland", "NYC", "NYC", "NYC"),
                 stringsAsFactors = TRUE)
str(df)
df

pieplotrr(df,
          Name)

mySum <- function(df, grouppie){


  colors <- c("#91a3b0","#344c3a","#a1caf1","#98777b","#537b5d",
              "#77AADD", "#117777", "gold1", "darkseagreen4","#6d9c79",
              "#77CCCC" ,"#771155", "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA",
              "#771122", "#AA4455", "plum4", "slateblue", "violetred","#5F7FC7",
              "#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711","#E69F00")

  grouppie_var <- enquo(grouppie)

  p <-  df %>%
    group_by(!!grouppie_var) %>%
    dplyr::summarise(Count = n())

  hi <-  df %>%
    group_by(!!grouppie_var) %>%
    dplyr::summarise(count=n())%>%
    ggplot2::ggplot(aes(x =reorder(!!grouppie_var,-count), y = count)) +
    geom_bar(stat = "identity", aes(fill = !!grouppie_var)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_light() +
    scale_fill_manual(values = colors)

  print(p)

  return(hi)
}

#
mySum(df, Name)

#
AnalyteData  <- read.csv("../MasterDataSet/master_analyte.csv", sep = ",")
AliquotData <- read.csv("../MasterDataSet/master_aliquot.csv", sep = ",")
BiospecimenData <- read.csv("../MasterDataSet/master_biospecimen.csv", sep = ",")
SampleData <- read.csv("../MasterDataSet/master_sample.csv", sep = ",")
QuantData <- read.csv("../MasterDataSet/master_quantification_assay.csv", sep = ",")

#
mySum(AnalyteData, cell_type)



library("ggplot2")
library("tidyr")
library("plotly")
library("rlang")
#


barr <- function (node, Property){
colors <- c("#91a3b0","#537b5d","#6d9c79","#344c3a","#a1caf1","#98777b","#a2a2d0","#66023c","#00755e",
              "#9966cc","#ffbf00","#cd9575","#e9d66b","#ff9966","#e2725b","#464EB4","#ecd540","#aa98a9",
              "#44AAAA", "#629c92", "#77AADD", "#117777", "#18402c", "#eb9886","#77CCCC" ,"#771155",
              "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA", "#771122", "#AA4455", "#cf3d1d", "#f2e88a",
              "#014411","#b4464e","#973b42","#7b3036","#7AB446","#512023","#cc7c82","#5d8aa8","#f0f8ff",
              "#7d200b","#5F7FC7","#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711")
# TableID <- data.frame(plyr::count(node, "Property_Name"))
# ymax <- as.numeric(max(TableID$freq))
# TableID$freq <- TableID$freq + (TableID$freq * 0.05)
# pcounts <- stats::aggregate(data = node, project_id ~ Property_Name, function(x) length(unique(x)))
# ymax <- as.numeric(max(TableID$freq))
# Property_Response_Variable <- reorder(Property_Name,Property_Name,function(x)-length(x))

Property_Name <- dplyr::enquo(Property)

PropertyName_Table <- node %>% #Creates a dataframe that counts Property_Name that will be used in ggplot2::annotate to specify text location on the y axis. y= PropertyName_Table$Count
  dplyr::group_by(!!Property_Name) %>%
  dplyr::summarise(Count = dplyr::n())
# PropertyName_Table$Count <- PropertyName_Table$Count + (PropertyName_Table$Count * 0.07)


PropertyName_ProjectID_Table <-  node %>% #Creates a dataframe that groups by Property_Name and ProjectID for usage in pcount below.
  dplyr::group_by(!!Property_Name, project_id) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::arrange(Count) %>%
  data.frame()
print(PropertyName_ProjectID_Table)

ProjectID_Count <- PropertyName_ProjectID_Table %>% #Creates a dataframe using the p from above to calculate the number of unique projectIDs within each response variable. Also specify text location on x axis. x=ProjectID_Count$Property_Name,
  dplyr::group_by(!!Property_Name) %>%
  dplyr::summarise(pCount = dplyr::n())
  colnames(ProjectID_Count)[1] <- "Property_Name" #Renames the first column to a generic name for use in ggplot2::annotate

graph = plotly::ggplotly(ggplot2::ggplot(node, aes(x = reorder(!!Property_Name,!!Property_Name,function(x)-length(x)),
                                         fill = project_id)) +
  ggplot2::geom_bar(width = .8) +
  theme_light() +
  ggplot2::annotate("text", x=ProjectID_Count$Property_Name, y= PropertyName_Table$Count, na.rm = TRUE,
                    label=paste("# Projects: ", ProjectID_Count$pCount), size = 3,vjust = -1) +
  labs(x = Property_Name, y = "Counts") +
  ggplot2::theme(text = element_text(family = "Times")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
                 axis.text.y = ggplot2::element_text(size = 15)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size=20, face = "bold", vjust = 5),
                 axis.title.y = ggplot2::element_text(size=15, vjust = 2)) +
  ggplot2::theme(legend.text = ggplot2::element_text(color = "black", size = 12)) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(legend.background = ggplot2::element_rect(linetype="longdash",colour ="darkgrey")) +
  ggplot2::guides(fill = guide_legend(title = "Project IDs", title.position = "top", title.hjust = .5,
                             title.theme = element_text(size = 15, face = "bold"))) +
  ggplot2::scale_fill_manual(values = colors))

st=format(Sys.time(), "%Y-%m-%d_%H:%M")
htmlwidgets::saveWidget(as_widget(graph), paste("Count_BarGraph_", st, ".html", sep = ""))

return(graph)

}

barr(AnalyteData, analyte_type)

colnames(AnalyteData)

 ########################

p <- AnalyteData %>%
  group_by(analyte_type, project_id) %>%
  dplyr::summarise(Count = n())



stats::aggregate(data = AnalyteData, project_id ~ analyte_type, function(x) length(unique(x)))


trial <- function (node, Property){
  Property_Name <- enquo(Property)

  p <-  node %>%
    group_by(!!Property_Name, project_id) %>%
    dplyr::summarise(Count = n())

  pcount <- p %>%
    group_by(!!Property_Name) %>%
    dplyr::summarise(pCount = n())%>%
    rename(Property_Name = !!Property_Name)

  return(pcount)
  return(p)
}

trial(AnalyteData, analyte_type)

##

TableID <- AnalyteData %>% #Creates a dataframe that counts Property_Name that will be used in ggplot2::annotate to specify text location on the y axis.
  group_by(analyte_type) %>%
  dplyr::summarise(Count = dplyr::n())

p <-  AnalyteData %>% #Creates a dataframe that groups by Property_Name and ProjectID for usage in pcount below.
  group_by(analyte_type, project_id) %>%
  dplyr::summarise(Count = dplyr::n())

OrderingDataFrame <- p
OrderingDataFrame <- data.frame(OrderingDataFrame)

OrderingDataFrame %>%
  dplyr::arrange(Count) %>%
  dplyr::mutate(project_id = factor(project_id, levels = unique(project_id)))


pcount <- p %>% #Creates a dataframe using the p from above to calculate the number of unique projectIDs within each response variable.
  group_by(analyte_type) %>%
  dplyr::summarise(pCount = n())
colnames(pcount)[1] <- "Property_Name"  #Renames the first column to a generic name for use in ggplot2::annotate


ggplot(AnalyteData, aes(x = days_to_assay)) +
  theme_light() +
  geom_bar(width = .8)

AnalyteData$days_to_assay = as.factor(AnalyteData$days_to_assay)
rgbcolors  <- c('rgb(114,147,203)','rgb(128,133,133)', 'rgb(144,103,167)',
           'rgb(171,104,87)')

#########

pieplotr <- function (df, Property){


  Property_Name <- rlang::enquo(Property)

  Pie <- rlang::eval_tidy(
    rlang::quo_squash(quo(
      plot_ly(df,
              labels = ~!!Property_Name,
              type = "pie",
              sort = T,
              marker = list(line = list(color = '#FFFFFF',
                                        width = .5))
    ))))

  return(Pie)
}

pieplotr(AnalyteData,
          analyte_type)

##### Additional testing for Stack Overflow
df <- data.frame(
  Sex = c("Male", "Female", "Female", "Male", "Male","NA"),
  Height = c(12,11,7,3,9,NA),
  Name = c("John", "Dora", "Dora","Dora", "Anna", "John"),
  Last = c("Henry", "Paul", "House", "Houze", "Henry", "Bill"),
  Location = c("Chicago", "Chicago", "Portland", "NYC", "NYC", "NYC"),
  stringsAsFactors = TRUE)

# str(df)
# df

pieplotrrr <- function (df, Property){

hexcolors <- c("#91a3b0","#537b5d","#6d9c79","#344c3a","#a1caf1","#98777b","#a2a2d0","#66023c","#00755e",
              "#9966cc","#ffbf00","#cd9575","#e9d66b","#ff9966","#e2725b","#464EB4","#ecd540","#aa98a9",
              "#44AAAA", "#629c92", "#77AADD", "#117777", "#18402c", "#eb9886","#77CCCC" ,"#771155",
              "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA", "#771122", "#AA4455", "#cf3d1d", "#f2e88a",
              "#014411","#b4464e","#973b42","#7b3036","#7AB446","#512023","#cc7c82","#5d8aa8","#f0f8ff",
              "#7d200b","#5F7FC7","#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711")

  Property_Name <- rlang::enquo(Property)

  Pie <- plot_ly(df,
                 labels = Property_Name,
                 type = "pie")

return(Pie)
}

pieplotrrr(df,
          Last)


##########

rlang::ns_env(xplorer)


plot_ly(AnalyteData,
        type = ~analyte_type,
        sort = T,
        showlegend = FALSE,
        type = "pie",
        marker = list(colors = rgbcolors,
                      line = list(color = '#FFFFFF',
                                  width = .1)))



###

TableID <- AnalyteData %>% #Creates a dataframe that counts Property_Name that will be used in ggplot2::annotate to specify text location on the y axis.
  group_by(analyte_type) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  arrange(desc(Count)) %>%
  mutate(prop = Count / sum(Count) * 100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

mycols <- c("#0073C2FF", "#EFC000FF", "#CC99BB", "#868686FF", "#CD534CFF")

ggplot(TableID, aes(x = "", y = prop, fill = analyte_type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos,label = analyte_type), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()

######

Prop <- TableID$prop
Labels = TableID$analyte_type

pie(Prop, labels = Labels, col= colors)

######Tabler

# Mean

tabler <- function (node, ..., y){

Property_Name <- dplyr::enquos(...)
y <- dplyr::enquo(y)

TableMean  <- node %>%
  dplyr::select(!!!Property_Name, !!y) %>%
  dplyr::group_by(!!!Property_Name) %>%
  tidyr::drop_na(!!y) %>%
  dplyr::summarize(N=sum(!is.na(!!y)),
                   Mean =mean(!!y),
                   Min = min(!!y, na.rm = TRUE),
                   Median=median(!!y, na.rm = TRUE),
                   Max = max(!!y, na.rm = TRUE),
                   SD  = sd(!!y, na.rm = TRUE),
                   SE  = SD / sqrt(N)) %>%
  data.frame()
return(TableMean)

}

tabler(QuantData,
       project_id,
       y = molecular_concentration)

# Count

tabler_count <- function(node, ...){

  Property_Name <- dplyr::enquos(...)

TableCount <- node %>%
  dplyr::group_by(!!!Property_Name) %>%
  dplyr::summarise(Count = sum(!is.na(!!!Property_Name))) %>%
  dplyr::arrange(desc(Count)) %>%
  data.frame()

return(TableCount)
}

tabler_count(QuantData,
       assay_kit_name)
