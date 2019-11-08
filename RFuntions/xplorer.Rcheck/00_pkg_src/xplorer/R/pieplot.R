#' Pie plot and nested pie plot
#'
#' @param node A dataframe
#' @param Property_Name A categorical column
#' @param ProjectID Specify TRUE or FALSE to include Project ID into pieplot
#'
#' @importFrom dplyr select
#' @importFrom stats aggregate median reorder sd
#'
#' @export pieplotr
pieplotr <- function (node, Property_Name, ProjectID = NULL){

  colors <- c("#91a3b0","#537b5d","#6d9c79","#344c3a","#a1caf1","#98777b","#a2a2d0","#66023c","#00755e",
              "#9966cc","#ffbf00","#cd9575","#e9d66b","#ff9966","#e2725b","#464EB4","#ecd540","#aa98a9",
              "#44AAAA", "#629c92", "#77AADD", "#117777", "#18402c", "#eb9886","#77CCCC" ,"#771155",
              "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA", "#771122", "#AA4455", "#cf3d1d", "#f2e88a",
              "#014411","#b4464e","#973b42","#7b3036","#7AB446","#512023","#cc7c82","#5d8aa8","#f0f8ff",
              "#7d200b","#5F7FC7","#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711")

  if(isTRUE(ProjectID)){
    Property_Name = factor(Property_Name)
    TableID = data.frame(plyr::count(node, c("Property_Name", "project_id")))
    TableID$project_id = gsub("-", "_", TableID$project_id)
    data <- TableID %>%
      dplyr::mutate(path = paste(Property_Name,project_id, sep="-")) %>%
      dplyr::select(path, freq)
    sunny = sunburstR::sund2b(data,colors = colors)
    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(sunny), paste("PiePlot_", st, ".html", sep = ""))

    return(sunny)
  }

  else if(isFALSE(ProjectID)){
    Property_Name = factor(Property_Name)
    Pie = plotly::plot_ly(node,
                  labels = ~Property_Name,
                  sort = T,
                  showlegend = FALSE,
                  colors = colors,
                  type = 'pie')
    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(Pie), paste("PiePlot_", st, ".html", sep = ""))

    return(Pie)
  }
  else {
    print("You got this!")
  }
}
