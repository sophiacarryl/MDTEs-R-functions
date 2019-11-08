#' Bar plot
#'
#' @param node A data frame
#' @param Property_Name A categorical column
#' @param y A numerical column. Specificy FALSE or node$columnname
#' @param MEAN Calculated average of numerical column known as y
#' @param Interactive An interactive html plot. Specificy TRUE or FALSE
#'
#' @importFrom dplyr select
#' @importFrom stats aggregate median reorder sd
#'
#' @export barplotr
barplotr <- function (node, Property_Name, y = NULL, MEAN = NULL, Interactive = NULL){

  custom_final28 = c("#44AAAA", "blueviolet", "#77AADD", "#117777", "gold1", "darkseagreen4",
                     "#77CCCC" ,"#771155", "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA",
                     "#771122", "#AA4455", "plum4", "slateblue", "violetred","#5F7FC7",
                     "#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
                     "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711","#E69F00")

  colors <- c("#91a3b0","#537b5d","#6d9c79","#344c3a","#a1caf1","#98777b","#a2a2d0","#66023c","#00755e",
              "#9966cc","#ffbf00","#cd9575","#e9d66b","#ff9966","#e2725b","#464EB4","#ecd540","#aa98a9",
              "#44AAAA", "#629c92", "#77AADD", "#117777", "#18402c", "#eb9886","#77CCCC" ,"#771155",
              "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA", "#771122", "#AA4455", "#cf3d1d", "#f2e88a",
              "#014411","#b4464e","#973b42","#7b3036","#7AB446","#512023","#cc7c82","#5d8aa8","#f0f8ff",
              "#7d200b","#5F7FC7","#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711")


  Property_Name <- factor(Property_Name)
  #    print(Property_Name)
  TableID <- data.frame(plyr::count(node, "Property_Name"))
  ymax <- as.numeric(max(TableID$freq))
  TableID$freq <- TableID$freq + (TableID$freq * 0.05)
  pcounts <- stats::aggregate(data = node, project_id ~ Property_Name, function(x) length(unique(x)))
  ymax <- as.numeric(max(TableID$freq))
  Property_Response_Variable <- reorder(Property_Name,Property_Name,function(x)-length(x))

  # Plot Count

  if(isFALSE(y) && isFALSE(MEAN)&& isFALSE(Interactive)){
    graph <- ggplot2::ggplot(node, ggplot2::aes(x =  Property_Response_Variable, fill = node$project_id)) +
      ggplot2::geom_bar(width = .8) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::annotate("text", x=pcounts$Property_Name, y= TableID$freq, na.rm = TRUE,
               label=paste("# Projects: ", pcounts$project_id), size = 3) +
      ggplot2::labs(y="Counts", x = "Property Name") +
      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
            legend.text = ggplot2::element_text(color = "black", size = 5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_y_continuous(breaks=seq(0, (ymax + (.02*ymax)), (ymax / 5))) +
      ggplot2::scale_fill_manual(values = custom_final28)

    return(graph)
  }

  else if (isFALSE(y) && isFALSE(MEAN) && isTRUE(Interactive)){
    graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(x =  Property_Response_Variable, fill = node$project_id)) +
                        ggplot2::geom_bar(width = .8) +
                        ggplot2::theme_bw() +
                        ggplot2::theme(legend.position = "bottom") +
                        ggplot2::annotate("text", x=pcounts$Property_Name, y= TableID$freq, na.rm = TRUE,
                                 label=paste("# Projects: ", pcounts$project_id), size = 3) +
                        ggplot2::labs(y="Counts", x = "Property Name") +
                        ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                              legend.text = ggplot2::element_text(color = "black", size = 5)) +
                        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                        ggplot2::scale_y_continuous(breaks=seq(0, (ymax + (.02*ymax)), (ymax / 5))) +
                        ggplot2::scale_fill_manual(values = custom_final28))
    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(graph), paste("Count_BarGraph_", st, ".html", sep = ""))

    return(graph)
  }

  # Plot Y in a boxplot

  else if (!is.null(y) && isFALSE(MEAN) && isFALSE(Interactive)) {
    #         print(node)
    # print(y)
    graph <- ggplot2::ggplot(node, ggplot2::aes(x = Property_Name, y)) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = node$project_id)) +
      ggplot2::theme_bw() +
      ggplot2::geom_jitter(position=ggplot2::position_jitter(0.0), size = 2) +
      ggplot2::theme(legend.position = "bottom") +
      #labs( x = "Property Name") +
      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
            legend.text = ggplot2::element_text(color = "black", size = 5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_fill_manual(values = custom_final28)

    return(graph)
  }

  else if (!is.null(y) && isFALSE(MEAN) && isTRUE(Interactive)) {
    print("Plotted below is a boxplot to show distribution of Y")
    graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(x = Property_Name, y)) +
                      ggplot2::geom_boxplot(ggplot2::aes(fill = node$project_id)) +
                      ggplot2::theme_bw() +
                      ggplot2::geom_jitter(position=ggplot2::position_jitter(0.0), size = 3) +
                      ggplot2::theme(legend.position = "bottom") +
                        #labs( x = "Property Name") +
                      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                              legend.text = ggplot2::element_text(color = "black", size = 5)) +
                      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                      ggplot2::scale_fill_manual(values = custom_final28))
    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(graph), paste("Boxplot_", st, ".html", sep = ""))


    return(graph)
  }

  # Plot Mean of Y

  else if (!is.null(y) && isTRUE(MEAN) && isFALSE(Interactive)) {
    Property_Name = reorder(x = Property_Name, -y)
    Mean = y
    graph <- ggplot2::ggplot(node, ggplot2::aes(Property_Name, Mean)) +
      ggplot2::theme_bw() +
      ggplot2::stat_summary(geom = "bar", fun.y = mean, ggplot2::aes(fill = Property_Name)) +
      ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se) +
      ggplot2::scale_fill_manual(values = custom_final28) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
            legend.text = ggplot2::element_text(color = "black", size = 5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    return(graph)
  }

  else if (!is.null(y) && isTRUE(MEAN) && isTRUE(Interactive)) {
    Property_Name = reorder(x = Property_Name, -y)
    Mean = y
    graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(Property_Name, Mean)) +
                  ggplot2::theme_bw() +
                  ggplot2::stat_summary(geom = "bar", fun.y = mean, ggplot2::aes(fill = Property_Name)) +
                  ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se) +
                  ggplot2::scale_fill_manual(values = custom_final28) +
                  ggplot2::theme(legend.position = "bottom") +
                  ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                              legend.text = ggplot2::element_text(color = "black", size = 5)) +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)))
    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(graph), paste("Mean_BarGraph_", st, ".html", sep = ""))
    return(graph)
  }

  else { stop("Specify MEAN = FALSE or TRUE; Interactive = TRUE or FALSE")}
}

