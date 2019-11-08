tidy_ply2 <- function(data, x) {
  x = enquo(x)
  print(x)

  data = filter(data, !!x > 5)

  # # https://rlang.r-lib.org/reference/quasiquotation.html
  # cat('\nUse qq_show() to debug the effect of unquoting operators\n')
  # qq_show(plot_ly(data, x = ~!!x, y = ~!!x/2))
  #
  # `base::eval` works too
  eval_tidy(
    quo_squash(
      quo({
        plot_ly(data, x = ~!!x, y = ~!!x/2)
      })
    )
  )
}

tidy_ply2(mtcars, wt)

tidy_ply = function(data, x) {
  x = enquo(x)
  data = filter(data, !!x > 5)
  plot_ly(data, x = x, y = x)
}

tidy_ply(mtcars, wt)



quo <- quo(foo(!!quo(bar)))
quo
quo_squash(quo)
quo_text(quo)
expr_text(quo)
quo_name(quo(sym))
quo_name(quo(!! sym))







else if (isFALSE(y) && isFALSE(MEAN) && isTRUE(Interactive)){
  graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(x =  Property_Response_Variable, fill = node$project_id)) +
                              ggplot2::geom_bar(width = .8) +
                              ggplot2::theme_bw() +
                              ggplot2::theme(legend.position = "bottom") +
                              ggplot2::annotate("text", x=pcounts$Property_Name, y= TableID$freq, na.rm = TRUE,
                                                label=paste("# Projects: ", pcounts$project_id), size = 3, vjust = -1) +
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




Property_Name <- dplyr::enquo(Property)


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
                        marker = list(colors = tryme,
                                      line = list(color = '#FFFFFF', width = 1)),
                        type = 'pie')
  st=format(Sys.time(), "%Y-%m-%d_%H:%M")
  htmlwidgets::saveWidget(as_widget(Pie), paste("PiePlot_", st, ".html", sep = ""))

  return(Pie)
}
else {
  print("You got this!")
}
}


