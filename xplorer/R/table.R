#' Table
#'
#' @param node A dataframe
#' @param Property_Name A categorical column
#' @param ProjectID Specify TRUE or FALSE to include Project ID in count
#' @param UniqueProjectID Specify TRUE or FALSE to count number of Unique Project IDs
#' @param y A numerical column. Specify FALSE or node$columnname
#' @param MEAN Calculated average of numerical column known as y
#'
#' @export tabler
tabler <- function (node, Property, ProjectID = NULL,
                    y = NULL, MEAN = NULL){

Property_Name <- dplyr::enquo(Property)
y <- dplyr::enquo(y)

# Makes a table of descriptive statistics of specified `y`.
  if(!is.null(y) && isTRUE(MEAN) && isFALSE(ProjectID)){
    TableMean <- node %>%
      select(!!Property_Name, !!y) %>%
      tidyr::drop_na(!!y) %>%
      dplyr::group_by(!!Property_Name) %>%
      dplyr::summarize(N=sum(!is.na(!!y)),
                       Mean=mean(!!y, na.rm=TRUE),
                       Min = min(!!y, na.rm = TRUE),
                       Median=median(!!y, na.rm = TRUE),
                       Max = max(!!y, na.rm = TRUE),
                       SD  = sd(!!y, na.rm = TRUE),
                       SE  = SD / sqrt(N)) %>%
      dplyr::arrange(desc(N)) %>%
      data.frame()

    return(TableMean)
  }

  else if(!is.null(y) && isTRUE(MEAN) && isTRUE(ProjectID)){
    TableMean <- node %>%
      select(!!Property_Name, !!y, project_id) %>%
      tidyr::drop_na(!!y) %>%
      dplyr::group_by(!!Property_Name, project_id) %>%
      dplyr::summarize(N=sum(!is.na(!!y)),
                       Mean=mean(!!y, na.rm=TRUE),
                       Min = min(!!y, na.rm = TRUE),
                       Median=median(!!y, na.rm = TRUE),
                       Max = max(!!y, na.rm = TRUE),
                       SD  = sd(!!y, na.rm = TRUE),
                       SE  = SD / sqrt(N)) %>%
      dplyr::arrange(desc(N)) %>%
      data.frame()

    return(TableMean)
  }

  else if(isTRUE(ProjectID)){
    TableID <- node %>%
      dplyr::select(!!Property_Name, project_id) %>%
      dplyr::group_by(!!Property_Name, project_id) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::arrange(desc(Count)) %>%
      data.frame()
    N = length(unique(TableID$project_id))
    n = c("Number Unique Projects:", N)
    print(n, quote = FALSE)

    return(TableID)
  }

  else if (isFALSE(ProjectID)){
    Table <- node %>%
      dplyr::select(!!Property_Name) %>%
      dplyr::group_by(!!Property_Name) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::arrange(desc(Count)) %>%
      data.frame()

    return(Table)
  }

  else if (isTRUE(UniqueProjectID)){
    pcounts = stats::aggregate(data = node, project_id ~ Property_Name, function(x) length(unique(x))) %>%
      dplyr::arrange(dplyr::desc(project_id))

    return(pcounts)
  }

  else{
    print("Specify TRUE or FALSE for projectID. e.g. projectID = TRUE ")
  }
}
