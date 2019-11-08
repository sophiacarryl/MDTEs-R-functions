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
tabler <- function (node, ..., ProjectID = NULL,UniqueProjectID = NULL,
                    y = NULL, MEAN = NULL){

Property_Name <- enquos(...)

# Makes a table of descriptive statistics of specified `y`.
  if(!is.null(y) && isTRUE(MEAN) && isFALSE(projectID)){
    new.df <- data.frame(Property_Name = !!!Property_Name, y)
    TableMean = new.df %>%
      dplyr::group_by(!!!Property_Name) %>%
      dplyr::summarize(N=sum(!is.na(y)),
                       mean=mean(y, na.rm=TRUE),
                       min = min(y, na.rm = TRUE),
                       median=median(y, na.rm = TRUE),
                       max = max(y, na.rm = TRUE),
                       sd  = sd(y, na.rm = TRUE),
                       se  = sd / sqrt(N))

    return(TableMean)
  }

  else if(!is.null(y) && isTRUE(MEAN) && isTRUE(projectID)){
    new.df <- data.frame(Property_Name = Property_Name, y, project_id = node$project_id)
    TableMean = new.df %>%
      dplyr::group_by(Property_Name, project_id) %>%
      dplyr::summarize(N=sum(!is.na(y)),
                       mean=mean(y, na.rm=TRUE),
                       min = min(y, na.rm = TRUE),
                       median=median(y, na.rm = TRUE),
                       max = max(y, na.rm = TRUE),
                       sd  = sd(y, na.rm = TRUE),
                       se  = sd / sqrt(N))

    return(TableMean)
  }

  else if(isTRUE(projectID)){
    TableID = data.frame(plyr::count(node, c("project_id","Property_Name")))
    TableID = subset(TableID, Property_Name != "NA")
    TableID$project_id = gsub("-", "_", TableID$project_id)
    TableID = dplyr::arrange(TableID, dplyr::desc(TableID$freq))
    names(TableID)[1] = "Project_ID"
    names(TableID)[3] = "Counts"
    N = length(unique(TableID$Project_ID))
    n = c("Number Unique Projects:", N)
    print(n, quote = FALSE)

    return(TableID)
  }

  else if (isFALSE(projectID)){
    Table = data.frame(plyr::count(node, "Property_Name"))
    Table = dplyr::arrange(Table, dplyr::desc(Table$freq))
    names(Table)[2] = "Counts"
    N = length(unique(Table$Property_Name))
    n = c("Number of Response Variables:", N)
    print(n, quote = FALSE)

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
