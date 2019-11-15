library("plyr")
library("ggplot2")
#install.packages(c("sunburstR","plotly", "doBy"))
library("plotly")
library("sunburstR")
library("tidyr")
library("htmlwidgets")
library("doBy")

AnalyteData  <- read.csv("../MasterDataSet/master_analyte.csv", sep = ",")
AliquotData <- read.csv("../MasterDataSet/master_aliquot.csv", sep = ",")
BiospecimenData <- read.csv("../MasterDataSet/master_biospecimen.csv", sep = ",")
SampleData <- read.csv("../MasterDataSet/master_sample.csv", sep = ",")
QuantData <- read.csv("../MasterDataSet/master_quantification_assay.csv", sep = ",")

########### To remove the blanks from disease_type
BiospecimenData <- BiospecimenData[as.character(BiospecimenData$disease_type)!= "" ,]
########## Rename blanks from disease_type to "Unknown"
BiospecimenData$disease_type[BiospecimenData$disease_type == ""] <- "Unknown"

#

Property <- QuantData$assay_kit_name

xplorer::pieplotr(BiospecimenData,
         disease_type,
         ProjectID = TRUE)

xplorer::barplotr(QuantData,
         assay_kit_name,
         y_vari =  molecular_concentration,
         Interactive = TRUE,
         MEAN = TRUE)

xplorer::barplotr(QuantData,
         Property = assay_kit_name,
         y_vari = FALSE,
         Interactive = FALSE,
         MEAN = FALSE)

xplorer::tabler(QuantData,
       assay_kit_name,
       STAT = TRUE,
       ProjectID = FALSE,
       y = molecular_concentration)

xplorer::tabler(QuantData,
                assay_kit_name,
                ProjectID = TRUE)

AnalyteData %>%
  group_by(days_to_assay, project_id) %>%
  summarize(N = sum(!is.na(days_to_assay)))

