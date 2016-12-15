#' ---
#' title: "A YUGE! heatmap"
#' author: "Bhaskar V. karambelkar"
#' date: "Oct 5, 2016"
#' ---

library(leaflet.extras)
library(magrittr)
#'
#'
#' Data downloaded and extracted from
#' https://data.police.uk/data/
#' Specify Date range from 2016-01 to 2016-07 and select all sources
#' unzip the downloaded zip in the data dir.<br/>
#' The files are organized by months and in each month's dir
#' there is one CSV file per police force.<br/<br/>
basedir <- './data'
uk.crimes.dirs <- Sys.glob(sprintf('%s/[0-9]*',basedir))
uk.crimes.months <- basename(uk.crimes.dirs)
names(uk.crimes.dirs) <- uk.crimes.months

#' For the code curious, walk each months directory,
#' read CSVs for each Police Force and collapse them in to one dataframe.
#' <br/>So you get a list with one data.frame per month with stats across all of UK.
#' The grand total is about 3.4M incidents.
suppressMessages(
 uk.crimes.dfs <<- purrr::map(
   uk.crimes.months,
   function(month) {
     dir <- uk.crimes.dirs[[month]]
     csvs <- Sys.glob(sprintf('%s/*csv',dir))
     purrr::map( csvs, ~readr::read_csv(.) %>%
                   dplyr::select(Latitude, Longitude) %>%
                   dplyr::filter(!is.na(Latitude))) %>%
       dplyr::bind_rows()
   }) %>% set_names(uk.crimes.months)
)

#' Per month crimes
formatC(sapply(uk.crimes.dfs,nrow), big.mark = ',', format='d')
#' Total Crimes
formatC(sum(sapply(uk.crimes.dfs,dim)[1,]), big.mark = ',', format='d')

#'
#'
#'
leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

#'
#' Again for the code curious add each months crime locations to a new group
purrr::walk(
  uk.crimes.months,
  function(month) {
    leaf <<- leaf %>%
      addWebGLHeatmap(
        data = uk.crimes.dfs[[month]],
        layerId = month, group = month,
        lng=~Longitude, lat=~Latitude,size=3,units='px'
        )
  })

leaf %>%
  setView(-4, 54, 6) %>%
  addLayersControl(
    baseGroups = uk.crimes.months,
    options = layersControlOptions(collapsed = FALSE)
  )
#'
#'
rm(uk.crimes.dfs)

