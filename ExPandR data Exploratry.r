library(ExPanDaR)
data("mtcars")
ExPanD(mtcars)

devtools::install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)
library(shiny)
ExPanD(mtcars,df_def = NULL)

ExPanD(df = worldbank,  
       df_def = worldbank_data_def, 
       var_def = worldbank_var_def,
       df_name = "World Bank Data",
       config_list = ExPanD_config_worldbank)

ExPanD(df = russell_3000,  
       df_def = russell_3000_data_def, 
       df_name = "Russell 3000",
       config_list = ExPanD_config_russell_3000,
       export_nb_option = TRUE)