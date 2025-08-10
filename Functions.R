#create_db("HeLa_OA10034")

library(RSQLite)
library(data.table)
library(ggplot2)
library(dplyr)

create_db <- function(db_name) {
  conn <- dbConnect(RSQLite::SQLite(), db_name) 
  RSQLite::dbDisconnect(conn)
}

read_table <- function(table_name, db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  df <- dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

write_table <- function(table_name, df, db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

list_tables <- function(db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  table_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}


rollup_sum <- function(df){
  cat(file = stderr(), "function rollup_sum...", "\n")
  
  protein_df <- df |> dplyr::group_by(PG.ProteinAccessions) |> dplyr::summarise_all(list(sum))
  protein_df <- data.frame(dplyr::ungroup(protein_df))
  
  #save(df, file="test_df"); save(protein_df, file="test_protein_df")
  #. load(file="test_df"); load(file="test_protein_df")
  
  cat(file = stderr(), "function rollup_sum...end", "\n\n")
  return(protein_df)
}
