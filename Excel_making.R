data_dir <- "/Users/davidwaitt/R./Intership/tsv"

# get list of files in data_dir
list_files <- list.files(data_dir, full.names = TRUE)

df_hela <- data.frame(matrix(ncol=6, nrow=0))

for (file in list_files) {
  df <- fread(file, header=TRUE)
  
  #remove rows that don't have totalquantity
  df <- df[!is.na(df$`EG.TotalQuantity (Settings)`),]
  
  #extract date from filename
  date <- df$R.FileName[1]
  #find last "_" in date string
  last_underscore <- max(gregexpr("_", date)[[1]])
  date <- substr(date, last_underscore + 1, nchar(date))  # remove ".tsv"
  #reformat date
  date <- paste0(substr(date, 5, 6), substr(date, 1, 2), substr(date, 3, 4)  )
  
  
  #protein, peptide, precursor stats
  protein_count <- length(unique(df$PG.ProteinAccessions))
  peptide_count <- length(unique(df$EG.ModifiedSequence))
  precursor_count <- length(unique(df$EG.PrecursorId))
  
  df$`EG.TotalQuantity (Settings)`<- as.numeric(df$`EG.TotalQuantity (Settings)`)
  
  q3_range = quantile(df$`EG.TotalQuantity (Settings)`, probs = 0.75, na.rm = TRUE)
  Sum_Last_Quartile <- sum(df$`EG.TotalQuantity (Settings)`[df$`EG.TotalQuantity (Settings)` >= q3_range], na.rm = TRUE)
  Sum_Last_Quartile <- round(Sum_Last_Quartile, digits = 1)
  
  
  # Set your range as a variable
  range_min <- 10/60  # adjust these values as needed
  range_max <- 30/60
  
  # Calculate the ratio
  ratio_pw <- sum(df$EG.PeakWidth >= range_min & df$EG.PeakWidth <= range_max, na.rm = TRUE) / 
    sum(!is.na(df$EG.PeakWidth))
  
  ratio_pw <- round((ratio_pw * 100), digits=1)
  
  
  
  df_hela <- rbind(df_hela, c(date, protein_count, peptide_count, precursor_count, Sum_Last_Quartile, ratio_pw))
  
}

#set column names
colnames(df_hela) <- c('Date', "Proteins", "Peptides", "Precursors", "Sum_Last_Quartile", "Ratio_pw")

#converting all columns to numeric
df_hela <- df_hela[order(as.Date(df_hela$Date, format = "%y%m%d")), ]
df_hela$Sum_Last_Quartile = as.numeric(df_hela$Sum_Last_Quartile)
df_hela$Ratio_pw = as.numeric(df_hela$Ratio_pw)
df_hela$Proteins = as.numeric(df_hela$Proteins)
df_hela$Precursors = as.numeric(df_hela$Precursors)
df_hela$Peptides = as.numeric(df_hela$Peptides)



#save to excel
write_xlsx(df_hela, "df_hela_OA10034.xlsx")

#read excel
test <- readxl::read_excel("df_hela_OA10034.xlsx")

