

server <- function(input, output, session) {
  
  rv <- reactiveVal(test)
  
  
  
  
  
  
  observeEvent(input$add_row, {
    
    #requires file to work
    req(input$file)
    
    df <- fread(input$file$datapath, header=TRUE)
    
    df <- df[!is.na(df$`EG.TotalQuantity (Settings)`),]
    
    #extract date from filename
    date <- df$R.FileName[1]
    #find last "_" in date string
    last_underscore <- max(gregexpr("_", date)[[1]])
    date <- substr(date, last_underscore + 1, nchar(date))  # remove ".tsv"
    #reformat date
    date <- paste0(substr(date, 5, 6), substr(date, 1, 2), substr(date, 3, 4)  )
    
    protein_count <- length(unique(df$PG.ProteinAccessions))
    peptide_count <- length(unique(df$EG.ModifiedSequence))
    precursor_count <- length(unique(df$EG.PrecursorId))
    

    #only keeping rows that need to be rolled up
    #temp_df <- df |> select(contains('PG.ProteinAccessions'))
    #temp_df$Precursors <- 1
    #roll you data
    #temp_df2 <<- temp_df
    #rolled_data = rollup_sum(temp_df)
    
    
    # Turning all values Numeric
    df$`EG.TotalQuantity (Settings)` <- as.numeric(df$`EG.TotalQuantity (Settings)`)
    df$EG.PeakWidth <- as.numeric(df$EG.PeakWidth)
    
    
    q3 = quantile(df$`EG.TotalQuantity (Settings)`, probs = 0.75, na.rm = TRUE)

    #getting sum of Q4
    numerator_df = df[df$`EG.TotalQuantity (Settings)` >= q3,]
    numerator = sum(numerator_df$`EG.TotalQuantity (Settings)`, na.rm = TRUE)
    
    # Set your range as a variable
    range_min <- 10/60  # adjust these values as needed
    range_max <- 30/60
    
    # Calculate the ratio
    ratio_pw <- sum(df$EG.PeakWidth >= range_min & df$EG.PeakWidth <= range_max, na.rm = TRUE) / 
      sum(!is.na(df$EG.PeakWidth))
    
    ratio_pw <- round((ratio_pw * 100), digits=1)

    
    new_row <- data.frame(Date = date,
                          Proteins = protein_count,
                          Peptides = peptide_count,
                          Precursors = precursor_count,
                          Sum_Last_Quartile = numerator,
                          Ratio_pw = ratio_pw,
                          stringsAsFactors = FALSE)
    
    updated_table = rv()
    
    # Check if duplicate row trying to be added
    is_duplicate <- any(updated_table$Date == new_row$Date & updated_table$Peptides == new_row$Peptides)
    
    if (is_duplicate) {
      shinyalert("Error! This data has already been uploaded", type = "error")
      return()
    }
   
    
    
    updated_table <- rbind(rv(), new_row)
    updated_table <- updated_table[order(as.Date(updated_table$Date, format = "%y%m%d")), ]
    
    
    
    
    
    write_xlsx(updated_table, "df_hela_OA10034.xlsx")
    
    rv(updated_table)
    
  
  })
  
  output$mytable <- renderDT({rv()})
  
  
  
  output$ratioPlot<- renderPlot({
    df <- rv()
    ggplot(df, aes(x = Date, y = Sum_Last_Quartile))+
      geom_line(group = 1, color = "blue", linewidth = 1.2) +
      geom_point(color = "red", size = 3) + 
      
      coord_cartesian(ylim = c(min(df$Sum_Last_Quartile)- 100000, max(df$Sum_Last_Quartile, na.rm = TRUE)*1.05)) +
      labs(title = "Sum of Fourth Quartile",
           x = "Date",
           y = "Q4") +
      theme_minimal()+
      theme(axis.text.x = element_text(size = 11))
    
  })
  
  output$ratio_brush_table <- renderTable({
    n = nrow(brushedPoints(rv(), brush = input$ratio_brush))
    if(n==0)
      return()
    else
      brushedPoints(rv(), brush = input$ratio_brush)
    
  })
  
  
  
  output$proteinPlot <- renderPlot ({
    df <- rv()
    ggplot(data=df, aes(x=Date, y=Proteins, group=1)) +
      geom_line(color="blue", linewidth=1.2)+
      geom_point(color = 'red', size = 3) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(color = "blue", size=16, vjust=0.5, hjust=0.5),
      ) +
      ggtitle("Proteins") + theme_minimal()
  })
  
  output$protein_brush_table <- renderTable({
    n = nrow(brushedPoints(rv(), brush = input$protein_brush))
    if(n==0)
      return()
    else
      brushedPoints(rv(), brush = input$protein_brush)
    
  })
  
  
  
  output$peptidePlot <- renderPlot ({
    df <- rv()
    ggplot(df, aes(x=Date, y=Peptides, fill = Date)) +
      geom_bar(stat="identity") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_text(color = "blue", size=16, vjust=0.5, hjust=0.5),
            legend.position="none"
      ) +
      coord_cartesian(ylim = c(0, max(df$Peptides))) +
      ggtitle("Peptides")
  })
  
  
  output$precursorPlot <- renderPlot ({
    df <- rv()
    ggplot(df, aes(x=Date, y=Precursors, fill = Date)) +
      geom_bar(stat="identity") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_text(color = "blue", size=16, vjust=0.5, hjust=0.5),
            legend.position="none"
      ) +
      coord_cartesian(ylim = c(0, max(df$Precursors))) +
      ggtitle("Precursors")
  })
  
  
  output$peakwidthPlot <- renderPlot ({
    df <<- rv()
    ggplot(df, aes(x=Date, y=Ratio_pw, fill = Date)) +
      geom_bar(stat="identity") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_text(color = "blue", size=16, vjust=0.5, hjust=0.5),
            legend.position="none"
      ) +
      coord_cartesian(ylim = c(0, max(df$Ratio_pw))) +
      ggtitle("Peak Width")
  })
  
  
}




