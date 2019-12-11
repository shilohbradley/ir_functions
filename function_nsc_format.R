##################################################
# A function to pull data and format it to send  #
# to National Student Clearinghouse.             #
##################################################

## Load packages -----
require(openxlsx)

## Function to create tab delimited txt file -----
create_excel_sheet_for_sch_batch_file_processing_from_emplid<-function(my_data,outputfile="sample.txt",search_date="14-SEP-12",inquiry_type="SE") {
  # my_data<-retrieve_data_for_sch_batch_file_processing_from_emplid(my_q,strm)
  # my_data <- list()
  my_data$MIDDLE_NAME<-substr(as.character(my_data$MIDDLE_NAME),1,1)
  my_data$BIRTHDATE<-format(as.Date(substr(as.character(my_data$BIRTHDATE),1,10)),"%Y%m%d")
  
  ### Launch Excel Spreadsheet and create TEXT Formatting
  owb<-createWorkbook()
  addWorksheet(owb,1)
  text_data<-createStyle(numFmt="TEXT")
  
  
  ### Insert Header Row
  header_row_info<-list(c(Row=1,Col=1,value="H1"),
                        c(Row=1,Col=2,value="002569"),
                        c(Row=1,Col=3,value="00"),
                        c(Row=1,Col=4,value="UNIVERSITY OF NEVADA LAS VEGAS"),
                        c(Row=1,Col=5,value=format(Sys.Date(),"%Y%m%d")),
                        c(Row=1,Col=6,value=inquiry_type),
                        c(Row=1,Col=7,value="I"))
  trash<-lapply(header_row_info,function(z) {writeData(owb,sheet=1,
                                                       x=as.character(z["value"]),
                                                       startRow=as.numeric(z["Row"]),
                                                       startCol=as.numeric(z["Col"]),
                                                       rowNames=FALSE,
                                                       colNames=FALSE)})
  
  ### Student Detail Rows
  for (i in 1:nrow(my_data)) {
    student_detail<-list(c(Row=i+1,Col=1,value="D1"),
                         c(Row=i+1,Col=3,value=substr(as.character(my_data$FIRST_NAME[i]),1,20)),
                         c(Row=i+1,Col=4,value=as.character(my_data$MIDDLE_NAME[i])),
                         c(Row=i+1,Col=5,value=as.character(my_data$LAST_NAME[i])),
                         c(Row=i+1,Col=7,value=as.character(my_data$BIRTHDATE[i])),
                         c(Row=i+1,Col=8,value=search_date[i]),
                         c(Row=i+1,Col=10,value="002569"),
                         c(Row=i+1,Col=11,value="00"),
                         c(Row=i+1,Col=12,value=as.character(my_data$EMPLID[i])))
    print(i)
    trash<-lapply(student_detail,function(z) {if (length(z["value"])>0) writeData(owb,sheet=1,
                                                                                  x=as.character(z["value"]),
                                                                                  startRow=as.numeric(z["Row"]),
                                                                                  startCol=as.numeric(z["Col"]),
                                                                                  rowNames=FALSE,
                                                                                  colNames=FALSE) 
      print(z)})
  }
  ### Insert Trailer Row
  trailer_detail<-list(c(Row=nrow(my_data)+2,Col=1,value="T1"),
                       c(Row=nrow(my_data)+2,Col=2,value=nrow(my_data)+2))
  
  trash<-lapply(trailer_detail,function(z) {writeData(owb,sheet=1,
                                                      x=as.character(z["value"]),
                                                      startRow=as.numeric(z["Row"]),
                                                      startCol=as.numeric(z["Col"]),
                                                      rowNames=FALSE,
                                                      colNames=FALSE)
    print(z)})
  

  trash<-lapply(as.list(1:(nrow(my_data)+2)),function(ii) {addStyle(owb,
                                                                    sheet=1,
                                                                    rows=ii,
                                                                    style=text_data,
                                                                    cols=1:12)
    print(ii)})
  ### Save Excel Workbook and exit  
  #write.table(x = owb, file = outputfile, sep = "\t", row.names = FALSE)
  saveWorkbook(wb=owb,file=outputfile,overwrite=TRUE)
  return(TRUE)
}


## Call the function -----
# create_excel_sheet_for_sch_batch_file_processing_from_emplid(my_data = final_df, outputfile = paste0("math_181_stop_outs_",Sys.Date(),".txt"), search_date = final_df$SEARCH_DATE)

