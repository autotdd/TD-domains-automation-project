# File: R/create_tv_domain.R
#' Create TV Domain
#'
#' This function creates the TV (Trial Visits) domain by extracting ALS excel file.
#' It will help to create VISIT related information in the TV data set.
#'
#' @param study_id A character string representing the Study ID.
#' @param ALS excel file to generate the trail visit domain.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A data frame representing the TV data set.
#' @export
#' @importFrom dplyr mutate select filter
#' @importFrom readxl read_excel
#' @importFrom openxlsx read.xlsx
#' @examples
#' \dontrun{
#'  #Get the path for the Als_file excel file
#' file_path <- system.file("extdata", "Als_file.xlsx", package = "autoTDD")
#'
#' tv_domain <- create_tv_domain(
#'   study_id ="AB12345",
#'   file_path = file_path
#'   )
#'
#' print(head(tv_domain))
#' }

create_tv_domain <- function(study_id , file_path = NULL , output_dir = getwd())
{

  # to check the Study number's length
  if (nchar(study_id) != 7 ) {
    stop("Please enter correct study number. It should have 7 character length: ", study_id)
  }

  # to check the File exist or not
  if (!file.exists(file_path)) {
    stop("File does not exist at this location: ", file_path)
  }

  # if file present then to check the file extension is xlsx or not
  if (grepl("xlsx", file_path, ignore.case = TRUE)) {

    cat("Xlsx file is present: ", file_path , "\n")
  } else {

    stop("Please provide xlsx format file, As you have selected wrong format file that is: ", file_path)
  }

  # if correct ALS file is present then check for "Folders" sheet in the excel file

  if(!"Folders" %in% readxl::excel_sheets(file_path) ){
    stop( "Excel file does not have sheet with the name Folders: , Please update correct ALS file: ",file_path )
  }


  # Read the Excel file
  data <- openxlsx::read.xlsx(file_path , sheet = 'Folders' )

  # to check if the column names exists or not

  if ( all(c("FolderName" ,"Ordinal", "Targetdays", "OverDueDays" ) %in% names(data)) ) {
    print("Excel sheet contains required column names (FolderName , Ordinal, Targetdays, OverDueDays)")
  }
  else {
    stop("Excel sheet does not have required column names (FolderName ,Ordinal, Targetdays, OverDueDays)")
  }

  # if Folders sheet present , all required columns present but there is no records

  if ( nrow(data) == 0 ) {
    stop("Folders sheet is empty in the ALS file")
  }


  data1 <- data %>% select (c("FolderName" ,"Ordinal", "Targetdays", "OverDueDays"))  %>%
    filter(!(is.na(Targetdays))  | (grepl("Treatment Discontinuation", FolderName, ignore.case = TRUE)) )  %>%
    mutate(t_day = as.numeric(Targetdays)  )  %>%
    mutate(seq_ord = as.numeric(Ordinal)  )  %>%
    arrange(seq_ord, t_day , FolderName)



  data2 <- data.frame(
    STUDYID = rep(study_id, nrow(data1)),
    DOMAIN = rep("TV", nrow(data1)),
    VISITNUM = seq(from=1 , to =nrow(data1) ),
    VISIT = data1$FolderName,
    VISITDY = data1$Targetdays,
    ARMCD = rep("", nrow(data1)),
    ARM = rep("", nrow(data1)),
    TVSTRL = rep("", nrow(data1)),
    TVENRL = rep("", nrow(data1)),
    due_day = data1$OverDueDays,
    stringsAsFactors = FALSE
  )

  data2 <- data2 %>%  arrange(VISITNUM)

  #Assign the Visit day for Screening and Cycle 1 Day 1 if both have 0 target day in ALS file

  data2$VISITDY[(grepl("SCREENING", data2$VISIT, ignore.case = TRUE) & data2$VISITDY == 0 ) ] <- -28

  data2$VISITDY[(grepl("CYCLE 1 Day 1", data2$VISIT, ignore.case = TRUE) & data2$VISITDY == 0 ) ] <- 1


  data3 <- data2 %>%  mutate (TVENRL =if_else(toupper(VISIT) == "SCREENING" , "One day before start of study drug" , "On the same day of visit")) %>%
    #  mutate (TVSTRL =if_else(toupper(VISIT) == "SCREENING" , "28 days prior to treatment" , "")) %>%
    mutate( TVSTRL = if_else(VISITDY == 1 ,  paste("First dose of treatment phase +/-",data2$due_day,"days", sep =" "),  paste( data2$VISITDY,"Days +/-", data2$due_day, "days from Cycle 1 Day 1", sep =" ") ))  %>%
    mutate( VISITDY = as.numeric(VISITDY) )


  data3$TVSTRL[grepl("SCREENING", data2$VISIT, ignore.case = TRUE) & data3$VISITDY == -28 ] <- "Informed consent obtained"

  data3$VISIT[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE)) ] <- 'Treatment Discontinuation'

  data3$VISITDY[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE) ) ] <- NA

  data3$TVSTRL[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE)) ] <- '30 Days from final dose'

  tv_domain <- data3 %>% select(-due_day)


  # Write the output to an Excel file

  debug_info <- ""

  if (nrow(tv_domain) > 0) {
    excel_file <- file.path(output_dir, paste0(study_id, "_TV.xlsx"))
    openxlsx::write.xlsx(tv_domain, excel_file)

    debug_info <- paste0(debug_info, "Excel file saved: ", excel_file)
  }

  else {
    debug_info <- paste0(debug_info, "No data to save to Excel")
  }

  print(debug_info)
  print(tv_domain)
  return(tv_domain)

}


