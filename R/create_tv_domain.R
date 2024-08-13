# File: R/create_tv_domain.R
#' Create TV Domain
#'
#' This function creates the TV (Trial Visits) domain by extracting ALS excel file.
#' It will help to create VISIT related information in the TV data set.
#'
#' @param study_id A character string representing the Study ID.
#' @param ALS excel file to generate the trail visit domain.
#' @return A data frame representing the TV data set.
#' @export
#' @importFrom dplyr readxl grepl read.xlsx
#' @examples
#' \dontrun{
#' study_id <- "AB12345"
#' file_path <- "als.xlsx"
#' tv_domain <- create_tv_domain(study_id, file_path)
#' print(head(tv_domain))
#' }

create_tv_domain <- function(study_id, file_path ) {

# Locate the TV summary file within the package
  input_file <- system.file("extdata", "Als_file.xlsx", package = "autoTDD")

# to check the Study number's length
  if (nchar(study_id) != 7 ) {
    stop("Please enter correct study number. It should have 7 character length: ", study_id)
  }

# to check the File exist or not
  if (!file.exists(file_path)) {
    stop("File does not exist at this location: ", file_path)
  }

# if file present then to check the file extension is xlsx or not
  if (grepl("xlsx", file_path, ignore.case = FALSE)) {

    cat("Xlsx file is present: ", file_path , "\n")
  } else {

    stop("Please provide xlsx format file, As you have selected wrong format file that is: ", file_path)
  }

# if correct ALS file is present then check for "Folders" sheet in the excel file

  if(!"Folders" %in% excel_sheets(file_path) ){
    stop( "Excel file does not have sheet with the name Folders: , Please update correct ALS file: ",file_path )
    }


  # Read the Excel file
  data <- read.xlsx(file_path , sheet = 'Folders' )

# to check if the column names exists or not

  if ( all(c("FolderName" , "Targetdays", "OverDueDays" ) %in% names(data)) ) {
    print("Excel sheet contains required column names (FolderName , Targetdays, OverDueDays)")
  }
  else {
    stop("Excel sheet does not have required column names (FolderName , Targetdays, OverDueDays)")
  }

# if Folders sheet present , all required columns present but there is no records

  if ( nrow(data) == 0 ) {
    stop("Folders sheet is empty in the ALS file")
  }


  data1 <- data %>% select (c("FolderName" , "Targetdays", "OverDueDays"))  %>% filter(!(is.na(Targetdays))  | (grepl("Treatment Discontinuation", FolderName, ignore.case = FALSE)) )  %>% mutate(t_day = as.numeric(Targetdays))  %>% arrange(t_day , FolderName)



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


  data3 <- data2 %>% mutate (TVENRL =if_else(toupper(VISIT) == "SCREENING" , "One day before start of study drug" , "On the same day of visit")) %>%
    #  mutate (TVSTRL =if_else(toupper(VISIT) == "SCREENING" , "28 days prior to treatment" , "")) %>%
    mutate( TVSTRL = if_else(VISITDY == 1 ,  paste("First dose of treatment phase +/-",data2$due_day,"days", sep =" "),  paste( data2$VISITDY,"Days +/-", data2$due_day, "days from Cycle 1 Day 1", sep =" ") ))

  data3$TVSTRL[data3$VISITDY == -28 ] <- "28 days prior to treatment"

 data3$VISIT[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = FALSE)) ] <- 'Treatment Discontinuation'

 data3$TVSTRL[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = FALSE)) ] <- '30 Days from final dose'

    data3 <- data3 %>% select(-due_day)

  return(data3)

}



