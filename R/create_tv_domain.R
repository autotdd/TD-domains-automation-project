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

  #To include non missing target days and Screening and treatment discontintion

  data1_1 <- data %>% select (c("FolderName" ,"Ordinal", "Targetdays", "OverDueDays"))  %>%
    filter(!(is.na(Targetdays))  |  toupper(FolderName) %in% c('SCREENING', 'TREATMENT DISCONTINUATION' , 'TREATMENT DISCONTINUATION VISIT' , 'STUDY DRUG DISCONTINUATION' ) )

  #filter(!(is.na(Targetdays))  | (grepl("Treatment Discontinuation|Screening|Study Drug Discontinuation", FolderName, ignore.case = TRUE)) )

  #To exclude other Screening visits

  data1 <- data1_1 %>%  filter(!(is.na(Targetdays))  | !(grepl("Tissue Sample Collection|Sample Collection-Screening|-screening|/screening|Sample Collection Screening|/Treatment Discontinuation PRO", FolderName, ignore.case = TRUE)) )  %>%
    mutate(t_day = as.numeric(Targetdays)  )  %>%
    mutate(seq_ord = as.numeric(Ordinal)  )  %>%
    arrange(seq_ord, t_day , FolderName)

  # Update the Visitdy if targetdays for cycle 1 is 0 in the ALS file

  #data1 <- data1 %>% mutate(t_day = t_day + 1 )

  data2 <- data.frame(
    STUDYID = rep(study_id, nrow(data1)),
    DOMAIN = rep("TV", nrow(data1)),
    VISITNUM = seq(from=1 , to =nrow(data1) ),
    VISIT = toupper(data1$FolderName),
    VISITDY = data1$t_day,
    ARMCD = rep("", nrow(data1)),
    ARM = rep("", nrow(data1)),
    TVSTRL = rep("", nrow(data1)),
    TVENRL = rep("", nrow(data1)),
    due_day = data1$OverDueDays,
    stringsAsFactors = FALSE
  )

  data2 <- data2 %>%  arrange(VISITNUM)

  #Assign the Visit day for Screening and Cycle 1 Day 1 if both have 0 target day in ALS file

  # Update the Visitdy if targetdays for cycle 1 is 0 in the ALS file, if it is 0 for Cycle 1 Day 1 then visitdy must be 1

  row_count <- data %>% filter(!(is.na(Targetdays)) & Targetdays ==1 & grepl("Cycle 1 day 1| Cycle 1(D1)|Day 1", FolderName, ignore.case = TRUE ))

  nrow(row_count)


  if (nrow(row_count) == 0) {
    data2 <- data2 %>% mutate(VISITDY = case_when(
      grepl("SCREENING", VISIT, ignore.case = TRUE)  ~ VISITDY , TRUE ~  VISITDY +1 ))
  }


  data2$VISIT[(grepl("RANDOMIZATION / DAY 1", data2$VISIT, ignore.case = TRUE)) ] <- 'DAY 1'

  data2$VISITDY[(grepl("SCREENING", data2$VISIT, ignore.case = TRUE) & data2$VISITDY == 0 ) ] <- NA

  data2$VISITDY[(grepl("CYCLE 1 Day 1", data2$VISIT, ignore.case = TRUE) & data2$VISITDY == 0 ) ] <- 1

  # Update the start rule if based on the DAY 1 visit
  row_count1 <- data2 %>% filter( VISITDY ==1 & VISIT == ('DAY 1' ))

  if (nrow(row_count1) == 1) {

    data3  <- data2 %>%  mutate (TVENRL =if_else(toupper(VISIT) == "SCREENING" , "One day before start of study drug" , "On the same day of visit")) %>%
      #  mutate (TVSTRL =if_else(toupper(VISIT) == "SCREENING" , "28 days prior to treatment" , "")) %>%
      mutate( TVSTRL = if_else(VISITDY == 1 ,  paste("First dose of treatment phase +/-",data2$due_day,"Days", sep =" "),  paste( data2$VISITDY,"Days +/-", data2$due_day, "Days from Day 1", sep =" ") ))
  }

  if (nrow(row_count1) == 0) {
    data3  <- data2 %>%  mutate (TVENRL =if_else(toupper(VISIT) == "SCREENING" , "One day before start of study drug" , "On the same day of visit")) %>%
      #  mutate (TVSTRL =if_else(toupper(VISIT) == "SCREENING" , "28 days prior to treatment" , "")) %>%
      mutate( TVSTRL = if_else(VISITDY == 1 ,  paste("First dose of treatment phase +/-",data2$due_day,"Days", sep =" "),  paste( data2$VISITDY,"Days +/-", data2$due_day, "Days from Cycle 1 Day 1", sep =" ") ))
  }

  #data3$TVSTRL[grepl("SCREENING", data3$VISIT, ignore.case = TRUE) & data3$VISITDY == -28 ] <- "Informed consent obtained"

  data3$TVSTRL[grepl("SCREENING", data3$VISIT, ignore.case = TRUE) & !(is.na(data3$VISITDY) ) ] <- "Informed consent obtained"


  data3 <- data3 %>% mutate(TVSTRL = case_when(
    VISITDY == 1 & due_day =='0' & TVSTRL == 'First dose of treatment phase +/- 0 Days'  ~
      "First dose of treatment phase", TRUE ~
      TVSTRL ))



  data3$VISIT[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE)) ] <- 'TREATMENT DISCONTINUATION'


  #Reassign the Visit start rule for TVSTRL for treatment/Study discontinuation and for follow up visits

  #data3$TVSTRL[(grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE)) ] <- '30 Days from final dose'

  data3 <- data3 %>% mutate(TVSTRL = case_when(
    grepl("Treatment Discontinuation", VISIT, ignore.case = TRUE) &  !(is.na(VISITDY))  ~
      paste( VISITDY,  "Days from final dose", sep =" "),   TRUE ~
      TVSTRL ))

  data3$TVENRL[grepl("Treatment Discontinuation", data3$VISIT, ignore.case = TRUE) & is.na(data3$TVSTRL)] <- ""

  data3 <- data3 %>% mutate(TVSTRL = case_when(
    grepl("Follow Up Month|Follow-Up Month|Follow - Up Month|FollowUp Month|Long Term Follow Up|Follow Up|Long-Term Follow-Up", VISIT, ignore.case = TRUE) ~
      gsub("Cycle 1 Day 1", "final dose", TVSTRL), TRUE ~
      TVSTRL ))


  # Assign missing visit day  for treatment/Study discontinuation and for followup visits

  data3$VISITDY[(grepl("Treatment Discontinuation|Study Discontinuation|STUDY DRUG DISCONTINUATION", data3$VISIT, ignore.case = TRUE) ) ] <- NA

  #data3$VISITDY[(grepl("Follow Up Month|Follow-Up Month|Follow - Up Month|FollowUp Month|Long Term Follow Up|Follow Up|Long-Term Follow-Up", data3$VISIT, ignore.case = TRUE) ) ] <- NA


  #Assign start and end rule as missing if Screening target day (visitday) is missing

  data3$TVSTRL[grepl("SCREENING", data3$VISIT, ignore.case = TRUE) & is.na(data3$VISITDY)] <- ""

  data3$TVENRL[grepl("SCREENING|Study Discontinuation|STUDY DRUG DISCONTINUATION", data3$VISIT, ignore.case = TRUE) & is.na(data3$VISITDY)] <- ""


  data3$TVSTRL[ data3$VISITDY == 1 & is.na(data3$due_day) ] <- 'First dose of treatment phase'

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
