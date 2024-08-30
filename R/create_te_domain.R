library(dplyr)
library(openxlsx)

#' Generate TE Domain from TA Domain
#'
#' This function generates the TE domain from the TA domain using the specifications from SDTM IG 3.4.
#'
#' @param ta_df A data frame representing the TA domain.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @return A data frame representing the TE domain.
#' @examples
#' ta_df <- data.frame(STUDYID = "STUDY001", DOMAIN = "TA", ARMCD = "ARM1", ARM = "Group 1",
#'                     TAETORD = 1, ETCD = "ET1", ELEMENT = "Screening", TABRANCH = NA, TATRANS = NA, EPOCH = "Screening")
#' te_rules <- data.frame(
#'   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of washout"),
#'   TEENRL = c("End of screening", "End of treatment period", "End of washout period", "End of follow-up period"),
#'   TEDUR = c("P7D", "P14D", "P7D", "P21D")
#' )
#' te_df <- create_te_domain(ta_df, te_rules)
#' print(te_df)
#' @export
create_te_domain <- function(ta_df, te_rules) {
  # Extract unique elements from the TA domain
  unique_elements <- ta_df %>%
    distinct(ELEMENT) %>%
    mutate(
      ETCD = paste0("ET", row_number()),
      DOMAIN = "TE"
    )
  
  # Merge the unique elements with the provided TE rules
  te_df <- unique_elements %>%
    left_join(te_rules, by = "ELEMENT") %>%
    select(STUDYID = ta_df$STUDYID[1], DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)
  
  return(te_df)
} 
