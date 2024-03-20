#' Search the NLM's ICD-10-CM API
#'
#' @description [icd10_search()] allows you to search the National Library of
#'    Medicine's ICD-10-CM API by code or associated term.
#'
#' @details ICD-10-CM (International Classification of Diseases, 10th Revision,
#' Clinical Modification) is a medical coding system for classifying
#' diagnoses and reasons for visits in U.S. health care settings.
#'
#' ## Links
#'  * [NIH NLM Clinical Table Service ICD-10-CM API](https://clinicaltables.nlm.nih.gov/apidoc/icd10cm/v3/doc.html)
#'  * [Learn more about ICD-10-CM.](http://www.cdc.gov/nchs/icd/icd10cm.htm)
#'
#' @note Current Version: ICD-10-CM **2024**
#' @source National Institute of Health/National Library of Medicine
#'
#' @param code All or part of an ICD-10-CM code
#' @param term Associated term describing an ICD-10 code
#' @param field options are "code" or "both"; default is "both"
#' @param limit API limit is 500; defaults to 10
#'
#' @return A [tibble][tibble::tibble-package] containing the search results.
#'
#' @examples
#' # Returns the seven codes beginning with "A15"
#' icd10_search(code = "A15")
#'
#' # Returns the first five codes associated with tuberculosis
#' icd10_search(term = "tuber", limit = 5)
#'
#' # Returns the two codes associated with pleurisy
#' icd10_search(term = "pleurisy")
#'
#' # If you're searching for codes beginning with a certain letter, you
#' # must set the `field` param to "code" or it will search for terms as well:
#'
#' # Returns terms containing the letter "Z"
#' icd10_search(code = "z", limit = 5)
#'
#' # Returns codes beginning with "Z"
#' icd10_search(code = "z", field = "code", limit = 5)
#' @autoglobal
#' @export
icd10_search <- function(code  = NULL,
                         term  = NULL,
                         field = c("both", "code"),
                         limit = 500) {

  stopifnot("Both `code` and `term` cannot be NULL" = all(!is.null(c(code, term))))

  args <- stringr::str_c(
    c(code = code,
      term = term),
    collapse = ",")

  field <- match.arg(field)

  switch(
      field,
      "code" = field <- "code",
      "both" = field <- "code,name",
      stop('`field` must be either `"code"` or `"both"`')
      )

  results <- httr2::request(
    "https://clinicaltables.nlm.nih.gov/api/icd10cm/v3/search?") |>
    httr2::req_url_query(terms   = args,
                         maxList = 500,
                         count   = limit,
                         offset  = 0L,
                         sf      = field) |>
    httr2::req_perform() |>
    httr2::resp_body_json(check_type     = TRUE,
                          simplifyVector = TRUE,
                          simplifyMatrix = TRUE)

  count <- results[[1]]

  if (limit < 500L | count <= 500) {
    results <- results[[4]] |>
      as.data.frame() |>
      dplyr::rename(code        = V1,
                    description = V2) |>
      dplyr::tibble()
  }

  if (limit == 500L && count > 500L) {

    pgs <- 1:round(count / 500) * 500

    res2 <- purrr::map(pgs, \(x) .multiple_request(offset = x, args = args, field = field)) |>
      purrr::list_rbind()

    results <- results[[4]] |>
      as.data.frame() |>
      dplyr::rename(code        = V1,
                    description = V2) |>
      dplyr::tibble() |>
      vctrs::vec_rbind(res2)
  }
  return(results)
}


#' @autoglobal
#' @noRd
.multiple_request <- function(offset, args, field) {

  results <- httr2::request(
    "https://clinicaltables.nlm.nih.gov/api/icd10cm/v3/search?") |>
    httr2::req_url_query(terms   = args,
                         maxList = 500,
                         count   = 500,
                         offset  = offset,
                         sf      = field) |>
    httr2::req_perform() |>
    httr2::resp_body_json(check_type     = TRUE,
                          simplifyVector = TRUE,
                          simplifyMatrix = TRUE)


  if (vctrs::vec_is_empty(results[[4]])) {return(NULL)}

 results[[4]] |>
    as.data.frame() |>
    dplyr::rename(code        = V1,
                  description = V2) |>
    dplyr::tibble()

}

#' Add ICD-10-CM Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `section` column
#' @examples
#' dplyr::tibble(code = c("F50.8", "G40.311", "Q96.8",
#'                        "Z62.890", "R45.4", "E06.3")) |>
#'                        case_section_icd10(code)
#' @export
#' @autoglobal
case_section_icd10 <- function(df, col) {

  df |>
    dplyr::mutate(section = dplyr::case_when(
      stringr::str_detect({{ col }}, pattern = stringr::regex("(^[A]|^[B])")) == TRUE ~ "Certain Infectious and Parasitic Diseases [A00 - B99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("(^[C]|^[D][0-4])")) == TRUE ~ "Neoplasms [C00 - D49]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[D][5-8]")) == TRUE ~ "Diseases of the Blood and Blood-Forming Organs and Certain Disorders Involving the Immune Mechanism [D50 - D89]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[E]")) == TRUE ~ "Endocrine, Nutritional and Metabolic Diseases [E00 - E89]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[F]")) == TRUE ~ "Mental, Behavioral and Neurodevelopmental Disorders [F01 - F99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[G]")) == TRUE ~ "Diseases of the Nervous System [G00 - G99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[H][0-5]\\d{1}\\.?\\d?")) == TRUE ~ "Diseases of the Eye and Adnexa [H00 - H59]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[H][6-9]\\d{1}\\.?\\d?")) == TRUE ~ "Diseases of the Ear and Mastoid Process [H60 - H95]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[I]")) == TRUE ~ "Diseases of the Circulatory System [I00 - I99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[J]")) == TRUE ~ "Diseases of the Respiratory System [J00 - J99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[K]")) == TRUE ~ "Diseases of the Digestive System [K00 - K95]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[L]")) == TRUE ~ "Diseases of the Skin and Subcutaneous Tissue [L00 - L99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[M]")) == TRUE ~ "Diseases of the Musculoskeletal System and Connective Tissue [M00 - M99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[N]")) == TRUE ~ "Diseases of the Genitourinary System [N00 - N99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[O]")) == TRUE ~ "Pregnancy, Childbirth and the Puerperium [O00 - O9A]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[P]")) == TRUE ~ "Certain Conditions Originating in the Perinatal Period [P00 - P96]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[Q]")) == TRUE ~ "Congenital Malformations, Deformations and Chromosomal Abnormalities [Q00 - Q99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[R]")) == TRUE ~ "Symptoms, Signs and Abnormal Clinical and Laboratory Findings, Not Elsewhere Classified [R00 - R99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("(^[S]|^[T])")) == TRUE ~ "Injury, Poisoning and Certain Other Consequences of External Causes [S00 - T88]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("(^[V]|^[W]|^[X]|^[Y])")) == TRUE ~ "External Causes of Morbidity [V00 - Y99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[Z]")) == TRUE ~ "Factors Influencing Health Status and Contact with Health Services [Z00 - Z99]",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[U]")) == TRUE ~ "Codes for Special Purposes [U00 - U85]",
      TRUE ~ "Unmatched"
    ),
    .after = {{ col }})
}
