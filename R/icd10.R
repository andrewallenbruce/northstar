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

#' 2024 National Physician Fee Schedule Relative Value File
#'
#' @param code ICD-10-CM code
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' icd10cm(c("H00.019", "D50.1", "C4A.70", "Z20.818")) |> dplyr::glimpse()
#' @autoglobal
#' @export
icd10cm <- function(code = NULL) {

  icd <- pins::pin_read(mount_board(), "icd10cm")

  if (!is.null(code)) {

    icd <- tidyr::unnest(icd, codes)
    icd <- vctrs::vec_slice(icd,
           vctrs::vec_in(icd$code,
           collapse::funique(code)))
  }
  return(icd)
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
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[A-B]")) == TRUE ~ "Certain Infectious And Parasitic Diseases",
      stringr::str_detect({{ col }}, pattern = stringr::regex("(^[C]|^[D][0-4])")) == TRUE ~ "Neoplasms",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[D][5-8]")) == TRUE ~ "Diseases Of The Blood And Blood-Forming Organs And Certain Disorders Involving The Immune Mechanism",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[E]")) == TRUE ~ "Endocrine, Nutritional And Metabolic Diseases",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[F]")) == TRUE ~ "Mental, Behavioral And Neurodevelopmental Disorders",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[G]")) == TRUE ~ "Diseases Of The Nervous System",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[H][0-5]\\d{1}\\.?\\d?")) == TRUE ~ "Diseases Of The Eye And Adnexa",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[H][6-9]\\d{1}\\.?\\d?")) == TRUE ~ "Diseases Of The Ear And Mastoid Process",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[I]")) == TRUE ~ "Diseases Of The Circulatory System",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[J]")) == TRUE ~ "Diseases Of The Respiratory System",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[K]")) == TRUE ~ "Diseases Of The Digestive System",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[L]")) == TRUE ~ "Diseases Of The Skin And Subcutaneous Tissue",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[M]")) == TRUE ~ "Diseases Of The Musculoskeletal System And Connective Tissue",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[N]")) == TRUE ~ "Diseases Of The Genitourinary System",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[O]")) == TRUE ~ "Pregnancy, Childbirth And The Puerperium",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[P]")) == TRUE ~ "Certain Conditions Originating In The Perinatal Period",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[Q]")) == TRUE ~ "Congenital Malformations, Deformations And Chromosomal Abnormalities",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[R]")) == TRUE ~ "Symptoms, Signs and Abnormal Clinical and Laboratory Findings, Not Elsewhere Classified",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[S-T]")) == TRUE ~ "Injury, Poisoning and Certain Other Consequences of External Causes",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[V-Y]")) == TRUE ~ "External Causes Of Morbidity",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[Z]")) == TRUE ~ "Factors Influencing Health Status And Contact With Health Services",
      stringr::str_detect({{ col }}, pattern = stringr::regex("^[U]")) == TRUE ~ "Codes For Special Purposes",
      TRUE ~ "Unmatched"
    ),
    .after = {{ col }})
}
