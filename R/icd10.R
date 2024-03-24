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

#' 2024 ICD-10-CM Codes
#'
#' ICD-10-CM (International Classification of Diseases, 10th Revision,
#' Clinical Modification) is a medical coding system for classifying
#' diagnoses and reasons for visits in U.S. health care settings.
#'
#'
#' @param code vector of ICD-10-CM codes
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' icd10cm(c("F50.8", "G40.311", "Q96.8", "Z62.890", "R45.4",
#'           "E06.3", "H00.019", "D50.1", "C4A.70", "Z20.818"))
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

#' Add ICD-10-CM Chapter Labels
#' @param df data frame
#' @param col column of ICD-10-CM codes to match on
#' @return A [tibble][tibble::tibble-package] with a `chapter` column
#' @examples
#' dplyr::tibble(code = c(
#'               "F50.8", "G40.311", "Q96.8", "Z62.890", "R45.4",
#'               "E06.3", "H00.019", "D50.1", "C4A.70", "Z20.818")) |>
#'               case_chapter_icd10(code)
#' @export
#' @autoglobal
case_chapter_icd10 <- function(df, col) {

  ch <- icd10_chapter_regex()

  df |>
    dplyr::mutate(chapter = dplyr::case_when(
      stringr::str_detect({{ col }}, stringr::regex(ch[1, ]$regex)) == TRUE ~ ch[1, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[2, ]$regex)) == TRUE ~ ch[2, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[3, ]$regex)) == TRUE ~ ch[3, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[4, ]$regex)) == TRUE ~ ch[4, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[5, ]$regex)) == TRUE ~ ch[5, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[6, ]$regex)) == TRUE ~ ch[6, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[7, ]$regex)) == TRUE ~ ch[7, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[8, ]$regex)) == TRUE ~ ch[8, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[9, ]$regex)) == TRUE ~ ch[9, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[10, ]$regex)) == TRUE ~ ch[10, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[11, ]$regex)) == TRUE ~ ch[11, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[12, ]$regex)) == TRUE ~ ch[12, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[13, ]$regex)) == TRUE ~ ch[13, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[14, ]$regex)) == TRUE ~ ch[14, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[15, ]$regex)) == TRUE ~ ch[15, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[16, ]$regex)) == TRUE ~ ch[16, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[17, ]$regex)) == TRUE ~ ch[17, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[18, ]$regex)) == TRUE ~ ch[18, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[19, ]$regex)) == TRUE ~ ch[19, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[20, ]$regex)) == TRUE ~ ch[20, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[21, ]$regex)) == TRUE ~ ch[21, ]$chapter,
      stringr::str_detect({{ col }}, stringr::regex(ch[22, ]$regex)) == TRUE ~ ch[22, ]$chapter,
      TRUE ~ "Unmatched"
    ),
    .after = {{ col }})
}


#' ICD-10-CM Chapter Labels and Regexes
#' @examples
#' icd10_chapter_regex()
#' @noRd
#' @autoglobal
icd10_chapter_regex <- function() {
  dplyr::tibble(
    chapter = c(
      "Certain infectious and parasitic diseases",
      "Neoplasms",
      "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
      "Endocrine, nutritional and metabolic diseases",
      "Mental, behavioral and neurodevelopmental disorders",
      "Diseases of the nervous system",
      "Diseases of the eye and adnexa",
      "Diseases of the ear and mastoid process",
      "Diseases of the circulatory system",
      "Diseases of the respiratory system",
      "Diseases of the digestive system",
      "Diseases of the skin and subcutaneous tissue",
      "Diseases of the musculoskeletal system and connective tissue",
      "Diseases of the genitourinary system",
      "Pregnancy, childbirth and the puerperium",
      "Certain conditions originating in the perinatal period",
      "Congenital malformations, deformations and chromosomal abnormalities",
      "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
      "Injury, poisoning and certain other consequences of external causes",
      "External causes of morbidity",
      "Factors influencing health status and contact with health services",
      "Codes for special purposes"
    ),
    regex = c(
      "^[A-B]",
      "(^[C]|^[D][0-4])",
      "^[D][5-8]",
      "^[E]",
      "^[F]",
      "^[G]",
      "^[H][0-5]",
      "^[H][6-9]",
      "^[I]",
      "^[J]",
      "^[K]",
      "^[L]",
      "^[M]",
      "^[N]",
      "^[O]",
      "^[P]",
      "^[Q]",
      "^[R]",
      "^[S-T]",
      "^[V-Y]",
      "^[Z]",
      "^[U]"
    ),
  )
}
