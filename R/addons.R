#' NCCI Add-On Codes
#'
#' Medicare NCCI Add-On Code Edits
#'
#' An AOC is a HCPCS/CPT code that describes a service that, with rare
#' exception, is performed in conjunction with another primary service by the
#' same practitioner. An AOC is rarely eligible for payment if it is the only
#' procedure reported by a practitioner.
#'
#' Some CPT codes are identified as Add-on Codes (AOCs), which describe a
#' service that can only be reported in addition to a primary procedure.
#'
#' For specific primary codes, AOCs shall not be reported as a supplemental
#' service for other HCPCS/CPT codes not listed as a primary code.
#'
#' AOCs permit the reporting of significant supplemental services commonly
#' performed in addition to the primary procedure. By contrast, incidental
#' services that are necessary to accomplish the primary procedure (e.g.,
#' lysis of adhesions in the course of an open cholecystectomy) are not
#' separately reportable with an AOC. Similarly, complications inherent in an
#' invasive procedure occurring during the procedure are not separately
#' reportable. For example, control of bleeding during an invasive procedure
#' is considered part of the procedure and is not separately reportable.
#'
#' Although the AOC and primary code are normally reported for the same date
#' of service, there are unusual circumstances where the two services may be
#' reported for different dates of service (e.g., 99291 and 99292).
#'
#' ## AOC Edit Types
#' **Type 1**: CPT Professional or HCPCS files define all acceptable primary codes.
#' MACs should not allow other primary codes with Type 1 AOCs.
#'
#' **Type 2**: CPT Professional and HCPCS files do not define any primary codes.
#' MACs should develop their own lists of acceptable primary codes.
#'
#' **Type 3**: CPT Professional or HCPCS files define some, but not all, acceptable
#' primary codes. MACs should allow the listed primary codes for these AOCs but
#' may develop their own lists of additional acceptable primary codes.
#'
#' ## PTP Edits
#' In general, NCCI PTP edits do not include edits with most AOCs because edits
#' related to the primary procedure(s) are adequate to prevent inappropriate
#' payment for an add-on coded procedure (i.e., if an edit prevents payment of
#' the primary procedure code, the AOC shall not be paid).
#'
#' @note Version: 2024-04-01
#'
#' [Add-Ons Link](https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits)
#'
#' @param hcpcs `<chr>` vector of HCPCS codes
#'
#' @param edit_type `<int>` AOC edit type; `1`, `2`, `3`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_addons(hcpcs = c("22633", "0074T"))
#'
#' get_addons(hcpcs = c("22630", "22532", "77001", "88334", "64727"))
#'
#' get_addons(hcpcs = c("11646", "39503", "43116", "33935"))
#'
#' @autoglobal
#'
#' @export
get_addons <- function(hcpcs     = NULL,
                       edit_type = NULL,
                       ...) {

    aoc_long <- pins::pin_read(
      mount_board(),
      "aoc_long"
      )

  if (!is.null(hcpcs)) {

    aoc_long <- search_in(
      aoc_long,
      aoc_long$hcpcs,
      unlist(
        get_aoc_type(hcpcs),
        use.names = FALSE
        )
      )
    }

  if (!is.null(edit_type)) {

    edit_type <- rlang::arg_match(
      edit_type,
      c("1", "2", "3"),
      multiple = TRUE
      )

    aoc_long  <- search_in(
      aoc_long,
      aoc_long$edit_type,
      edit_type
      )
  }
  return(aoc_long)
}

#' What type of Add-On Code is a HCPCS Code?
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @returns `<list>` of four `<chr>` vectors indicating group membership:
#'   `primary`, `addon`, `both`, and `neither`
#'
#' @examples
#' get_aoc_type(c("11646", "0074T"))
#'
#' c("22633", "22630", "22532", "77001", "88334", "0715T", "64727") |>
#' get_aoc_type()
#'
#' @export
#'
#' @autoglobal
get_aoc_type <- function(hcpcs, ...) {

  hcpcs <- collapse::funique(hcpcs)

  vc <- pins::pin_read(mount_board(), "aoc_vecs")

  list(
    primary = search_in(hcpcs, hcpcs, vc$primary),
    addon   = search_in(hcpcs, hcpcs, vc$addon),
    both    = search_in(hcpcs, hcpcs, vc$both),
    neither = vctrs::vec_set_difference(
    hcpcs, vctrs::vec_c(vc$both, vc$primary, vc$addon))
    )
}
