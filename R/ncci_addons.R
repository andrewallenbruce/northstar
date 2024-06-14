#' NCCI Add-On Code Edits
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
#'    * Type 1: CPT Professional or HCPCS files define all acceptable primary codes. MACs should not allow other primary codes with Type 1 AOCs.
#'    * Type 2: CPT Professional and HCPCS files do not define any primary codes. MACs should develop their own lists of acceptable primary codes.
#'    * Type 3: CPT Professional or HCPCS files define some, but not all, acceptable primary codes. MACs should allow the listed primary codes for these AOCs but may develop their own lists of additional acceptable primary codes.
#'
#' ## PTP Edits
#' In general, NCCI PTP edits do not include edits with most AOCs because edits
#' related to the primary procedure(s) are adequate to prevent inappropriate
#' payment for an add-on coded procedure (i.e., if an edit prevents payment of
#' the primary procedure code, the AOC shall not be paid).
#'
#' @note Version: 2024-04-01
#'
#'    * [NCCI Add-On Code Edits](https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits)
#'
#' @template args-hcpcs
#'
#' @param aoc_type `<chr>` AOC type; `addon` or `primary`
#'
#' @param edit_type `<int>` AOC edit type:
#'    * `1`: Only Paid if Primary is Paid. Payment Eligible if Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'    * `2`: Some Specific Primaries. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'    * `3`: No Specific Primary Codes. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'
#' @param current `<lgl>` return only current edits, default is `TRUE`
#'
#' @param na.rm remove empty rows and columns; default is `TRUE`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_addon_edits(hcpcs     = c("22633", "0074T"),
#'                 aoc_type  = "primary",
#'                 edit_type = 1)
#'
#' get_addon_edits(hcpcs = c("22630",
#'                           "22532",
#'                           "77001",
#'                           "88334",
#'                           "64727"))
#'
#' get_addon_edits(hcpcs = c("11646",
#'                           "39503",
#'                           "43116",
#'                           "33935"))
#'
#' @autoglobal
#'
#' @export
get_addon_edits <- function(hcpcs     = NULL,
                            aoc_type  = NULL,
                            edit_type = NULL,
                            current   = TRUE,
                            na.rm     = TRUE,
                            ...) {

  aoc <- get_pin("aoc_long")
  # nms <- c("primary", "addon", "both")

  aoc <- fuimus::search_in_if(
    aoc,
    aoc$hcpcs,
    hcpcs)

  aoc  <- fuimus::search_in_if(
    aoc,
    aoc$aoc_edit_type,
    edit_type)

  # TODO
  aoc <- fuimus::search_in_if(
    aoc,
    aoc$aoc_type,
    aoc_type)

  if (current) {
    aoc <- vctrs::vec_slice(
      aoc,
      is.na(aoc$aoc_edit_deleted)
    )
  }

  if (na.rm) {
    aoc <- remove_quiet(aoc)
  }

  return(.add_class(aoc))
}

#' HCPCS Add-On code type
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @returns A `<list>` of four `<chr>` vectors, indicating set membership:
#'   `primary`, `addon`, `both`, and `neither`
#'
#' @examples
#' get_aoc_type(hcpcs = c("11646", "0074T"))
#'
#' c("22633", "22630", "22532", "77001", "88334", "0715T", "64727") |>
#' get_aoc_type()
#'
#' @export
#'
#' @autoglobal
get_aoc_type <- function(hcpcs, ...) {

  hcpcs <- collapse::funique(hcpcs)
  vc    <- get_pin("aoc_vecs")

  list(
    primary = fuimus::search_in(hcpcs, hcpcs, vc$primary),
    addon   = fuimus::search_in(hcpcs, hcpcs, vc$addon),
    both    = fuimus::search_in(hcpcs, hcpcs, vc$both),
    neither = vctrs::vec_set_difference(hcpcs,
              vctrs::vec_c(vc$both, vc$primary, vc$addon))
    )
}
