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
#' @param addon `<chr>` vector of HCPCS codes
#' @param primary `<chr>` vector of HCPCS codes
#' @param type `<int>` AOC edit type; `1`, `2`, `3`
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_addons(primary = c("39503", "43116", "33935", "11646"))
#' @autoglobal
#' @export
search_addons <- function(addon   = NULL,
                          primary = NULL,
                          type    = NULL,
                          ...) {

  aoc <- pins::pin_read(mount_board(), "aoc")

  if (!is.null(addon)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$addon,
           collapse::funique(addon)))
  }

  if (!is.null(primary)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$primary,
           collapse::funique(primary)))
  }

  if (!is.null(type)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$type,
           collapse::funique(type)))
  }
  return(aoc)
}

#' What type of Add-On Code is a HCPCS Code?
#' @param hcpcs `<chr>` vector of HCPCS codes
#' @param ... Empty
#' @return `<list>` of three `<chr>` vectors: `primary`, `addon`, `both`
#' @examples
#' is_aoc_type(c("11646", "0074T"))
#'
#' c("22633", "22630", "22532", "77001", "88334", "0715T", "64727") |>
#' is_aoc_type()
#' @export
#' @autoglobal
is_aoc_type <- function(hcpcs, ...) {

  hcpcs <- collapse::funique(hcpcs)

  vc <- pins::pin_read(mount_board(), "aoc_vecs")

  list(
    primary = vctrs::vec_slice(hcpcs, vctrs::vec_in(hcpcs, vc$primary)),
    addon   = vctrs::vec_slice(hcpcs, vctrs::vec_in(hcpcs, vc$addon)),
    both    = vctrs::vec_slice(hcpcs, vctrs::vec_in(hcpcs, vc$both))
  ) |>
    purrr::compact(.p = vctrs::vec_is_empty)
}

#' Return a HCPCS Code's Add-On Code Complement
#' @param hcpcs `<chr>` vector of HCPCS codes
#' @param ... Empty
#' @return [tibble][tibble::tibble-package]
#' @examples
#' compare_addons(c("22633", "0074T"))
#'
#' compare_addons(c("22633", "22630", "22532",
#'                  "77001", "88334", "64727"))
#'
#' compare_addons(c("11646", "0074T", "39503",
#'                  "43116", "33935", "11646"))
#'
#' @export
#' @autoglobal
compare_addons <- function(hcpcs, ...) {

  x <- is_aoc_type(hcpcs)

  aoc <- pins::pin_read(mount_board(), "aoc")[c("addon", "primary", "type")]

  both <- list(
    addon = if (!vctrs::vec_is_empty(x$both)) {

      vctrs::vec_slice(aoc,
                       vctrs::vec_in(aoc$addon, x$both)) |>
        tidyr::nest(primary = c(primary, type))

    } else {

      NULL

    },
    primary = if (!vctrs::vec_is_empty(x$both)) {

      vctrs::vec_slice(aoc, vctrs::vec_in(aoc$primary, x$both)) |>
        tidyr::nest(addon = c(addon, type))

    } else {

      NULL

    }
  )

  main <- list(
    addon = if (!vctrs::vec_is_empty(x$addon)) {

      vctrs::vec_slice(aoc, vctrs::vec_in(aoc$addon, x$addon)) |>
        tidyr::nest(primary = c(primary, type))

    } else {

      NULL

    },
    primary = if (!vctrs::vec_is_empty(x$primary)) {

      vctrs::vec_slice(aoc, vctrs::vec_in(aoc$primary, x$primary)) |>
        tidyr::nest(addon = c(addon, type))

    } else {

      NULL

    }
  )

  add <- vctrs::vec_rbind(both$addon, main$addon)

  if (!vctrs::vec_is_empty(add)){

    names(add) <- c("hcpcs", "complement")
    add$identifier <- "addon:primary"

  }

  prim <- vctrs::vec_rbind(both$primary, main$primary)

  if (!vctrs::vec_is_empty(prim)){

    names(prim) <- c("hcpcs", "complement")
    prim$identifier <- "primary:addon"

  }
  vctrs::vec_rbind(add, prim)
}
