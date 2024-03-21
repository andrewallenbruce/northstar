#' Adjustment Codes
#'
#' Claim Adjustment Reason Codes (CARCs) and
#' Remittance Advice Remark Codes (RARCs)
#'
#' @section Claim Adjustment Reason Codes:
#'
#' _X12 External Code Source 139_
#'
#' These codes describe why a claim or service line was paid differently
#' than it was billed and generally assign responsibility for the
#' adjustment amounts. The format is always two alpha characters.
#'
#' The Claim Adjustment Group Codes (e.g., PR, OA) are internal to the X12 standard.
#'
#' @section Remittance Advice Remark Codes:
#'
#' _X12 External Code Source 411_
#'
#' These codes provide additional explanation for an adjustment already
#' described by a Claim Adjustment Reason Code (CARC) or convey information
#' about remittance processing.
#'
#' Remittance Advice Remark Codes (RARCs) are used to provide additional
#' explanation for an adjustment already described by a Claim Adjustment Reason
#' Code (CARC) or to convey information about remittance processing.
#'
#' There are two types of RARCs, supplemental and informational. The majority
#' of the RARCs are supplemental; these are generally referred to as RARCs
#' without further distinction.
#'
#' Supplemental RARCs provide additional explanation for an adjustment already
#' described by a CARC.
#'
#' The second type of RARC is informational; these RARCs are all prefaced
#' with `Alert:` and are often referred to as Alerts.
#'
#' Alerts are used to convey information about remittance processing and are
#' never related to a specific adjustment or CARC.
#' @param df data.frame
#' @param col column of Adjustment codes to match on
#' @param type type of Adjustment code; `all` (default), `carc`, `rarc`
#' @param ... description
#' @return a [dplyr::tibble()]
#' @examples
#' adjustment_codes()$group
#'
#' dplyr::tibble(code = c("CO-253", "OA-23", "PI-185")) |>
#' adjustment_codes(type = "carc", col = "code")
#' @export
#' @autoglobal
adjustment_codes <- function(df   = NULL,
                             col  = NULL,
                             type = c("all", "carc", "rarc"),
                             ...) {

  type <- match.arg(type)

  adj <- pins::pin_read(mount_board(), "rarc_carc")

  if (type == "all") {return(adj)}

  if (type == "carc") {

    adj$carc <- dplyr::select(adj$carc, -c(usage:end_date))

    adj <- df |>
      tidyr::separate_wider_delim({{ col }},
                                  delim = "-",
                                  names = c("group", "code"),
                                  too_few = "align_start") |>
      dplyr::left_join(adj$group, by = dplyr::join_by(group == code)) |>
      dplyr::left_join(adj$carc, by = dplyr::join_by(code == code)) |>
      tidyr::unite("adj_code", group, code, sep = "-", na.rm = TRUE) |>
      dplyr::rename(group = description.x, description = description.y)
  }

  if (type == "rarc") {

    adj$rarc <- dplyr::select(adj$rarc, -c(notes:last_modified))
  }
  return(adj)
}
