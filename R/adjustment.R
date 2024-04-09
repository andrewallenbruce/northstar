#' Search Adjustment Codes
#'
#' CARC and RARC Codes
#'
#' @details Claim Adjustment Reason Codes:
#'
#' _X12 External Code Source 139_
#'
#' These codes describe why a claim or service line was paid differently
#' than it was billed and generally assign responsibility for the
#' adjustment amounts.
#'
#' The Claim Adjustment *Group Codes* are internal to the X12 standard.
#' The format is always two alpha characters:
#' - **CO**: Contractual Obligations
#' - **CR**: Corrections and Reversals
#' - **OA**: Other Adjustments
#' - **PI**: Payer Initiated Reductions
#' - **PR**: Patient Responsibility
#'
#' @details Remittance Advice Remark Codes:
#'
#' _X12 External Code Source 411_
#'
#' Remittance Advice Remark Codes (RARCs) are used to provide additional
#' explanation for an adjustment already described by a Claim Adjustment Reason
#' Code (CARC) or to convey information about remittance processing.
#'
#' There are two types of RARCs: **Supplemental** and **Informational**:
#'
#' The majority of RARCs are *supplemental* and, as such, are generally referred
#' to as RARCs without further distinction. Supplemental RARCs provide additional
#' explanation for an adjustment already described by a CARC.
#'
#' The second type is *informational*; these are all prefaced with `Alert:` and
#' are referred to as Alerts. They are used to convey information about
#' remittance processing and are *never* related to a specific adjustment or CARC.
#'
#' @param df `<data.frame>` data.frame
#'
#' @param col `<chr>` column of Adjustment codes to match on
#'
#' @param type `<chr>` type of Adjustment code; `all` (default), `carc`, `rarc`
#'
#' @param action `<chr>` action to take; `review` (default), `join`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' dplyr::tibble(code = c("CO-253", "OA-23", "PI-185")) |>
#' search_adjustments(col    = code,
#'                    type   = "carc",
#'                    action = "join")
#'
#' @export
#'
#' @autoglobal
search_adjustments <- function(df     = NULL,
                               col    = NULL,
                               type   = c("all", "carc", "rarc"),
                               action = c("review", "join"),
                               ...) {

  type   <- match.arg(type)

  action <- match.arg(action)

  adj    <- pins::pin_read(mount_board(), "rarc_carc")

  if (type == "all") {

    return(adj)

    }

  if (type == "carc" && action == "review") {

    adj$rarc <- NULL

    return(adj)

    }

  if (type == "rarc" && action == "review") {

    return(adj$rarc)

    }

  if (type == "carc" && action == "join") {

    adj$carc <- dplyr::select(adj$carc, -c(usage:end_date))

    adj <- df |>
      tidyr::separate_wider_delim({{ col }},
                                  delim = "-",
                                  names = c("group", "code"),
                                  too_few = "align_start") |>
      dplyr::left_join(adj$group,
                       by = dplyr::join_by(group == code)) |>
      dplyr::left_join(adj$carc,
                       by = dplyr::join_by(code == code)) |>
      tidyr::unite("adj_code",
                   group,
                   code,
                   sep = "-",
                   na.rm = TRUE) |>
      dplyr::rename(group = description.x,
                    description = description.y)

  }
  return(adj)
}
