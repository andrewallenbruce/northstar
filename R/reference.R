#' @noRd
global <- dplyr::tibble(
  glob_days = c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ"),
  description = c(
    "Endoscopic or minor procedure with related preoperative and postoperative relative values on the day of the procedure only included in the fee schedule payment amount. Evaluation and Management services on the day of the procedure generally not payable.",
    "Minor procedure with preoperative relative values on the day of the procedure and postoperative relative values during a 10-day postoperative period included in the fee schedule amount. Evaluation and Management services on the day of the procedure and during the 10-day postoperative period generally not payable.",
    "Major surgery with a 1-day preoperative period and 90-day postoperative period included in the fee schedule amount.",
    "Maternity codes; usual global period does not apply.",
    "Global concept does not apply.",
    "Carrier determines whether the global concept applies and establishes postoperative period, if appropriate, at time of pricing.",
    "Code is related to another service and is always included in the global period of the other service."
  )
) |>
  dplyr::mutate(
    glob_days = forcats::fct_relevel(glob_days, "000", "010", "090", "MMM", "XXX", "YYY", "ZZZ")
  )
