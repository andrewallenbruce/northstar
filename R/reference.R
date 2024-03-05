#' @noRd
global_days <- function() {

  dplyr::tibble(
  glob_days = c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ"),
  description = c(
    "Endoscopic or minor procedure with related preoperative and postoperative relative values on the day of the procedure only included in the fee schedule payment amount. Evaluation and Management services on the day of the procedure generally not payable.",
    "Minor procedure with preoperative relative values on the day of the procedure and postoperative relative values during a 10-day postoperative period included in the fee schedule amount. Evaluation and Management services on the day of the procedure and during the 10-day postoperative period generally not payable.",
    "Major surgery with a 1-day preoperative period and 90-day postoperative period included in the fee schedule amount.",
    "Maternity codes; usual global period does not apply.",
    "Global concept does not apply.",
    "Carrier determines whether the global concept applies and establishes postoperative period, if appropriate, at time of pricing.",
    "Code is related to another service and is always included in the global period of the other service."))
}

#' @autoglobal
#' @noRd
status_codes <- function() {

  status_codes <- dplyr::tribble(
  ~code, ~name,
  "A", "Active Code",
  "B", "Payment Bundled",
  "C", "Carrier Priced",
  "D", "Deleted Codes",
  "E", "Regulatory Exclusion",
  "F", "Deleted/Discontinued Codes",
  "X", "Statutory Exclusion",
  "I", "Not Valid for Medicare Purposes",
  "M", "Measurement Code",
  "R", "Restricted Coverage",
  "N", "Non-Covered Service",
  "J", "Anesthesia Service",
  "P", "Bundled/Excluded Code",
  "T", "No Other Services Payable")

  lookup <- c(
    "A" = "Separately paid under the Physician Fee Schedule if covered. There will be RVUs and payment amounts. Does not mean that Medicare has made a National Coverage Determination regarding the service. Carriers remain responsible for coverage decisions in the absence of a national Medicare policy.",
    "X" = "Item or service that is not in the statutory definition of 'physician services' for fee schedule payment purposes. No RVUs or payment amounts are shown for these codes and no payment may be made under the physician fee schedule. Ex: Ambulance Services and Clinical Diagnostic Laboratory Services.",
    "I" = "Medicare uses another code for reporting of, and payment for, these services. Code is NOT subject to a 90-day grace period.",
    "E" = "Item or service that CMS chose to exclude from the fee schedule payment by regulation. No RVUs or payment amounts are shown and no payment may be made under the fee schedule. Payment for them, when covered, continues under reasonable charge procedures.",
    "M" = "Used for reporting purposes only.",
    "C" = "Carriers will establish RVUs and payment amounts for these services, generally on an individual case basis following review of documentation such as an operative report.",
    "R" = "Special coverage instructions apply.",
    "N" = "Noncovered services.",
    "J" = "No RVUs or payment amounts for anesthesia codes on the database, only used to facilitate the identification of anesthesia services.",
    "P" = "No RVUs and no payment amounts for these services. No separate payment is made for them under the fee schedule. If the item or service is covered as incident to a physician service and is provided on the same day as a physician service, payment for it is bundled into the payment for the physician service to which it is incident (an example is an elastic bandage furnished by a physician incident to a physician service). If the item or service is covered as other than incident to a physician service, it is excluded from the fee schedule (for example, colostomy supplies) and is paid under the other payment provision of the Act.",
    "B" = "Payment for covered services are always bundled into payment for other services not specified. No RVUs or payment amounts and no separate payment is ever made. When these services are covered, payment for them is subsumed by the payment for the services to which they are incident (an example is a telephone call from a hospital nurse regarding care of a patient).",
    "T" = "There are RVUs and payment amounts for these services, but they are only paid if there are no other services payable under the physician fee schedule billed on the same date by the same provider. If any other services payable under the physician fee schedule are billed on the same date by the same provider, these services are bundled into the physician services for which payment is made.")

  status_codes |>
  dplyr::mutate(description = lookup[code])
}
