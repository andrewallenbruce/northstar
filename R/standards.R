#' Describe HCPCS Codes
#'
#' @template args-hcpcs
#'
#' @param column If `hcpcs_code` is a [data.frame] or a
#'   [tibble][tibble::tibble-package], this is the quoted name of the column
#'   containing HCPCS codes; default is `"hcpcs_code"`
#'
#' @param hcpcs_desc_type `<chr>` vector of code description types; `All` (default),
#'   `Short`, `Long`, `Medium`, `Medical`, `Consumer`, `Clinician`, `Proprietary Name`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_descriptions(hcpcs_code = c("39503", "43116", "33935", "11646"))
#'
#' search_descriptions(
#'   hcpcs_code = dplyr::tibble(
#'   code = c("A0021", "V5362", "J9264", "G8916")),
#'   column = "code")
#'
#' dplyr::tibble(
#'   code = c("A0021", "V5362", "J9264", "G8916")) |>
#'   search_descriptions(column = "code")
#'
#' search_descriptions() |>
#' dplyr::filter(is_cpt_category_III(hcpcs_code))
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_descriptions <- function(hcpcs_code = NULL, hcpcs_desc_type = "All", column = "hcpcs_code", ...) {

  hcp <- get_pin("hcpcs_descriptions")

  hcpcs_desc_type <- match.arg(
    hcpcs_desc_type,
    c("All", "Short", "Long",
      "Medium", "Medical", "Consumer",
      "Clinician", "Proprietary Name"),
    several.ok = TRUE
  )

  if (not_null(hcpcs_code)) {

    obj_type <- names(
      which(c(vec = is.vector(hcpcs_code),
              dfr = is.data.frame(hcpcs_code))))

    hcp <- switch(
      obj_type,
      vec = search_in(hcp, "hcpcs_code", hcpcs_code),
      dfr = search_in(hcp, "hcpcs_code", hcpcs_code[[column]]))
  }

  if (hcpcs_desc_type != "All") {

    hcp <- search_in(hcp, "hcpcs_desc_type", hcpcs_desc_type)
  }
  return(.add_class(hcp))
}

#' Level I and II HCPCS Modifiers
#'
#' A modifier provides the means to report or indicate that a service or
#' procedure that has been performed has been altered by some specific
#' circumstance but not changed in its definition or code.
#'
#' Modifiers also enable health care professionals to effectively respond to
#' payment policy requirements established by other entities.
#'
#' @param mod_code `<chr>` vector of 2-character HCPCS modifiers; default is
#'   `NULL`
#'
#' @param mod_type `<chr>` Modifier type, one of `HCPCS`, `CPT`,
#'   `Anesthesia`, or `Performance Measure`; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_modifiers(mod_code = c("25", "59"))
#'
#' search_modifiers(mod_type = "CPT")
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_modifiers <- function(mod_code = NULL, mod_type = NULL, ...) {

  md <- get_pin("modifiers")
  md <- search_in(md, "mod_type", mod_type)
  md <- search_in(md, "mod_code", mod_code)

  return(.add_class(md))
}

#' Level II HCPCS Codes
#'
#' The Healthcare Common Procedure Coding System (HCPCS) is a collection of
#' codes that represent procedures, supplies, products and services which may be
#' provided to Medicare beneficiaries and to individuals enrolled in private
#' health insurance programs. The codes are divided into two levels, or groups,
#' as described below.
#'
#'
#'    * Level I Codes and descriptors copyrighted by the American Medical
#'    Association's current procedural terminology, fourth edition (CPT-4). These
#'    are 5 position numeric codes representing physician and non-physician
#'    services.
#'
#'    * Level II Includes codes and descriptors copyrighted by the American Dental
#'    Association's current dental terminology, (CDT-2023). These are 5 position
#'    alpha-numeric codes comprising the d series.
#'
#'
#' All level II codes and descriptors are approved and maintained jointly by the
#' alpha-numeric editorial panel (consisting of CMS, the Health Insurance
#' Association of America, and the Blue Cross and Blue Shield Association).
#'
#' These are 5 position alpha-numeric codes representing primarily items and
#' non-physician services that are not represented in the level I codes.
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_hcpcs(hcpcs_code = c("C9039", "J1835"))
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_hcpcs <- function(hcpcs_code = NULL, ...) {

  lv <- get_pin("hcpcs_lvl2")
  lv <- search_in(lv, "hcpcs_code", hcpcs_code)
  return(.add_class(lv))

}


#' Proprietary Laboratory Analyses (PLA) Codes
#'
#'
#' ## U codes
#' <https://www.ama-assn.org/practice-management/cpt/cpt-pla-codes>
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_plas()
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_plas <- function(hcpcs_code = NULL, ...) {

  pla <- get_pin("cpt_pla")
  pla <- search_in(pla, "hcpcs_code", hcpcs_code)
  return(.add_class(pla))

}

#' Place of Service (POS) Codes
#'
#' Place of Service Codes are two-digit codes placed on health care professional
#' claims to indicate the setting in which a service was provided. The Centers
#' for Medicare & Medicaid Services (CMS) maintain POS codes used throughout the
#' health care industry.
#'
#' This code set is required for use in the implementation guide adopted as the
#' national standard for electronic transmission of professional health care
#' claims under the provisions of the Health Insurance Portability and
#' Accountability Act of 1996 (HIPAA).
#'
#' HIPAA directed the Secretary of HHS to adopt national standards for
#' electronic transactions. These standard transactions require all health plans
#' and providers to use standard code sets to populate data elements in each
#' transaction.
#'
#' The Transaction and Code Set Rule adopted the ASC X12N-837 Health Care Claim:
#' Professional, volumes 1 and 2, version 4010, as the standard for electronic
#' submission of professional claims.
#'
#' This standard names the POS code set currently maintained by CMS as the code
#' set to be used for describing sites of service in such claims. POS
#' information is often needed to determine the acceptability of direct billing
#' of Medicare, Medicaid and private insurance services provided by a given
#' provider.
#'
#' ## Code Structure
#'
#' The first digit denotes service category and the second specifies location or
#' service type.
#'
#' POS code 11 represents an Office for direct patient services.
#'
#' For instance, code 11 designates an Office, encompassing
#' physician offices, clinics, group practices, and standalone facilities
#' providing direct patient services.
#'
#' Conversely, place of service code 22
#' denotes an On Campus-Outpatient Hospital, covering services in a
#' hospital-based outpatient department where the patient is admitted as an
#' outpatient.
#'
#'    * Structure: First digit for category, second for location/type
#'    * Choose POS based on the majority of services in a specific encounter
#'    * Modify POS codes if the location changes during a single encounter
#'
#' ## Location Types
#'
#' Based on the location of service, POS codes can be grouped into four
#' categories: Facility, Non-Facility, Telehealth, and Other.
#'
#' ### Facility Codes
#'
#' Facility POS codes are used to indicate services provided in a facility
#' setting such as hospitals, nursing homes, or skilled nursing facilities.
#' These include:
#'
#'    * Urgent Care Facility (20)
#'    * Inpatient Hospital (21)
#'    * Outpatient Hospital (22)
#'    * Emergency Room-Hospital (23)
#'    * Ambulatory Surgical Center (24)
#'    * Skilled Nursing Facility (31)
#'    * Hospice Facility (32)
#'
#' These codes indicate that the services were provided in a facility that is
#' owned and operated by a healthcare provider.
#'
#' ### Non-Facility Codes
#'
#' Non-facility POS codes are used for services provided in non-facility
#' settings such as physician offices or independent clinics. These include:
#'
#'    * School (03)
#'    * Office (11)
#'    * Home (12)
#'    * Independent Clinic (49)
#'
#' The above codes indicate that the services were provided in a setting not
#' owned or operated by a healthcare provider.
#'
#' ### Telehealth POS Codes
#'
#' Telehealth place-of-service codes are used to indicate services provided
#' through telecommunication technology. These include:
#'
#'    * Telehealth (02)
#'    * Store and Forward Telemedicine Services (18)
#'
#' This category of POS codes was introduced due to the increasing use of
#' telehealth services in healthcare, and to differentiate them from traditional
#' in-person services.
#'
#' ### Other POS Codes
#'
#' In addition to facility-specific POS codes, there are other codes that play a
#' vital role in accurately describing healthcare encounters. These codes are
#' designed for specific scenarios, such as visits to retail clinics, public
#' health clinics, or rural health clinics. Each code serves a distinct purpose
#' in the healthcare landscape.
#'
#'    * Homeless Shelter (04)
#'    * Retail Clinic (17)
#'    * Rural Health Clinic (72)
#'
#' These codes, including Retail Clinic (17), Home Shelter (04), and Rural
#' Health Clinic (72), cater to unique healthcare settings, ensuring
#' comprehensive coverage in billing and reporting. Understanding and applying
#' these codes appropriately contributes to the precision of healthcare
#' documentation and facilitates effective reimbursement processes.
#'
#' @param pos_code `<chr>` vector of 2-character Place of Service codes; default
#'   is `NULL`
#'
#' @param pos_type `<chr>` Place of Service type, one of `Facility` or
#'   `Non-Facility`; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_pos(pos_code = c("11", "21"))
#'
#' search_pos(pos_type = "Facility")
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_pos <- function(pos_code = NULL, pos_type = NULL, ...) {

  pos <- get_pin("pos_codes")
  pos <- search_in(pos, "pos_type", pos_type)
  pos <- search_in(pos, "pos_code", pos_code)
  return(.add_class(pos))
}


#' Restructured BETOS Classifications
#'
#' [search_rbcs()] allows the user to group HCPCS codes into clinically
#' meaningful categories based on the original _Berenson-Eggers Type of Service_
#' (BETOS) classification.
#'
#' @section From BETOS to RBCS:
#'
#' The Restructured BETOS Classification System (RBCS) is a taxonomy that allows
#' researchers to group Medicare Part B healthcare service codes into clinically
#' meaningful categories and subcategories.
#'
#' Based on the original Berenson-Eggers Type of Service (BETOS) classification
#' created in the 1980s, it includes notable updates such as Part B non-physician
#' services and undergoes annual updates by a technical expert panel of
#' researchers and clinicians.
#'
#' The general framework for grouping service codes into the new RBCS taxonomy
#' largely follows the same structure of BETOS. Like BETOS, the RBCS groups
#' HCPCS codes into categories, subcategories, and families – with categories
#' as the most aggregate level and families as the more granular level.
#'
#' All Medicare Part B service codes, including non-physician services, are
#' assigned to a 6-character RBCS taxonomy code.
#'
#' @section Links:
#'
#' + [Restructured BETOS Classification System](https://data.cms.gov/provider-summary-by-type-of-service/provider-service-classifications/restructured-betos-classification-system)
#' + [RBCS Data Dictionary](https://data.cms.gov/resources/restructured-betos-classification-system-data-dictionary)
#'
#' @section Update Frequency: Annually
#'
#' @template args-hcpcs
#'
#' @param category `<chr>` vector of RBCS categories:
#' + `Procedure` (n = 6920)
#' + `Test` (n = 3015)
#' + `DME` (n = 2971)
#' + `Treatment` (n = 1795)
#' + `Imaging` (n = 1097)
#' + `E&M` (n = 695)
#' + `Anesthesia` (n = 307)
#' + `Other` (n = 233)
#'
#' @param subcategory `<chr>` vector of RBCS subcategories (53 unique)
#'
#' @param family `<chr>` vector of RBCS families (178 unique)
#'
#' @param procedure `<chr>` Procedure Type:
#' * `Major` (n = 3676)
#' * `Non-Procedure` (n = 10113)
#' * `Other` (n = 3244)
#'
#' @param concatenate `<lgl>` Concatenate output, default is `TRUE`
#'
#' @template args-dots
#'
#' @returns A [tibble][tibble::tibble-package] with the columns:
#'
#' |**Column**          |**Description**                              |
#' |:-------------------|:--------------------------------------------|
#' |`hcpcs`             |HCPCS or CPT code                            |
#' |`rbcs`              |RBCS Identifier                              |
#' |`rbcs_category`     |RBCS Category                                |
#' |`rbcs_subcategory`  |RBCS Subcategory                             |
#' |`rbcs_family`       |RBCS Family                                  |
#' |`rbcs_procedure`    |RBCS Major Procedure Indicator               |
#' |`date_hcpcs_add`    |Date HCPCS Code was added                    |
#' |`date_hcpcs_end`    |Date HCPCS Code was no longer effective      |
#' |`date_rbcs_assign`  |Earliest Date that the RBCS ID was effective |
#'
#' @examples
#' search_rbcs(hcpcs_code = c("J9264", "39503", "43116", "70170", "0001U"))
#'
#' search_rbcs(hcpcs_code = "0001U")
#'
#' @autoglobal
#'
#' @family HIPAA Standards
#'
#' @export
search_rbcs <- function(hcpcs_code  = NULL,
                        category    = NULL,
                        subcategory = NULL,
                        family      = NULL,
                        procedure   = NULL,
                        concatenate = FALSE,
                        ...) {

  rb <- get_pin("rbcs")

  rb <- search_in(rb, "hcpcs_code", hcpcs_code)
  rb <- search_in(rb, "rbcs_family", family)
  rb <- search_in(rb, "rbcs_subcategory", subcategory)

  if (not_null(procedure)) {

    procedure <- rlang::arg_match(
      procedure,
      c("Major", "Non-Procedure", "Other"),
      multiple = TRUE)

    rb <- search_in(rb, "rbcs_procedure", procedure)
  }

  if (not_null(category)) {

    category <- rlang::arg_match(
      category,
      c("Procedure", "Test", "DME", "Treatment",
        "Imaging", "E&M", "Anesthesia", "Other"),
      multiple = TRUE)

    rb <- search_in(rb, "rbcs_category", category)
  }

  if (concatenate) {

    rb <- tidyr::unite(
      rb,
      "rbcs_category",
      c(rbcs_procedure, rbcs_category),
      sep = " ") |>
      tidyr::unite(
        "rbcs_family",
        c(rbcs_subcategory, rbcs_family),
        sep = ": ",
        na.rm = TRUE) |>
      dplyr::select(
        hcpcs_code,
        rbcs_category,
        rbcs_family)
  }
  return(.add_class(rb))
}
