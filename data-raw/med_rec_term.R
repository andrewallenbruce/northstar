medical_records <- arrow::read_parquet(
  "C:/Users/Andrew/Desktop/train-00000-of-00001-67e4e7207342a623.parquet"
  ) |>
  dplyr::collect() |>
  tidyr::unpack(
    cols = inputs,
    names_sep = "_"
    ) |>
  tidyr::unnest(
    cols = c(prediction)
    ) |>
  dplyr::select(
    label,
    input = inputs_text,
    text
  ) |>
  dplyr::mutate(
    label = stringr::str_trim(label),
    label = dplyr::na_if(label, "")
  )

qs::qsave(medical_records, "C:/Users/Andrew/Desktop/medical_records")

medical_terminology <- arrow::read_parquet("C:/Users/Andrew/Desktop/train-00000-of-00001.parquet") |>
  dplyr::select(definition = page_text) |>
  dplyr::reframe(
    term = strex::str_before_first(definition, " is "),
    definition = strex::str_after_first(definition, " is ")
    # definition = stringr::str_remove_all(definition, '"'),
    # definition = stringr::str_remove_all(definition, "'")
  )

qs::qsave(medical_terminology, "C:/Users/Andrew/Desktop/medical_terminology")


med_term <- qs::qread(
  "C:/Users/Andrew/Desktop/medical_terminology",
  use_alt_rep = TRUE
  ) |>
  dplyr::filter(!is.na(term)) |>
  dplyr::reframe(
    aka = stringr::str_to_sentence(strex::str_after_first(term, ", ")),
    term = dplyr::if_else(is.na(aka), term, stringr::str_to_title(strex::str_before_first(term, ", "))) ,
    definition
    ) |>
  tidyr::unite(
    col = "definition",
    c(aka, definition),
    sep = " ",
    na.rm = TRUE) |>
  dplyr::mutate(
    term = stringr::str_remove_all(term, "\n"),
    term = stringr::str_remove_all(term, stringr::fixed("()")),
    term = stringr::str_squish(term)
  )


med_term |>
  dplyr::mutate(
    after = strex::str_after_first(term, "\\) "),
    term = strex::str_before_first(term, " \\(")
    ) |>
  dplyr::select(term, after, definition) |>
  dplyr::filter(!is.na(after))
  dplyr::reframe(
    term = stringr::str_replace_all(term, stringr::regex("\\((.+?)\\)"), toupper),
    # term = stringr::str_to_title(term),
    definition = stringr::str_to_sentence(definition),
    definition = stringr::str_squish(definition),
    definition = stringr::str_replace_all(definition, stringr::regex("\\((.+?)\\)"), toupper)
  ) |>
  dplyr::count(term, sort = TRUE) |>
  dplyr::filter(n > 1) |>
  print(n = 200)

A Fasciculation                                                                                                                               4
A Cardiac Stress Test (Also Referred To As A Cardiac Diagnostic Test
An Induced Coma – Also Known As A Medically Induced Coma (MIC)


In Medicine                                                                                                                                  18
In Anatomy                                                                                                                                    6
In Human Anatomy                                                                                                                              4
In Pathology
In Cardiology

Chancroid ( SHANG-KROYD)

Anticholinergics (ANTICHOLINERGIC AGENTS) Are Substances That Block The Action Of The Neurotransmitter Called Acetylcholine (ACH) At Syn
Benzodiazepine Overdose Describes The Ingestion Of One Of The Drugs In The Benzodiazepine Class In Quantities Greater Than Are Recommend
