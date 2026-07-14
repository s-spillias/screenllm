# Matching criteria for `toy_cbfm.csv`. Ships alongside the toy corpus
# so users can reproduce a screening run end-to-end from the paper's
# CBFM (community-based fisheries management) review.
#
# From Spillias et al. (2026), screening_criteria.json.

toy_cbfm_criteria <- list(
  scope = paste0(
    "Articles potentially relevant to community-based fisheries ",
    "management (CBFM) in Pacific Island contexts."
  ),
  inclusions = c(
    paste0(
      "It is possible that the study includes a case study from one or ",
      "more of: Cook Islands, Federated States of Micronesia, Fiji, ",
      "Kiribati, Marshall Islands, Nauru, Niue, Palau, Papua New Guinea, ",
      "Samoa, Solomon Islands, Tonga, Tuvalu, or Vanuatu."
    ),
    paste0(
      "It is possible that the study discusses fisheries and/or marine ",
      "resource management."
    ),
    "It is possible that the study discusses a community-based approach."
  )
)
