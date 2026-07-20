# Matching criteria for `toy_cbfm.csv`. Ships alongside the toy corpus
# so users can reproduce a screening run end-to-end from the CBFM
# (community-based fisheries management) case study.
#
# Source: the screening criteria applied in the Human-AI Collaboration
# paper --
#   Spillias, S., Tuohy, P., Andreotta, M., Annand-Jones, R.,
#   Boschetti, F., Cvitanovic, C., Duggan, J., Fulton, E. A.,
#   Karcher, D. B., Paris, C., Shellock, R., & Trebilco, R. (2024).
#   Human-AI collaboration to identify literature for evidence
#   synthesis. Cell Reports Sustainability, 1(7), 100132.
#   https://doi.org/10.1016/j.crsus.2024.100132

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
