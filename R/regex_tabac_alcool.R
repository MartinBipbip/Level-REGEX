# =============================================================================
# Extrait LeVeL — REGEX Tabac & Alcool (comportements / facteurs de risque)
# (patterns uniquement, aucune donnée patient)
#
# Particularité : scoring BINAIRE 0 / 1, avec CONTRAINTE DE PROXIMITÉ —
# le terme et son qualifiant doivent être dans la MÊME phrase.
#   1 = consommation mentionnée (ACTIVE ou ancienne / SEVRÉE)
#   0 = sinon
#
# Hors périmètre Charlson (Elixhauser / comportements). EDS-NLP possède
# eds.tobacco / eds.alcohol, mais le scoring ci-dessous est spécifique à LeVeL.
# =============================================================================

# Termes principaux ----------------------------------------------------------
p_tabac  <- "(?i)\\b(tabagisme|tabac|cigarettes?|fum(?:e|er|ent|eur|euse)|paquets?[\\s\\-]?ann[eé]es?|joints?|p[eé]tards?)\\b"
p_alcool <- "(?i)\\b(alcool|alcoolisme|[eé]thylisme|[eé]thylique|intoxication\\s+alcoolique|\\bOH\\b)\\b"

# Qualifiants (recherchés dans la MÊME phrase que le terme principal) ---------
p_actif <- paste0("(?i)(consommation|addiction|chronique|d[eé]pendance|actif|actuellement|",
                  "par\\s+jours?|/\\s*jours?|quotidien)")
p_sevre <- "(?i)(sevr[eé]e?s?|ancien(?:ne)?s?|stopp[eé]e?s?|arr[eê]t[eé]e?s?)"

# Logique de scoring (par document, par substance) ---------------------------
#   1) on isole les phrases contenant le terme principal (p_tabac / p_alcool)
#   2) dans CHAQUE phrase concernée :
#        - si p_actif OU p_sevre present -> 1
#        - sinon                          -> 0
#   3) score du document = max sur les phrases (1 dès qu'une phrase est positive)
score_substance <- function(phrase) {
  if (grepl(p_actif, phrase) || grepl(p_sevre, phrase)) 1L else 0L
}

# Exemple :
#   "tabagisme actif à 20 PA"        -> 1
#   "tabac sevré depuis 2018"        -> 1
#   "tabac" (sans qualifiant phrase) -> 0
