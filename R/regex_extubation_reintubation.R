# =============================================================================
# Extrait LeVeL — REGEX Ventilation : Intubation / Extubation / Réintubation
# (patterns uniquement, aucune donnée patient)
#
# Ces REGEX corrigent / complètent l'algorithme Metavision de durée de VM en
# extrayant des DATES et HEURES depuis le texte libre (CRH + obs quotidiennes).
# Règles de date/heure : fenêtre ±2 phrases autour du terme, repli ±400 car.,
# puis START_AT du document. Résolutions « hier / avant-hier » et « J<N> ».
# =============================================================================

# --- Intubation (1ère intubation uniquement) --------------------------------
# Lookbehind négatif devant intub* pour exclure ré-intub* / ext-ub*.
p_intubation <- paste0(
  "(?<![A-Za-z\\u00C0-\\u00FF\\-])",
  "(?:intub[eé]?s?|intubation|ventilation\\s+m[eé]canique|s[eé]quence\\s+rapide|",
  "assistance\\s+respiratoire|ventilation\\s+invasive|orotrach[eé]ale|\\bISR\\b|\\bIOT\\b)")

# --- Extubation -------------------------------------------------------------
# VSAI / VS-AI exclus (= sevrage en VM invasive, patient encore intubé) ;
# VS-PEP / VSP conservés (spécifiques VNI).
p_extubation <- paste0(
  "(?i)(extub[eé]e?s?|extubation|d[eé]\\-?ventil[eé]e?s?|d[eé]\\-?ventilation|",
  "fin\\s+d[eu]\\s+(?:la\\s+)?ventilation(?:\\s+m[eé]canique)?|respiration\\s+spontan[eé]e|",
  "sevr[eé]e?\\s+(?:de\\s+)?(?:la\\s+)?ventilation|ablation\\s+de\\s+la\\s+sonde|",
  "\\bEVS\\b|\\bEOT\\b|VS\\-?PEP|\\bVSP\\b)")

# --- Réintubation (ne capte PAS la 1ère intubation) -------------------------
p_reintubation <- paste0(
  "(?i)(r[eé]\\-?intub[eé]?e?s?|r[eé]\\-?intubation|re\\-?ventil[eé]e?s?|",
  "reprise\\s+(?:de\\s+)?(?:la\\s+)?ventilation(?:\\s+m[eé]canique)?|",
  "nouvelle\\s+intubation|(?:2[eè]me|deuxi[eè]me|seconde)\\s+intubation|",
  "intub[eé]\\s+[aà]\\s+nouveau|apr[eè]s\\s+(?:son\\s+|l[''])extubation)")

# --- Heures (extraction, ±2 phrases) ----------------------------------------
# Chiffres : 14h, 14h30, 14:30, 14 h 30 ; lettres : "quatorze heures trente",
# midi, minuit, etc. (une seule heure retenue : la plus proche du terme).
p_heure_chiffres <- "(?i)\\b([01]?\\d|2[0-3])\\s*[hH:]\\s*([0-5]\\d)?\\b"

# Sources scannées : CRH (table crh) + observations quotidiennes
# (obsQuotConcat -> table obs_quot_extubation, avec heure + flags).
# Sorties : *_trouvee, date_*_texte, heure_*_texte, *_from_obs, date_extubation_jn_offset.
