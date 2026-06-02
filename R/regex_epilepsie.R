# =============================================================================
# Extrait LeVeL — REGEX Crises d'épilepsie (patterns uniquement, aucune donnée)
#
# Détection en 2 temps, AU NIVEAU DE LA PHRASE :
#   1) p_epilepsie_inc  : présence d'un terme épileptique
#   2) p_epi_neg        : si une négation/résolution est trouvée dans la MÊME
#                         phrase que le terme -> la mention est reclassée à 0
#
# Source : observations quotidiennes (obsQuotConcat). Résolution = JOUR
# (date_jour = CAST(START_AT AS DATE)), agrégé par VISIT_ID x jour.
# Hors périmètre Charlson d'EDS-NLP (pattern LeVeL maison).
# =============================================================================

# --- 1. Pattern d'inclusion (présence d'un terme épileptique) ---------------
p_epilepsie_inc <- paste0("(?i)(",
  "\\b[eé]pilepsie?s?\\b",
  "|\\b[eé]pileptiques?\\b",
  "|\\b[eé]pileptiformes?\\b",
  "|\\bcomitialit[eé]\\b|\\bcomitiaux?\\b|\\bcomitiale?s?\\b",
  "|\\bconvulsions?\\b",
  "|\\bclonies?\\b",
  "|\\bpost[\\s\\-]?critiques?\\b",
  "|\\bEME\\b",                                  # état de mal épileptique
  "|\\bparoxysmes?[^.!?\\n]{0,40}(?:[eé]pileptiques?|comitiaux?|[eé]pilept[oï][dì]es?)",
  "|\\bactivit[eé]s?\\s+[eé]pileptiformes?(?:[^.!?\\n]{0,30}EEG)?",
  "|\\bd[eé]charges?\\s+[eé]pileptiques?",
  "|\\b[eé]pisodes?\\s+de\\s+(?:crises?\\s+)?",
  "(?:g[eé]n[eé]ralis[eé]e?s?|tonico[\\s\\-]?cloniques?",
  "|focales?|motrices?|sensitives?|avec\\s+g[eé]n[eé]ralisation(?:\\s+secondaire)?)",
  "|\\bcrises?\\s+(?:g[eé]n[eé]ralis[eé]e?s?|tonico[\\s\\-]?cloniques?|focales?)",
  # 'crises' générique — exclusion des contextes non-épileptiques par lookahead
  "|\\bcrises?(?!\\s*(?:neurov[eé]g[eé]tatives?|hypertensives?|spastiques?",
  "|douloureuses?|algiques?|de\\s+(?:larmes?|panique|angoisse|col[eè]re)))",
  ")")

# --- 2. Pattern de négation (niveau phrase) ---------------------------------
# Couvre ~80 tournures : négation directe, absence, fin/arrêt/résolution (nom),
# verbes conjugués de résolution, verbes actifs passés, infinitifs d'action.
# Extrait représentatif ci-dessous :
p_epi_neg <- paste0("(?i)(?:",
  # négation directe
  "\\bpas\\s+d[e'\\s]|\\bpoint\\s+d[e'\\s]|\\bsans\\s+|\\bni\\b|\\bplus\\s+d[e'\\s]|\\bjamais\\b|\\bnon\\s+",
  # absence / aucun
  "|\\babsenc[e]?s?\\s+(?:de\\s+toute?\\s+|d[e'\\s]|des?\\s+)|\\baucun[e]?\\b|\\bz[eé]ro\\b|\\bn[eé]ant\\b",
  # fin / arrêt / résolution (nom)
  "|\\b(?:fin|arr[eê]t|terme|lev[eé]e?|sortie?)\\s+d",
  "|\\b(?:r[eé]solution|disparition|c[eè]ssation|ma[iî]trise|contr[oô]le|gestion|",
  "normalisation|r[eé]gression|r[eé]mission)\\s+d",
  # verbes conjugués de résolution/arrêt
  "|(?:a|ont|est|sont|s[''']?est|se\\s+sont?)\\s+(?:[eé]t[eé]\\s+)?",
  "(?:stopp[eé]|cess[eé]|disparu?e?s?|r[eé]solu[eé]?s?|termin[eé]|ma[iî]tris[eé]|",
  "contr[oô]l[eé]|[eé]limin[eé]|jugu[leé]|c[eé]d[eé]|amend[eé]|r[eé]gl[eé]|normalis[eé])(?:e[s]?)?",
  # antériorité (historique, pas de crise active)
  "|\\bant[eé]c[eé]dents?\\s+d|\\bATCD\\s+d",
  ")")

# Exemple :
#   "crise tonico-clonique ce jour"        -> inclusion=1, négation=0  => CRISE=1
#   "pas de convulsion"                    -> inclusion=1, négation=1  => CRISE=0
#   "les crises ont cessé"                 -> inclusion=1, négation=1  => CRISE=0
#   "antécédent d'épilepsie, équilibrée"   -> inclusion=1, négation=1  => CRISE=0
