# =============================================================================
# LeVeL — Documentation des REGEX (application de démonstration, shinydashboard)
# Étude LeVeL : Lévétiracétam, Lacosamide et sevrage ventilatoire en réa chir.
#
# But : présenter, de façon pédagogique, les règles d'extraction par expressions
# régulières (REGEX) appliquées aux textes cliniques Metavision / CRH du CHU de
# Rennes — SANS aucune donnée patient (uniquement les patterns et leur logique).
#
# Les patterns sont aussi disponibles comme extraits .R autonomes dans le dossier
# R/ du dépôt (regex_charlson.R, regex_tabac_alcool.R,
# regex_extubation_reintubation.R, regex_epilepsie.R).
#
# Déploiement : application Shiny statique, exportée en WebAssembly via
# {shinylive} et hébergée sur GitHub Pages (aucun serveur R requis).
#
# Attribution EDS-NLP (https://aphp.github.io/edsnlp/) : best-effort, AU NIVEAU
# DU CONCEPT (les 16 comorbidités de Charlson). La majorité des patterns LeVeL
# préexistait et a été ALIGNÉE/COMPLÉTÉE avec EDS-NLP, non réécrite.
# =============================================================================

library(shiny)
library(shinydashboard)

# -----------------------------------------------------------------------------
# Helpers d'affichage
# -----------------------------------------------------------------------------
css <- "
.term { display:inline-block; background:#eafaf1; color:#196f3d; border-radius:6px;
        padding:1px 8px; margin:2px; font-size:0.9em; }
.termx{ display:inline-block; background:#fdecea; color:#a93226; border-radius:6px;
        padding:1px 8px; margin:2px; font-size:0.9em; }
.badge-eds  { display:inline-block; background:#d5f5e3; color:#1e8449;
              border:1px solid #1e8449; border-radius:10px; padding:2px 10px;
              font-size:0.85em; font-weight:bold; margin:4px 0; }
.badge-home { display:inline-block; background:#eef2f7; color:#5d6d7e;
              border:1px solid #b0bcc9; border-radius:10px; padding:2px 10px;
              font-size:0.85em; font-weight:bold; margin:4px 0; }
pre.regex { background:#1f2d3d; color:#e8f0fe; padding:12px; border-radius:8px;
            white-space:pre-wrap; word-break:break-word; font-size:0.82em; }
.note { background:#fef9e7; border-left:4px solid #f1c40f; padding:8px 12px;
        border-radius:4px; color:#5d4e0a; white-space:pre-wrap; }
.lead { color:#34495e; font-size:1.02em; }
"

terms_ui <- function(x, cls = "term") {
  if (length(x) == 0) return(tags$em("—"))
  lapply(x, function(t) tags$span(class = cls, t))
}
eds_badge <- function(eds) {
  if (is.null(eds) || is.na(eds) || !nzchar(eds))
    tags$span(class = "badge-home",
              "Hors périmètre Charlson d'EDS-NLP — pattern LeVeL (maison)")
  else
    tags$span(class = "badge-eds", paste0("Concept couvert par EDS-NLP — ", eds))
}
code_block <- function(txt) tags$pre(class = "regex", txt)

concept_panel <- function(co) {
  tagList(
    eds_badge(co$eds),
    tags$h4(tags$span(style = "color:#1e8449;", "Termes d'inclusion")),
    terms_ui(co$inclusion, "term"),
    tags$h4(tags$span(style = "color:#c0392b;", "Termes d'exclusion / négation")),
    terms_ui(co$exclusion, "termx"),
    if (!is.null(co$note) && nzchar(co$note)) div(class = "note", co$note),
    if (!is.null(co$regex) && nzchar(co$regex))
      tagList(tags$h4("Bloc de code — pattern"), code_block(co$regex))
  )
}

# -----------------------------------------------------------------------------
# Données — catalogue (verbatim depuis l'app finale LeVeL)
# -----------------------------------------------------------------------------
charlson <- list(
  idm_sca = list(label = "Infarctus du myocarde / SCA", eds = "eds.myocardial_infarction",
    inclusion = c("infarctus du myocarde","IDM","syndrome coronarien aigu","SCA","NSTEMI","STEMI",
                  "angor instable","angioplastie coronaire","pontage aorto-coronarien","stent coronaire",
                  "ATL (angioplastie transluminale)","pontage mammaire","endoprothèse coronarienne"),
    exclusion = character(0),
    note = "Inclut les antécédents d'angioplastie et de pontage coronarien comme marqueurs indirects."),
  insuf_card = list(label = "Insuffisance cardiaque", eds = "eds.congestive_heart_failure",
    inclusion = c("insuffisance cardiaque","IC systolique","IC diastolique","FEVG","cardiopathie dilatée",
                  "cardiopathie ischémique","défaillance cardiaque","dysfonction ventriculaire",
                  "choc cardiogénique","greffe cardiaque","valve mécanique","RAO/RAC/RM","CMH",
                  "cœur pulmonaire chronique","foie cardiaque"),
    exclusion = c("aiguë","décompensation aiguë","OAP aigu isolé","pacemaker seul","valvulopathie minime/modérée"),
    note = "Seule l'insuffisance cardiaque chronique est retenue ; les décompensations aiguës isolées sont exclues."),
  vasc_periph = list(label = "Pathologie vasculaire périphérique", eds = "eds.peripheral_vascular_disease",
    inclusion = c("AOMI","artérite","maladie vasculaire périphérique","anévrysme aortique","anévrisme",
                  "pontage fémoral","pontage poplité","endoprothèse aortique","TVP","EP (embolie pulmonaire)",
                  "MTEV","SHU/MAT/PTT","Budd-Chiari","Churg-Strauss","cryoglobulinémie","colite ischémique",
                  "CAPS","embolie de cholestérol"),
    exclusion = c("vasculite","vascularite","thrombose superficielle","cathéter intra-veineux"),
    note = "Les vasculites sont exclues (confusion avec « Maladies de système »). TVP/EP/MTEV ajoutés."),
  avc_ait = list(label = "AVC / AIT (maladie cérébrovasculaire)", eds = "eds.cerebrovascular_accident",
    inclusion = c("AVC","AIT","accident vasculaire cérébral","accident ischémique transitoire",
                  "infarctus cérébral","hémorragie cérébrale","lacune cérébrale","thrombophlébite cérébrale",
                  "thrombose du sinus","Moyamoya","syndrome de Susac","maladie des petites artères cérébrales",
                  "leucoaraïose","OVCR/OACR"),
    exclusion = c("traumatique","post-traumatique","iatrogénique","chute"),
    note = "Les AVC traumatiques, iatrogéniques ou liés à une chute sont exclus."),
  demence = list(label = "Démence", eds = "eds.dementia",
    inclusion = c("démence","maladie d'Alzheimer","Alzheimer","DFT","démence vasculaire",
                  "démence à corps de Lewy","démence fronto-temporale","trouble majeur neurocognitif",
                  "détérioration cognitive sévère","troubles cognitifs chroniques","troubles mnésiques",
                  "syndrome démentiel","TNC","syndrome frontal","atrophie corticale/hippocampique",
                  "Binswanger","Creutzfeldt-Jakob","Huntington","Korsakoff"),
    exclusion = c("syndrome confusionnel","confusion","delirium","Parkinson seul","MCI","pied de Charcot",
                  "Charcot-Marie-Tooth","anti-Alzheimer (médicament)"),
    note = "Les syndromes confusionnels aigus (delirium) sont exclus. « Démenti » (nié) exclu via lookahead."),
  pulm_chron = list(label = "Pathologie pulmonaire chronique", eds = "eds.copd",
    inclusion = c("BPCO","COPD","EABPCO","bronchopneumopathie chronique","emphysème","bronchectasies/DDB",
                  "fibrose pulmonaire/FPI","asthme","PID","mucoviscidose","insuffisance respiratoire chronique",
                  "syndrome obstructif chronique","ICS+LABA","triple thérapie inhalée","SAOS/SAS/SAHOS",
                  "OLD (oxygénothérapie longue durée)","oxygénodépendance","BOOP","alvéolite fibrosante","FID"),
    exclusion = c("PAVM","pneumopathie aiguë","pneumonie aiguë","infection pulmonaire","corps étranger bronchique"),
    note = "Pneumopathies infectieuses aiguës et obstructions aiguës exclues. ICS+LABA ou triple thérapie ⇒ BPCO=1."),
  maladie_syst = list(label = "Maladie de système (connectivite)", eds = "eds.connective_tissue_disease",
    inclusion = c("lupus érythémateux","LEDS","polyarthrite rhumatoïde","sclérodermie","syndrome de Sjögren",
                  "polymyosite","dermatomyosite","connectivite mixte","GPA/Wegener","polyangéite microscopique",
                  "vascularite à ANCA","périartérite noueuse","artérite de Horton","Takayasu","Behçet",
                  "spondylarthrite ankylosante","rhumatisme psoriasique","myasthénie","SAPL","Crohn","RCH","MICI",
                  "PPR","Raynaud","maladie de Still/AJI","Felty","Gougerot-Sjögren"),
    exclusion = character(0),
    note = "« LES » (article) et « PAR » (préposition) non utilisés (trop génériques). LES retenu seulement si suivi de systémique/érythémateux/actif/quiescent."),
  ulcere_peptique = list(label = "Ulcère peptique", eds = "eds.peptic_ulcer_disease",
    inclusion = c("ulcère gastrique","ulcère duodénal","maladie ulcéreuse","gastrite ulcéreuse",
                  "perforation gastrique","perforation duodénale","hémorragie ulcéreuse","ulcère peptique",
                  "ulcère de Curling","antrite ulcérée"),
    exclusion = c("ulcère cutané","ulcère veineux","ulcère artériel","ulcère de décubitus","ulcère du pied",
                  "ulcère de jambe","escarre"),
    note = "Les ulcères cutanés, veineux, artériels et de pression sont explicitement exclus."),
  hepato_mod = list(label = "Hépatopathie modérée", eds = "eds.liver_disease",
    inclusion = c("hépatite chronique B/C","NASH","NAFLD","stéatohépatite","stéatose hépatique",
                  "fibrose hépatique (≤ F1)","hépatopathie chronique/alcoolique/virale","cirrhose Child A",
                  "cirrhose sans mention de Child"),
    exclusion = c("cirrhose Child B ou C","aiguë","fulminante","médicamenteuse","gravidique","toxique"),
    note = "Cirrhose Child A ou sans mention → modérée ; Child B/C → basculé en sévère (post-traitement R)."),
  hepato_sev = list(label = "Hépatopathie sévère", eds = "eds.liver_disease",
    inclusion = c("hépatopathie sévère/grave/avancée","cirrhose Child B ou C","hypertension portale","HTP",
                  "varices œsophagiennes/gastriques","encéphalopathie hépatique","ascite réfractaire/cirrhotique",
                  "cirrhose biliaire primitive (CBP)","cholangite sclérosante primitive (CSP)",
                  "insuffisance hépatique sévère/terminale","TIPS","SHR (syndrome hépato-rénal)","VO stade 1/2/3",
                  "sclérose hépato-portale","nécrose hépatique"),
    exclusion = c("cirrhose Child A (→ modérée)"),
    note = "Seule la cirrhose avec mention explicite Child B/C dans la même phrase est sévère. CBP/CSP toujours sévères."),
  diabete_sc = list(label = "Diabète sans complication", eds = "eds.diabetes",
    inclusion = c("diabète type 1","diabète type 2","DID","DNID","diabète insulino-dépendant",
                  "diabète non insulino-dépendant","T1DM","T2DM"),
    exclusion = c("diabète insipide","diabète néphrogénique","diabète gestationnel","prédiabète","intolérance au glucose"),
    note = "Les complications spécifiques (néphropathie, rétinopathie…) sont codées séparément."),
  diabete_cc = list(label = "Diabète avec complication", eds = "eds.diabetes",
    inclusion = c("néphropathie diabétique","rétinopathie diabétique","neuropathie diabétique","pied diabétique",
                  "amputation diabétique","complication diabétique","macroangiopathie diabétique","microangiopathie diabétique"),
    exclusion = character(0), note = NULL),
  hemiplegie = list(label = "Hémiplégie / déficit neuro chronique", eds = "eds.hemiplegia",
    inclusion = c("hémiplégie","hémiplégie droite","hémiplégie gauche","paraplégie","tétraplégie",
                  "déficit moteur chronique","paralysie permanente","Charcot-Marie-Tooth (CMT)",
                  "locked-in syndrome (LIS)","paralysie hémicorps/membre","paralysie cérébrale spastique"),
    exclusion = c("transitoire","déficit transitoire"),
    note = "Les déficits transitoires (ex. paralysie post-ictus régressant en < 24 h) sont exclus."),
  irc = list(label = "Insuffisance rénale chronique", eds = "eds.ckd",
    inclusion = c("insuffisance rénale chronique","IRC","stade G3","stade G4","stade G5","IRCT","dialyse",
                  "hémodialyse","dialyse péritonéale","insuffisance rénale terminale","glomérulonéphrite",
                  "glomérulopathie","GNIgA","syndrome néphrotique","néphro-angiosclérose","maladie de Berger (IgA)",
                  "syndrome d'Alport","Goodpasture","tubulopathie","EER/épuration extra-rénale","DPCA"),
    exclusion = c("aiguë","IRA","fonctionnelle","obstructive","déshydratation"),
    note = "Les IRA et causes réversibles (fonctionnelle, obstructive) sont exclues."),
  tumeur_solide = list(label = "Tumeur solide (≥ 2015)", eds = "eds.solid_tumor",
    inclusion = c("cancer","carcinome","adénocarcinome","néoplasie","tumeur maligne","ADK/adénoK",
                  "CHC (carcinome hépatocellulaire)","GIST","carcinoïde","mésothéliome","linite plastique",
                  "K prostate/K sein…","paragangliome","thymome","syndrome de Lynch","Li-Fraumeni"),
    exclusion = c("carcinome in situ","cancérologie (titre de service)","antécédents familiaux de cancer",
                  "myélodysplasies (→ leucémies)"),
    note = "Filtrage temporel : seules les mentions associées à une année ≥ 2015 sont retenues. Année < 2015 → score = −1."),
  leuco_lymphome = list(label = "Leucémie / Lymphome / Myélome", eds = "eds.leukemia / eds.lymphoma",
    inclusion = c("leucémie","LLC","LMC","LLA","LMA","lymphome","lymphome de Hodgkin","lymphome non hodgkinien",
                  "myélome","myélome multiple","MM","LNH","LH","myélofibrose","maladie de Vaquez",
                  "thrombocytémie essentielle","syndrome de Sézary","lymphome de Burkitt","amylose AL",
                  "histiocytose maligne","mycosis fongoïde","EATL/LAGC/LDGCB"),
    exclusion = character(0), note = NULL),
  metastase = list(label = "Tumeur métastatique", eds = "eds.solid_tumor (drapeau métastase)",
    inclusion = c("métastase","métastatique","carcinose péritonéale","extension métastatique","M1","dissémination tumorale"),
    exclusion = character(0), note = NULL),
  vih_sida = list(label = "VIH / SIDA", eds = "eds.aids",
    inclusion = c("VIH","HIV","SIDA","AIDS","infection rétrovirale","traitement antirétroviral","ARV"),
    exclusion = character(0),
    note = "EDS-NLP distingue SIDA (×6 Charlson) du VIH seul (non compté), en exigeant une infection opportuniste. LeVeL capture le VIH+ seul → plus sensible mais peut surestimer.")
)

tabac_alcool <- list(
  tabac = list(label = "Tabac (actif / sevré)", eds = NULL,
    inclusion = c("tabagisme","fume / fumer / fument","tabac","cigarettes","paquets années","joint(s)","pétard(s)"),
    exclusion = character(0),
    note = paste0("Facteur comportemental (EDS-NLP a eds.tobacco, mais le scoring LeVeL est maison).\n",
      "Contrainte de proximité : terme et qualifiant dans la MÊME phrase.\n",
      "Scoring : +1 ACTIF (consommation, addiction, chronique, actif, /jour, quotidien) ; ",
      "+0.5 SEVRÉ (sevré, ancien, stoppé, arrêté) ; 0 sinon."),
    regex = "(?i)\\b(tabagisme|tabac|cigarettes?|fum(?:e|er|ent|eur)|paquets?[\\s\\-]?ann[eé]es?|joints?|p[eé]tards?)\\b\n# + qualifiant dans la meme phrase -> 1 (actif) / 0.5 (sevre)"),
  alcool = list(label = "Alcool (actif / sevré)", eds = NULL,
    inclusion = c("alcool","alcoolisme","éthylisme","éthylique","OH (consommation)","intoxication alcoolique"),
    exclusion = character(0),
    note = paste0("Même mécanisme que le tabac (fonction detect_substance, qualifiants actif/sevré ⇒ 0 / 0.5 / 1).\n",
      "EDS-NLP possède eds.alcohol (comportements) ; le scoring LeVeL est maison."),
    regex = "(?i)\\b(alcool|alcoolisme|[eé]thylisme|[eé]thylique|\\bOH\\b)\\b\n# + qualifiant actif/sevre dans la meme phrase -> 1 / 0.5")
)

# Épilepsie — patterns verbatim (REGEX exploration.Rmd)
p_epilepsie_inc <- paste0("(?i)(",
  "\\b[eé]pilepsie?s?\\b|\\b[eé]pileptiques?\\b|\\b[eé]pileptiformes?\\b",
  "|\\bcomitialit[eé]\\b|\\bcomitiaux?\\b|\\bcomitiale?s?\\b|\\bconvulsions?\\b|\\bclonies?\\b",
  "|\\bpost[\\s\\-]?critiques?\\b|\\bEME\\b",
  "|\\bparoxysmes?[^.!?\\n]{0,40}(?:[eé]pileptiques?|comitiaux?|[eé]pilept[oï][dì]es?)",
  "|\\bactivit[eé]s?\\s+[eé]pileptiformes?(?:[^.!?\\n]{0,30}EEG)?",
  "|\\bd[eé]charges?\\s+[eé]pileptiques?",
  "|\\b[eé]pisodes?\\s+de\\s+(?:crises?\\s+)?(?:g[eé]n[eé]ralis[eé]e?s?|tonico[\\s\\-]?cloniques?|focales?|motrices?|sensitives?)",
  "|\\bcrises?\\s+(?:g[eé]n[eé]ralis[eé]e?s?|tonico[\\s\\-]?cloniques?|focales?)",
  "|\\bcrises?(?!\\s*(?:neurov[eé]g[eé]tatives?|hypertensives?|spastiques?|douloureuses?|de\\s+(?:panique|angoisse)))",
  ")")
p_epi_neg <- paste0("(?i)(?:",
  "\\bpas\\s+d[e'\\s]|\\bpoint\\s+d[e'\\s]|\\bsans\\s+|\\bni\\b|\\bplus\\s+d[e'\\s]|\\bjamais\\b|\\bnon\\s+",
  "|\\babsenc[e]?s?\\s+(?:de\\s+toute?\\s+|d[e'\\s]|des?\\s+)|\\baucun[e]?\\b|\\bz[eé]ro\\b|\\bn[eé]ant\\b",
  "|\\b(?:fin|arr[eê]t|terme|lev[eé]e?|sortie?)\\s+d",
  "|\\b(?:r[eé]solution|disparition|c[eè]ssation|ma[iî]trise|contr[oô]le|normalisation|r[eé]gression|r[eé]mission)\\s+d",
  "|(?:a|ont|est|sont)\\s+(?:[eé]t[eé]\\s+)?(?:stopp[eé]|cess[eé]|disparu?e?s?|r[eé]solu[eé]?s?|ma[iî]tris[eé]|contr[oô]l[eé]|c[eé]d[eé]|amend[eé])",
  "  …  /* extrait — le pattern complet couvre ~80 tournures */",
  ")")

vent <- list(
  intubation = list(label = "Intubation (1ère intubation)", eds = NULL,
    inclusion = c("intubé(e)(s) / intubation","ventilation mécanique","séquence rapide","assistance respiratoire",
                  "ventilation invasive","orotrachéale","ISR (séquence rapide)","IOT (oro-trachéale)"),
    exclusion = c("ré-intubation (cf. Réintubation)","extubation (lookbehind négatif)"),
    note = paste0("SOURCES (3 scans, priorité P1>P2>P3) : P1 obs entrée Histoire de la maladie ; ",
      "P2 CRH section Histoire de la maladie ; P3 obs entrée Examen respiratoire.\n",
      "Lookbehind (?<![A-Za-zÀ-ÿ\\-]) devant intub* pour exclure ré-intub* / ext-ub*.\n",
      "Date/heure : la plus proche du terme (avant/après), sur tout le document. Fallback START_AT."),
    regex = "(?<![A-Za-z\\u00C0-\\u00FF\\-])(?:intub|ventilation\\s+m[eé]canique|s[eé]quence\\s+rapide|ISR|IOT|orotrach[eé]ale|ventilation\\s+invasive)"),
  extubation = list(label = "Extubation", eds = NULL,
    inclusion = c("extubé(e)(s)","extubation","déventilé(e)(s)","fin de la VM","respiration spontanée",
                  "VS-PEP/VSP (VSAI exclu)","sevré(e) de la ventilation","ablation de la sonde/SIT",
                  "EVS (épreuve de ventilation spontanée)","EOT","ventilé jusqu'au"),
    exclusion = c("VSAI / VS-AI (= sevrage en VM invasive, patient encore intubé)"),
    note = paste0("SOURCES : CRH (table crh) + observations quotidiennes (table obs_quot_extubation, + heure).\n",
      "DATE : fenêtre ±2 phrases, date la plus proche du mot-clé. HEURE (obs) : ±2 phrases, fallback ±400 car.\n",
      "FALLBACK START_AT (obs). « hier/avant-hier » = start_at −1/−2 j. « J<N> » (obs) = start_at −N j."),
    regex = "(?i)(extub[eé]|d[eé]\\-?ventil|fin\\s+d[eu]\\s+(?:la\\s+)?ventilation|respiration\\s+spontan[eé]e|sevr[eé]\\s+(?:de\\s+)?(?:la\\s+)?ventilation|EVS|EOT|VS\\-?PEP|VSP)"),
  reintubation = list(label = "Réintubation", eds = NULL,
    inclusion = c("réintubé(e)(s)","réintubation","re-intubé","reventilé(e)(s)","reprise de la VM",
                  "intubé à nouveau","nouvelle intubation","2ème/deuxième/seconde intubation","après son extubation"),
    exclusion = c("premières intubations (cf. Intubation)"),
    note = paste0("SOURCES : identiques à l'extubation. Capte ré-intubation et équivalents, PAS les 1ères intubations.\n",
      "DATE : ±2 phrases. HEURE (obs) : ±2 phrases, fallback ±400 car. « hier/avant-hier » activé. « J<N> » NON activé."),
    regex = "(?i)(r[eé]\\-?intub|re\\-?ventil[eé]|reprise\\s+(?:de\\s+)?(?:la\\s+)?ventilation|nouvelle\\s+intubation|(?:2[eè]me|deuxi[eè]me|seconde)\\s+intubation|intub[eé]\\s+[aà]\\s+nouveau)")
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
charlson_choices <- setNames(names(charlson), vapply(charlson, function(x) x$label, character(1)))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "LeVeL — REGEX"),
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Généralités",   tabName = "gen",     icon = icon("book")),
      menuItem("Charlson",      tabName = "charlson", icon = icon("notes-medical")),
      menuItem("Tabac / Alcool", tabName = "tabac",   icon = icon("wine-bottle")),
      menuItem("Épilepsie",     tabName = "epi",      icon = icon("bolt")),
      menuItem("Intubation",    tabName = "intub",    icon = icon("lungs")),
      menuItem("Extubation",    tabName = "extub",    icon = icon("wind")),
      menuItem("Réintubation",  tabName = "reintub",  icon = icon("rotate"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
      tabItem("gen",
        box(width = 12, title = "Étude LeVeL — documentation des REGEX", status = "primary", solidHeader = TRUE,
            p(class = "lead",
              "Extraction d'informations cliniques (comorbidités, épilepsie, ventilation) par expressions ",
              "régulières sur les textes Metavision / CRH — étude LeVeL (Lévétiracétam vs Lacosamide, ",
              "réanimation chirurgicale, CHU de Rennes). Démonstration — aucune donnée patient.")),
        tabBox(width = 12,
          tabPanel("Prétraitement",
            tags$ul(
              tags$li(tags$b("Sources concaténées"), " : CRH, observations d'entrée, observations quotidiennes."),
              tags$li(tags$b("Insensibilité à la casse"), " : tous les patterns commencent par ", tags$code("(?i)"), "."),
              tags$li(tags$b("Robustesse aux accents"), " : lettres accentuées doublées (ex. ", tags$code("[eé]pilepsie"), ")."),
              tags$li(tags$b("Découpage en phrases"), " sur ", tags$code(". ! ? \\n"), " — négation et proximité au niveau phrase."),
              tags$li(tags$b("Fenêtres contextuelles"), " ±2 phrases pour dates/heures, repli ±400 caractères puis ", tags$code("START_AT"), "."))),
          tabPanel("Incertitude",
            tags$ul(
              tags$li(tags$b("Proximité"), " : terme + qualifiant dans la même phrase (ex. tabac actif/sevré)."),
              tags$li(tags$b("Filtres temporels"), " : tumeur solide retenue si année ≥ 2015 (sinon −1)."),
              tags$li(tags$b("Lookahead / lookbehind"), " : exclusions ciblées (crises non épileptiques ; « démenti »)."),
              tags$li(tags$b("Scoring gradué"), " : tabac/alcool cotés 0 / 0,5 / 1."),
              tags$li(tags$b("Hiérarchies post-traitement"), " (R) : cirrhose Child B/C → hépatopathie sévère."))),
          tabPanel("Négation",
            p(class = "lead", "Négation gérée AU NIVEAU DE LA PHRASE, séparément de la détection du terme :"),
            tags$ul(
              tags$li("Étape 1 : détecter le terme (pattern d'inclusion)."),
              tags$li("Étape 2 : si une négation/résolution est dans la même phrase → reclassé à 0."),
              tags$li("Couvre négation directe (pas de, sans, aucun, jamais, ni), résolution (les crises ont cessé, contrôlée, a cédé) et antériorité (antécédent d'épilepsie).")),
            p(tags$em("Analogie EDS-NLP : "), tags$code("eds.negation"), " / ", tags$code("eds.hypothesis"), " / ", tags$code("eds.history"), "."),
            tags$h4("Exemple — négation épilepsie (extrait)"), code_block(p_epi_neg)),
          tabPanel("Validation",
            p("Performance évaluée contre une ", tags$b("annotation manuelle de référence"), " (gold standard) dans une app dédiée :"),
            tags$ul(
              tags$li("Le clinicien annote la présence réelle (0/1, ou −1/0/0,5/1)."),
              tags$li("Comparaison REGEX vs annotation en temps réel."),
              tags$li("Métriques : ", tags$b("sensibilité, spécificité, VPP, VPN, % de concordance"), "."),
              tags$li("Surlignage des termes détectés (", tags$code("highlight_spans"), ")."))),
          tabPanel("Origine EDS-NLP",
            div(class = "note",
              paste0("ATTRIBUTION BEST-EFFORT, AU NIVEAU DU CONCEPT. Le code ne trace pas la provenance ligne ",
                "par ligne. La majorité des patterns PRÉEXISTAIT dans LeVeL et a été ALIGNÉE / COMPLÉTÉE avec ",
                "EDS-NLP (AP-HP), pas réécrite. Estimation à valider par l'investigateur.")),
            tags$h4("Couvert par EDS-NLP (16 comorbidités de Charlson)"),
            terms_ui(c("eds.myocardial_infarction","eds.congestive_heart_failure","eds.peripheral_vascular_disease",
                       "eds.cerebrovascular_accident","eds.dementia","eds.copd","eds.connective_tissue_disease",
                       "eds.peptic_ulcer_disease","eds.liver_disease","eds.diabetes","eds.hemiplegia","eds.ckd",
                       "eds.solid_tumor","eds.leukemia","eds.lymphoma","eds.aids"), "term"),
            tags$h4("Hors périmètre Charlson — patterns LeVeL (maison)"),
            terms_ui(c("HTA (Elixhauser)","Tabac / Alcool","Épilepsie","Intubation","Extubation","Réintubation"), "termx"))
        )
      ),
      tabItem("charlson",
        box(width = 12, title = "Comorbidités de l'index de Charlson", status = "primary", solidHeader = TRUE,
            selectInput("charlson_pick", "Comorbidité :", choices = charlson_choices, width = "60%"),
            p(tags$small("Badge vert = concept couvert par EDS-NLP ; badge gris = pattern LeVeL hors périmètre Charlson."))),
        box(width = 12, uiOutput("charlson_detail"))
      ),
      tabItem("tabac",
        box(width = 12, title = "Tabac", status = "primary", solidHeader = TRUE, concept_panel(tabac_alcool$tabac)),
        box(width = 12, title = "Alcool", status = "primary", solidHeader = TRUE, concept_panel(tabac_alcool$alcool))
      ),
      tabItem("epi",
        box(width = 12, title = "Crises d'épilepsie (REGEX)", status = "primary", solidHeader = TRUE,
            eds_badge(NULL),
            p("Détection en 2 temps : (1) inclusion des termes épileptiques ; (2) négation niveau phrase ⇒ 0 ",
              "si négation/résolution dans la même phrase."),
            p(tags$em("Résolution : JOUR (observations quotidiennes "), tags$code("obsQuotConcat"), tags$em(")."))),
        box(width = 12, title = "Pattern d'inclusion", status = "success", solidHeader = TRUE, code_block(p_epilepsie_inc)),
        box(width = 12, title = "Pattern de négation (extrait)", status = "danger", solidHeader = TRUE, code_block(p_epi_neg))
      ),
      tabItem("intub",   box(width = 12, title = vent$intubation$label,   status = "primary", solidHeader = TRUE, concept_panel(vent$intubation))),
      tabItem("extub",   box(width = 12, title = vent$extubation$label,   status = "primary", solidHeader = TRUE, concept_panel(vent$extubation))),
      tabItem("reintub", box(width = 12, title = vent$reintubation$label, status = "primary", solidHeader = TRUE, concept_panel(vent$reintubation)))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  output$charlson_detail <- renderUI({
    req(input$charlson_pick)
    concept_panel(charlson[[input$charlson_pick]])
  })
}

shinyApp(ui, server)
