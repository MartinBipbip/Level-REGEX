# =============================================================================
# Étude LeVeL — Documentation des REGEX (application de démonstration)
#
# Application illustrant, pour des cliniciens, la librairie d'expressions
# régulières (REGEX) utilisée pour extraire des informations cliniques des
# comptes rendus de l'étude LeVeL (Lévétiracétam vs Lacosamide et sevrage
# ventilatoire en réanimation chirurgicale, CHU de Rennes).
#
# Aucune donnée patient. Les patterns sont aussi fournis comme extraits .R
# autonomes dans le dossier R/ du dépôt.
#
# Déploiement : Shiny statique exporté en WebAssembly via {shinylive}, hébergé
# sur GitHub Pages (aucun serveur R requis).
# =============================================================================

library(shiny)
library(shinydashboard)

# -----------------------------------------------------------------------------
# Styles
# -----------------------------------------------------------------------------
css <- "
.term { display:inline-block; background:#eafaf1; color:#196f3d; border-radius:6px;
        padding:1px 8px; margin:2px; font-size:0.9em; }
.termx{ display:inline-block; background:#fdecea; color:#a93226; border-radius:6px;
        padding:1px 8px; margin:2px; font-size:0.9em; }
.hl   { background:#fff3a0; padding:0 4px; border-radius:3px; font-weight:700; }
pre.regex { background:#1f2d3d; color:#e8f0fe; padding:12px; border-radius:8px;
            white-space:pre-wrap; word-break:break-word; font-size:0.82em; }
.note { background:#fef9e7; border-left:4px solid #f1c40f; padding:10px 14px;
        border-radius:4px; color:#5d4e0a; line-height:1.6; }
.lead { color:#34495e; font-size:1.04em; line-height:1.6; }
.block{ background:#fff; border:1px solid #e1e8ee; border-radius:8px; padding:12px 16px; }
"

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
terms_ui <- function(x, cls = "term") {
  if (length(x) == 0) return(tags$em("—"))
  lapply(x, function(t) tags$span(class = cls, t))
}
code_block <- function(txt) tags$pre(class = "regex", txt)

concept_panel <- function(co) {
  tagList(
    tags$h4(tags$span(style = "color:#1e8449;", "Termes d'inclusion")),
    terms_ui(co$inclusion, "term"),
    tags$h4(tags$span(style = "color:#c0392b;", "Termes d'exclusion / négation")),
    terms_ui(co$exclusion, "termx"),
    if (!is.null(co$note) && nzchar(co$note)) div(class = "note", co$note),
    if (!is.null(co$regex) && nzchar(co$regex))
      tagList(tags$h4("Pattern (extrait)"), code_block(co$regex))
  )
}

# -----------------------------------------------------------------------------
# Données — comorbidités de Charlson
# -----------------------------------------------------------------------------
charlson <- list(
  idm_sca = list(label = "Infarctus du myocarde / SCA",
    inclusion = c("infarctus du myocarde","IDM","syndrome coronarien aigu","SCA","NSTEMI","STEMI",
                  "angor instable","angioplastie coronaire","pontage aorto-coronarien","stent coronaire",
                  "endoprothèse coronarienne"),
    exclusion = character(0),
    note = "Les antécédents d'angioplastie et de pontage coronarien sont inclus comme marqueurs indirects de maladie coronarienne."),
  insuf_card = list(label = "Insuffisance cardiaque",
    inclusion = c("insuffisance cardiaque","IC systolique","IC diastolique","FEVG altérée","cardiopathie dilatée",
                  "cardiopathie ischémique","défaillance cardiaque","dysfonction ventriculaire","choc cardiogénique",
                  "cœur pulmonaire chronique"),
    exclusion = c("aiguë","décompensation aiguë","OAP aigu isolé","pacemaker seul"),
    note = "Seule l'insuffisance cardiaque chronique est retenue ; une décompensation aiguë isolée n'est pas comptée."),
  vasc_periph = list(label = "Pathologie vasculaire périphérique",
    inclusion = c("AOMI","artérite","maladie vasculaire périphérique","anévrysme aortique","pontage fémoral",
                  "pontage poplité","endoprothèse aortique","thrombose veineuse profonde","embolie pulmonaire"),
    exclusion = c("vasculite","vascularite","thrombose superficielle"),
    note = "Les vasculites sont exclues pour éviter la confusion avec les maladies de système. La maladie thrombo-embolique veineuse chronique est incluse."),
  avc_ait = list(label = "AVC / AIT (maladie cérébrovasculaire)",
    inclusion = c("AVC","AIT","accident vasculaire cérébral","accident ischémique transitoire","infarctus cérébral",
                  "hémorragie cérébrale","lacune cérébrale"),
    exclusion = c("traumatique","post-traumatique","iatrogénique","chute"),
    note = "Les accidents d'origine traumatique, iatrogénique ou consécutifs à une chute sont exclus."),
  demence = list(label = "Démence",
    inclusion = c("démence","maladie d'Alzheimer","Alzheimer","démence vasculaire","trouble majeur neurocognitif",
                  "syndrome démentiel"),
    exclusion = c("syndrome confusionnel","confusion","delirium","Parkinson isolé"),
    note = "Les syndromes confusionnels aigus (delirium) sont exclus. Une maladie de Parkinson isolée, sans démence associée, n'est pas comptée."),
  pulm_chron = list(label = "Pathologie pulmonaire chronique",
    inclusion = c("BPCO","emphysème","bronchectasies","fibrose pulmonaire","asthme","pneumopathie interstitielle",
                  "mucoviscidose","insuffisance respiratoire chronique","apnées du sommeil"),
    exclusion = c("pneumopathie aiguë","pneumonie aiguë","infection pulmonaire"),
    note = "Les pneumopathies infectieuses aiguës sont exclues. La présence d'un traitement inhalé de fond (corticoïde + bronchodilatateur de longue durée) suffit à retenir une BPCO."),
  maladie_syst = list(label = "Maladie de système (connectivite)",
    inclusion = c("lupus érythémateux","polyarthrite rhumatoïde","sclérodermie","syndrome de Sjögren",
                  "dermatomyosite","granulomatose avec polyangéite","Behçet","maladie de Crohn","rectocolite hémorragique"),
    exclusion = character(0),
    note = "Certaines abréviations trop génériques en français ne sont pas utilisées seules, pour éviter les faux positifs."),
  ulcere_peptique = list(label = "Ulcère peptique",
    inclusion = c("ulcère gastrique","ulcère duodénal","maladie ulcéreuse","ulcère peptique",
                  "perforation gastrique","hémorragie ulcéreuse"),
    exclusion = c("ulcère cutané","ulcère veineux","ulcère artériel","ulcère du pied","escarre"),
    note = "Les ulcères cutanés, veineux, artériels et de pression (escarres) sont explicitement exclus."),
  hepato_mod = list(label = "Hépatopathie modérée",
    inclusion = c("hépatite chronique","stéatohépatite (NASH)","cirrhose Child A","hépatopathie chronique",
                  "fibrose hépatique"),
    exclusion = c("cirrhose Child B ou C","aiguë","fulminante"),
    note = "Une cirrhose Child A (ou sans précision) est classée modérée ; une cirrhose Child B ou C est reclassée en hépatopathie sévère."),
  hepato_sev = list(label = "Hépatopathie sévère",
    inclusion = c("cirrhose Child B","cirrhose Child C","hypertension portale","varices œsophagiennes",
                  "encéphalopathie hépatique","ascite réfractaire","cirrhose biliaire primitive",
                  "cholangite sclérosante primitive"),
    exclusion = c("cirrhose Child A"),
    note = "Seule une cirrhose explicitement Child B ou C est classée sévère. Les cirrhoses et cholangites biliaires sont toujours considérées comme sévères."),
  diabete_sc = list(label = "Diabète sans complication",
    inclusion = c("diabète type 1","diabète type 2","diabète insulino-dépendant","diabète non insulino-dépendant"),
    exclusion = c("diabète insipide","diabète gestationnel","prédiabète","intolérance au glucose"),
    note = "Les complications spécifiques (néphropathie, rétinopathie…) sont comptées dans la catégorie suivante."),
  diabete_cc = list(label = "Diabète avec complication",
    inclusion = c("néphropathie diabétique","rétinopathie diabétique","neuropathie diabétique","pied diabétique",
                  "complication diabétique"),
    exclusion = character(0), note = NULL),
  hemiplegie = list(label = "Hémiplégie / déficit neuro chronique",
    inclusion = c("hémiplégie","paraplégie","tétraplégie","déficit moteur chronique","paralysie permanente",
                  "locked-in syndrome"),
    exclusion = c("transitoire","déficit transitoire"),
    note = "Les déficits transitoires, régressant en moins de 24 heures, sont exclus."),
  irc = list(label = "Insuffisance rénale chronique",
    inclusion = c("insuffisance rénale chronique","stade G3","stade G4","stade G5","dialyse","hémodialyse",
                  "insuffisance rénale terminale","glomérulonéphrite","syndrome néphrotique"),
    exclusion = c("aiguë","insuffisance rénale aiguë","fonctionnelle","obstructive"),
    note = "Les insuffisances rénales aiguës et les causes réversibles (fonctionnelle, obstructive) sont exclues."),
  tumeur_solide = list(label = "Tumeur solide",
    inclusion = c("cancer","carcinome","adénocarcinome","néoplasie","tumeur maligne","mésothéliome"),
    exclusion = c("carcinome in situ","antécédents familiaux de cancer"),
    note = "Un cancer dont toutes les mentions datées sont antérieures à 2005 est considéré comme ancien / guéri et n'est pas compté ; une mention sans date, ou datée à partir de 2005, est retenue."),
  leuco_lymphome = list(label = "Leucémie / Lymphome / Myélome",
    inclusion = c("leucémie","lymphome","lymphome de Hodgkin","lymphome non hodgkinien","myélome","myélome multiple"),
    exclusion = character(0), note = NULL),
  metastase = list(label = "Tumeur métastatique",
    inclusion = c("métastase","métastatique","carcinose péritonéale","dissémination tumorale"),
    exclusion = character(0), note = NULL),
  vih_sida = list(label = "VIH / SIDA",
    inclusion = c("VIH","SIDA","infection rétrovirale","traitement antirétroviral"),
    exclusion = character(0),
    note = "Le VIH est capté de façon sensible ; le SIDA, plus sévère, pèse davantage dans le score de Charlson.")
)

# Tabac & alcool ------------------------------------------------------------
tabac_alcool <- list(
  tabac = list(label = "Tabac (actif ou sevré)",
    inclusion = c("tabagisme","fume / fumer / fument","tabac","cigarettes","paquets-années","joints"),
    exclusion = character(0),
    note = paste0("Le terme et son qualifiant doivent figurer dans la même phrase. La cotation est binaire : ",
                  "1 si une consommation est mentionnée — qu'elle soit actuelle (« actif », « consommation », ",
                  "« par jour ») ou ancienne (« sevré », « stoppé », « arrêté ») —, 0 sinon."),
    regex = "(?i)\\b(tabagisme|tabac|cigarettes?|fum(?:e|er|ent|eur)|paquets?[\\s\\-]?ann[eé]es?|joints?)\\b"),
  alcool = list(label = "Alcool (actif ou sevré)",
    inclusion = c("alcool","alcoolisme","éthylisme","éthylique","intoxication alcoolique"),
    exclusion = character(0),
    note = "Même logique que le tabac : cotation binaire — 1 si une consommation (actuelle ou ancienne / sevrée) est trouvée dans la même phrase que le terme, 0 sinon.",
    regex = "(?i)\\b(alcool|alcoolisme|[eé]thylisme|[eé]thylique|intoxication\\s+alcoolique)\\b")
)

# Épilepsie — patterns (verbatim) -------------------------------------------
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
  "|\\bant[eé]c[eé]dents?\\s+d|\\bATCD\\s+d",
  ")")

# Ventilation — intubation / extubation / réintubation (notes rédigées) -----
vent <- list(
  intubation = list(label = "Intubation (1ère intubation)",
    inclusion = c("intubation","intubé","ventilation mécanique","séquence rapide","assistance respiratoire",
                  "ventilation invasive","intubation orotrachéale"),
    exclusion = c("ré-intubation / re-intubation","extubation"),
    note = paste0("La date de la première intubation est recherchée dans le récit d'admission (histoire de la ",
      "maladie et examen respiratoire d'entrée) ainsi que dans le compte rendu d'hospitalisation. Le terme ",
      "n'est retenu que s'il ne s'agit pas d'une ré-intubation : un filtre écarte les mentions de re-/ré-intubation ",
      "et d'extubation. Lorsqu'une date et une heure figurent à proximité du terme, on retient celles qui en sont ",
      "le plus proche dans le texte — qu'elles soient écrites en chiffres (« 14h30 ») ou en toutes lettres ",
      "(« quatorze heures trente », « midi », « minuit »). Si aucune date n'est mentionnée dans le texte, on ",
      "utilise la date du document comme repère."),
    regex = "(?<![A-Za-z\\u00C0-\\u00FF\\-])(?:intub|ventilation\\s+m[eé]canique|s[eé]quence\\s+rapide|orotrach[eé]ale|ventilation\\s+invasive)"),
  extubation = list(label = "Extubation",
    inclusion = c("extubé","extubation","déventilé","fin de la ventilation mécanique","respiration spontanée",
                  "sevrage de la ventilation","ablation de la sonde d'intubation","épreuve de ventilation spontanée"),
    exclusion = c("modes de sevrage encore invasifs (patient toujours intubé)"),
    note = paste0("La date d'extubation est recherchée à la fois dans le compte rendu d'hospitalisation et dans les ",
      "observations quotidiennes. Autour de la mention d'extubation, on retient la date — et l'heure quand elle est ",
      "disponible — la plus proche du mot-clé dans la phrase. Les expressions relatives au temps sont résolues ",
      "automatiquement : « hier » et « avant-hier » sont convertis par rapport à la date de l'observation, et une ",
      "formulation comme « J3 d'extubation » est ramenée au bon jour. Enfin, certains modes de sevrage au cours ",
      "desquels le patient reste intubé sont volontairement écartés, pour ne pas confondre un sevrage en cours avec ",
      "une extubation réelle."),
    regex = "(?i)(extub[eé]|d[eé]\\-?ventil|fin\\s+d[eu]\\s+(?:la\\s+)?ventilation|respiration\\s+spontan[eé]e|sevr[eé]\\s+(?:de\\s+)?(?:la\\s+)?ventilation)"),
  reintubation = list(label = "Réintubation",
    inclusion = c("réintubation","ré-intubé","reprise de la ventilation","intubé à nouveau","nouvelle intubation",
                  "2ème intubation"),
    exclusion = c("première intubation"),
    note = paste0("Une réintubation est repérée par des formulations telles que « ré-intubation », « reprise de la ",
      "ventilation », « nouvelle intubation » ou « 2ᵉ intubation ». Ce repérage ne capte volontairement pas la ",
      "première intubation. Comme pour l'extubation, on recherche la date la plus proche du terme dans le compte ",
      "rendu d'hospitalisation ou les observations quotidiennes, et les expressions « hier » / « avant-hier » sont ",
      "résolues automatiquement par rapport à la date de l'observation."),
    regex = "(?i)(r[eé]\\-?intub|re\\-?ventil[eé]|reprise\\s+(?:de\\s+)?(?:la\\s+)?ventilation|nouvelle\\s+intubation|intub[eé]\\s+[aà]\\s+nouveau)")
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
charlson_choices <- setNames(names(charlson), vapply(charlson, function(x) x$label, character(1)))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "LeVeL — REGEX"),
  dashboardSidebar(
    sidebarMenu(id = "tab",
      menuItem("Généralités",    tabName = "gen",     icon = icon("book")),
      menuItem("Charlson",       tabName = "charlson", icon = icon("notes-medical")),
      menuItem("Tabac / Alcool", tabName = "tabac",   icon = icon("wine-bottle")),
      menuItem("Épilepsie",      tabName = "epi",      icon = icon("bolt")),
      menuItem("Intubation",     tabName = "intub",    icon = icon("lungs")),
      menuItem("Extubation",     tabName = "extub",    icon = icon("wind")),
      menuItem("Réintubation",   tabName = "reintub",  icon = icon("rotate"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
      tabItem("gen",
        box(width = 12, title = "Étude LeVeL — documentation des REGEX", status = "primary", solidHeader = TRUE,
            p(class = "lead",
              "Cette librairie ReGex a été utilisée pour extraire des informations cliniques ",
              "(comorbidités, épilepsie, cut-off de ventilation) à partir des comptes rendus d'hospitalisation ",
              "et d'observation quotidienne."),
            p(class = "lead",
              "Cette application ShinyLive montre son utilisation pour la réalisation de notre étude comparant les ",
              "durées de ventilation mécanique sous Lévétiracétam ou Lacosamide en réanimation chirurgicale du CHU de Rennes."),
            p("Le code source est disponible sur le dépôt GitHub : ",
              tags$a(href = "https://github.com/MartinBipbip/Level-REGEX.git", target = "_blank",
                     "github.com/MartinBipbip/Level-REGEX"))),
        tabBox(width = 12,
          tabPanel("Prétraitement",
            tags$ul(
              tags$li(tags$b("Sources de texte"), " : comptes rendus d'hospitalisation, observations d'entrée et observations quotidiennes."),
              tags$li(tags$b("Mise en lisibilité du texte"),
                " : les comptes rendus sont stockés au format HTML (mise en forme, polices, images encodées en base64). ",
                "Avant toute recherche, ils sont convertis en texte brut lisible : on retire les balises de mise en forme ",
                "(styles, scripts, feuilles de style), les images encodées en base64 et les autres parasites HTML/CSS, ",
                "puis on normalise les espaces et les apostrophes mal encodées."),
              tags$li(tags$b("Insensibilité à la casse"), " : majuscules et minuscules sont traitées indifféremment."),
              tags$li(tags$b("Robustesse aux accents"), " : le texte est normalisé sans accent avant la recherche (« épilepsie » et « epilepsie » sont traités de la même manière)."),
              tags$li(tags$b("Découpage en phrases"), " : le texte est segmenté en phrases aux signes « . », « ! » et « ? » (un point encadré par deux chiffres ne coupe pas la phrase) ; la négation et la proximité sont évaluées au sein d'une même phrase.")),
            code_block("# Exemple — robustesse a la casse et aux accents\n(?i)\\b[eé]pilepsie?s?\\b   # capture : Epilepsie, épilepsies, EPILEPSIE…")),
          tabPanel("Cotation",
            tags$ul(
              tags$li(tags$b("Cotation binaire"),
                " : chaque comorbidité est cotée 1 (présente) ou 0 (absente ou niée). ",
                "Une mention explicite et une mention incertaine valent toutes deux 1."),
              tags$li(tags$b("Antécédents familiaux"),
                " : lorsqu'une comorbidité figure dans un cadre familial (« antécédents familiaux », ",
                "« chez le père / la mère / un frère »…), elle n'est pas attribuée au patient (cotée 0), ",
                "sauf si la même comorbidité est aussi retrouvée hors de ce cadre."),
              tags$li(tags$b("Dates des antécédents"),
                " : les antécédents étant par nature datés dans le passé, aucun filtre temporel ne les écarte. ",
                "Seule exception : un cancer dont toutes les mentions datées sont antérieures à 2005 ",
                "(ancien / guéri) n'est pas compté."),
              tags$li(tags$b("Proximité"),
                " : pour le tabac et l'alcool, le terme et son qualifiant doivent figurer dans la même phrase. ",
                "Exemple : « Tabagisme ", tags$span(class = "hl", "sevré"), ", actif pendant plus de 20 ans » → tabac présent (1)."))),
          tabPanel("Négation",
            p(class = "lead", "La négation est gérée au niveau de la phrase, séparément de la détection du terme :"),
            tags$ul(
              tags$li("Étape 1 : détecter le terme."),
              tags$li("Étape 2 : si une négation ou une résolution figure dans la même phrase, la mention est reclassée comme absente."),
              tags$li("Sont couvertes la négation directe (« pas de », « sans », « aucun », « jamais »), la résolution ",
                      "(« les crises ont cessé », « épilepsie contrôlée », « a cédé ») et l'antériorité (« antécédent d'épilepsie »).")),
            p("Les négations sont inspirées des sections ", tags$code("eds.negation"), " … ",
              tags$code("eds.history"), " de la librairie EDS-NLP."),
            tags$h4("Exemple — négation épilepsie (extrait)"), code_block(p_epi_neg)),
          tabPanel("Validation",
            p("La performance de chaque ReGex est évaluée contre une annotation manuelle de référence ",
              "(gold standard) dans une application dédiée :"),
            tags$ul(
              tags$li("Le clinicien annote manuellement une série aléatoire de comptes rendus et voit en direct les termes retenus par les ReGex."),
              tags$li("Les performances sont évaluées à ", tags$span(style = "color:#bbb;", "…"), "."))),
          tabPanel("Origine",
            div(class = "block",
              p("Cette librairie a été générée par des cliniciens du CHU de Rennes."),
              p("Les patterns concernant le calcul du score de Charlson ont ensuite été enrichis par la librairie ",
                "open access « EDS-NLP » de l'AP-HP : ",
                tags$a(href = "https://aphp.github.io/edsnlp/latest/", target = "_blank", "aphp.github.io/edsnlp"), ".")))
        )
      ),
      tabItem("charlson",
        box(width = 12, title = "Comorbidités de l'index de Charlson", status = "primary", solidHeader = TRUE,
            selectInput("charlson_pick", "Comorbidité :", choices = charlson_choices, width = "60%"),
            p(tags$small("Sélectionnez une comorbidité pour afficher les termes d'inclusion / d'exclusion et la logique appliquée."))),
        box(width = 12, uiOutput("charlson_detail"))
      ),
      tabItem("tabac",
        box(width = 12, title = "Tabac", status = "primary", solidHeader = TRUE, concept_panel(tabac_alcool$tabac)),
        box(width = 12, title = "Alcool", status = "primary", solidHeader = TRUE, concept_panel(tabac_alcool$alcool))
      ),
      tabItem("epi",
        box(width = 12, title = "Crises d'épilepsie", status = "primary", solidHeader = TRUE,
            p("Détection en deux temps : (1) repérage des termes évoquant une crise épileptique ; ",
              "(2) si une négation ou une résolution figure dans la même phrase, la mention est écartée."),
            p(tags$em("Les crises sont recherchées dans les observations quotidiennes (résolution à la journée)."))),
        box(width = 12, title = "Pattern d'inclusion", status = "success", solidHeader = TRUE, code_block(p_epilepsie_inc)),
        box(width = 12, title = "Pattern de négation (extrait)", status = "danger", solidHeader = TRUE, code_block(p_epi_neg))
      ),
      tabItem("intub",   box(width = 12, title = vent$intubation$label,   status = "primary", solidHeader = TRUE, concept_panel(vent$intubation))),
      tabItem("extub",   box(width = 12, title = vent$extubation$label,   status = "primary", solidHeader = TRUE, concept_panel(vent$extubation))),
      tabItem("reintub", box(width = 12, title = vent$reintubation$label, status = "primary", solidHeader = TRUE, concept_panel(vent$reintubation)))
    )
  )
)

server <- function(input, output, session) {
  output$charlson_detail <- renderUI({
    req(input$charlson_pick)
    concept_panel(charlson[[input$charlson_pick]])
  })
}

shinyApp(ui, server)
