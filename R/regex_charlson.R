# =============================================================================
# Extrait LeVeL — REGEX des comorbidités de l'index de Charlson
# (patterns uniquement, aucune donnée patient)
#
# Principe : le pattern de chaque comorbidité est assemblé à partir d'une liste
# de termes d'inclusion -> (?i)\b(terme1|terme2|...)\b, complété par des termes
# d'exclusion (lookahead / post-filtre) et des hiérarchies appliquées en R
# (ex. cirrhose Child B/C -> hépatopathie sévère ; cancer dont toutes les
#  mentions datées sont < 2005 -> ancien/guéri, non compté).
#
# Attribution EDS-NLP (best-effort, niveau concept) : champ `eds` = composant
# EDS-NLP correspondant (https://aphp.github.io/edsnlp/). NULL = pattern maison.
# =============================================================================

assembler_pattern <- function(termes) {
  paste0("(?i)\\b(", paste(termes, collapse = "|"), ")\\b")
}

regex_charlson <- list(

  idm_sca = list(eds = "eds.myocardial_infarction",
    inclusion = c("infarctus du myocarde", "IDM", "syndrome coronarien aigu", "SCA", "NSTEMI",
                  "STEMI", "angor instable", "angioplastie coronaire", "pontage aorto-coronarien",
                  "stent coronaire", "endoprothèse coronarienne"),
    exclusion = character(0)),

  insuf_card = list(eds = "eds.congestive_heart_failure",
    inclusion = c("insuffisance cardiaque", "IC systolique", "IC diastolique", "FEVG altérée",
                  "cardiopathie dilatée", "cardiopathie ischémique", "défaillance cardiaque",
                  "dysfonction ventriculaire", "choc cardiogénique", "cœur pulmonaire chronique"),
    exclusion = c("aiguë", "décompensation aiguë", "OAP aigu isolé", "pacemaker seul")),

  vasc_periph = list(eds = "eds.peripheral_vascular_disease",
    inclusion = c("AOMI", "artérite", "maladie vasculaire périphérique", "anévrysme aortique",
                  "pontage fémoral", "pontage poplité", "endoprothèse aortique", "TVP",
                  "embolie pulmonaire", "MTEV"),
    exclusion = c("vasculite", "vascularite", "thrombose superficielle")),

  avc_ait = list(eds = "eds.cerebrovascular_accident",
    inclusion = c("AVC", "AIT", "accident vasculaire cérébral", "accident ischémique transitoire",
                  "infarctus cérébral", "hémorragie cérébrale", "lacune cérébrale"),
    exclusion = c("traumatique", "post-traumatique", "iatrogénique", "chute")),

  demence = list(eds = "eds.dementia",
    inclusion = c("démence", "maladie d'Alzheimer", "Alzheimer", "démence vasculaire",
                  "trouble majeur neurocognitif", "syndrome démentiel", "TNC"),
    # 'démenti' (= nié) exclu par lookahead négatif : démenti(?!l)
    exclusion = c("syndrome confusionnel", "confusion", "delirium", "MCI", "Parkinson seul")),

  pulm_chron = list(eds = "eds.copd",
    inclusion = c("BPCO", "COPD", "emphysème", "bronchectasies", "fibrose pulmonaire", "asthme",
                  "PID", "mucoviscidose", "insuffisance respiratoire chronique", "SAOS"),
    exclusion = c("PAVM", "pneumopathie aiguë", "pneumonie aiguë", "infection pulmonaire")),

  maladie_syst = list(eds = "eds.connective_tissue_disease",
    inclusion = c("lupus érythémateux", "polyarthrite rhumatoïde", "sclérodermie",
                  "syndrome de Sjögren", "dermatomyosite", "GPA", "Wegener", "Behçet", "Crohn", "RCH"),
    # \bLES\b retenu seulement si suivi de systémique/érythémateux/actif/quiescent
    exclusion = character(0)),

  ulcere_peptique = list(eds = "eds.peptic_ulcer_disease",
    inclusion = c("ulcère gastrique", "ulcère duodénal", "maladie ulcéreuse", "ulcère peptique",
                  "perforation gastrique", "hémorragie ulcéreuse"),
    exclusion = c("ulcère cutané", "ulcère veineux", "ulcère artériel", "escarre", "ulcère de jambe")),

  hepato_mod = list(eds = "eds.liver_disease",
    inclusion = c("hépatite chronique", "NASH", "stéatohépatite", "cirrhose Child A",
                  "hépatopathie chronique", "fibrose hépatique"),
    exclusion = c("cirrhose Child B ou C", "aiguë", "fulminante")),

  hepato_sev = list(eds = "eds.liver_disease",
    inclusion = c("cirrhose Child B", "cirrhose Child C", "hypertension portale",
                  "varices œsophagiennes", "encéphalopathie hépatique", "ascite réfractaire",
                  "cirrhose biliaire primitive", "cholangite sclérosante primitive", "TIPS"),
    exclusion = c("cirrhose Child A")),

  diabete_sc = list(eds = "eds.diabetes",
    inclusion = c("diabète type 1", "diabète type 2", "DID", "DNID", "T1DM", "T2DM"),
    exclusion = c("diabète insipide", "diabète gestationnel", "prédiabète", "intolérance au glucose")),

  diabete_cc = list(eds = "eds.diabetes",
    inclusion = c("néphropathie diabétique", "rétinopathie diabétique", "neuropathie diabétique",
                  "pied diabétique", "complication diabétique"),
    exclusion = character(0)),

  hemiplegie = list(eds = "eds.hemiplegia",
    inclusion = c("hémiplégie", "paraplégie", "tétraplégie", "déficit moteur chronique",
                  "paralysie permanente", "locked-in syndrome"),
    exclusion = c("transitoire", "déficit transitoire")),

  irc = list(eds = "eds.ckd",
    inclusion = c("insuffisance rénale chronique", "IRC", "stade G3", "stade G4", "stade G5",
                  "dialyse", "hémodialyse", "insuffisance rénale terminale", "glomérulonéphrite"),
    exclusion = c("aiguë", "IRA", "fonctionnelle", "obstructive")),

  tumeur_solide = list(eds = "eds.solid_tumor",
    inclusion = c("cancer", "carcinome", "adénocarcinome", "néoplasie", "tumeur maligne",
                  "CHC", "GIST", "mésothéliome"),
    # filtre temporel : cancer dont toutes les mentions datées sont < 2005
    #                   (ancien / guéri) -> non compté (0)
    exclusion = c("carcinome in situ", "antécédents familiaux de cancer")),

  leuco_lymphome = list(eds = "eds.leukemia / eds.lymphoma",
    inclusion = c("leucémie", "LLC", "LMC", "LLA", "LMA", "lymphome", "lymphome de Hodgkin",
                  "lymphome non hodgkinien", "myélome", "myélome multiple"),
    exclusion = character(0)),

  metastase = list(eds = "eds.solid_tumor (drapeau métastase)",
    inclusion = c("métastase", "métastatique", "carcinose péritonéale", "M1", "dissémination tumorale"),
    exclusion = character(0)),

  vih_sida = list(eds = "eds.aids",
    inclusion = c("VIH", "HIV", "SIDA", "AIDS", "infection rétrovirale", "traitement antirétroviral", "ARV"),
    exclusion = character(0))
)

# Exemple d'utilisation :
#   p_diabete <- assembler_pattern(regex_charlson$diabete_sc$inclusion)
#   grepl(p_diabete, texte_clinique)   # détection (avant gestion de la négation)
