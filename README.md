# LeVeL — Documentation des REGEX (app de démonstration)

Application **Shiny (shinydashboard)** démonstrative présentant les règles d'extraction par **expressions régulières (REGEX)** de l'étude **LeVeL** (Lévétiracétam vs Lacosamide et sevrage ventilatoire en réanimation chirurgicale, CHU de Rennes), pour extraire des informations cliniques des textes Metavision / CRH.

> ⚠️ **Aucune donnée patient** : ce dépôt ne contient que les *patterns* et leur logique.

## Contenu du dépôt

```
app/app.R                              # application shinydashboard (déployée)
R/regex_charlson.R                     # extraits .R — comorbidités de Charlson
R/regex_tabac_alcool.R                 # extraits .R — tabac & alcool (scoring binaire 0/1)
R/regex_extubation_reintubation.R      # extraits .R — intubation / extubation / réintubation
R/regex_epilepsie.R                    # extraits .R — crises d'épilepsie (inclusion + négation)
.github/workflows/deploy.yml           # CI : export shinylive + déploiement Pages
```

L'**application** (menu latéral) : Généralités (prétraitement, cotation, négation, validation par annotation, **origine EDS-NLP**), Charlson (un sélecteur par comorbidité), Tabac/Alcool, Épilepsie, Intubation, Extubation, Réintubation. Chaque concept affiche termes d'inclusion/exclusion, note et **bloc de code** (pattern).

Les **extraits `.R`** (dossier `R/`) contiennent les patterns réels, réutilisables hors application.

### Origine EDS-NLP

Attribution **best-effort, au niveau du concept** : les 16 comorbidités de Charlson ont un composant [EDS-NLP](https://aphp.github.io/edsnlp/) dédié (`eds.diabetes`, `eds.aids`, …) → badge vert. Les autres concepts (HTA, tabac/alcool, ventilation, épilepsie) → badge gris « pattern LeVeL (maison) ». La majorité des patterns **préexistait** dans LeVeL et a été *alignée/complétée* avec EDS-NLP, non réécrite.

## Déploiement (shinylive + GitHub Pages)

Automatique via GitHub Actions à chaque `push` sur `main` : l'app est exportée en WebAssembly avec [`shinylive`](https://posit-dev.github.io/r-shinylive/) (aucun serveur R) puis publiée sur **GitHub Pages**.

**À faire une fois** : `Settings → Pages → Build and deployment → Source = GitHub Actions`.

## Lancer en local

```r
# install.packages(c("shiny", "shinydashboard"))
shiny::runApp("app")
```
