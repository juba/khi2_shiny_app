hdv_description <- list(
  tags$p("L'enquete Histoire de vie a été réalisée par l'INSEE en 2003. Elle comprenait à la fois un questionnaire sur des thèmes assez larges (activité, gouts, engagements, handicap, discriminations) et une parti biographique."),
  tags$p("Plus d'informations et téléchargement des données disponibles ", tags$a(href="http://www.insee.fr/fr/themes/detail.asp?ref_id=fd-hdv03", "sur le site de l'INSEE.")),
  tags$p("Ici on travaille sur un extrait de l'enquete, comprenant ", nrow(d), "individus et ", ncol(d), "variables."))

hdv_listevars <- list(tags$p("Descriptif des variables : "),
                      tags$pre("
| Variable     | Description                                   |
|--------------+-----------------------------------------------|
| sexe         | Sexe de l'enqueté                             |
| age2cl       | Âge de l'enquêté en deux classes              |
| age6cl       | Âge de l'enquêté en six classes               |
| nivetud      | Niveau d'études                               |
| occup        | Statut d'occupation                           |
| qualif       | Niveau de qualification                       |
| couture      | Pratique le tricot, la couture ou la broderie |
| journal      | A déjà tenu un journal intime                 |
| series       | Préfère les séries comme genre d'émission     |
| heures.tv    | Nombre d'heures par jour devant la télévision |
| sport        | Pratique une activité sportive                |
| cinema       | Est allé au cinéma l'an dernier               |
| bricol       | Pratique le bricolage                         |
| cuisine      | Pratique la cuisine                           |
| peche.chasse | Pratique la pêche ou la chasse                |
| lecture.bd   | Lit des bandes dessinées                      |
| hard.rock    | Écoute du Hard rock                           |
| trav.satisf  | Satisfaction au travail                       |
| trav.imp     | Importance accordée au travail                |
| relig        | Pratique religieuse                           |
| clso         | Sentiment d'appartenance à une classe sociale |
| freres.soeurs| Nombre de (demi-)frères et (demi-)soeurs      |"))

css_string <- "
@import url(http://fonts.googleapis.com/css?family=Open+Sans);
@import url(http://fonts.googleapis.com/css?family=Roboto);
* {font-family: 'Roboto', 'Open Sans', 'Helvetica', sans-serif;}
.container-fluid {max-width: 1170px;}
.nav-tabs {margin-bottom: 25px;}
table td {text-align: right;}
table tbody tr:hover {background-color: #F0F0F0;}
table.data td[align=right] {font-family: 'Roboto', 'Open Sans', 'Helvetica', sans-serif;}
h1 { margin-bottom: 1em; font-size: 2.2em;}
.form-inline .form-control {width: 7em !important;}
span.khid_tab, span.indep_tab {width: 7em; display:inline-block; text-align: right;}
span.sim1_tab {width: 4em; display:inline-block; text-align: right;}

"


## BIAIS ---------------------------------------------------------------

row.names <- c("Cerastoculteur","Venericulteur","Pectiniculteur","Halioticulteur")
col.names <- c("Potjevleesch","Kouign-amann", "Waterzooi")

show_alert <- function(texte) {
  column(12,
         tags$div(class="alert alert-info alert-dismissable",
                  HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                  HTML(texte)
         )
  )
} 

ui_biais <- fluidRow(
    headerPanel("Biais d'échantillonnage"),
    show_alert("Cette page simule la réalisation d'une enquête par questionnaire. On interroge une population au sujet de deux variables parfaitement indépendantes pour visualiser l'effet du biais d'échantillonnage."),
    column(3,
           wellPanel(
             checkboxGroupInput("biais_rowmods", HTML("Modalités en lignes"), 
                                choices=row.names, selected=c("Cerastoculteur","Venericulteur")),
             checkboxGroupInput("biais_colmods", HTML("Modalités en colonnes"), 
                                choices=col.names, selected=c("Potjevleesch","Kouign-amann")),
             numericInput("biais_ntot", HTML("Effectif total"), value=1000, min=1, max=1000000),
             actionButton("biais_rerun", "Générer", class="btn btn-success", icon=icon("refresh"))
           )),
    column(8,
           conditionalPanel(condition="input.biais_rerun==0",
                            tags$p("Cliquez sur \"Générer\"")),
           conditionalPanel(condition="input.biais_rerun>0",
                            tags$h4("Effectifs"), tableOutput("biais_tabEff"),
                            tags$h4("Pourcentages", style="margin-top: 2em;"), tableOutput("biais_tabPourc"))
         )
)


## INDÉPEDANCE ---------------------------------------------------------

ui_indep <- list(fluidRow(headerPanel(HTML("Tableau théorique <small>sous l'hypothèse d'indépendance</small>")),
    show_alert("Cette page permet de saisir librement les valeurs d'un tableau observé et de calculer les tableaux théoriques sous l'hypothèse d'indépendance correspondants.")),
    wellPanel(
      fluidRow(column(12,
                      tags$p(HTML("<strong>Tableau observé :</strong>")),
tags$div(class="form-inline",           
                               tags$span(class="indep_tab", ""),
                               tags$span(class="indep_tab", "Marrons"),
                               tags$span(class="indep_tab", "Bleus"),
                               tags$span(class="indep_tab", "Verts"),
                               tags$br(),
                               tags$span(class="indep_tab", "Bruns"),
                               numericInput("indep_v1", "", value=10, min=0, max=100000),
                               numericInput("indep_v2", "", value=10, min=0, max=100000),
                               numericInput("indep_v3", "", value=10, min=0, max=100000),
                               tags$br(),
                               tags$span(class="indep_tab", "Blonds"),
                               numericInput("indep_v4", "", value=10, min=0, max=100000),
                               numericInput("indep_v5", "", value=10, min=0, max=100000),
                               numericInput("indep_v6", "", value=10, min=0, max=100000),
                               tags$br(),
                               tags$span(class="indep_tab", "Roux"),
                               numericInput("indep_v7", "", value=10, min=0, max=100000),
                               numericInput("indep_v8", "", value=10, min=0, max=100000),
                               numericInput("indep_v9", "", value=10, min=0, max=100000)
                      )))),
    fluidRow(
      column(12,
             tabsetPanel(
               tabPanel("Tableau observé",
                        fluidRow(
                          column(12,
                                 tags$p("Effectifs :"), 
                                 tableOutput("indep_tabobs"))),
                        fluidRow(
                          column(6,
                                 tags$p("Pourcentages ligne sur les effectifs observés :"),
                                 tableOutput("indep_tabopl")),
                          column(6,
                                 tags$p("Pourcentages colonne sur les effectifs observés :"),
                                 tableOutput("indep_tabopc"))
                        )),
               tabPanel("Tableaux théoriques",
                        show_alert(HTML("Ces tableaux sont les tableaux théoriques calculés <em>sous l'hypothèse d'indépendance des lignes et des colonnes</em>.")),
                        fluidRow(
                          column(6, tags$p("Tableau des effectifs théoriques :"),
                                 tableOutput("indep_tabEff")),
                          column(6, tags$p("Tableau des pourcentages théoriques :"),
                                 tableOutput("indep_tabPourc"))
                        ),
                        fluidRow(
                          column(6, tags$p("Pourcentages ligne sur les effectifs théoriques :"),
                                 tableOutput("indep_tabtpl")),
                          column(6, tags$p("Pourcentages colonne sur les effectifs théoriques :"),
                                 tableOutput("indep_tabtpc"))
                        )))))
)
    
## KHI2 D'UN TABLEAU ARBITRAIRE --------------------------------------

ui_khid <- list(fluidRow(headerPanel("Calcul du χ² d'un tableau"),
  show_alert("Cette page permet de saisir librement les valeurs d'un tableau observé et de calculer la valeur de la statistique du χ² correspondante.")),
  wellPanel(
    fluidRow(column(7,
           tags$p(HTML("<strong>Tableau observé :</strong>")),
           tags$div(class="form-inline",           
            tags$span(class="khid_tab", ""),
            tags$span(class="khid_tab", "Oui"),
            tags$span(class="khid_tab", "Non"),
            tags$span(class="khid_tab", "NSP"),
            tags$br(),
            tags$span(class="khid_tab", "Homme"),
            numericInput("khid_v1", "", value=10, min=0, max=100000),
            numericInput("khid_v2", "", value=10, min=0, max=100000),
            numericInput("khid_v3", "", value=10, min=0, max=100000),
            tags$br(),
            tags$span(class="khid_tab", "Femme"),
            numericInput("khid_v4", "", value=10, min=0, max=100000),
            numericInput("khid_v5", "", value=10, min=0, max=100000),
            numericInput("khid_v6", "", value=10, min=0, max=100000)
          )),
    column(5,
           tags$div(style="font-size: 4em;", textOutput("khid_val"))
    ))
  ),
  fluidRow(column(12,
         tabsetPanel(
           tabPanel("Tableau observé",
                    fluidRow(
                      column(6, tags$p("Effectifs observés :"),
                                tableOutput("khid_obseff")),
                      column(6, tags$p("Pourcentages observés :"),
                                tableOutput("khid_obspourc"))),
                    fluidRow(
                      column(6, tags$p("Pourcentages ligne :"),
                             tableOutput("khid_obspl")),
                      column(6, tags$p("Pourcentages colonne :"),
                             tableOutput("khid_obspc")))),
           tabPanel("Tableau théorique et écarts",
                    fluidRow(
                      column(6, tags$p("Effectifs théoriques sous l'hypothèse d'indépendance :"),
                                tableOutput("khid_theff")),
                      column(6, tags$p("Pourcentages théoriques sous l'hypothèse d'indépendance :"),
                                tableOutput("khid_thpourc"))),
                    fluidRow(
                      column(6, tags$p("Écarts observés - théoriques :"),
                                tableOutput("khid_ecarts")),
                      column(6, tags$p("χ² partiels :"),
                                tableOutput("khid_partiels"))))
           )))
 )
  

## SIMULATION D'UN KHI2 SOUS HYPOTHESE D'INDEPENDANCE -------------------------------

ui_sim1 <- list(fluidRow(
  headerPanel(HTML("Simulation des valeurs du χ² <small>sous l'hypothèse d'indépendance</small>"))),
  fluidRow(show_alert("Ici on part d'un tableau observé, on simule un grand nombre de tableaux équivalents sous l'hypothèse d'indépendance, et on calcule et représente la distribution des valeurs de la statistique du χ² obtenues.")),
  wellPanel(
    fluidRow(
      column(3,
             tags$p(HTML("<strong>Tableau observé de départ :</strong>")),
             tags$table(class="data table table-condensed",
               tags$thead(
                 tags$tr(tags$th(""),tags$th("Oui"),tags$th("Non"),tags$th("NSP"))),
                tags$tbody(
                  tags$tr(tags$td("Homme"),tags$td("446"),tags$td("445"),tags$td("8")),
                  tags$tr(tags$td("Femme"),tags$td("490"),tags$td("592"),tags$td("19"))
                ))),
      column(3, tags$div(style="font-size: 1.5em; margin-top: 2.5em;", "χ² = 7.06")),
      column(5, tags$p(HTML("<strong>Tableau théorique correspondant :</strong>")),
           tableOutput("sim1_thq")
    ))),
  column(3,
         wellPanel(
           numericInput("sim1_nb", "Nombre de simulations", value=1, min=1, max=100000),
           conditionalPanel("input.sim1_nb>=100",
              checkboxGroupInput("sim1_opts", "Options", 
                                 choices=c("Histogramme", "Comparaison", "p-value", "Courbe"))),
           actionButton("sim1_run", "Générer", class="btn btn-success", icon=icon("refresh"))
         )),
  column(8,
         #tags$h3("Tableau théorique"),
         #tableOutput("sim1_thq"),
         conditionalPanel("input.sim1_nb<5",
                          tags$h3("Effectifs observés simulés"),
                          conditionalPanel("input.sim1_nb>=1",
                            fluidRow(
                              column(6, tableOutput("sim1_obs1")),         
                              column(6, tags$div(style="font-size: 2em;", 
                                                 textOutput("sim1_khid1"))))),
                          conditionalPanel("input.sim1_nb>=2",
                            fluidRow(
                              column(6, tableOutput("sim1_obs2")),         
                              column(6, tags$div(style="font-size: 2em;", 
                                                 textOutput("sim1_khid2"))))),
                          conditionalPanel("input.sim1_nb>=3",
                            fluidRow(
                              column(6, tableOutput("sim1_obs3")),         
                              column(6, tags$div(style="font-size: 2em;", 
                                                textOutput("sim1_khid3"))))),
                          conditionalPanel("input.sim1_nb>=4",
                            fluidRow(
                              column(6, tableOutput("sim1_obs4")),         
                              column(6, tags$div(style="font-size: 2em;", 
                                                 textOutput("sim1_khid4")))))
         ),
         conditionalPanel("input.sim1_nb>=5 && input.sim1_nb<100",
                          tags$h3("Valeurs obtenues par simulation"),
                          tags$div("Valeurs du χ² obtenues : ",
                                  textOutput("sim1_valeurs", inline=TRUE))
         ),                  
         conditionalPanel("input.sim1_nb>=100",
                          tags$h3("Valeurs obtenues par simulation"),
                          tags$p("Dix dernières valeurs du χ² obtenues : ", 
                                 textOutput("sim1_valeurs_head", inline=TRUE), "(...)"),
                          plotOutput("sim1_hist"),
                          htmlOutput("sim1_comp"),
                          htmlOutput("sim1_pval")
                          
         )                  
  )
  )
  

## EXERCICES D'APPLICATION SUR HDV 2003 ------------------------------------

ui_pq <- fluidRow(
    headerPanel(HTML("Tableaux croisés et test du χ² à partir de l'enquête Histoire de vie")),
    show_alert("Cette page permet d'effectuer des tris croisés et tests du χ² sur un extrait de l'enquete <em>Histoire de vie</em> (2003)."),
    column(3,
           wellPanel(
               selectInput("pq_varl", "Variable en ligne", choices=c("---", d.vars)),
               selectInput("pq_varc", "Variable en colonne", choices=c("---", d.vars)),
               actionButton("pq_inverse", "Inverser", class="btn", icon=icon("retweet")),
               tags$hr(),
               selectInput("pq_subset", "Sous-population",
                           choices=c("Tout le monde", "Hommes seulement", "Femmes seulement")),
               tags$hr(),
               htmlOutput("pq_khid")
           )),
    column(8,
           tabsetPanel(
               tabPanel("Effectif",
                        tableOutput("pq_eff")
                        ),
               tabPanel("% ligne",
                        tableOutput("pq_rprop")),
               tabPanel("% colonne",
                        tableOutput("pq_cprop")),
               tabPanel("Résidus",
                        tableOutput("pq_resid"),
                        plotOutput("pq_mosaic",height=600)
                        ),
               tabPanel("À propos", 
                        hdv_description,
                        hdv_listevars)
    )
    
))


navbarPage("Formation χ²",
            header=tags$head(tags$style(HTML(css_string))),
            tabPanel("Biais", ui_biais),
            tabPanel("Indépendance", ui_indep),
            tabPanel("χ² d'un tableau", ui_khid),
            tabPanel("Simulations", ui_sim1),
            tabPanel("Pratique", ui_pq)
)





