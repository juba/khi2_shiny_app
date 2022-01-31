hdv_description <- list(
  tags$p("L'enquête Histoire de vie a été réalisée par l'INSEE et l'INED en 2003. Elle comprend à la fois un questionnaire sur des thèmes assez larges (activité, goûts, engagements, handicap, discriminations) et une parti biographique."),
  tags$p("Plus d'informations et téléchargement des données sont disponibles ", tags$a(href="https://www.insee.fr/fr/statistiques/2532244", "sur le site de l'INSEE.")),
  tags$p("Ici on travaille sur un extrait de l'enquête, comprenant ", nrow(d), "individus et ", ncol(d), "variables."))

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
@import url(https://fonts.googleapis.com/css?family=Open+Sans);
@import url(https://fonts.googleapis.com/css?family=Roboto);
* {font-family: 'Roboto', 'Open Sans', 'Helvetica', sans-serif;}
body {padding-bottom: 6em;}
.container-fluid {max-width: 1170px;}
.nav-tabs {margin-bottom: 25px;}
table td {text-align: right;}
table tbody tr:hover {background-color: #F0F0F0;}
table.data td[align=right] {font-family: 'Roboto', 'Open Sans', 'Helvetica', sans-serif;}
h1 { margin-bottom: 1em; font-size: 2.2em;}
.form-inline .form-control {width: 7em !important;}
span.khid_tab, span.indep_tab {width: 7em; display:inline-block; text-align: right;}
span.sim1_tab {width: 4em; display:inline-block; text-align: right;}
.echantillon {
  background-color: #F0F0F0;
  padding: 1em;
  border-radius: 1em;
  width: 100%; height: 15em;
  overflow-y: scroll;
}
.echantillon-small {
  height: 11em;
}

"

show_alert <- function(texte) {
  column(12,
    tags$div(class="alert alert-info alert-dismissable",
      HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
      HTML(texte)
    )
  )
} 


## PROPORTION - BIAIS ----------------------------------------------------------

ui_prop_biais <- fluidRow(
  headerPanel("Estimation d'une proportion - Biais d'échantillonnage"),
  column(3,
    wellPanel(
      sliderInput("prop_biais_value", 
        "Proportion de femmes dans la population",
        min = 0, max = 100, step = 1, value = 50, round = TRUE),
      numericInput("prop_biais_size",
        "Taille de l'échantillon",
        min = 2, max = 10000, value = 200),
      actionButton("prop_biais_rerun", "Générer", class = "btn btn-success", icon = icon("refresh"))
    )
  ),
  column(8,
    uiOutput("prop_biais_sim")  
  )
  
)

## PROPORTION - SIMULATIONS ----------------------------------------------------

ui_prop <- fluidRow(
  headerPanel("Estimation d'une proportion - Simulations"),
  column(3,
    wellPanel(
      sliderInput("prop_value", 
        "Proportion de femmes dans la population",
        min = 0, max = 100, step = 1, value = 50, round = TRUE),
      numericInput("prop_size",
        "Taille de l'échantillon",
        min = 2, max = 10000, value = 200),
      numericInput("prop_sim", 
        "Nombre de tirages",
        min = 1, max = 1000, value = 1),
      conditionalPanel("input.prop_sim > 10",
        checkboxInput("prop_show_plot", "Afficher l'histogramme", value = FALSE)),
      conditionalPanel("input.prop_sim > 10 &input.prop_show_plot",
        checkboxInput("prop_fix_x", "Fixer l'axe horizontal à 0-100", value = FALSE),
        numericInput("prop_ech", "Pourcentage obtenu dans notre échantillon",
          min = 0, max = 100, step = 1, value = 53),
        checkboxInput("prop_show_value", "Surligner la valeur dans l'échantillon", value = FALSE),
        checkboxInput("prop_show_zones", "Surligner les valeurs plus extrêmes", value = FALSE),
        checkboxInput("prop_show_test", "Afficher test", value = FALSE),
        checkboxInput("prop_show_curve", "Afficher courbe", value = FALSE)),  
      actionButton("prop_rerun", "Générer", class = "btn btn-success", icon = icon("refresh"))
    )),
  column(8,
    conditionalPanel("input.prop_sim <= 3",
      uiOutput("prop_sim3")
    ),
    conditionalPanel("input.prop_sim > 3",
      conditionalPanel("input.prop_sim <= 100 | !input.prop_show_plot",
        uiOutput("prop_sim")),
      conditionalPanel("input.prop_show_plot",
        h4("Histogramme des valeurs obtenues"),
        plotOutput("prop_plot"),
        conditionalPanel("input.prop_show_zones",
          uiOutput("prop_extr_values")),
        conditionalPanel("input.prop_show_test",
          uiOutput("prop_p"))
      )
    )
  )
)

## KHI2 - BIAIS ----------------------------------------------------------


ui_biais <- fluidRow(
    headerPanel("χ² - Biais d'échantillonnage"),
    show_alert("Cette page simule la réalisation d'une enquête par questionnaire. On interroge une population au sujet de deux variables parfaitement indépendantes pour visualiser l'effet du biais d'échantillonnage."),
    column(12,
           wellPanel(
             fluidRow(
               column(4,
                  HTML("<p><strong>Répartition du <tt>sexe</tt> dans la population :</strong></p>",
                  HTML("<table class='data table table-condensed' style='width: auto;'>"),
                  HTML("<tr><td>Homme</td><td>45 %</td></tr>"),
                  HTML("<tr><td>Femme</td><td>55 %</td></tr>"),
                  HTML("</table>"))),
               column(4,
                 HTML("<p><strong>Répartition de <tt>clso</tt> dans la population :</strong></p>"),
                 HTML("<table class='data table table-condensed' style='width: 
                 auto;'>"),
                 HTML("<tr><td>Oui</td><td>Non</td><td>NSP</td></tr>"),
                 HTML("<tr><td>47 %</td><td>52 %</td><td>1 %</td></tr>"),
                 HTML("</table>"))
              )
            )
           ),
    column(3,
           wellPanel(
             numericInput("biais_ntot", HTML("Nombre de personnes interrogées"), value=1000, min=1, max=1000000),
             actionButton("biais_rerun", "Générer", class="btn btn-success", icon=icon("refresh"))
           )),
    column(8,
           conditionalPanel(condition="input.biais_rerun==0",
                            tags$p("Cliquez sur \"Générer\"")),
           conditionalPanel(condition="input.biais_rerun>0",
              tabsetPanel(
                tabPanel("Effectifs", tags$h4("Effectifs"), tableOutput("biais_tabEff")),
                tabPanel("Pourcentages ligne", tags$h4("Pourcentages ligne"), tableOutput("biais_tablprop")),
                tabPanel("Pourcentages colonne", tags$h4("Pourcentages colonne"), tableOutput("biais_tabcprop"))
              )
           )
         )
)

## KHI2 - SIMULATIONS ----------------------------------------------

ui_sim1 <- list(fluidRow(
  headerPanel(HTML("χ² - Simulation <small>sous l'hypothèse d'indépendance</small>"))),
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
              checkboxGroupInput("sim1_opts", "Afficher", 
                                 choices=c("Histogramme", "Valeur obtenue", "Valeurs plus extrêmes", "Test", "Courbe"))),
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


navbarPage("Formation inférence",
            header=tags$head(tags$style(HTML(css_string))),
            tabPanel("Proportion - Biais", ui_prop_biais),
            tabPanel("Proportion - Simulation", ui_prop),
            tabPanel("χ² - Biais", ui_biais),
            tabPanel("χ² - Simulation", ui_sim1),
            tabPanel("χ² - Pratique", ui_pq)
)





