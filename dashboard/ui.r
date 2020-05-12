bootstrapPage(div(class = "outer",
      tags$head(includeCSS("styles.css")),
    fluidPage(id = "ARTPage", 
      titlePanel("V-Forecast: Predicting Adverse Regime Transitions (PART)"), 
        fluidRow(
          column(12, 
            wellPanel(id = "intro", class = "panel panel-default", #fixed = FALSE,
              h3(tags$span(style = "font-size: 80%; color: black;", "The PART project uses data from the", a(href= "https://www.v-dem.net", "Varieties of Democracy (V-Dem)", target="_blank"), "Institute and other sources to estimate the risk of adverse regime transitions during the next", tags$b("two years (2019-2020)."), 
                br(),
                br(), 
                "We define an adverse regime transition as a year-to-year decrease in the", 
                a(href = "https://www.v-dem.net/files/5/Regimes%20of%20the%20World%20-%20Final.pdf", "Regimes of the World (RoW) index,", target="_blank"), 
                "which classifies political regimes as either a ", tags$b("closed autocracy, electoral autocracy, electoral democracy, or liberal democracy. "), "The PART project estimates the risk of moving down this scale within a two-year window.", 
                br(),
                br(), 
                "For more details, please see our", a(href= "https://www.v-dem.net/media/filer_public/b9/b2/b9b2c233-ec45-425d-a397-1cd80dadb63a/v-dem_working_paper_2019_89.pdf", " working paper.", target="_blank"), "We welcome feedback."
                )
              )
            )
          )
        ),

      fluidRow(
        column(7, id = "mapPanel",   
          leafletOutput("map1", width = "100%", height = "550px")), 
        column(5, id = "hcbarplotID",
          highchartOutput("hcbarplot", height = "575px"))
        ),

      fluidRow(
        column(12,  
                h3(tags$span(style = "font-size: 80%; color: black;",
                "Click on a country in the map, the plot, or select a country from the list below to view its estimated risk and select regime characteristics.")), 
        h3(tags$b("Country Characteristics:"))
        )
      ),

      fluidRow(
        column(4, 
        selectInput("countrySelect", choices = countryNamesText, 
          label = h4(tags$b("1) Select a country")), selectize = TRUE)), 
        column(8, 
          h3(textOutput("ROWclass")))
        ),
      fluidRow(
        column(2,
          checkboxGroupInput("checkGroup", label = h4(tags$b("2) Select V-Dem indices to compare")), inline = FALSE, 
            choiceNames = list(
            tags$span("Liberal Component", style = paste("color:", v2x_liberal_color, "; font-weight: bold; font-size: 90%;", sep = "")), 
            tags$span("Equality Before the Law & Ind. Liberty", style = paste("color:", v2xcl_rol_color, "; font-weight: bold; font-size: 90%;", sep = "")), 
            tags$span("Judicial Constraints on the Executive", style = paste("color:", v2x_jucon_color, "; font-weight: bold; font-size: 90%;", sep = "")), 
            tags$span("Legislative Constraints on the Executive", style = paste("color:", v2xlg_legcon_color, "; font-weight: bold; font-size: 90%;", sep = "")),
            tags$span("Civil Liberties", style = paste("color:", v2x_civlib_color, "; font-weight: bold; font-size: 90%;", sep = "")),
            tags$span("Electoral Democracy", style = paste("color:", v2x_polyarchy_color, "; font-weight: bold; font-size: 90%;", sep = "")), 
            tags$span("Clean Elections", style = paste("color:", v2xel_frefair_color, "; font-weight: bold; font-size: 90%;", sep = "")), 
            tags$span("Freedom of Expression & Alt. Sources of Info", style = paste("color:", v2x_freexp_altinf_color, "; font-weight: bold; font-size: 90%;", sep = "")),
            tags$span("Freedom of Association (thick)", style = paste("color:", v2x_frassoc_thick_color, "; font-weight: bold; font-size: 90%;", sep = ""))),
        choiceValues = c(
              "v2x_liberal",
              "v2xcl_rol",
              "v2x_jucon",
              "v2xlg_legcon",
              "v2x_civlib",
              "v2x_polyarchy",
              "v2xel_frefair",
              "v2x_freexp_altinf",
              "v2x_frassoc_thick")),
          checkboxInput("plotCIs", label = "Confidence Intervals", value = FALSE)),
        column(6, id = "hcbarplotID2",
          highchartOutput("TimeSeriesPlot", height = "475px")),
        column(4, id = "hcbarplotID1",
          highchartOutput("riskPlot", height = "475px"))#, 
        ),
     fluidRow(
      column(2, ""),
      column(6, 
        wellPanel(id = "note1", class = "panel panel-default",
          h5(tags$span(style = "font-size: 100%;", 
            tags$b("Interpretation:"),"The complexity of the machine learning models we use makes it difficult to identify which specific variables are driving predictions. Therefore, ",tags$b("the time-series plot is for descriptive purposes only."), "It is meant to help identify trends, not causal relationships, across key V-Dem indices. For access to country-level time series across all V-Dem variables, please use V-Dem's Country Graph tool ",tags$b(a(href= "https://www.v-dem.net/en/analysis/CountryGraph/", "here", target="_blank"))))
          )
        ),
      column(4, 
        wellPanel(id = "note2", class = "panel panel-default",
          h5(tags$span(style = "font-size: 100%; font-weight: bold; color: #0498F0;", "Blue bars"), " indicate that an ART occurred within 2-year forecast window", br(), tags$span(style = "font-size: 100%; font-weight: bold; color: #C48BC8;", "Purple bars"), " indicate that no ART occurred within 2-year forecast window", br(), tags$span(style = "font-size: 100%; font-weight: bold; color: #A29C97;", "Gray bars"), " indicate the estimated risk for current 2-year forecast window"))
        )
      ),

     fluidRow( 
      column(12, 
        wellPanel(id = "methods", class =  "panel panel-default",
          # h3(tags$b("Note")), 
          h3(tags$b("Methodology"),  tags$span(style = "font-size: 70%; color: black;", "(See", a(href= "https://www.v-dem.net/media/filer_public/b9/b2/b9b2c233-ec45-425d-a397-1cd80dadb63a/v-dem_working_paper_2019_89.pdf", " working paper", target="_blank"), " for more details)")), 
          h4(tags$span(style = "font-size: 90%; color: black;", "It is important to note that these forecasts are probabilistic: a high estimated risk does not mean that an ART will occur with certainty; similarly, a low estimated risk does not mean that there is zero change an ART will not occur.", 
            br(),
            br(),
            "Further, given that this is the first year of this project, we do not yet know how accurately this project can forecast future transitions in the real world as opposed to statistical simulations and out-of-sample tests. However, we are posting these forecasts in the interest of transparency.")), 
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Yearly Risk Estimates"), " - We derive risk estimates for 2011-2017 using a simulation framework that mimics the process we use to produce our 2019-2020 forecasts. In short, we first train our models using all data from 1970 to 2009. We then use data from 2010 to produce estimated risk forecasts for 2011-12. We then retrain our models using all data from 1970 to 2010, use data from 2011 to produce estimates for 2012-12. We conduct this iterative model check procedure for all years, 2011 to 2017.")),
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Data"), "- To produce our estimated risk forecasts, we use", a(href= "https://www.v-dem.net/en/data/data-version-9/", "V-Dem data version 9", target="_blank"), "along with" , a(href= "https://unstats.un.org/unsd/snaama/Index", "UN GDP and population data,", target="_blank"), a(href= "https://icr.ethz.ch/publications/integrating-data-on-ethnicity-geography-and-conflict/", "ethnic power relations data (Vogt et al. 2015),", target="_blank"), a(href= "https://www.jonathanmpowell.com/coup-detat-dataset.html", "coup event data (Powell and Thyne 2011),", target="_blank"), "and", a(href= "https://ucdp.uu.se/downloads/", "UCDP armed conflict data (Pettersson and Eck 2018),", target="_blank"), "over 400 variables altogether. We lag all variables one year and derive the first differences for a number of variables. All of variables we use in our models are lagged one year. Thus, data for 2016 is used to estimate the risk of ARTs for 2017/18.")), 
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Unit of analysis "), "- We use country-years as our unit of analysis and limit our temporal frame to 1970-2018. We reconcile the differences between the V-Dem country-year set and the", a(href= "https://www.andybeger.com/states/reference/gwstates.html", "Gleditsch and Ward", target="_blank"), "country-year set. This leaves 169 countries for our 2019-2020 forecasts. The number of countries in our data per year ranges from 135 to 169. Our training and validation country-year set covers 7,754 country-year observations.")), 
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Effective positive rate"), "- The yearly rate of adverse regime transitions of our dependent variable in any given year ranges from around 1.4 percent to 10 percent; 75 percent of our yearly positive rates are between 3.8 percent and 5.6 percent.")), 
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Models"),"- We use three machine learning models: logit with elastic-net regularization, random forest, and gradient boosted forest. To help account for differences across these models, we use an unweighted model average ensemble. This is our preferred approach, as it helps smooth out our predicted risk estimates while improving accuracy. The estimates shown above are from this ensemble model.")),
          h4(tags$span(style = "font-size: 90%; color: black;", tags$b("Model Assessment"),"- Our unweighted model average ensemble model reports an AUC-PR score of 0.46 in a 2x7 repeated cross-validation procedure (1970-2017) and an AUC-PR score of 0.39 in our set of yearly test forecasts (2011-2017). As a general benchmark of performance, an AUC-PR score that is higher than the observed frequency of events in the data is a signal that the model is an improvement over chance. With an observed frequency of ARTs at roughly 4 percent, our unweighted model average ensemble exceeds performance expectations."))
          )
        )
      )
  )  ))