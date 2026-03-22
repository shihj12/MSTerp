page_tutorial_ui <- function() {
  msterp_page(
    title = "Tutorial",
    actions = tags$span(class = "pill", "Draft"),
    tags$p("Documentation, statistical guides, sample data, and walkthroughs."),
    div(
      class = "grid",
      div(
        class = "card",
        tags$h3("Statistical guide"),
        tags$p("Explaining pros and cons of parameters for different datasets."),
        tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Draft")
      ),
      div(
        class = "card",
        tags$h3("Pipeline patterns"),
        tags$p("Analysis sequences and how to compose them."),
        tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Draft")
      ),
      div(
        class = "card",
        tags$h3("Sample Data & Walkthrough"),
        tags$p("A guided tutorial using published data to generate an example analysis."),
        tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Draft")
      )
    )
  )
}

page_tutorial_server <- function(session) { invisible(NULL) }
