library(shiny)
library(arules)
library(arulesViz)

# to read data, it has to be of two columns
tr <- read.transactions("data.csv", format = "single", sep = ",", cols = c(1, 2), rm.duplicates = TRUE)

data("Groceries")
items <- sample(Groceries@itemInfo$labels, 20)

ar <- apriori(Groceries,
              parameter = list(
                support = 0.0003,
                confidence = 0.6,
                maxlen = 5
              ))

#plot(ar, method = "graph", engine = "htmlwidget")
#plot(ar, method = "paracoord")

# redundant <- which(colSums(is.subset(ar, ar)) > 1)
#
# ar <- ar[-redundant]

# Build UI.
ui <- fluidPage(titlePanel("Market Basket"),
                sidebarLayout(
                  sidebarPanel(
                    br(),
                    strong("Cart Item(s)"),
                    checkboxGroupInput(
                      "items",
                      label = "",
                      choices = items,
                      selected = sample(items, 3)
                    )
                  ),
                  mainPanel(
                    br(),
                    br(),
                    strong("Frequently Bought With: "),
                    tableOutput("recommendations"),
                    br(),
                    plotOutput("rulegraph")
                  )
                ))


server <- function(input, output) {
  recos <- reactive({
    subs <- subset(ar,
                   subset = lhs %in% input$items)
    s <- sort(subs, by = "confidence")
    head(s)
  })
  
  output$recommendations <- renderTable({
    tryCatch(
      expr = {
        recs <- data.frame()
        r <- recos()
        if (length(r) < 5) {
          recs <- r
        } else {
          recs <- r[1:5,]
        }
        recs <- unique(rhs(sort(recs, by = "lift")))
        inspect(recs)
      },
      error = function(error) {
        msg <- data.frame(msg = "Please Select an item!")
        colnames(msg) <- NULL
        msg
      }
    )
  })
  
  output$rulegraph <- renderPlot({
    plot(recos(), method = "graph")
  })
  
}

shinyApp(ui, server)
