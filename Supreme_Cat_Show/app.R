library(shiny)
library(bslib)
library("tidyverse")

scs_winners <- read.csv("/cloud/project/Supreme_Cat_Show/data/scs_winners.csv")

ui <- fluidPage(
  titlePanel("Supreme Cat Show Winners Data"),
  navset_tab(
    nav_panel("Data Columns",
              titlePanel("Supreme Cat Show Winners Data"),
              card(
                card_header("year"),
                "The year that the show was held in."
              ),
              card(
                card_header("name"),
                "The name of the winning cat, including its cattery prefix and any other identifiers."
              ),
              card(
                card_header("sex"),
                "The sex of the cat, including whether or not it has been spayed/neutered. Some cats did not have this information available."
              ),
              card(
                card_header("breed"),
                "The specific breed of the cat."
              ),
              card(
                card_header("coat"),
                "The coat color and/or pattern of the cat. This was not specified for all cats."
              ),
              card(
                card_header("title"),
                "Competition title(s) held by the cat at the time it won."
              ),
              card(
                card_header("owner_1"),
                "The owner of the cat, or one of the owners if it has two. No cat in this dataset was co-owned by more than two people."
              ),
              card(
                card_header("owner_2"),
                "If the cat had more than one owner, the second will be listed here."
              ),
              card(
                card_header("breeder_1"),
                "The breeder of the cat, or one of the breeders if it has two."
              ),
              card(
                card_header("breeder_2"),
                "If the cat had more than one breeder, the second will be listed here."
              ),
              card(
                card_header("breeder_owner_match"),
                "A boolean value for whether or not one or more owners of the cat are also the breeder(s)."
              ),
              card(
                card_header("section"),
                "The category a cat is judged in. From 1-7, the sections are Persian, Semi-longhair, British, Foreign, Burmese, Oriental, and Siamese."
              ),
              card(
                tableOutput("table")
              )
              ),
    nav_panel("Line Graph",
              card(plotOutput("male_female_plot"))
              ),
    nav_panel("Wins by Section, Accumulating by Year",
              card(sliderInput(inputId = "in_year", label = "Year", min = 1976, max = 2026, value = 2026)),
              card(plotOutput("wins_section_plot"))
              )
              )
  )
    

server <- function(input, output) {
    
    output$table <- renderTable({
      scs_winners
    })
    
    output$wins_section_plot <- renderPlot({
    scs_winners |>
        filter(year <= input$in_year) |>
        ggplot(mapping = aes(x = section)) + 
        geom_bar() + 
        scale_x_continuous(breaks = scs_winners$section) +
        labs(x = "Section", y = "Count", title = "Supreme Cat Show Winners by Section", subtitle = "1976-2026")
    })
    
    output$male_female_plot <- renderPlot({
      scs_winners |> 
        filter(!str_detect(sex, "unknown")) |> 
        arrange(year) |>
        mutate(ccount = row_number()) |>
        ggplot(mapping = aes(x = year, y = ccount, color = sex)) +
        geom_line() + 
        labs(x = "Year", y = "Count", title = "Winners of Supreme Cat Show Over Time By Sex", subtitle = "1976-2026")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
