## app.R ##
library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(3,
           selectInput("player_selection", "Player",
                       selected = c("Darius Garland"),
                       choices = df %>% mutate(player = as.character(PLAYER_NAME)) %>% arrange(player) %>% pull(player),
                       multiple = T)
           ),
    column(3, 
           selectInput("pos_selection", "Selection", choices = c("Guard", "Wing", "Big"))
           )
  ),
  
  fluidRow(
    column(4,
           plotOutput('rim_plt')
           ),
    column(4,
           plotOutput('nonra_plt')
           ),
    column(4,
           plotOutput('mid_plt')
           )
  ),
  
  fluidRow(
    column(6,
           plotOutput('ab3_plt')
           ),
    column(6,
           plotOutput('c3_plt')
           )
  ),
  
  fluidRow(
    DT::dataTableOutput('tbl', width = 1500)
  )
  
)

server <- function(input, output) {
  
  player_selected = reactive({
    input$player_selection
  })
  
  this_season_df = reactive({
    seasonal_df %>%
      filter(season == "2019-20") %>%
      rename(Rim_FGM=FGM, Rim_FGA=FGA) %>%
      mutate_at(vars(ends_with('FGM'), 
                     ends_with('FGA')), as.character) %>%
      mutate_at(vars(ends_with('FGM'), 
                     ends_with('FGA')), as.numeric) %>%
      group_by(PLAYER_ID, PLAYER_NAME) %>%
      summarise_if(is.numeric, sum) %>%
      mutate(C3_FGM = LC3_FGM + RC3_FGM, C3_FGA = LC3_FGA + RC3_FGA) %>%
      mutate(total_FGM = Rim_FGM + NonRA_FGM + Mid_FGM + AB3_FGM + C3_FGM,
             total_FGA = Rim_FGA + NonRA_FGA + Mid_FGA + AB3_FGA + C3_FGA,
             FGPCT = total_FGM/total_FGA,
             eFG = (Rim_FGM + NonRA_FGM + Mid_FGM + AB3_FGM * 1.5 + C3_FGM * 1.5)/total_FGA) %>%
      ungroup()
  })
  
  output$rim_plt = renderPlot({
    rim_prior = build_prior(df %>% filter(basic_pos == input$pos_selection) %>% mutate(FG_PCT = Rim_FGM/Rim_FGA), Rim_FGM, Rim_FGA)
    rim_x = seq(0.1, 0.8, 0.001)
    
    setup_df <- career_rim %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      tidyr::crossing(x = rim_x) %>%
      ungroup() %>%
      mutate(density = dbeta(x, alpha1, beta1))
    
    ggplot(setup_df, aes(x, density, color = PLAYER_NAME)) +
      geom_line() +
      ggtitle("Rim PCT") +
      stat_function(fun = function(y) dbeta(y, rim_prior$alpha0, rim_prior$beta0),
                    lty = 2, color = "black") +
      theme_classic()
  })
  
  output$nonra_plt = renderPlot({
    nonra_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = NonRA_FGM/NonRA_FGA), NonRA_FGM, NonRA_FGA)
    nonra_x = seq(0.1, 0.8, 0.001)
    
    setup_df <- career_nonra %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      tidyr::crossing(x = nonra_x) %>%
      ungroup() %>%
      mutate(density = dbeta(x, alpha1, beta1))
    
    ggplot(setup_df, aes(x, density, color = PLAYER_NAME)) +
      geom_line() +
      ggtitle("Non-RA PCT") +
      stat_function(fun = function(y) dbeta(y, nonra_prior$alpha0, nonra_prior$beta0),
                    lty = 2, color = "black") +
      theme_classic()
  })

  output$mid_plt = renderPlot({
    mid_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = Mid_FGM/Mid_FGA), Mid_FGM, Mid_FGA)
    mid_x = seq(0.1, 0.8, 0.001)
    
    setup_df <- career_mid %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      tidyr::crossing(x = mid_x) %>%
      ungroup() %>%
      mutate(density = dbeta(x, alpha1, beta1))
    
    ggplot(setup_df, aes(x, density, color = PLAYER_NAME)) +
      geom_line() +
      ggtitle("Midrange PCT") +
      stat_function(fun = function(y) dbeta(y, mid_prior$alpha0, mid_prior$beta0),
                    lty = 2, color = "black") +
      theme_classic()
  })
  
  output$ab3_plt = renderPlot({
    ab3_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = AB3_FGM/AB3_FGA), AB3_FGM, AB3_FGA)
    ab3_x = seq(0.1, 0.8, 0.001)
    
    setup_df <- career_ab3 %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      tidyr::crossing(x = ab3_x) %>%
      ungroup() %>%
      mutate(density = dbeta(x, alpha1, beta1))
    
    ggplot(setup_df, aes(x, density, color = PLAYER_NAME)) +
      geom_line() +
      ggtitle("Above Break 3PT PCT") +
      stat_function(fun = function(y) dbeta(y, ab3_prior$alpha0, ab3_prior$beta0),
                    lty = 2, color = "black") +
      theme_classic()
  })
  
  output$c3_plt = renderPlot({
    c3_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = C3_FGM/C3_FGA), C3_FGM, C3_FGA)
    c3_x = seq(0.1, 0.8, 0.001)
    
    setup_df <- career_c3 %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      tidyr::crossing(x = c3_x) %>%
      ungroup() %>%
      mutate(density = dbeta(x, alpha1, beta1))
    
    ggplot(setup_df, aes(x, density, color = PLAYER_NAME)) +
      geom_line() +
      ggtitle("Corner 3PT PCT") +
      stat_function(fun = function(y) dbeta(y, c3_prior$alpha0, c3_prior$beta0),
                    lty = 2, color = "black") +
      theme_classic()
  })
  
  tbl_df = reactive({
    df %>%
      filter(PLAYER_NAME %in% player_selected()) %>%
      left_join(., career_rim %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
      rename(EstRim_FGPCT = estimate,
             EstRim_Perc = est_percentile) %>%
      left_join(., career_nonra %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
      rename(EstNonRA_FGPCT = estimate,
             EstNonRA_Perc = est_percentile) %>%
      left_join(., career_mid %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
      rename(EstMid_FGPCT = estimate,
             EstMid_Perc = est_percentile) %>%
      left_join(., career_ab3 %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
      rename(EstAB3_FGPCT = estimate,
             EstAB3_Perc = est_percentile) %>%
      left_join(., career_c3 %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
      rename(EstC3_FGPCT = estimate,
             EstC3_Perc = est_percentile) %>%
      left_join(., this_season_df() %>% dplyr::select(PLAYER_NAME, eFG), by = "PLAYER_NAME") %>%
      mutate(Rim_FGPCT = Rim_FGM/Rim_FGA,
             NonRA_FGPCT = NonRA_FGM/NonRA_FGA,
             Mid_FGPCT = Mid_FGM/Mid_FGA,
             AB3_FGPCT = AB3_FGM/AB3_FGA,
             C3_FGPCT = C3_FGM/C3_FGA
      ) %>%
      mutate(EsteFG = (Rim_FGA * EstRim_FGPCT + NonRA_FGA * EstNonRA_FGPCT + 
                       Mid_FGA * EstMid_FGPCT + AB3_FGA * EstAB3_FGPCT * 1.5 + C3_FGA * EstC3_FGPCT * 1.5)/
               (Rim_FGA + NonRA_FGA + Mid_FGA + AB3_FGA + C3_FGA)) %>%
      dplyr::select(PLAYER_NAME, basic_pos,
                    Rim_FGA, Rim_FGPCT, EstRim_FGPCT, EstRim_Perc,
                    NonRA_FGA, NonRA_FGPCT, EstNonRA_FGPCT, EstNonRA_Perc,
                    Mid_FGA, Mid_FGPCT, EstMid_FGPCT, EstMid_Perc,
                    AB3_FGA, AB3_FGPCT, EstAB3_FGPCT, EstAB3_Perc,
                    C3_FGA, C3_FGPCT, EstC3_FGPCT, EstC3_Perc,
                    eFG, EsteFG
      )
  })
  
  output$tbl = DT::renderDataTable({
    DT::datatable(tbl_df(), options = list(scrollX = TRUE)) %>% 
      DT::formatPercentage(c("Rim_FGPCT", "EstRim_FGPCT", 
                             "NonRA_FGPCT", "EstNonRA_FGPCT", 
                             "Mid_FGPCT", "EstMid_FGPCT", 
                             "AB3_FGPCT", "EstAB3_FGPCT", 
                             "C3_FGPCT", "EstC3_FGPCT",
                             "eFG", "Est.eFG"), 1)
  })
  
}

shinyApp(ui = ui, server = server)