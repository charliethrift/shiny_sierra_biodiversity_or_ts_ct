# theme
# for shiny app
# 16 february 2025
my_theme <- bs_theme(bootswatch = 'sketchy') %>% 
  bs_theme_update(bg='#dcdec8',
                  fg='#323133',
                  primary="#1e17a6",
                  secondary="#645df5",
                  success = "#26de57",
                  info="#1b8c76",
                  warning = "#f0ec26",
                  danger = "#f04b26",
                  base_font = font_google("Rasa"),
                  code_font = font_google("Atkinson Hyperlegible Mono"),
                  heading_font = font_google("Rubik Dirt"),
                  font_scale = 1.25)