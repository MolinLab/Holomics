library(fresh)
# Create the theme
mytheme <- create_theme(
  # navbar
  
  bs4dash_vars(
    navbar_dark_color = "#FFFFFF",
    navbar_dark_active_color = "#EA8620",
    navbar_dark_hover_color = "#EA8620"
  ),
  
  # main bg
  
  bs4dash_layout(
    main_bg = "#FFFFFF"
  ),
  
  # sidebar
  
  bs4dash_sidebar_dark(
    bg = "#336B87",
    color = "#FFFFFF",
    hover_color = "#EA8620",
    active_color = "000000",
    
    # submenu
    
    submenu_bg = "#336B87",
    submenu_active_color = "#000000",
    submenu_active_bg = "#EA8620",
    submenu_color = "#FFFFFF",
    submenu_hover_color = "#EA8620"
  ),
  
  # status
  
  bs4dash_status(
    primary = "#336B87", warning = "#EA8620"
  )
)