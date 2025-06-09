library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

server <- analogsea::droplets()$`climr-server`
reset_ssh_sessions()

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/climr-app")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/climr-app")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/climr-app")
droplet_upload(server, "./app.R", "/srv/shiny-server/climr-app")
droplet_upload(server, "./scripts", "/srv/shiny-server/climr-app")
droplet_upload(server, "./www", "/srv/shiny-server/climr-app")
droplet_upload(server, "./assets", "/srv/shiny-server/climr-app")
droplet_upload(server, c("./data", "./fonts", "./lib"), "/srv/shiny-server/climr-app")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")

library(terra)
dat <- rast("downscale_PPTD2R49_202506061656_map_draw_1.tif")
install.packages("sf")
install.packages("terra")
