library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

server <- analogsea::droplets()$`climr-server`
reset_ssh_sessions()

droplet_ssh(server, "R -e \"remotes::install_github('bcgov/climr@devl', upgrade = FALSE)\"")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/climr-app")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/climr-app")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/climr-app")
droplet_upload(server, "./app.R", "/srv/shiny-server/climr-app")
droplet_upload(server, c("./scripts","./server"), "/srv/shiny-server/climr-app")
droplet_upload(server, "./www", "/srv/shiny-server/climr-app")
droplet_upload(server, "./assets", "/srv/shiny-server/climr-app")
droplet_upload(server, c("./data", "./fonts", "./lib"), "/srv/shiny-server/climr-app")

cmd <- "grep -q '^SHINY_DEPLOY=' /srv/shiny-server/climr-app/.Renviron && sed -i 's/^SHINY_DEPLOY=.*/SHINY_DEPLOY=server/' /srv/shiny-server/climr-app/.Renviron || echo 'SHINY_DEPLOY=server' >> /srv/shiny-server/climr-app/.Renviron"
droplet_ssh(server, cmd)
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")

##documentation
droplet_ssh(server, "rm -r /srv/shiny-server/climr-docs")
droplet_upload(server, "./documentation/_book/","/srv/shiny-server/climr-docs")
droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server/climr-docs")



library(terra)
dat <- rast("downscale_PPTD2R49_202506061656_map_draw_1.tif")
install.packages("sf")
install.packages("terra")
