# For now, keep the ui.R file separate so it'll be easier to combine and reduce
# modules. 
# 
# The shinyapps.io server doesn't allow more than 10,000 files so we cannot 
# show all lorenz curves remotely (it should still work locally). We'll use 
# two versions of this app -- one is the fully functional app on a subset
# of drugs and the other is on all drugs. 
shinyUI(
    lorenz_panel()
    # lorenz_panel_mini()
)
