# The following checks to see if the environment variable FLOW_TOOL_BASE
# has been defined.

if (!exists("FLOW_TOOLS_BASE")) {
  stop(paste("please define a variable FLOW_TOOLS_BASE pointing to this directory.\n",
             "Suggestion: put it in your .Rprofile."))
} else {
  cat("Source'ing Wade's flow cytometry tools\n")
}

source(paste(FLOW_TOOLS_BASE, "/computils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/transutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/logutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/classutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/distribution_analysis/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/visutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/ffutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/interactutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/dbutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/heatutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/cleanutils/sourceDirectory.R", sep = ""))
source(paste(FLOW_TOOLS_BASE, "/genericutils/sourceDirectory.R", sep = ""))
