# Sankey scratch file - for use developing visualizations until ready to integrate into template file

# Load packages
library(plyr)
library(dplyr)
library(reshape2)
library(networkD3)

# Load most recently cleaned data file
df_og <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data_clean_2024-02-15.csv")
df_og$cough_dx_binary <- as.integer(ifelse(df_og$cough_dx==0, 0, 1))

# We will start with preparing an example of imci process fidelity illustrated via sankey diagram. Our flow of interest will be:
## cg reported cough (y) -> provider assessed all danger signs? (y/n) -> provider assessed all main symptoms? (y/n) -> imci classification assignment for cough

# Create a data frame  with all unique nodes involved in imci flow of interest
cough_nodes <- data.frame(name = c("Cough reported", "Danger signs assessed", "Main symptoms assessed", "IMCI classification - Cough")) # indices 0, 1, 2, 3

# Create a subset df's with source and target links based on indices in nodes df
cough_df <- subset(df_og, cg_report_cough==1) %>% select(screening_id, cg_report_cough, danger_assessed_yn, mainsxs_assessed_yn, cough_dx_binary)
## Node 0
cough_df0 <- cough_df %>% select(screening_id, cg_report_cough)
cough_mdf0 <- melt(cough_df0)
cough_mdf0 <- cough_mdf0 %>% select(-c("value")) # remove value because it is source (target node determines value for line)
cough_mdf0$variable <- 0 #node index
colnames(cough_mdf0)[colnames(cough_mdf0) == "variable"] = "source"
## Node 1
cough_df1 <- cough_df %>% select(screening_id, danger_assessed_yn)
cough_mdf1 <- melt(cough_df1)
cough_mdf1$variable <- 1 #node index
colnames(cough_mdf1)[colnames(cough_mdf1) == "variable"] = "target"
cough_node01 <- left_join(cough_mdf0, cough_mdf1, by="screening_id") #join nodes 0 and 1 together to form base, repeat steps for node 0 and 1 to create nodes 3 and 4 before rbinding to base

## Node 1a (becomes source for node 2)
cough_mdf1a <- cough_mdf1 %>% select(-c("value")) # remove value because it is source (target node determines value for line)
colnames(cough_mdf1a)[colnames(cough_mdf1a) == "target"] = "source"

## Node 2
cough_df2 <- cough_df %>% select(screening_id, mainsxs_assessed_yn)
cough_mdf2 <- melt(cough_df2)
cough_mdf2$variable <- 2 #node index
colnames(cough_mdf2)[colnames(cough_mdf2) == "variable"] = "target"
cough_node1a2 <- left_join(cough_mdf1a, cough_mdf2, by="screening_id") #join nodes 1a and 2 to rbind to previous join

cough_node012 <- rbind(cough_node01, cough_node1a2) #new base node

## Node 2a (becomes source for node 3)
cough_mdf2a <- cough_mdf2 %>% select(-c("value")) # remove value because it is source (target node determines value for line)
colnames(cough_mdf2a)[colnames(cough_mdf2a) == "target"] = "source"

## Node 3
cough_df3 <- cough_df %>% select(screening_id, cough_dx_binary)
cough_mdf3 <- melt(cough_df3)
cough_mdf3$variable <- 3
colnames(cough_mdf3)[colnames(cough_mdf3) == "variable"] = "target"
cough_node2a3 <- left_join(cough_mdf2a, cough_mdf3, by="screening_id") #join nodes 2a and 3 to rbind to new base node

cough_nodes0123 <- rbind(cough_node012, cough_node2a3)
cough_nodes0123 <- cough_nodes0123 %>% select(-c("screening_id"))

## create sankey
cough_sankey <- sankeyNetwork(
    Links = cough_nodes0123,
    Nodes = cough_nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    units = "Binary",
    #colourScale = "steelblue",
    fontSize=12,
    nodeWidth=30,
    iterations = 100
)

## Sankey diagram is maybe not the best viz option...let's try a funnel diagram?
library(plotly)

funnel_cough <- plot_ly()
funnel_cough <- funnel_cough %>%
                  add_trace(
                    type = "funnel",
                    y = c("Reported cough", "Danger signs assessed", "Main symptoms assessed", "Yellow/Red IMCI classification for cough"),
                    x = c(sum(cough_df$cg_report_cough, na.rm = T), sum(cough_df$danger_assessed_yn, na.rm = T), sum(cough_df$mainsxs_assessed_yn, na.rm = T), sum(cough_df$cough_dx_binary, na.rm = T)),
                    textinfo = "value+percent previous",
                    marker = list(color = c("paleturquoise", "darksalmon", "#00ffff", "coral")),
                    connector = list(line = list(color = "darkgray", width = 2)),
                    hoverinfo = 'skip'
                  )
funnel_cough <- funnel_cough %>%
                  layout(yaxis = list(categoryarray = c("Reported cough", "Danger signs assessed", "Main symptoms assessed", "Yellow/Red IMCI classification for cough")),
                         title = "IMCI Process Fidelity for Cough (N, % of previous)")

# Getting closer but not quite there...let's try a proper consort...maybe
library(consort)
df_consort <- df_og %>% select(screening_id, cg_report_cough, cg_report_fever, cg_report_rapidbreathing, cg_report_diarrhea, cg_report_vomit, danger_assessed_yn, cough_yn, dyspnea_yn, diarrhea_yn, fever_yn, earproblem_yn, anemia_yn, rr_yn, spo2_yn, pr_yn, temp_yn, weight_yn, height_yn, muac_yn, wfh_yn, hb_yn, cough_dx, diarrhea_dx, fever_dx, ear_dx, malnutrition_dx, anemia_dx, hiv_dx)
dispo_df_cough <- df_consort %>% select(screening_id, cg_report_cough, danger_assessed_yn, cough_yn, dyspnea_yn, fever_yn, rr_yn, spo2_yn, pr_yn, temp_yn, cough_dx)
dispo_df_cough$cg_report_cough <- ifelse(dispo_df_cough$cg_report_cough==1, NA, 1) #make cough reported NA to continue to next node
colnames(dispo_df_cough)[colnames(dispo_df_cough) == "cg_report_cough"] = "excl1_cough_not_reported"
dispo_df_cough$danger_assessed_yn <- ifelse(dispo_df_cough$danger_assessed_yn==1, dispo_df_cough$screening_id, NA)
