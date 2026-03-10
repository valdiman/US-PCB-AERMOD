# Code to obtain PCB fraction
# Aroclor 1248 to be used in Anacostia River
# Arolcor 1016 to be used in Kalamazoo River
# Arolcor 1260 to be used in Housatonic River
# for flux calculations
# Profiles from Frame

library(dplyr)
library(tidyr)
library(purrr)

Congener <- c(
  "PCB1", "PCB2", "PCB3", "PCB4+10", "PCB5+8", "PCB6", "PCB7+9", "PCB11", "PCB12+13",
  "PCB14", "PCB15", "PCB16+32", "PCB17", "PCB18+30", "PCB19", "PCB20+21+28+31+33+50+53",
  "PCB22", "PCB23", "PCB24+27", "PCB25", "PCB26+29", "PCB34", "PCB35", "PCB36", "PCB37+42",
  "PCB38", "PCB39", "PCB40+41+64+71+72", "PCB43+49+52+69+73", "PCB44+47+65", "PCB45+51",
  "PCB46", "PCB48+59+62+75", "PCB54", "PCB55", "PCB56+60", "PCB57", "PCB58",
  "PCB61+66+70+74+76+93+95+98+100+102", "PCB63", "PCB67", "PCB68",
  "PCB77+85+110+111+115+116+117", "PCB78", "PCB79", "PCB80",
  "PCB81+86+87+97+107+108+109+112+119+124+125", "PCB82+135+144+151+154",
  "PCB83+99", "PCB84+92", "PCB88+91", "PCB89", "PCB90+101+113", "PCB94", "PCB96",
  "PCB103", "PCB104", "PCB105", "PCB106+118", "PCB114+122+131+133+142+146+165",
  "PCB120", "PCB121", "PCB123+139+140+147+149", "PCB126", "PCB127",
  "PCB128+162+166+167", "PCB129+137+138+158+160+163+164+176+178", "PCB130",
  "PCB132+153+161+168", "PCB134+143", "PCB136", "PCB141", "PCB145", "PCB148",
  "PCB150", "PCB152", "PCB155", "PCB156+157+172+197+200", "PCB159", "PCB169",
  "PCB170+190", "PCB171+173+202", "PCB174", "PCB175", "PCB177", "PCB179",
  "PCB180+193", "PCB181", "PCB182+187", "PCB183+185", "PCB184", "PCB186",
  "PCB188", "PCB189", "PCB191", "PCB192", "PCB194+205", "PCB195+208",
  "PCB196+203", "PCB198+199+201", "PCB204", "PCB206", "PCB207", "PCB209"
)

# read
df <- read.csv("Data/AroclorPCBProfile.csv", stringsAsFactors = FALSE)

# Normalize congener names (trim whitespace and uppercase) to avoid mismatch
df$congener <- toupper(trimws(df$congener))

# Clean NM and blanks -> NA, then coerce sample columns to numeric
sample_cols <- setdiff(names(df), "congener")
df <- df %>%
  mutate(across(all_of(sample_cols), ~ ifelse(. %in% c("NM", "", " "), NA, .))) %>%
  mutate(across(all_of(sample_cols), as.numeric))

# -----------------------------
# Build mapping table: grouped Congener -> individual member names
# -----------------------------
mapping <- tibble(Congener = Congener) %>%
  mutate(members = strsplit(gsub("PCB", "", Congener), "\\+")) %>%
  unnest_longer(members) %>%
  mutate(member = paste0("PCB", members)) %>%
  select(Congener, member)

# Make sure mapping member names match df format (upper, trimmed)
mapping$member <- toupper(trimws(mapping$member))

# -----------------------------
# Pivot df to long and join/aggregate
# -----------------------------
df_long <- df %>%
  pivot_longer(cols = -congener, names_to = "Sample", values_to = "Value")

out_long <- mapping %>%
  left_join(df_long, by = c("member" = "congener")) %>%
  group_by(Congener, Sample) %>%
  summarize(
    # Current behavior: if ALL group members are NA -> return NA.
    # Otherwise sum numeric values ignoring NA.
    Sum = if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Convert back to wide format (one row per grouped Congener)
result <- out_long %>%
  pivot_wider(names_from = Sample, values_from = Sum) %>%
  arrange(match(Congener, Congener))  # preserve original order of Congener vector

# Organize by PCB congener
result <- result %>%
  mutate(
    first_num = as.numeric(sub("PCB", "", sub("\\+.*", "", Congener)))
  ) %>%
  arrange(first_num) %>%
  select(-first_num)

write.csv(result, "Data/AroclorPCBProfileGrouped.csv", row.names = FALSE)

