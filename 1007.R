#### Data Wrangling ####
# 載入packages
pkgs <- c("tidyverse", "haven", "janitor", "stringr", "scales")
to_install <- setdiff(pkgs, rownames(installed.packages()))

if(length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only=TRUE))

dir.create("C:/Users/jef81/Desktop/Health_Big_Data/outputs", showWarnings = FALSE)
data_dir <- "C:/Users/jef81/Desktop/Health_Big_Data"

setwd("C:/Users/jef81/Desktop/Health_Big_Data")
getwd()
list.files()

# read data
demo <- read_xpt(file.path(data_dir, "DEMO_L.XPT")) %>%
  clean_names()
bmx  <- read_xpt(file.path(data_dir, "BMX_L.XPT"))  %>%
  clean_names()
bpx  <- read_xpt(file.path(data_dir, "BPXO_L.XPT")) %>%
  clean_names()

glimpse(demo)
glimpse(bmx)
glimpse(bpx)

stopifnot(!anyDuplicated(demo$seqn))
stopifnot(!anyDuplicated(bmx$seqn))
stopifnot(!anyDuplicated(bpx$seqn))

x_id <- c(101, 102, 103, 102, 104)
anyDuplicated(x_id)
duplicated(x_id)
sum(duplicated(x_id))

# III
demo_adult <- demo %>%
  transmute(
    seqn,
    age = ridageyr,
    sex = factor(riagendr,
                 levels = c(1, 2),
                 labels = c("Male", "Female"))
  ) %>%
  filter(age >= 20)

# IV
bmi_raw <- bmx %>%
  transmute(seqn, bmi_raw = bmxbmi)

dat_bmi_raw <- demo_adult %>%
  left_join(bmi_raw, by = "seqn")

miss_bmi_before <- dat_bmi_raw %>%
  summarize(
    n = n(),
    bmi_missing = sum(is.na(bmi_raw)),
    p_missing = bmi_missing / n
  )
print(miss_bmi_before)

# V
p_bmi_before <- ggplot(dat_bmi_raw, aes(y=bmi_raw)) +
  geom_boxplot(width = 0.35, outlier.alpha = 0.25, na.rm = TRUE) +
  labs(title = "BMI before cleaning", x = NULL, y = "BMI (kg/m^2)") +
  theme_minimal(base_size = 12)

print(p_bmi_before)
ggsave("outputs/week5_bmi_before.png", p_bmi_before)


# VI






