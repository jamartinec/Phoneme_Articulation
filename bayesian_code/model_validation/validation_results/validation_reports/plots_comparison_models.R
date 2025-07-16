library(tidyverse)

# your data
df <- tribble(
  ~phoneme_group, ~model0, ~model1, ~model2, ~model5, ~model4, ~model6, ~model3, ~model7,
  "Vowels_Level1_Level2", 2372.400587, 2353.462898, 2156.426225, 2390.860938, 2390.81347, 2389.414702, 2403.365305, 2390.616417,
  "Vowels_Level4_Level5", 2404.282138, 2400.335408, 2109.843857, 2408.223292, 2408.925052, 2408.521105, 2403.968617, 2405.101681,
  "Vowels_Level3", 2137.326231, 2133.969663, 1847.266725, 2143.506476, 2143.099494, 2142.880588, 2137.805181, 2133.954355,
  "Consonants_Level3", 1588.81245, 1585.822502, 1454.607328, 1593.913208, 1593.434684, 1592.722768, 1587.373022, 1585.398045,
  "Consonants_Level4", 2206.720434, 2199.826015, 1091.629515, 2209.201032, 2209.313019, 2210.508856, 2205.396913, 2199.311573,
  "Consonants_Level5", 1997.365109, 1988.928397, 1705.228679, 2002.034362, 2003.447985, 2003.822602, 1998.000827, 1989.429841,
  "Consonants_Level6", 3392.083198, 3394.443317, 3015.966297, 3395.632649, 3395.838768, 3394.411248, 3419.949923, 3419.082837
)

df_long <- df %>%
  pivot_longer(-phoneme_group, names_to = "model", values_to = "elpd_loo")

ggplot(df_long, aes(x = model, y = phoneme_group, fill = elpd_loo)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "ELPD-LOO Comparison",
    x = "Model",
    y = "Phoneme Group",
    fill = "elpd_loo"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long, aes(x = phoneme_group, y = elpd_loo, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ELPD-LOO by Phoneme Group and Model",
    x = "Phoneme Group",
    y = "elpd_loo",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(tidyverse)

# same data as before
df <- tribble(
  ~phoneme_group, ~model0, ~model1, ~model2, ~model5, ~model4, ~model6, ~model3, ~model7,
  "Vowels_Level1_Level2", 2372.400587, 2353.462898, 2156.426225, 2390.860938, 2390.81347, 2389.414702, 2403.365305, 2390.616417,
  "Vowels_Level4_Level5", 2404.282138, 2400.335408, 2109.843857, 2408.223292, 2408.925052, 2408.521105, 2403.968617, 2405.101681,
  "Vowels_Level3", 2137.326231, 2133.969663, 1847.266725, 2143.506476, 2143.099494, 2142.880588, 2137.805181, 2133.954355,
  "Consonants_Level3", 1588.81245, 1585.822502, 1454.607328, 1593.913208, 1593.434684, 1592.722768, 1587.373022, 1585.398045,
  "Consonants_Level4", 2206.720434, 2199.826015, 1091.629515, 2209.201032, 2209.313019, 2210.508856, 2205.396913, 2199.311573,
  "Consonants_Level5", 1997.365109, 1988.928397, 1705.228679, 2002.034362, 2003.447985, 2003.822602, 1998.000827, 1989.429841,
  "Consonants_Level6", 3392.083198, 3394.443317, 3015.966297, 3395.632649, 3395.838768, 3394.411248, 3419.949923, 3419.082837
)

df_long <- df %>%
  pivot_longer(-phoneme_group, names_to = "model", values_to = "elpd_loo")

# filter out model2
df_long_filtered <- df_long %>%
  filter(model != "model2")

ggplot(df_long_filtered, aes(x = phoneme_group, y = elpd_loo, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ELPD-LOO by Phoneme Group and Model (excluding model2)",
    x = "Phoneme Group",
    y = "elpd_loo",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)

# your data
df <- tribble(
  ~phoneme_group, ~model0, ~model1, ~model2, ~model5, ~model4, ~model6, ~model3, ~model7, ~model9,
  "Vowels_Level1_Level2", 2372.400587, 2353.462898, 2156.426225, 2390.860938, 2390.81347, 2389.414702, 2403.365305, 2390.616417,2423.47379785022,
  "Vowels_Level4_Level5", 2404.282138, 2400.335408, 2109.843857, 2408.223292, 2408.925052, 2408.521105, 2403.968617, 2405.101681,2413.19149610249,
  "Vowels_Level3", 2137.326231, 2133.969663, 1847.266725, 2143.506476, 2143.099494, 2142.880588, 2137.805181, 2133.954355, 2142.20169783886,
  "Consonants_Level3", 1588.81245, 1585.822502, 1454.607328, 1593.913208, 1593.434684, 1592.722768, 1587.373022, 1585.398045, 1594.02018660699,
  "Consonants_Level4", 2206.720434, 2199.826015, 1091.629515, 2209.201032, 2209.313019, 2210.508856, 2205.396913, 2199.311573,2210.14613816042,
  "Consonants_Level5", 1997.365109, 1988.928397, 1705.228679, 2002.034362, 2003.447985, 2003.822602, 1998.000827, 1989.429841,2003.64794510735,
  "Consonants_Level6", 3392.083198, 3394.443317, 3015.966297, 3395.632649, 3395.838768, 3394.411248, 3419.949923, 3419.082837,3403.35519432732,
)

# reshape to long
df_long <- df %>%
  pivot_longer(-phoneme_group, names_to = "model", values_to = "elpd_loo")

# compute Δelpd
df_delta <- df_long %>%
  group_by(phoneme_group) %>%
  mutate(best_elpd = max(elpd_loo),
         delta_elpd = elpd_loo - best_elpd) %>%
  ungroup() %>%
  filter(model != "model2")  # optional: exclude model2

# plot
ggplot(df_delta, aes(x = phoneme_group, y = delta_elpd, color = model, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Δelpd-LOO from Best Model per Phoneme Group",
    subtitle = "Higher is better (best model per group = 0)",
    x = "Phoneme Group",
    y = "Δelpd_loo",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)

# your WAIC data
df_waic <- tribble(
  ~phoneme_group, ~model0, ~model1, ~model2, ~model5, ~model4, ~model6, ~model3, ~model7,~model9,
  "Vowels_Level1_Level2", 2393.091942, 2374.486932, 2182.939077, 2410.380269, 2410.226911, 2409.419963, 2419.991459, 2402.076974,2437.25183739366,
  "Vowels_Level4_Level5", 2415.94795, 2411.209648, 2164.973024, 2418.886505, 2419.572331, 2419.195411, 2414.910266, 2417.44826,2418.64655926078,
  "Vowels_Level3", 2147.667259, 2144.205509, 1872.456387, 2152.775275, 2152.42123, 2152.359044, 2147.684196, 2143.89446,2151.97248061237,
  "Consonants_Level3", 1600.887501, 1597.499784, 1463.830073, 1605.668552, 1605.023293, 1604.04684, 1599.492792, 1597.262348,1605.52846555561,
  "Consonants_Level4", 2229.396854, 2221.439075, 1130.13514, 2231.872889, 2231.87649, 2231.778211, 2227.829619, 2222.062492,2232.0072878835,
  "Consonants_Level5", 2020.428204, 2012.201825, 1736.182954, 2025.57095, 2026.500314, 2026.690243, 2021.514845, 2012.906702,2026.73759218063,
  "Consonants_Level6", 3404.867638, 3406.603227, 3022.616948, 3407.665037, 3407.336967, 3406.832192, 3428.673419, 3428.014152,3413.291561112,
)

# reshape to long
df_waic_long <- df_waic %>%
  pivot_longer(-phoneme_group, names_to = "model", values_to = "elpd_waic")

# compute Δelpd_waic
df_waic_delta <- df_waic_long %>%
  group_by(phoneme_group) %>%
  mutate(best_waic = max(elpd_waic),
         delta_waic = elpd_waic - best_waic) %>%
  ungroup() %>%
  filter(model != "model2")  # optional: exclude model2

# plot
ggplot(df_waic_delta, aes(x = phoneme_group, y = delta_waic, color = model, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Δelpd_WAIC from Best Model per Phoneme Group",
    subtitle = "Higher is better (best model per group = 0)",
    x = "Phoneme Group",
    y = "Δelpd_WAIC",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

