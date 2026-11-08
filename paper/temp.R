#     - GUIDING QUESTION: What are the motivations of OOD donors? (i.e. to answer: "why is OOD occuring?")

#     - Operational questions:
#              - Are out-of-district donations more politically strategic than in-district donors?
#                     - two angles fo this: "competitiveness" in-district AND in the target district
#              - Do out-of-district donations exhibit more ethnic affinity than in-district donations?
#               - If the answer to both above questions is yes:
#                     - Then we can say that ODDs are more politically motivated.
#                     - And we can test whether donor characteristics modulate the effects in the way we would expect. That is, that OODs from racialized areas have stronger ethnic affinity effects, and less racialized areas have stronger political strategic effects.

#     - Above I have laid out my ideas for how this works. This can be tested with 2 regression models; one for "strategicness" and one for "affinity". The main effects are tested directly, and the modulation of race is tested with an interaction effect.
#     - Keep in mind that Garnett also suggests "transactional" motivations, which we can't test since we have no proxies for this. Make sure to mention that.
#     - We should expect OODs to be more political because of theory I already established in the Background.

# ------ Analysis I -------

# Competitiveness measures to try:
#     - margin of victory (standard one used)
#     - previous election's margin of victory (measure of a donor's "expectation" of race-closeness.)

# Confounders to definitely include:
#     - distance
#     - party (need to change the variable to put all the smaller parties in "Other")
#     - characteristics of the recipient's district

# My "affinity" model doesn't currently test affinity of OODs, although it does provide strong support for the idea that OOD depends less on competitiveness and more on the personal characteristics of the candidate.

donations_data_08 <- donations_data_08 %>%
  mutate(
    is_non_white = !ethnicity == "White",
    vismin_total = vismin_total - mean(vismin_total, na.rm = TRUE) # NOTE: centred, so OOD main effect is at the mean visible minority proportion.
  )

check_competitiveness <- lm(
  -district_margin_of_victory ~ is_out_of_district *
    vismin_total +
    donation_distance +
    is_out_of_district * electoral_event +
    political_party +
    total_amount,
  data = donations_data_08
)
summary(check_competitiveness)
plot(check_competitiveness)

check_competitiveness <- glm(
  I(1 - district_margin_of_victory / 100) ~ is_out_of_district *
    vismin_total +
    donation_distance,
  data = donations_data_08,
  family = quasibinomial(link = "logit")
)
check_competitiveness_incumbency <- glm(
  as.factor(incumbency != "Incumbent") ~ is_out_of_district *
    vismin_total +
    donation_distance,
  data = donations_data_08,
  family = binomial(link = "logit")
)
summary(check_competitiveness_incumbency)

check_affinity_race <- glm(
  is_non_white ~ is_out_of_district * vismin_total + donation_distance,
  data = donations_data_08,
  family = binomial(link = "logit")
)
check_affinity_gender <- glm(
  as.factor(gender != "Male") ~ is_out_of_district *
    vismin_total +
    donation_distance,
  data = donations_data_08,
  family = binomial(link = "logit")
)
check_affinity_age <- lm(
  birth_year ~ is_out_of_district * vismin_total + donation_distance,
  data = donations_data_08
)
summary(check_affinity_age)

# ------ Analysis II -------

# The idea of Analysis II is: to directly answer my question I should use candidate traits as the response, HOWEVER,
# there are many of these, and I would have to fit like 15 MAIN models. Instead, I can still use a model where OOD
# propensity is the outcome, but where the covariates are candidate characteristics. This is essentially flipping
# the covariates and the responses, so that the responses can account for each other, AND more succinct. Interpretation
# of the flipped model can itself be flipped to argue my points.

# CONSIDERATION: Claude suggested also having two models that just model OOD count and IDD count. This way, I can be
# sure that "change in OOD" in the main model corresponds to increased OOD, and not decreased IDD. This check is
# essential if I want to make claims about donor motivation.

# CONSIDERATION: This model should for sure be nested at two levels; by race (i.e. district x year), then by candidate.
# This is both easy to do and because of (rough) balance among districts we get good estimates of the variance.
# Simplify to one level (candidate) if race doesn't clearly add variance beyond candidates.

# TODO: Remove the second explanation of the covariates for the second model (I literally already say in-text that
# ) MENTION THIS IN THE REVIEWER RESPONSE.
# RESPONSE.

# ------ Writeup Notes -------

# NOTES BELOW ARE OUT OF DATE
# Start this section with: The previous sections have established a strong profile for out-of-district donors.
# The goal of this section is broadly, among those donor who donate out-of-district, to get a better understanding of why these donations are being made.
