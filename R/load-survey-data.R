###
# Load and clean data
###

#LIBRARIES AND IMPORTS
#Run install.packages("import") to use the below syntax 
#if you do not already have the import package.
import::from(magrittr, `%>%`, `%<>%`, `%$%`)
import::from(dplyr, transmute, group_by, mutate, filter, select, left_join, summarise, one_of)
import::from(tidyr, gather, gather_, separate, spread)

#DATA LOADING AND CLEANING
df = read.csv("data/survey.csv", na.strings = c("NA", "")) %>%
    #keep only the columns we want for analysis
    transmute(
        participant = factor(Response.ID),
        link_name = Link.Name,
        mturk = !is.na(URL.Variable..mturk) & URL.Variable..mturk == 1,
        layout = viz_type,
        scenario_1_viz,
        scenario_2_viz,
        scenario_3_viz,
        scenario_4_viz,
        Random.first.scenario,
        Random.first.scenario.type,
        used_onebusaway_before,
        onebusaway_use_frequency,
        onebusaway_inaccurate_frequency,
        onebusaway_trust,
        scenario_1.lt10.p=             scenario_1_lt10_p,
        scenario_1.lt10.confidence=    scenario_1_lt10_confidence,
        scenario_1.lt15.p=             scenario_1_lt15_p,
        scenario_1.lt15.confidence=    scenario_1_lt15_confidence,
        scenario_2.5to10.p=            scenario_2_5to10_p,
        scenario_2.5to10.confidence=   scenario_2_5to10_confidence,
        scenario_2.40to45.p=           scenario_2_40to45_p,
        scenario_2.40to45.confidence=  scenario_2_40to45_confidence,
        get_coffee=         scenario_2_get_coffee == "Yes",
        scenario_3.lt15.p=             scenario_3_lt15_p,
        scenario_3.lt15.confidence=    scenario_3_lt15_confidence,
        scenario_3.25to30.p=           scenario_3_25to30_p,
        scenario_3.25to30.confidence=  scenario_3_25to30_confidence,
        scenario_4.lt20.p=             scenario_4_lt20_p,
        scenario_4.lt20.confidence=    scenario_4_lt20_confidence,
        scenario_4.gt25.p=             scenario_4_gt25_p,
        scenario_4.gt25.confidence=    scenario_4_gt25_confidence,
        scenario_1.lt10.p.1=           scenario_1_lt10_p.1,
        scenario_1.lt10.confidence.1=  scenario_1_lt10_confidence.1,
        scenario_1.lt15.p.1=           scenario_1_lt15_p.1,
        scenario_1.lt15.confidence.1=  scenario_1_lt15_confidence.1,
        scenario_2.5to10.p.1=          scenario_2_5to10_p.1,
        scenario_2.5to10.confidence.1= scenario_2_5to10_confidence.1,
        scenario_2.40to45.p.1=         scenario_2_40to45_p.1,
        scenario_2.40to45.confidence.1=scenario_2_40to45_confidence.1,
        get_coffee.1=       scenario_2_get_coffee.1 == "Yes",
        scenario_3.lt15.p.1=           scenario_3_lt15_p.1,
        scenario_3.lt15.confidence.1=  scenario_3_lt15_confidence.1,
        scenario_3.25to30.p.1=         scenario_3_25to30_p.1,
        scenario_3.25to30.confidence.1=scenario_3_25to30_confidence.1,
        scenario_4.lt20.p.1=           scenario_4_lt20_p.1,
        scenario_4.lt20.confidence.1=  scenario_4_lt20_confidence.1,
        scenario_4.gt25.p.1=           scenario_4_gt25_p.1,
        scenario_4.gt25.confidence.1=  scenario_4_gt25_confidence.1,
        ease_of_use.b100 = Image.A.For.each.type.of.screenshot.you.saw..how.easy.was.it.to.use.the.screenshot.to.answer.questions.about.bus.arrival.time.,
        ease_of_use.b20 = Image.B.For.each.type.of.screenshot.you.saw..how.easy.was.it.to.use.the.screenshot.to.answer.questions.about.bus.arrival.time.,
        ease_of_use.fill = Image.C.For.each.type.of.screenshot.you.saw..how.easy.was.it.to.use.the.screenshot.to.answer.questions.about.bus.arrival.time.,
        ease_of_use.draws = Image.D.For.each.type.of.screenshot.you.saw..how.easy.was.it.to.use.the.screenshot.to.answer.questions.about.bus.arrival.time.,
        visual_appeal.b100 = Image.A.How.visually.appealing.was.each.type.of.screenshot.you.saw.,
        visual_appeal.b20 = Image.B.How.visually.appealing.was.each.type.of.screenshot.you.saw.,
        visual_appeal.fill = Image.C.How.visually.appealing.was.each.type.of.screenshot.you.saw.,
        visual_appeal.draws = Image.D.How.visually.appealing.was.each.type.of.screenshot.you.saw.,
        gender,
        gender_other = Other.gender,
        age,
        occupation,
        education,
        statistics_experience,
        risk_averse_1,                                                                                                                                                                                                             
        risk_averse_2,                                                                                                                                                                                                             
        risk_averse_3,                                                                                                                                                                                                             
        risk_averse_4,                                                                                                                                                                                                             
        risk_averse_5,                                                                                                                                                                                                             
        risk_averse_6
    )
    
#DROP PARTIAL RESPONSES
#A very small number of people didn't answer all the question we care about (9 out of 541, <2%).
#We'll describe them in cross tabs below and (because there are so few) just drop them to simplify
#analysis. The stopifnot() calls are guards to ensure we aren't accidentally dropping a bunch of data
#if we modify this script in the future / have new data.

#First, let's describe the 9 people we're dropping and include some 
#overall drops:
cat("CROSS-TABS OF DROPPED DATA\n")
dropped_tabs = xtabs(~(is.na(ease_of_use.fill) | is.na(gender) | is.na(statistics_experience) | gender=="Other"), data=df)
#stop if we're suddenly dropping a bunch of data
stopifnot(dropped_tabs["TRUE"] / nrow(df) < .02)
#breakdowns:    
print(dropped_tabs)
print(xtabs(~is.na(ease_of_use.fill), data=df))
print(summary(df$gender))
print(xtabs(~is.na(statistics_experience), data=df))
print(xtabs(~is.na(ease_of_use.fill) + (gender == "Other" | is.na(gender)) + is.na(statistics_experience), data=df))

#someone (somehow) didn't answer ease_of_use.fill even though it's required (there should be only one)
stopifnot(sum(is.na(df$ease_of_use.fill)) == 1)

#now that we've described the dropped data, let's drop it
df %<>% 
    filter(
        #unanswered required question
        !is.na(df$ease_of_use.fill),
        #stats experience and gender
        !is.na(gender),
        gender != "Other",
        !is.na(statistics_experience)
    ) %>%
    mutate(
        #drop unused levels
        participant = factor(participant),
        gender = factor(gender)
    ) %>%
    select(
        -gender_other    
    )


#CLEAN UP RESPONSES
#calculate risk aversion scores
df$risk_averse = df %>% 
    select(risk_averse_1:risk_averse_6) %>% 
    rowSums()
    
#ordered version of statistics_experience
df %<>% mutate(
    statistics_experience = ordered(statistics_experience, levels=
        c("Never studied it", "Studied it in high school", "Studied it in college", "Work with it regularly (in schoolwork/research/internship/work)")),

    #numeric version of statistics_experience, from -.5 to .5
    #so that other coefficients are with respect to halfway between
    #"studied it in high school" and "studied it in college" and
    #the coefficient for statistics_experience describes the
    #difference between "never studied it" and "work with it regularly"
    statistics_experience_coding = as.numeric(statistics_experience) %>% 
        {(. - min(., na.rm=TRUE)) / (max(., na.rm=TRUE) - 1) - .5}
)

#various question types we need to clean up
probability_questions = c(
        "scenario_1.lt10.p",
        "scenario_1.lt10.confidence",
        "scenario_1.lt15.p",
        "scenario_1.lt15.confidence",
        "scenario_2.5to10.p",
        "scenario_2.5to10.confidence",
        "scenario_2.40to45.p",
        "scenario_2.40to45.confidence",
        "scenario_3.lt15.p",
        "scenario_3.lt15.confidence",
        "scenario_3.25to30.p",
        "scenario_3.25to30.confidence",
        "scenario_4.lt20.p",
        "scenario_4.lt20.confidence",
        "scenario_4.gt25.p",
        "scenario_4.gt25.confidence")
coffee_question = "get_coffee"  
comparison_questions = c(
        "ease_of_use.b100",
        "ease_of_use.b20",
        "ease_of_use.fill",
        "ease_of_use.draws",
        "visual_appeal.b100",
        "visual_appeal.b20",
        "visual_appeal.fill",
        "visual_appeal.draws"
    )
#combine answers from different pages (there are two different
#versions of every scenario question that were not actually different,
#but were needed because of how we implemented randomization in 
#SurveyGizmo---we only wanted to show the tutorial once at the
#beginning of any viz type, so we needed to have a different version
#of the first page for each viz type that had the tutorial).
for (col in c(probability_questions, coffee_question)) {
    col2 = paste0(col,".1")
    df[[col]] = ifelse(is.na(df[[col]]), df[[col2]], df[[col]])
    df[[col2]] = NULL
}

#Make a long-format data frame for the probability questions
df_prob = df %>%
    select(participant, one_of(probability_questions)) %>%
    gather_("scenario.pquestion.response", "value", probability_questions) %>% 
    separate(scenario.pquestion.response, c("scenario", "pquestion", "response"), "\\.") %>%
    spread(response, value) %>%
    mutate(
        scenario = factor(scenario),
        pquestion = factor(paste0(scenario, "_", pquestion)) #questions are unique within scenario
    )

#Make a long-format data frame for the comparison questions
df_comp = df %>%
    select(participant, one_of(comparison_questions)) %>%
    gather_("cquestion.viz", "value", comparison_questions) %>% 
    separate(cquestion.viz, c("cquestion", "viz"), "\\.") %>%
    spread(cquestion, value)

#Build the complete long-format data frame
#first, merge in the probability questions
dfl = df %>%
    select(-one_of(probability_questions, comparison_questions)) %>%   #these will be merged back in 
    left_join(df_prob, by="participant")
#then, determine the viz used for the probability questions
dfl$viz = sapply(1:nrow(dfl), function(i) as.character(dfl[i,paste0(dfl[i,"scenario"], "_viz")]))
#then, merge in the comparison questions
dfl %<>% 
    left_join(df_comp, by=c("participant","viz")) %>%
    mutate(viz = factor(viz))

#merge in answers to probability questions
known_p = read.csv("data/known_p.csv")
dfl %<>% left_join(known_p, by=c("pquestion"))

#change all probability and confidence answers to be in [0,1]
dfl %<>% mutate(
        p = p / 100,
        confidence = confidence / 100,
        ease_of_use = ease_of_use / 100,
        visual_appeal = visual_appeal / 100
    )

#restricted version of p and confidence for sigmoid transformations
restrict = function(x) ifelse(x == 0, 0.001, ifelse(x == 1, .999, x))
dfl %<>% mutate(
        restricted_p = restrict(p),
        restricted_confidence = restrict(confidence),
        restricted_ease_of_use = restrict(ease_of_use),
        restricted_visual_appeal = restrict(visual_appeal)
    )

#relevel viz so that it is in the approx order of dispersion we will see (makes a bunch of charts easier to interpret)
dfl$viz = factor(dfl$viz, levels=c("b20", "b100", "fill", "draws"))

#version of dfl with all complete cases (for use with gamlss,
#because gamlss will complain about columns containing NAs even
#if you don't use them in the model).
#For this data frame we'll just remove the columns that are okay to be NA since
#they aren't in our models anyway.
#this also helps with validation below
dfc = dfl %>%
    select(
        -age, 
        -onebusaway_trust,
        -onebusaway_use_frequency,
        -onebusaway_inaccurate_frequency,
        -education,
        -occupation
    )

#DATA VERIFICATION
#comparison question df should have 4 times the rows of df (due to 4 answers per participant)
stopifnot(nrow(df_comp) == 4 * nrow(df))
#long version of df should have 8 times the rows (due to 8 answers per participant)
stopifnot(nrow(dfl) == 8 * nrow(df))
#put this here until and unless anyone answers "other" on gender; then we'll deal with it
stopifnot(all(dfl$gender %in% c("Male","Female")))
#dfc should have all complete cases
stopifnot(all(complete.cases(dfc)))
stopifnot(all(complete.cases(dfc)))
