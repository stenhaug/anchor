constrained_baseline_test_flex_items <- function(data, groups, flex_items){
    quiet(anova(
        mod_constrained_baseline(data, groups),
        mod_flexible(data, groups, flex_items = flex_items)
    ))$p[2]
}

test_with_known_flex <- function(data, groups, flex_known, flex_test){
    quiet(anova(
        mod_flexible(data, groups, flex_items = flex_known),
        mod_flexible(data, groups, flex_items = c(flex_known, flex_test))
    ))$p[2]
}
