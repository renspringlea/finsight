---
layout: page
---

# Parameters

In this page, we explain the biological parameters used to produce the various estimates on this website.

The production data from FAO, which is the data that we use to calculate our estimates of total numbers of individuals and other key results, are expressed in tonnes of live weight. We need to convert this into values that are more relevant for strategic decision-making by the animal advocacy movement, such as numbers of animals and average duration of a fish's life in aquaculture.

The key parameters needed for such decision-making are:
- Body weight at slaughter, i.e. how much a fish weighs at the point of slaughter/harvest
- Duration of production cycle, i.e. how long it takes for a fish to progress from hatching to slaughter/harvest
- Mortality rates, i.e. how many fish die before slaughter/harvest

There are a few points of caution that we would like to keep in mind:
- There is large variation in these parameters. Any parameter will vary drastically across countries, across farms within a country, and even in a single farm over time.
- The data quality is imperfect. We do not have perfect coverage of every species-country combination.

The quality and coverage of the data does not allow us to estimate parameters at this fine resolution. If the published literature indicates that the average body weight at slaughter of gilt-head seabream is 400 g in Turkey but 300 g in Italy, it is unclear whether this actually represents a real-world difference between these countries or whether the difference is due to limited sampling methods and random chance.

Therefore, in our calculations, we select values of the above parameters for each species, but *not* for each species-country combination. For example, we use the same value for the body weight of gilt-head seabream in Turkey as we do in Italy.

The main weakness of this approach is that parameters sometimes do vary systematically across countries. However, we contend that it is difficult to capture these true differences given the insufficient quality and coverage of the data in the published literature.

For the same reasons, it is often necessary to use informal reasoning to derive some parameters. This is the case when, for example, we have mortality rates for grow-out but not for other stages of production. We also often round values. We would prefer not to use values that represent many decimal places, as this frequently implies a level of precision that simply does not exist in the data.

Lastly, we have ensured that **all parameters are easily visible on the pages for each country**. This means that these parameters, which are really assumptions, are clear; if you disagree with the values we have chosen for any of these parameters, then you are very welcome to calculate your own values.

# Parameter estimates
This section lists the parameters that we have derived.

When we refer to a "literature estimate", this originates in our systematic literature review on European aquaculture (Ryba 2025, *The economics of fish farming and fish welfare in Europe: A systematic review*, Animal Ask). See that report and its accompanying data for the sources behind all literature estimates listed below. All such estimates are specific to Europe.

When we have conducted additional calculations for a certain parameter, this is listed directly underneath the parameter.

When we have resorted to additional searches for a particular parameter (usually a parameter for a particular stage of production), we indicate this with the phrase "additional search" plus a link to the relevant source. These parameters are generally not specific to Europe, and we would be delighted to replace them with Eruope-specific estimates when such estimates become available in the literature.

## European seabass

Body weight at slaughter: 400 g (median of several literature estimates)

Duration of whole production cycle: 25 months
- Duration of hatchery = ~4 months ([additional search](https://openknowledge.fao.org/server/api/core/bitstreams/c76d3979-0dd5-4162-84ed-3418c837220d/content))
- Duration of grow-out = 21 months (median of several literature estimates)
- Thus, the duration of the full production cycle can be calculated as: ~4 + 21 = ~25 months

Mortality during whole production cycle: 0.214
- Mortality in hatchery = 0.075 (literature estimate)
- Mortality during grow-out = 0.15 (best literature estimate)
- Thus, the cumulative *survival* can be calculated as: (1 - 0.075) * (1 - 0.15) = 0.786
- Thus, the cumulative mortality can be calculated as: 1 - 0.786 = 0.214

## Gilt-head seabream
Body weight at slaughter: 350 g (literature estimate)

Duration of whole production cycle: 25 months
- Duration of hatchery = ~4 months ([additional search](https://openknowledge.fao.org/server/api/core/bitstreams/c76d3979-0dd5-4162-84ed-3418c837220d/content))
- Duration of grow-out = 21 months (median of several literature estimates)
- Thus, the duration of the full production cycle can be calculated as: ~4 + 21 = ~25 months

Mortality during whole production cycle: 0.26
- Mortality in hatchery = 0.075 (literature estimate)
- Mortality during grow-out = 0.2 (best literature estimate)
- Thus, the cumulative *survival* can be calculated as: (1 - 0.075) * (1 - 0.2) = 0.74
- Thus, the cumulative mortality can be calculated as: 1 - 0.74 = 0.26

## Atlantic salmon
Body weight at slaughter: 4600 g (best literature estimate)
- Note that the body weight at harvest of Atlantic salmon in Norway has gradually decreased over the past decade or so (Barrett et al 2022, [Declining size-at-harvest in Norwegian salmon aquaculture: lice, disease, and the role of stunboats](https://www.sciencedirect.com/science/article/pii/S0044848622005567)).

Duration of whole production cycle: 25 months
- Duration of hatchery = 10 months (literature estimate)
- Duration of grow-out = 15 months (literature estimate)
- Thus, the duration of the whole production cycle can be calculated as: 10 + 15 = 25 months
- Note that the hatchery stage and the grow-out stage are becoming increasingly blurred for salmonids (Sandvold and Tveteras 2014, [Innovation and productivity growth in Norwegian production of juvenile salmonids](https://www.tandfonline.com/doi/full/10.1080/13657305.2014.903313)).

Mortality during whole production cycle: 0.376 (literature estimate)

## Rainbow trout
Body weight at slaughter: 1,300 g
- In 2020, 38% of trout produced in the EU is "large" (over 1.2 kg). The top large trout-producing countries in the EU are France, Finland, Poland, Denmark, and Sweden. Finland and Sweden produce exclusively large trout. The other 62% is "portion" trout (below 1.2 kg), though the term is also sometimes split into "medium" and "portion" (the cut-off being 500 g). The top portion trout-producing countries are Italy, denmark, France, Spain, and Germany. Italy and Germany produce *mostly* portion trout. See EUMOFA 2023 ([Case study: Large trout in the EU](https://eumofa.eu/documents/20124/35623/PTAT_Large+trout.pdf/4923045b-73a7-e7ce-16ec-bd33301286a8?t=1686569699668)).
- Body weight after marine harvest = 3,000 g (literature estimate, Denmark)
- Body weight after freshwater harvest in Denmark = 300 g (median of literature estimates)
- Thus, if we use 300 g as a rough stand-in for the weight of a portion trout at harvest and 3,000 g for large trout, we can calculate the EU-wide average as: (300 * 0.62) + (3000 * 0.38) = ~1,300 g.
- This notably excludes the large rainbow trout-producing countries that are not EU members, such as Turkey and Norway. Of global trout production by weight, the EU accounts for around 19%; Turkey, 15%; and Norway, 10% (EUMOFA 2023).
- For Turkey, our literature review gave three estimates: 210 g, 400 g, and 800 g (literature estimates). For Norway, [Fishcount](https://fishcount.org.uk/estimates/farmedfishes/data01/fishcount_global_farmed_fish_estimate.php?selyear=2022&selcountry=pleaseselect&selspecies=Rainbow+trout&selsort=Number) indicates that the mean body weight is in the range of 3,000 - 5,000 g (additional research).
- We could thus calculate a rough indicative mean across the EU plus Turkey and Norway as a simple weighted average: ((1,300 * 0.19) + (400 * 0.15) + (4,000 * 0.1))/(0.19 + 0.15 + 0.1) = ~1,600 g.
- This weighted average (~1,600 g) is quite close to our original EU average (~1,300 g), especially when we consider that both values are rough approximations based on imperfect data. However, this result captures the intuition that the major trout-producing countries that are *in Europe but not in the EU* roughly balance out in terms of the average body weight of trout.
- Thus, we retain our estimate of ~1,300 g for Europe as a whole, while noting that this estimate is wobbly and based on imperfect data. We think further research to refine this estimate would be valuable.

Duration of whole production cycle: 25 months
- Duration of hatchery = 8 months (literature estimate; from hatch until 50 g body weight)
- Duration of grow-out, freshwater stage = 14 months (median literature estimate)
- Duration of grow-out, marine stage = 8.5 months (median literature estimate)
- Thus, if we assume that around 62% of trout are slaughtered after the freshwater stage and 38% after the marine stage, the average duration of the whole production cycle can be calculated as: 8 + 14 + (8.5*0.38) = ~25 months
- Note that the hatchery stage and the grow-out stage are becoming increasingly blurred for salmonids (Sandvold and Tveteras 2014, [Innovation and productivity growth in Norwegian production of juvenile salmonids](https://www.tandfonline.com/doi/full/10.1080/13657305.2014.903313)).

Mortality during whole production cycle: 0.343 (best literature estimate)
- We have used this estimate as the best single estimate from the literature that covers the full production process.
- This estimate is for Norway, which seems to be producing large trout rather than portion trout. Large trout take longer to produce, which would increase mortality. This might bias the estimate upwards. As such, we think that it would also be valid to use a lower estimate here. However, Norway is also a country that has a technically well-developed salmonid farming industry, which might bias the estimate downwards.

It is also important to note that, in the case of rainbow trout, these three parameters are correlated with each other. Specifically, the farm's decision about whether to produce portion trout or large trout will affect all three parameters at the same time. Larger trout means a higher body weight, which takes a longer time to reach and will expose fish to a higher risk of mortality before slaughter. [This Brazilian paper] (additional search) indicates that the rough average duration and body weight that we have derived above are approximately reasonable.

## Common carp
Body weight at slaughter: 1500 g (best literature estimate)

Duration of whole production cycle: 36 months (literature estimate)

Mortality during whole production cycle: 0.68 (best literature estimate)

# Parameters that we have taken from other sources

Beyond these key species, we have taken parameters for other numerically important farmed aquatic species in Europe.

For shrimp, we have obtained parameters from Waldhorn and Autric @ [Rethink Priorities](https://rethinkpriorities.org/research-area/shrimp-the-animals-most-commonly-used-and-killed-for-food-production/).

Beyond the five most significant farmed fish species in Europe (detailed above), there are some farmed fish species that are less numerically significant for Europe as a whole but still meaningful in some countries. For these finfish, we have obtained estimated body weights from [Fishcount](https://fishcount.org.uk/estimates/farmedfishes/data01/fishcount_fctab_emw_part_level1.php). Since these finfish are less numerically significant (though still certainly important), we have invested less effort into estimating the other biological parameters. As such, for these species, we use rough placeholders for duration and mortality. **We welcome any improvement to these numbers.**

These species include:
- North African catfish (1,500 g)
- Meagre (2,500 g)
- Turbot (1,300 g)
- Sturgeons nei 46,000 g)

For those four species, we arbitrarily use a lifespan of 12 months and a mortality of 0.3. This is simply a placeholder value, but it is better than using no placeholder at all (e.g. mortality of 0). **We welcome any improvement to these numbers.**

Lastly, for these species, we use the same values as the main five fish species (described above):
- Crucian carp (same values as common carp)
- Silver carp (same values as common carp)
- Bighead carp (same values as common carp)
- Sea trout (same values as rainbow trout)
- Cyprinids nei (same values as common carp)
- Trouts nei (same values as rainbow trout)
- Salmonids nei (same values as Atlantic salmon)
- Freshwater breams nei (same values as gilt-head seabream)
- Freshwater bream (same values as gilt-head seabream)

("nei" = "not elsewhere included")

Other species are not included in our calculations. The production tonnage of these species is still listed on individual country pages.
