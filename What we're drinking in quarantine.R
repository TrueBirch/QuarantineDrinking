library(conflicted)
library(tidyverse)
library(lubridate)
library(svglite)
library(tictoc)
filter <- dplyr::filter

tic("Load data")
iowa_raw <- read_csv(
	"https://data.iowa.gov/api/views/m3tr-qhgy/rows.csv?accessType=DOWNLOAD",
	col_types = cols_only(col_character(),
		`Category Name` = col_character(),
		`Item Description` = col_character(),
		Date = col_date(format = "%m/%d/%Y"),
		`Invoice/Item Number` = col_character(),
		`Sale (Dollars)` = col_double()
	)
) %>% 
	rlang::set_names(c("invoice_number", "date", "category", "item_description", "sale_dollars"))
toc()

super_category_table <- 
	structure(list(category = c("100 PROOF VODKA", "100% AGAVE TEQUILA", 
														"AGED DARK RUM", "AMERICAN ALCOHOL", "AMERICAN AMARETTO", "AMERICAN BRANDIES", 
														"AMERICAN COCKTAILS", "AMERICAN CORDIALS & LIQUEUR", "AMERICAN DISTILLED SPIRIT SPECIALTY", 
														"AMERICAN DRY GINS", "AMERICAN FLAVORED VODKA", "AMERICAN GRAPE BRANDIES", 
														"AMERICAN SCHNAPPS", "AMERICAN SLOE GINS", "AMERICAN VODKAS", 
														"ANISETTE", "APPLE SCHNAPPS", "APRICOT BRANDIES", "BARBADOS RUM", 
														"BLACKBERRY BRANDIES", "BLENDED WHISKIES", "BOTTLED IN BOND BOURBON", 
														"BUTTERSCOTCH SCHNAPPS", "CANADIAN WHISKIES", "CHERRY BRANDIES", 
														"CINNAMON SCHNAPPS", "COCKTAILS /RTD", "COFFEE LIQUEURS", "CORN WHISKIES", 
														"CREAM LIQUEURS", "CREME DE ALMOND", "DARK CREME DE CACAO", "DECANTERS & SPECIALTY PACKAGES", 
														"DISTILLED SPIRITS SPECIALTY", "FLAVORED GIN", "FLAVORED GINS", 
														"FLAVORED RUM", "GOLD RUM", "GRAPE SCHNAPPS", "GREEN CREME DE MENTHE", 
														"IMPORTED AMARETTO", "IMPORTED BRANDIES", "IMPORTED CORDIALS & LIQUEURS", 
														"IMPORTED DISTILLED SPIRIT SPECIALTY", "IMPORTED DRY GINS", "IMPORTED FLAVORED VODKA", 
														"IMPORTED GRAPE BRANDIES", "IMPORTED SCHNAPPS", "IMPORTED VODKA", 
														"IMPORTED VODKA - MISC", "IMPORTED VODKAS", "IOWA DISTILLERIES", 
														"IRISH WHISKIES", "JAMAICA RUM", "MEZCAL", "MISC. AMERICAN CORDIALS & LIQUEURS", 
														"MISC. IMPORTED CORDIALS & LIQUEURS", "MISCELLANEOUS  BRANDIES", 
														"MISCELLANEOUS SCHNAPPS", "MIXTO TEQUILA", "NEUTRAL GRAIN SPIRITS", 
														"NEUTRAL GRAIN SPIRITS FLAVORED", "OTHER PROOF VODKA", "PEACH BRANDIES", 
														"PEACH SCHNAPPS", "PEPPERMINT SCHNAPPS", "PUERTO RICO & VIRGIN ISLANDS RUM", 
														"RASPBERRY SCHNAPPS", "ROCK & RYE", "ROOT BEER SCHNAPPS", "SCOTCH WHISKIES", 
														"SINGLE BARREL BOURBON WHISKIES", "SINGLE MALT SCOTCH", "SPEARMINT SCHNAPPS", 
														"SPECIAL ORDER ITEMS", "SPICED RUM", "STRAIGHT BOURBON WHISKIES", 
														"STRAIGHT RYE WHISKIES", "STRAWBERRY SCHNAPPS", "TEMPORARY & SPECIALTY PACKAGES", 
														"TENNESSEE WHISKIES", "TEQUILA", "TRIPLE SEC", "TROPICAL FRUIT SCHNAPPS", 
														"VODKA 80 PROOF", "VODKA FLAVORED", "WATERMELON SCHNAPPS", "WHISKEY LIQUEUR", 
														"WHITE CREME DE CACAO", "WHITE CREME DE MENTHE", "WHITE RUM"), 
							 supercategory = c("Vodka", "Tequilla", "Rum", "General Alcohol", 
							 									"Liqueur", "Brandy", "Cocktails", "Liqueur", "Distilled Spirit Specialty", 
							 									"Gin", "Vodka", "Brandy", "Schnapps", "Gin", "Vodka", "Liqueur", 
							 									"Schnapps", "Brandy", "Rum", "Brandy", "Whiskey", "Bourbon", 
							 									"Schnapps", "Whiskey", "Brandy", "Schnapps", "Cocktails", 
							 									"Liqueur", "Whiskey", "Liqueur", "Liqueur", "Liqueur", "Specialty", 
							 									"Specialty", "Gin", "Gin", "Rum", "Rum", "Schnapps", "Liqueur", 
							 									"Liqueur", "Brandy", "Liqueur", "Specialty", "Gin", "Vodka", 
							 									"Brandy", "Schnapps", "Vodka", "Vodka", "Vodka", "General Alcohol", 
							 									"Whiskey", "Rum", "Tequilla", "Liqueur", "Liqueur", "Brandy", 
							 									"Schnapps", "Tequilla", "Grain Alcohol", "Grain Alcohol", 
							 									"Vodka", "Brandy", "Schnapps", "Schnapps", "Rum", "Schnapps", 
							 									"Bourbon", "Schnapps", "Whiskey", "Whiskey", "Scotch", "Schnapps", 
							 									"General Alcohol", "Rum", "Whiskey", "Whiskey", "Schnapps", 
							 									"General Alcohol", "Whiskey", "Tequilla", "Triple Sec", "Schnapps", 
							 									"Vodka", "Vodka", "Schnapps", "Liqueur", "Liqueur", "Liqueur", "Rum")),
						row.names = c(NA, -91L), class = c("tbl_df", "tbl", "data.frame"))

super_category_cleanup_table <- tibble(item_description = 
      c("Evan Williams Honey","Wild Turkey Longbranch",
                            "Evan Williams Honey","Jim Beam White Triage",
                            "Woodford Reserve Kentucky Derby 2015",
                            "Iowa Legendary Rye Black Label","Midnight Moon Apple Pie",
                            "Midnight Moon Strawberry","Midnight Moon Cherry",
                            "Midnight Moon Blackberry","Midnight Moon Original",
                            "Midnight Moon Blueberry","Tim Smith's Climax Moonshine",
                            "Ole Smoky Cherry Moonshine","Ole Smoky Apple Pie Moonshine",
                            "Sir Winston Peach","Saints N Sinners Apple Pie",
                            "Ole Smoky Blackberry Moonshine","Tim Smith's Climax Moonshine",
                            "Firefly Apple Pie Moonshine","Firefly Peach Moonshine",
                            "Ole Smoky Original Corn Moonshine","American Born Moonshine Dixie",
                            "American Born Moonshine Original",
                            "Ole Smoky Harley Davidson Road House Charred Moonshine",
                            "Firefly Caramel Moonshine",
                            "Ole Smoky Tennesse Moonshine- White Lightnin","Jagermeister w/Flask",
                            "Jagermeister Liqueur Pewter Shooter","Jagermeister T-Shirt Pack",
                            "Juarez Gold Dss","Juarez Gold Dss","Tortilla Gold Dss",
                            "Tortilla Gold Dss","Tortilla Gold DSS","Juarez Gold DSS",
                            "Montezuma Blue","Montezuma Blue","Patron Silver Special Tin",
                            "Patron Rainbow Pack","Absolut Electrik w/2 Glasses",
                            "Absolut w/ Zing Zang Bloody Mary Mix","Absolut w/Cranberry Juice",
                            "Boru Irish Vodka","Jack Daniels Music Carton w/2 Glasses",
                            "Jameson w/Shot Glass",
                            "Crown Royal Peach", "Members Mark Vodka", "Everclear Alcohol",
                            "Smirnoff Red, White & Berry", "Member's Mark Vodka",
                            "Captain Morgan Orange Vanilla Twist", "Members Mark Spiced Rum",
                            "Absolut 80prf w/50ml Absolut Lime", "Crown Royal Deluxe w/50ml Vanilla & 50ml Apple",
                            "Templeton 6YR Rye",
                            "Hennessy VS w/Hennessy Black 50ml", "Jim Beam Operation Homefront",
                            "Patron Reposado Citronage Summer Pack",
                            "Midnight Moon Apple Pie w/Pourer & 2 Shot Glasses",
                            "Skrewball Peanut Butter Flavored Whiskey", "Hennessy VS Flask",
                            "Crown Royal w/Alternate Bag", "Ciroc Summer Watermelon",
                            "Old Ezra 7YR", "Captain Morgan Watermelon Smash", "Ciroc Summer Colada",
                            "Buffalo Trace Bourbon buy the Barrel", "Ciroc Black Raspberry",
                            "Member's Mark Spiced Rum", "Captain Morgan w/50ml Cannonblast & Loconut",
                            "Hornitos Reposado w/ Hornitos Plata 200ml", "Johnnie Walker Black Glass Pack",
                            "Johnnie Walker White Walker", "Jameson w/2-50mls",
                            "Smirnoff Red, White & Berry Mini", "Firefly Strawberry Moonshine",
                            "Jack Daniel's w/2 Glasses", "Rittenhouse Rye Whiskey",
                            "SOOH Breckenridge Bourbon", "HA Wild Turkey Masters Keep Decades",
                            "Sauza Silver w/Margarita Mix", "Malibu USA", "Jane Walker by Johnnie Walker",
                            "Crown Royal Tri Pack", "Jack Daniels w/2 Glasses",
                            "SOOH Chartreuse Green French Liqueur", "Jagermeister Green Glass Shot Set",
                            "Smirnoff 1.75L PET w/2-50ml Smirnoff Peppermint",
                            "Captain Morgan Watermelon Smash Mini", "Woodford Reserve Kentucky Derby 2020",
        "Bookers Beaten Biscuits", "Jose Cuervo Gold w/1L Classic Margarita Mix",
        "Patron Anejo Barrel Select Program", "HA Buffalo Trace Bourbon Cream"),
       supercategory2 = c("Bourbon","Bourbon","Bourbon","Bourbon","Bourbon","Bourbon",
                          "Grain Alcohol","Grain Alcohol","Grain Alcohol","Grain Alcohol",
                          "Grain Alcohol","Grain Alcohol","Grain Alcohol","Grain Alcohol",
                          "Grain Alcohol","Grain Alcohol","Grain Alcohol","Grain Alcohol",
                          "Grain Alcohol","Grain Alcohol","Grain Alcohol","Grain Alcohol",
                          "Grain Alcohol","Grain Alcohol","Grain Alcohol","Grain Alcohol",
                          "Grain Alcohol","Liqueur","Liqueur","Liqueur","Tequilla",
                          "Tequilla","Tequilla","Tequilla","Tequilla","Tequilla","Tequilla",
                          "Tequilla","Tequilla","Tequilla","Vodka","Vodka","Vodka","Vodka",
                          "Whiskey","Whiskey",
                          "Whiskey", "Vodka", "Grain Alcohol", "Vodka", "Vodka", "Rum",
                          "Rum", "Vodka", "Whiskey", "Whiskey",
                          "Brandy", "Bourbon", "Tequilla", "Grain Alcohol",
                          "Whiskey", "Brandy", "Whiskey", "Vodka", "Bourbon",
                          "Rum", "Vodka", "Bourbon", "Vodka", "Rum", "Rum", "Tequilla",
                          "Whiskey", "Whiskey", "Whiskey", "Vodka", "Grain Alcohol",
                          "Whiskey", "Whiskey", "Bourbon", "Bourbon", "Tequilla",
                          "Rum", "Whiskey", "Whiskey", "Whiskey", "Liqueur",
                          "Liqueur", "Vodka", "Rum", "Bourbon", "Bourbon", "Tequilla",
                          "Tequilla", "Bourbon")) %>% 
  distinct()

tic("Processing data")
iowa <- iowa_raw %>% 
	na.omit() %>% 
	filter(year(date) > 2018,
	       month(date) %in% 3:4,
				 category != "SPECIAL ORDER ITEMS",
				 category != "TEMPORARY & SPECIALTY PACKAGES") %>%
	mutate(category = str_to_upper(category)) %>% 
	inner_join(super_category_table, by = "category") %>% 
  left_join(super_category_cleanup_table, by = "item_description") %>% 
  mutate(supercategory = ifelse(is.na(supercategory2), supercategory, supercategory2)) %>% 
	mutate(year2020 = ifelse(year(date) == 2020, "current", "historical")) %>% 
	group_by(year2020, supercategory) %>% 
	summarise(n = n(), sale_dollars = sum(sale_dollars)) %>% 
	ungroup() %>% 
	filter(n > 99) %>% 
	group_by(year2020) %>% 
	mutate(total_sales = sum(sale_dollars)) %>% 
	ungroup() %>% 
	mutate(percent_sales = sale_dollars / total_sales) %>% 
  filter(supercategory != "General Alcohol",
         supercategory != "Specialty",
         supercategory != "Distilled Spirit Specialty")
toc()

# iowa %>%
# 	select(year2020, supercategory, percent_sales) %>%
# 	spread(key = year2020, value = percent_sales) %>%
# 	na.omit() %>% 
# 	mutate(
# 		percent_delta = (current - historical) / historical,
# 		point_delta = current - historical
# 	) %>% 
# 	arrange(percent_delta)

iowa <- iowa %>%
  select(year2020, supercategory, sale_dollars) %>%
  spread(key = year2020, value = sale_dollars) %>%
  na.omit() %>% 
  mutate(
    delta = (current - historical) / historical,
    dollar_delta = current - historical
  ) %>% 
  arrange(delta)

iowa_plot <- ggplot(iowa, aes(x = reorder(supercategory,-delta), y = delta)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "What are we drinking in quarantine?",
    subtitle = "Iowa liquor stores saw an increase in sales,\nwith grain alcohol showing the largest boost.",
    x = NULL,
    y = "Change in money spent in given category\nMarch/April 2019 vs 2020",
    caption = "Created by TrueBirch using data from Iowa Department of Commerce"
  ) + 
  theme(
    plot.subtitle = element_text(size = 21,
                                 hjust = 0.5),
    plot.caption = element_text(size = 15),
    panel.grid.major = element_line(colour = "gray95"),
    panel.grid.minor = element_line(colour = NA),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 37,
                              hjust = 0.5),
    panel.background = element_rect(fill = NA)
  ) + labs(
    subtitle = "Iowa liquor stores have seen a big increase in year-over-year
sales, with grain alcohol experiencing the largest boost.",
    caption = "
Created by TrueBirch using data from Iowa Department of Commerce.
Small categories omitted."
  )

plot(iowa_plot)

svglite(file = "iowa_plot.svg", width = 14, height = 12)
iowa_plot
dev.off()
