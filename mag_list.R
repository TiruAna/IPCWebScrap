mag_list <- list(cora = list(nume = "cora",
                             url_mag = "https://www.cora.ro", 
                             navigation = list(
                               css_lvl1 = c("div.header-nav-open.navbar-collapse > ul >li > a"),
                               css_lvl2 = c("div.accordion-inner >  ul >li > a"),
                               css_lvl3 = c("div.product-list-info > a", next_page = NA)
                             ),
                             extraction = list(nume = "span.title", pret = "div.details-common > div.product-price", pret_cant = "p.unit", categorie = "ol.breadcrumb > li.active > a", descriere = "div.product-text")
),

carrefour = list(nume = "carrefour",
                 url_mag = "https://carrefour.ro/supermarket-online/",
                 navigation = list(
                   css_lvl1 = c("a.category-link"),
                   css_lvl2 = c("div.filter-options-content > ol > li > a", next_page = "a.action.next"),
                   css_lvl3 = c("ol.products.list.items > li > div > a", next_page = NA)
                 ),
                 extraction = list(nume = "span.base", pret = "div.product-info-price", pret_cant = NA, categorie = "div.breadcrumbs > ul > li:nth-last-child(2)" ,descriere = "div.value")
)

# h_m = list(nume = "hm",
#            url_mag = "https://www2.hm.com/ro_ro/index.html",
#            navigation = list(
#              css_lvl1 = c("a.menu__super-link", categorie = "a.menu__super-link > span"),
#              css_lvl2 = c("ul.menu > li.item > a", categorie = "h1.heading"),
#              css_lvl3 = c( "a.item-link",  next_page = "button.button.js-load-more")
#            ),
#            extraction = list(nume = "h1.product-item-headline", pret = "span.price-value", pret_cant = NA, categorie = NA ,descriere = "div.content.pdp-text")
# )

)

write_json(mag_list, "mag_list.json")
