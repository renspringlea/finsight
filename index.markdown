---
layout: home
---

This website presents, in a format convenient for fish welfare advocates, key industry and economic data relating to fish farming in Europe. Data is automatically updated monthly.

# Production
![map_production.png]({{ site.url }}assets/images/map_production.png)  

<table>
  {% for row in site.data.prodtable %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}

    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>

# Cleaner fish
Cleaner fish, which are fish used to control lice on salmon farms, are not included in statistics of production. However, there are a few published estimates for key countries.  
1. Norway: **34 million** cleaner fish used in 2023 ([Sommerset et al., 2024](https://www.vetinst.no/rapporter-og-publikasjoner/rapporter/2024/fiskehelserapporten-2023)). About half of these originate from cleaner fish farms and about half originate from wild-catch fisheries ([Treasurer, 2018](https://www.cabidigitallibrary.org/doi/book/10.1079/9781800629066.0000)). The species are lumpfish (56%), goldsinny wrasse (18%), corkwing wrasse (17%), Ballan wrasse (5%), and other wrasse (4%) ([Barrett et al., 2020](https://doi.org/10.1016/j.ijpara.2019.12.005)).
2. UK: **4.8 million** cleaner fish used in 2018 (Treasurer, 2018). About two-thirds of these are from cleaner fish farms and about one-third from wild-catch fisheries. The species are lumpfish (4.3 million) and Ballan wrasse (500,000).

# Trade
![map_trade_flow.png]({{ site.url }}assets/images/map_trade_flow.png)  

# Retail prices

<table>
  {% for row in site.data.eu_retail_aggregate %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}

    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>

We have found four additional sources of price information that may be of interest.  
1. Annual Norway ex-farm (producer) price data can be downloaded from the [Norwegian Directorate of Fisheries](https://www.fiskeridir.no/English/Aquaculture/Statistics/Atlantic-salmon-and-rainbow-trout/grow-out-production)
2. Quartlerly UK (Scottish) price data can be download from [Salmon Scotland](https://www.salmonscotland.co.uk/reports) (see "Economic Quarterly" reports)
3. UK retail prices from the Seafish report ["Farmed seafood in multiple retail"](https://www.seafish.org/insight-and-research/seafood-retail-data-and-insight/) reports average prices for March 2024 in GBP/kg: salmon = 20.53; warm water prawns = 17.18; sea bass = 16.25; sea bream = 15.23; shrimps = 16.19; tilapia 10.75.  
4. Türkiye producer prices from Turkish Statistical Institute database [Fishery Statistics](https://biruni.tuik.gov.tr/medas/?kn=97&locale=en) (Price of aquaculture products). However, due to the ongoing Turkish currency crisis, producer prices are experiencing drastic changes that are challenging to interpret in a meaningful way (e.g. distinguishing between currency value changes and the change in value of specific fish species).  


# Countries
